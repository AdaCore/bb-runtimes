------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Term; use Term;
with Console;
with Memwrite; use Memwrite;

package body Srec is
   Has_Error : Boolean;

   procedure Error (Msg : String) is
   begin
      Has_Error := True;
      Console.Put_Line (Msg);
   end Error;

   procedure Read_Srec_Line is
      C : Character;
   begin
      --  Read a line
      Line_Len := 0;
      loop
         Console.Get (C);
         case C is
            when ASCII.LF =>
               exit;
            when ASCII.CR =>
               null;
            when others =>
               if Line_Len = Line'Last then
                  Error ("Line too long");
                  Console.Put ("Line: ");
                  Console.Put_Line (Line);
                  return;
               end if;
               Line_Len := Line_Len + 1;
               Line (Line_Len) := C;
         end case;
      end loop;
   end Read_Srec_Line;

   function Read_Hex (Pos : Line_Range) return Unsigned_8 is
      H, L : Hex_Digit_Type;
   begin
      H := Read_Hex_Digit (Pos);
      L := Read_Hex_Digit (Pos + 1);
      if H = Bad_Hex or else L = Bad_Hex then
         Error ("Bad hexa character");
         return 0;
      else
         return Unsigned_8 (H * 16 + L);
      end if;
   end Read_Hex;

   procedure Read_Srec is
      Reclen : Unsigned_8;
      Chksum : Unsigned_8;
      P : Line_Range;
      Addr : Unsigned_32;
      B : Storage_Array (1 .. 1);
   begin
      Memwrite.Init;
      Has_Error := False;
      loop
         --  Read a line
         Read_Srec_Line;
         if Has_Error then
            return;
         end if;

         --  Check S-rec
         if Line_Len = 0 then
            --  Skip empty line (to easily deal with CR+LF).
            null;
         elsif Line_Len < 4
           or else Line (1) /= 'S' or else Line (2) not in '0' .. '9'
         then
            Error ("Not an s-record");
            return;
         else
            --  Check line length.
            Reclen := Read_Hex (3);
            if Line_Len /= Natural (Reclen) * 2 + 4 then
               Error ("bad s-rec line length");
            end if;
            if Has_Error then
               --  Either from read_hex or previous message
               return;
            end if;

            --  Check integrity
            Chksum := 0;
            P := 3;
            while P < Line_Len loop
               Chksum := Chksum + Read_Hex (P);
               P := P + 2;
            end loop;
            if Chksum /= 16#ff# then
               Error ("Bad checksum");
            end if;
            if Has_Error then
               --  Either from read_hex or previous message
               return;
            end if;
            Reclen := Reclen - 1;

            --  Handle record
            case Line (2) is
               when '0' =>
                  --  Block header are ignored
                  null;
               when '5' =>
                  --  Record count is ignored
                  null;
               when '1' | '2' | '8' | '9' =>
                  Error ("Only 4-bytes addresses handled");
                  return;
               when '3' | '7' =>
                  --  Decode address
                  if Reclen < 4 then
                     Error ("Packet too short");
                     return;
                  end if;
                  Addr := 0;
                  for I in 1 .. 4 loop
                     Addr := Addr * 256 + Unsigned_32 (Read_Hex (3 + 2 * I));
                     --  Read_Hex already checked by checksum.
                  end loop;

                  if Line (2) = '7' then
                     --  Start address
                     if Reclen /= 4 then
                        Error ("Garbage for S7 record");
                        return;
                     end if;

                     Memwrite.Flush;
                     Memwrite.Exec_Addr := Addr;
                     Console.Put ('.');
                     return;
                  else
                     for I in 5 .. Reclen loop
                        B (1) :=
                          Storage_Element (Read_Hex (3 + Natural (2 * I)));
                        Memwrite.Write (Addr, B);
                        Addr := Addr + 1;
                     end loop;
                  end if;
               when others =>
                  --  Won't happen
                  null;
            end case;
            Console.Put ('+');
         end if;
      end loop;
   end Read_Srec;

end Srec;
