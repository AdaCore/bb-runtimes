------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

with Console; use Console;

package body Term is
   function Read_Hex_Digit (Pos : Line_Range) return Hex_Digit_Type is
      C : constant Character := Line (Pos);
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            return Bad_Hex;
      end case;
   end Read_Hex_Digit;

   procedure Get_Line (Prompt : String) is
      C : Character;
      Prev_Len : Natural := Line_Len;
   begin
      Put (Prompt);
      Line_Len := 0;
      loop
         Get (C);
         case C is
            when ' ' .. Character'Val (126) =>
               if Line_Len < Line_Range'Last then
                  Line_Len := Line_Len + 1;
                  Line (Line_Len) := C;
                  Put (C);
               else
                  Put (ASCII.BEL);
               end if;
            when ASCII.CR | ASCII.LF =>
               New_Line;
               exit;
            when Character'Val (21) => -- ^U
               New_Line;
               Put (Prompt);
               Line_Len := 0;
            when Character'Val (8)  --  ^H
              | Character'Val (127) => -- DEL
               if Line_Len = 0 then
                  Put (ASCII.BEL);
               else
                  Put (ASCII.BS);
                  Put (' ');
                  Put (ASCII.BS);
                  Line_Len := Line_Len - 1;
                  Prev_Len := 0;
               end if;
            when Character'Val (16) => -- ^P
               if Line_Len = 0 and then Prev_Len > 0 then
                  --  Recall previous line
                  for I in 1 .. Prev_Len loop
                     Put (Line (I));
                  end loop;
                  Line_Len := Prev_Len;
               else
                  Put (ASCII.BEL);
               end if;
            when others =>
               Put (ASCII.BEL);
         end case;
      end loop;
      Pos := Line_Range'First;
      End_Pos := Line_Range'First - 1;
   end Get_Line;

   procedure Next_Word is
   begin
      --  Next word
      Pos := End_Pos + 1;

      --  Discard spaces
      loop
         if Pos > Line_Len then
            End_Pos := Pos;
            return;
         end if;
         exit when Line (Pos) /= ' ';
         Pos := Pos + 1;
      end loop;

      --  Look for end of the word
      End_Pos := Pos;
      loop
         if End_Pos >= Line_Len then
            End_Pos := Line_Len;
            return;
         end if;
         if Line (End_Pos + 1) = ' ' then
            return;
         end if;
         End_Pos := End_Pos + 1;
      end loop;
   end Next_Word;
end Term;
