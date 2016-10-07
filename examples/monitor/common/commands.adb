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

pragma Ada_2012;

pragma Warnings (Off);
with System.Machine_Reset;
pragma Warnings (On);
with Console; use Console;
with Term; use Term;
with Dumps; use Dumps;

package body Commands is
   First_Cmd : Command_List_Acc;
   Last_Cmd : Command_List_Acc;
   --  Simply linked list of commands.

   procedure Register_Commands (Cmd : Command_List_Acc) is
   begin
      if Last_Cmd = null then
         First_Cmd := Cmd;
      else
         Last_Cmd.Next := Cmd;
      end if;
      Last_Cmd := Cmd;
   end Register_Commands;

   function Command_Name_Eq (Cmd : Command_Type) return Boolean is
      Word_Len : constant Natural := End_Pos - Pos + 1;
   begin
      for I in 1 .. Cmd.Name'Length loop
         if Cmd.Name (I) = ' ' and then I = Word_Len + 1 then
            return True;
         end if;
         if Cmd.Name (I) /= Line (Pos + I - 1) then
            return False;
         end if;
      end loop;
      return Word_Len = Cmd.Name'Length;
   end Command_Name_Eq;

   procedure Handle_Command
   is
      Cmd : Command_List_Acc;
   begin
      Next_Word;
      if Pos > Line_Len then
         return;
      end if;
      Cmd := First_Cmd;
      while Cmd /= null loop
         for I in Cmd.Commands'Range loop
            if Command_Name_Eq (Cmd.Commands (I)) then
               Cmd.Commands (I).Proc.all;
               return;
            end if;
         end loop;
         Cmd := Cmd.Next;
      end loop;
      Put ("unknown command: ");
      Put_Line (Line (Pos .. End_Pos));
      Put_Line ("Try help");
   end Handle_Command;

   procedure Proc_Help is
      Cmds : Command_List_Acc;
   begin
      Put_Line ("List of commands:");
      Cmds := First_Cmd;
      while Cmds /= null loop
         for I in Cmds.Commands'Range loop
            Put_Line (Cmds.Commands (I).Name.all);
         end loop;
         Cmds := Cmds.Next;
      end loop;
   end Proc_Help;

   procedure Proc_Reset is
   begin
      Put ("Reset");
      Put (ASCII.SUB); -- ^Z
      Put_Line ("........");
      System.Machine_Reset.Stop;
   end Proc_Reset;

   procedure Parse_Unsigned32 (Res : out Unsigned_32; Ok : out Boolean)
   is
      H : Hex_Digit_Type;
      C : Character;
   begin
      Res := 0;
      Ok := False;
      if Pos > Line_Len then
         Put_Line ("missing argument");
         return;
      end if;
      if End_Pos > Pos + 1
        and then Line (Pos) = '0'
        and then Line (Pos + 1) = 'x'
      then
         --  Parse as hex.
         for I in Pos + 2 .. End_Pos loop
            H := Read_Hex_Digit (I);
            if H = Bad_Hex then
               Put_Line ("Bad character in hex number");
               return;
            else
               Res := Res * 16 + Unsigned_32 (H);
            end if;
         end loop;
      else
         --  Parse as dec.
         for I in Pos .. End_Pos loop
            Res := Res * 10;
            C := Line (I);
            if C in '0' .. '9' then
               Res := Res + Character'Pos (C) - Character'Pos ('0');
            else
               Put_Line ("Bad character in decimal number");
               return;
            end if;
         end loop;
      end if;
      Ok := True;
   end Parse_Unsigned32;

   procedure Proc_Conv is
      V : Unsigned_32;
      Ok : Boolean;
   begin
      loop
         Next_Word;
         exit when Pos > Line_Len;
         Parse_Unsigned32 (V, Ok);
         exit when not Ok;
         Put_Line (Image8 (V));
      end loop;
   end Proc_Conv;

   procedure Parse_Dump_Args
     (Addr : out Unsigned_32; Len : out Unsigned_32; Ok : out Boolean) is
   begin
      Next_Word;
      Parse_Unsigned32 (Addr, Ok);
      if not Ok then
         return;
      end if;
      Next_Word;
      if Pos > Line_Len then
         Len := 32;
      else
         Parse_Unsigned32 (Len, Ok);
         if not Ok then
            return;
         end if;
      end if;
   end Parse_Dump_Args;

   --  Hexa + char byte dump
   procedure Proc_Dump is
      Addr : Unsigned_32;
      Len : Unsigned_32;
      Eaddr : Unsigned_32;
      Ok : Boolean;
   begin
      Parse_Dump_Args (Addr, Len, Ok);
      if not Ok then
         return;
      end if;

      Eaddr := Addr + Len - 1;
      loop
         Put (Image8 (Addr));
         Put (": ");
         for I in Unsigned_32 range 0 .. 15 loop
            if Addr > Eaddr then
               Put ("  ");
            else
               declare
                  B : Unsigned_8;
                  for B'Address use System'To_Address (Addr);
                  pragma Import (Ada, B);
               begin
                  Put (Image2 (Unsigned_32 (B)));
               end;
            end if;
            if I = 7 then
               Put ('-');
            else
               Put (' ');
            end if;
            Addr := Addr + 1;
         end loop;
         Addr := Addr - 16;
         Put (' ');
         for I in Unsigned_32 range 0 .. 15 loop
            if Addr > Eaddr then
               Put (' ');
            else
               declare
                  C : Character;
                  for C'Address use System'To_Address (Addr);
                  pragma Import (Ada, C);
               begin
                  if C not in ' ' .. '~' then
                     Put ('.');
                  else
                     Put (C);
                  end if;
               end;
            end if;
            Addr := Addr + 1;
         end loop;

         New_Line;
         exit when Addr - 1 >= Eaddr; --  Handle wrap around 0
      end loop;
   end Proc_Dump;

   --  Word dump
   procedure Proc_Dump32 is
      Addr : Unsigned_32;
      Len : Unsigned_32;
      Eaddr : Unsigned_32;
      Ok : Boolean;
   begin
      Parse_Dump_Args (Addr, Len, Ok);
      if not Ok then
         return;
      end if;

      --  Align address
      Addr := Addr and not 3;

      Eaddr := Addr + Len - 1;
      loop
         Put (Image8 (Addr));
         Put (": ");
         for I in Unsigned_32 range 0 .. 3 loop
            if Addr > Eaddr then
               Put ("        ");
            else
               declare
                  W : Unsigned_32;
                  for W'Address use System'To_Address (Addr);
                  pragma Import (Ada, W);
               begin
                  Put (Image8 (W));
               end;
            end if;
            Put (' ');
            Addr := Addr + 4;
         end loop;

         New_Line;
         exit when Addr - 1 >= Eaddr; --  Handle wrap around 0
      end loop;
   end Proc_Dump32;

   procedure Proc_Dump_Srec
   is
      use System;

      Chksum : Unsigned_8;
      procedure Dump_Byte (B : Unsigned_8) is
      begin
         Chksum := Chksum + B;
         Put (Image2 (Unsigned_32 (B)));
      end Dump_Byte;

      Addr : Unsigned_32;
      L : Unsigned_32;
      Ll : Unsigned_32;
      Ok : Boolean;
   begin
      Next_Word;
      Parse_Unsigned32 (Addr, Ok);
      if not Ok then
         return;
      end if;
      Next_Word;
      if Pos > Line_Len then
         L := 32;
      else
         Parse_Unsigned32 (L, Ok);
         if not Ok then
            return;
         end if;
      end if;

      while L > 0 loop
         Ll := Unsigned_32'Min (L, 32);

         Put ("S3");
         Chksum := 0;

         --  Len
         Dump_Byte (Unsigned_8 (Ll + 5));

         --  Address
         Dump_Byte (Unsigned_8 (Shift_Right (Addr, 24) and 16#ff#));
         Dump_Byte (Unsigned_8 (Shift_Right (Addr, 16) and 16#ff#));
         Dump_Byte (Unsigned_8 (Shift_Right (Addr,  8) and 16#ff#));
         Dump_Byte (Unsigned_8 (Shift_Right (Addr,  0) and 16#ff#));

         --  Data
         for I in 1 .. Ll loop
            declare
               B : Unsigned_8
                 with Address => System'To_Address (Addr), Import;
            begin
               Dump_Byte (B);
               Addr := Addr + 1;
            end;
         end loop;

         --  Chksum
         Dump_Byte (not Chksum);
         New_Line;

         L := L - Ll;
      end loop;

      null;
   end Proc_Dump_Srec;

   procedure Proc_Write is
      Addr : Unsigned_32;
      Val : Unsigned_32;
      Ok : Boolean;
   begin
      Next_Word;
      Parse_Unsigned32 (Addr, Ok);
      if not Ok then
         return;
      end if;
      Next_Word;
      Parse_Unsigned32 (Val, Ok);
      if not Ok then
         return;
      end if;
      declare
         W : Unsigned_32;
         for W'Address use System'To_Address (Addr);
         pragma Import (Ada, W);
         pragma Volatile (W);
      begin
         W := Val;
      end;
   end Proc_Write;

   Commands : aliased Command_List :=
     (7,
      ((new String'("help - Print this help"), Proc_Help'Access),
       (new String'("reset - Reboot the board"), Proc_Reset'Access),
       (new String'("conv NUM - Print NUM in hexa"), Proc_Conv'Access),
       (new String'("dump ADDR [LEN] - Hexa byte dump"), Proc_Dump'Access),
       (new String'("dump32 ADDR [LEN] - Hexa word dump"), Proc_Dump32'Access),
       (new String'("dump_srec ADDR [LEN] - SREC dump"),
        Proc_Dump_Srec'Access),
       (new String'("w ADDR VAL - Write a word to memory"),
        Proc_Write'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Commands;
