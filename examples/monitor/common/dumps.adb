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

package body Dumps is
   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   function Hex8 (V : Unsigned_64) return String16 is
      Res : String16;
   begin
      for I in Res'Range loop
         Res (I) :=
           Hex_Digits (Natural (Shift_Right (V, 4 * (16 - I)) and 15));
      end loop;
      return Res;
   end Hex8;

   function Hex4 (V : Unsigned_32) return String8 is
      Res : String8;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;
      return Res;
   end Hex4;

   function Hex2 (V : Unsigned_32) return String4 is
      Res : String4;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (4 - I)) and 15));
      end loop;
      return Res;
   end Hex2;

   function Hex1 (V : Unsigned_32) return String2 is
      Res : String2;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (2 - I)) and 15));
      end loop;
      return Res;
   end Hex1;

   function Image1 (V : Unsigned_32) return Character is
   begin
      return  Hex_Digits (Natural (V and 15));
   end Image1;

   procedure Put (V : Natural) is
      Buf : String (1 .. 9);
      P : Natural := Buf'Last;
      V1 : Natural := V;
   begin
      loop
         Buf (P) := Hex_Digits (V1 rem 10);
         V1 := V1 / 10;
         exit when V1 = 0;
         P := P - 1;
      end loop;
      Put (Buf (P .. Buf'Last));
   end Put;

   procedure Put_Bit (Set : Boolean; Name : Character) is
   begin
      if Set then
         Put (Name);
      else
         Put ('-');
      end if;
   end Put_Bit;

   procedure Put_Register_Desc (Val : Unsigned_32; Desc : String)
   is
      Len : Natural;
      Shift : Natural := 32;
      P : Natural := Desc'First;
      V : Unsigned_32;
      C : Character;
      Show_Name : Boolean;
   begin
      loop
         pragma Assert (P < Desc'Last);
         --  Extract length
         Len := 0;
         while P <= Desc'Last loop
            C := Desc (P);
            exit when C not in '0' .. '9';
            Len := Len * 10
              + Character'Pos (Desc (P)) - Character'Pos ('0');
            P := P + 1;
         end loop;
         if Len = 0 then
            --  Special case, we suppose length is missing and defaults to 1.
            --  Display name only if the bit is set.
            Shift := Shift - 1;
            V := Shift_Right (Val, Shift) and 1;
            Show_Name := V = 1;
         else
            pragma Assert (Len in 1 .. 31);
            Shift := Shift - Len;
            V := Shift_Right (Val, Shift);
            V := V and (Shift_Left (1, Len) - 1);

            if P < Desc'Last and then Desc (P) = ':' then
               --  Skip ':'
               P := P + 1;
               Show_Name := True;
            else
               Show_Name := False;
            end if;
         end if;
         --  Display field name
         if Show_Name then
            Put (' ');
         end if;
         while P <= Desc'Last loop
            C := Desc (P);
            P := P + 1;
            exit when C = ',';
            if Show_Name then
               Put (C);
            end if;
         end loop;
         if Len /= 0 and then Show_Name then
            --  Display value
            Put ('=');

            if Len <= 4 then
               Put (Image1 (V));
            elsif Len <= 8 then
               Put (Image2 (V));
            elsif Len <= 16 then
               Put (Image4 (V));
            else
               Put (Image8 (V));
            end if;
         end if;
         exit when P > Desc'Last;
      end loop;
      --  All bits must have be used
      pragma Assert (Shift = 0);
   end Put_Register_Desc;

   procedure Put_Register (Name : String;
                           Val : Unsigned_32;
                           Desc : String)
   is
   begin
      Put (Name);
      Put (": ");
      Put (Image8 (Val));
      Put_Register_Desc (Val, Desc);
      New_Line;
   end Put_Register;
end Dumps;
