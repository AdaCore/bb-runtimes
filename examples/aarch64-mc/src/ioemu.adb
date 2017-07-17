------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body IOEmu is
   function Read8  (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_8
   is
      Idx : constant Off_T := Off mod 4;
      V : Unsigned_32;
   begin
      V := IOEmu_Dev32'Class (Dev).Read32 (Off - Idx);
      V := Shift_Right (V, Natural (Idx * 8));
      return Unsigned_8 (V and 16#ff#);
   end Read8;

   function Read16 (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_16
   is
      Idx : constant Off_T := Off mod 4;
      V : Unsigned_32;
   begin
      V := IOEmu_Dev32'Class (Dev).Read32 (Off - Idx);
      V := Shift_Right (V, Natural (Idx * 8));
      return Unsigned_16 (V and 16#ffff#);
   end Read16;

   function Read64 (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_64
   is
      L, H : Unsigned_32;
   begin
      L := IOEmu_Dev32'Class (Dev).Read32 (Off);
      H := IOEmu_Dev32'Class (Dev).Read32 (Off + 4);
      return Unsigned_64 (L) + Shift_Left (Unsigned_64 (H), 32);
   end Read64;

   procedure Write8
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_8)
   is
      Idx : constant Off_T := Off mod 4;
   begin
      IOEmu_Dev32'Class (Dev).Write32_Mask
        (Off - Idx,
         Shift_Left (Unsigned_32 (Val), Natural (Idx * 8)),
         Shift_Left (16#ff#, Natural (Idx * 8)));
   end Write8;

   procedure Write16
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_16)
   is
      Idx : constant Off_T := Off mod 4;
   begin
      IOEmu_Dev32'Class (Dev).Write32_Mask
        (Off - Idx,
         Shift_Left (Unsigned_32 (Val), Natural (Idx * 8)),
         Shift_Left (16#ffff#, Natural (Idx * 8)));
   end Write16;

   procedure Write32
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_32) is
   begin
      IOEmu_Dev32'Class (Dev).Write32_Mask (Off, Val, 16#ffff_ffff#);
   end Write32;

   procedure Write64
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_64) is
   begin
      IOEmu_Dev32'Class (Dev).Write32_Mask
        (Off, Unsigned_32 (Val and 16#ffff_ffff#), 16#ffff_ffff#);
      IOEmu_Dev32'Class (Dev).Write32_Mask
        (Off + 4, Unsigned_32 (Shift_Right (Val, 32) and 16#ffff_ffff#),
         16#ffff_ffff#);
   end Write64;

   procedure Update
     (Reg : in out Unsigned_32; Val : Unsigned_32; Mask : Unsigned_32) is
   begin
      Reg := Val or (Reg and not Mask);
   end Update;

   procedure Set_Enable (Reg : in out Unsigned_32; Val : Unsigned_32) is
   begin
      Reg := Reg or Val;
   end Set_Enable;

   procedure Set_Disable (Reg : in out Unsigned_32; Val : Unsigned_32) is
   begin
      Reg := Reg and (not Val);
   end Set_Disable;

   procedure Find_IO
     (Map : IOEmu_Map_Array;
      Addr : Address;
      Dev : out IOEmu_Dev_Acc;
      Off : out Off_T)
   is
   begin
      for I in Map'Range loop
         declare
            E : IOEmu_Map_Entry renames Map (I);
         begin
            if Addr >= E.Base and then Addr < E.Base + E.Len then
               Dev := E.Dev;
               Off := Addr - E.Base;
               return;
            end if;
         end;
      end loop;

      Dev := null;
      Off := 0;
   end Find_IO;

end IOEmu;
