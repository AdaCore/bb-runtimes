------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.Arm_V7ar; use Interfaces.Arm_V7ar;

package body Video is
   Buf : Video_Struct := (Phys_Width => 640,
                          Phys_Height => 480,
                          Virt_Width => 640,
                          Virt_Height => 480,
                          Pitch => 0,
                          Depth => 32,
                          X_Off => 0,
                          Y_Off => 0,
                          Fb_Addr => 0,
                          Fb_Size => 0);
   pragma Volatile (Buf);
   for Buf'Alignment use 16;

   Channel_Frame_Buffer : constant Unsigned_32 := 1;

   Mail_Base_Addr : constant := 16#3f00_b880#;

   Mail_Read_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#00#),
     Volatile, Import;
   Mail_Status_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#18#),
     Volatile, Import;
   Mail_Write_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#20#),
     Volatile, Import;

   --  For status:
   Mail_Empty : constant Unsigned_32 := 16#4000_0000#;  -- Cannot read
   Mail_Full  : constant Unsigned_32 := 16#8000_0000#;  -- Cannot write

   subtype String8 is String (1 .. 8);

   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   function Image8 (V : Unsigned_32) return String8 is
      Res : String8;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;
      return Res;
   end Image8;

   procedure Mailbox_Write (Val : Unsigned_32; Channel : Unsigned_32) is
   begin
      if False then
         Put ("MB: write ");
         Put (Image8 (Val));
         New_Line;
      end if;

      while (Mail_Status_Reg and Mail_Full) /= 0 loop
         null;
      end loop;
      Mail_Write_Reg := Val or Channel;
   end Mailbox_Write;

   function Mailbox_Read (Channel : Unsigned_32) return Unsigned_32 is
      Res : Unsigned_32;
   begin
      loop
         while (Mail_Status_Reg and Mail_Empty) /= 0 loop
            null;
         end loop;
         Res := Mail_Read_Reg;
         if (Res and 16#0f#) = Channel then
            return Res;
         end if;
      end loop;
   end Mailbox_Read;

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_32);

   function To_Frame_Buffer_Acc is new Ada.Unchecked_Conversion
     (Unsigned_32, Frame_Buffer_Acc);

   procedure Init_Video
   is
      use System.Machine_Code;
      Res : Unsigned_32;
   begin
      Asm ("dsb", Volatile => True);

      Put_Line ("Init video");
      loop
         --  Clean and invalidate so that GPU can read it
         ARM_V7AR.Cache.Dcache_Flush_By_Range (Buf'Address, Buf'Size / 8);

         Mailbox_Write (To_Unsigned_32 (Buf'Address) or 16#4000_0000#,
                        Channel_Frame_Buffer);
         Res := Mailbox_Read (Channel_Frame_Buffer);
         exit when Buf.Fb_Addr /= 0;
      end loop;

      Put ("FB address: ");
      Put_Line (Image8 (Buf.Fb_Addr));
      Put ("FB size: ");
      Put_Line (Image8 (Buf.Fb_Size));

      Fb := To_Frame_Buffer_Acc
        ((Buf.Fb_Addr and 16#3fff_ffff#) or 16#8000_0000#);
   end Init_Video;
end Video;
