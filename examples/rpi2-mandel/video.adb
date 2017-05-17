------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.Raspberry_Pi; use Interfaces.Raspberry_Pi;
with Interfaces.Cache;
--  with Interfaces.ARM_V7AR; use Interfaces.ARM_V7AR;

package body Video is
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

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_32);

   procedure Mailbox_Write (Val : Unsigned_32; Channel : Unsigned_32) is
   begin
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

   function To_Frame_Buffer_Acc is new Ada.Unchecked_Conversion
     (Unsigned_32, Frame_Buffer_Acc);

   procedure Init_Video
   is
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      type Unsigned_32_Arr is array (Natural range <>) of Unsigned_32;
      Msg : Unsigned_32_Arr := (0,
                                Request_Code,

                                Tag_Allocate_Buffer,
                                8,
                                Request_Indicator,
                                16,
                                0,

                                Tag_Set_Physical_Size,
                                8,
                                Request_Indicator,
                                Width,
                                Height,

                                Tag_Set_Virtual_Size,
                                8,
                                Request_Indicator,
                                Width,
                                Height,

                                Tag_Set_Depth,
                                4,
                                Request_Indicator,
                                32,

                                0);
      for Msg'Alignment use 16;
   begin
      Msg (0) := Msg'Length * 4;

      --  Clean and invalidate so that GPU can read it
      Cache.Dcache_Flush_By_Range (Msg'Address, Msg'Length * 4);

      Mailbox_Write (To_Unsigned_32 (Msg'Address), Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);

      Put_Line ("Answer:");
      for I in Msg'Range loop
         Put_Line (Image8 (Msg (I)));
      end loop;

      Put ("FB address: ");
      Put_Line (Image8 (Msg (5)));
      Put ("FB size: ");
      Put_Line (Image8 (Msg (6)));

      --  Map to uncached address
      Fb := To_Frame_Buffer_Acc ((Msg (5) and 16#3fff_ffff#));
   end Init_Video;
end Video;
