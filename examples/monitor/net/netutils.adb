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

with Ada.Text_IO; use Ada.Text_IO;

package body Netutils is
   function To_In_Addr (Addr : Ip_Addr) return In_Addr is
   begin
      return Shift_Left (Unsigned_32 (Addr (1)), 24)
        or Shift_Left (Unsigned_32 (Addr (2)), 16)
        or Shift_Left (Unsigned_32 (Addr (3)), 8)
        or Shift_Left (Unsigned_32 (Addr (4)), 0);
   end To_In_Addr;

   function To_Ip_Addr (Addr : In_Addr) return Ip_Addr is
      Res : Ip_Addr;
      Tmp : In_Addr := Addr;
   begin
      for I in reverse Res'Range loop
         Res (I) := Unsigned_8 (Tmp and 16#ff#);
         Tmp := Shift_Right (Tmp, 8);
      end loop;
      return Res;
   end To_Ip_Addr;

   procedure Put_Dec (V : Unsigned_8) is
      Res : String (1 .. 3);
      P : Natural := Res'Last;
      Val : Unsigned_8 := V;
   begin
      loop
         Res (P) := Character'Val (Character'Pos ('0') + Val mod 10);
         Val := Val / 10;
         exit when Val = 0;
         P := P - 1;
      end loop;
      Put (Res (P .. Res'Last));
   end Put_Dec;

   procedure Put_Dec (V : Unsigned_16) is
      Res : String (1 .. 5);
      P : Natural := Res'Last;
      Val : Unsigned_16 := V;
   begin
      loop
         Res (P) := Character'Val (Character'Pos ('0') + Val mod 10);
         Val := Val / 10;
         exit when Val = 0;
         P := P - 1;
      end loop;
      Put (Res (P .. Res'Last));
   end Put_Dec;

   procedure Put_Hex (V : Unsigned_8) is
   begin
      pragma Warnings (Off, "loop range may be null");
      for I in reverse 0 .. 1 loop
         Put (Hex_Digits (Natural (Shift_Right (V, 4 * I) and 15)));
      end loop;
      pragma Warnings (On, "loop range may be null");
   end Put_Hex;

   procedure Put_Hex (V : Unsigned_16) is
   begin
      for I in reverse 0 .. 3 loop
         Put (Hex_Digits (Natural (Shift_Right (V, 4 * I) and 15)));
      end loop;
   end Put_Hex;

   procedure Put_Hex (V : Unsigned_32) is
   begin
      for I in reverse 0 .. 7 loop
         Put (Hex_Digits (Natural (Shift_Right (V, 4 * I) and 15)));
      end loop;
   end Put_Hex;

   procedure Disp_Ip_Addr (Addr : Ip_Addr) is
   begin
      for I in 1 .. 3 loop
         Put_Dec (Addr (I));
         Put ('.');
      end loop;
      Put_Dec (Addr (4));
   end Disp_Ip_Addr;

   procedure Disp_Ip_Addr (Addr : In_Addr) is
   begin
      Disp_Ip_Addr (To_Ip_Addr (Addr));
   end Disp_Ip_Addr;

   procedure Disp_Eth_Addr (Addr : Eth_Addr) is
   begin
      for I in Addr'Range loop
         Put_Hex (Addr (I));
         exit when I = Addr'Last;
         Put (':');
      end loop;
   end Disp_Eth_Addr;

   function Read_BE1 (Off : Natural) return Unsigned_8 is
   begin
      return Packet (Off);
   end Read_BE1;

   function Read_BE2 (Off : Natural) return Unsigned_16 is
   begin
      return Shift_Left (Unsigned_16 (Packet (Off)), 8)
        or Unsigned_16 (Packet (Off + 1));
   end Read_BE2;

   function Read_BE4 (Off : Natural) return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32 (Packet (Off)), 24)
        or Shift_Left (Unsigned_32 (Packet (Off + 1)), 16)
        or Shift_Left (Unsigned_32 (Packet (Off + 2)), 8)
        or Shift_Left (Unsigned_32 (Packet (Off + 3)), 0);
   end Read_BE4;

   function Read_Eth (Off : Natural) return Eth_Addr is
   begin
      return Eth_Addr (Packet (Off .. Off + 5));
   end Read_Eth;

   procedure Write_Eth (Addr : Eth_Addr) is
   begin
      Packet (Packet_Off .. Packet_Off + Addr'Length - 1) := Netbuf (Addr);
      Packet_Off := Packet_Off + Addr'Length;
   end Write_Eth;

   procedure Write_BE1 (V : Unsigned_8; Off : Natural) is
   begin
      Packet (Off) := V;
   end Write_BE1;

   procedure Write_BE2 (V : Unsigned_16; Off : Natural) is
   begin
      Packet (Off + 0) := Unsigned_8 (Shift_Right (V, 8) and 16#ff#);
      Packet (Off + 1) := Unsigned_8 (Shift_Right (V, 0) and 16#ff#);
   end Write_BE2;

   procedure Write_2 (V : Unsigned_16; Off : Natural) is
      pragma Warnings (Off);
      Tmp : Unsigned_16;
      for Tmp'Address use Packet (Off)'Address;
      pragma Warnings (On);
   begin
      Tmp := V;
   end Write_2;

   procedure Write_BE4 (V : Unsigned_32; Off : Natural) is
   begin
      Packet (Off + 0) := Unsigned_8 (Shift_Right (V, 24) and 16#ff#);
      Packet (Off + 1) := Unsigned_8 (Shift_Right (V, 16) and 16#ff#);
      Packet (Off + 2) := Unsigned_8 (Shift_Right (V, 8) and 16#ff#);
      Packet (Off + 3) := Unsigned_8 (Shift_Right (V, 0) and 16#ff#);
   end Write_BE4;

   procedure Write_BE1 (V : Unsigned_8) is
   begin
      Write_BE1 (V, Packet_Off);
      Packet_Off := Packet_Off + 1;
   end Write_BE1;

   procedure Write_BE2 (V : Unsigned_16) is
   begin
      Write_BE2 (V, Packet_Off);
      Packet_Off := Packet_Off + 2;
   end Write_BE2;

   procedure Write_BE4 (V : Unsigned_32) is
   begin
      Write_BE4 (V, Packet_Off);
      Packet_Off := Packet_Off + 4;
   end Write_BE4;
end Netutils;
