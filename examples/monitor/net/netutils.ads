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

with Interfaces; use Interfaces;

package Netutils is
   type Eth_Addr is array (1 .. 6) of Unsigned_8;
   --  Ethernet address

   Null_Eth_Addr : constant Eth_Addr := (others => 0);
   Broadcast_Eth_Addr : constant Eth_Addr := (others => 16#ff#);

   subtype In_Addr is Unsigned_32;
   --  Ip address (as an unsigned number)

   Null_Ip_Addr : constant In_Addr := 0;

   type Ip_Addr is array (1 .. 4) of Unsigned_8;
   --  IP address (as array of bytes)

   function To_In_Addr (Addr : Ip_Addr) return In_Addr;
   function To_Ip_Addr (Addr : In_Addr) return Ip_Addr;
   --  Conversion routines

   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Put_Hex (V : Unsigned_8);
   procedure Put_Hex (V : Unsigned_16);
   procedure Put_Hex (V : Unsigned_32);
   procedure Put_Dec (V : Unsigned_8);
   procedure Put_Dec (V : Unsigned_16);
   --  Routines to display a number

   procedure Disp_Ip_Addr (Addr : Ip_Addr);
   procedure Disp_Ip_Addr (Addr : In_Addr);
   procedure Disp_Eth_Addr (Addr : Eth_Addr);
   --  Routines to display an address

   type Netbuf is array (Natural range <>) of Unsigned_8;

   Packet : Netbuf (0 .. 1560 - 1);
   for Packet'Alignment use 16;
   --  Packet to be sent or packet just received

   Packet_Len : Natural;
   --  Length of the packet
   Packet_Off : Natural := 0;
   --  Next byte to be read/written in the packet

   Eth_Type_Arp : constant := 16#806#;
   Eth_Type_Ip : constant := 16#800#;
   --  Some well known ethernet types

   function Read_BE1 (Off : Natural) return Unsigned_8;
   function Read_BE2 (Off : Natural) return Unsigned_16;
   function Read_BE4 (Off : Natural) return Unsigned_32;
   function Read_Eth (Off : Natural) return Eth_Addr;
   --  Extract a value from the current ethernet packet

   procedure Write_Eth (Addr : Eth_Addr);
   procedure Write_BE1 (V : Unsigned_8);
   procedure Write_BE2 (V : Unsigned_16);
   procedure Write_BE4 (V : Unsigned_32);
   --  Write a value in the packet

   procedure Write_BE1 (V : Unsigned_8; Off : Natural);
   procedure Write_BE2 (V : Unsigned_16; Off : Natural);
   procedure Write_2 (V : Unsigned_16; Off : Natural);
   procedure Write_BE4 (V : Unsigned_32; Off : Natural);

   Ip_Proto_Udp : constant := 17;
   Ip_Proto_Icmp : constant := 1;
   --  Some well known IP protocols
end Netutils;
