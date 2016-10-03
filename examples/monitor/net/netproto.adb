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
with Netcfg; use Netcfg;
with Ethdrv; use Ethdrv;
with Clkdrv; use Clkdrv;
with Netutils; use Netutils;

package body Netproto is

   procedure Dump_Pkt (Off : Natural := 0) is
   begin
      --  Display the packet
      Put ("Pkt:");
      for I in Off .. Packet_Len - 1 loop
         Put (' ');
         Put_Hex (Packet (I));
      end loop;
      New_Line;
   end Dump_Pkt;

   function In_My_Network (Ip : In_Addr) return Boolean;
   --  Return true if IP is in my network

   function In_My_Network (Ip : In_Addr) return Boolean is
   begin
      return (Ip and My_Netmask) = (My_Ip_Addr and My_Netmask);
   end In_My_Network;

   --  ARP constants
   Arp_Htype : constant := 1;
   Arp_Ptype : constant := 16#800#;

   --  ARP operation
   Arp_Request : constant := 1;
   Arp_Reply : constant := 2;

   function Handle_Arp return Rcv_Status
   is
      Htype : Unsigned_16;
      Ptype : Unsigned_16;
      Hlen : Unsigned_8;
      Plen : Unsigned_8;
      Oper : Unsigned_16;
   begin
      --  Check length
      if Packet_Len - Packet_Off < 8 then
         return Rcv_Err;
      end if;

      --  Extract header
      Htype := Read_BE2 (Packet_Off);
      Ptype := Read_BE2 (Packet_Off + 2);
      Hlen := Read_BE1 (Packet_Off + 4);
      Plen := Read_BE1 (Packet_Off + 5);
      Oper := Read_BE2 (Packet_Off + 6);
      Packet_Off := Packet_Off + 8;

      --  Sanity check
      if Htype /= Arp_Htype or else Ptype /= Arp_Ptype
        or else Hlen /= 6 or else Plen /= 4
      then
         return Rcv_Err;
      end if;
      if Packet_Len - Packet_Off < 2 * (6 + 4) then
         return Rcv_Err;
      end if;

      case Oper is
         when Arp_Request =>
            if Read_BE4 (Packet_Off + 6 + 4 + 6) = My_Ip_Addr then
               Put_Line ("Got ARP request for me");
               --  Dump_Pkt;
               --  Create a reply
               declare
                  Sender_Ip  : constant In_Addr := Read_BE4 (Packet_Off);
                  Sender_Eth : constant Eth_Addr := Read_Eth (Packet_Off);
               begin
                  Eth_Send_Init;
                  --  Eth frame
                  Write_Eth (Sender_Eth);
                  Write_Eth (My_Eth_Addr);
                  Write_BE2 (Eth_Type_Arp);
                  --  Arp frame
                  Write_BE2 (Arp_Htype);
                  Write_BE2 (Arp_Ptype);
                  Write_BE1 (Eth_Addr'Length);
                  Write_BE1 (Ip_Addr'Length);
                  Write_BE2 (Arp_Reply);
                  Write_Eth (My_Eth_Addr);
                  Write_BE4 (My_Ip_Addr);
                  Write_Eth (Sender_Eth);
                  Write_BE4 (Sender_Ip);

                  Packet_Len := Packet_Off;
                  --  Dump_Pkt;
                  Eth_Send_Packet;
               end;
            end if;
            return Rcv_None;
         when Arp_Reply =>
            if Srv_Ip_Addr = Null_Ip_Addr then
               --  We don't care about ARP unless we have a server address
               return Rcv_Ign;
            end if;

            declare
               Ip : constant In_Addr := Read_BE4 (Packet_Off + 6);
            begin
               --  Put_Line ("Got ARP reply");
               --  Dump_Pkt;
               if Ip = Srv_Ip_Addr
                 or else (Ip = Gw_Ip_Addr
                            and then not In_My_Network (Srv_Ip_Addr))
               then
                  Srv_Eth_Addr := Read_Eth (Packet_Off);
               end if;
            end;
            return Rcv_Arp;
         when others =>
            return Rcv_Err;
      end case;
   end Handle_Arp;

   function Ip_Chksum (Length : Natural;
                       Offset : Natural := Packet_Off;
                       Seed : Unsigned_16 := 0)
                      return Unsigned_16
   is
      Sum : Unsigned_32 := Unsigned_32 (Seed);

      pragma Assert (Offset mod 2 = 0);
      --  Assume the packet is correctly aligned.

      Raw_Buf : array (1 .. Length / 2) of Unsigned_16;
      pragma Warnings (Off);
      for Raw_Buf'Address use Packet (Offset)'Address;
      pragma Warnings (On);
   begin
      for I in Raw_Buf'Range loop
         Sum := Sum + Unsigned_32 (Raw_Buf (I));
      end loop;
      if Length mod 2 = 1 then
         declare
            Tmp : Netbuf (0 .. 1);
            for Tmp'Alignment use 2;
            Raw_Tmp : Unsigned_16;
            for Raw_Tmp'Address use Tmp'Address;
         begin
            Tmp (0) := Packet (Offset + Length - 1);
            Tmp (1) := 0;
            Sum := Sum + Unsigned_32 (Raw_Tmp);
         end;
      end if;
      --  One complement addition
      Sum := (Sum and 16#ffff#) + Shift_Right (Sum, 16);
      Sum := (Sum and 16#ffff#) + Shift_Right (Sum, 16);
      return Unsigned_16 (Sum and 16#ffff#);
   end Ip_Chksum;

   Mf_Flag : constant := 16#2000#;
   Frag_Off_Mask : constant := 16#1fff#;

   function Udp_Checksum (Ip_Off : Natural; Udp_Off : Natural; Len : Natural)
                         return Unsigned_16
   is
      Tmp : Netbuf (0 .. 1);
      for Tmp'Alignment use 2;
      Raw_Tmp : Unsigned_16;
      for Raw_Tmp'Address use Tmp'Address;

      Csum : Unsigned_16;
   begin
      Tmp (0) := 0;
      Tmp (1) := Ip_Proto_Udp;
      Csum := Ip_Chksum (8, Ip_Off + 12, Raw_Tmp);
      Csum := Ip_Chksum (2, Udp_Off + 4, Csum);
      Csum := Ip_Chksum (Len, Udp_Off, Csum);

      return Csum;
   end Udp_Checksum;

   function Handle_Udp (Ip_Off : Natural) return Rcv_Status
   is
      Sport, Dport : Unsigned_16;
      Len : Unsigned_16;
   begin
      --  Sanity check
      if Packet_Len - Packet_Off < 8 then
         return Rcv_Err;
      end if;

      Sport := Read_BE2 (Packet_Off + 0);
      Dport := Read_BE2 (Packet_Off + 2);
      Len := Read_BE2 (Packet_Off + 4);
      if Packet_Len - Packet_Off < Natural (Len) then
         return Rcv_Err;
      end if;

      --  Checksum
      if Udp_Checksum (Ip_Off, Packet_Off, Natural (Len)) /= 16#ffff# then
         Put_Line ("Bad UDP checksum");
         Dump_Pkt (Ip_Off);
         return Rcv_Err;
      end if;

      if False then
         Put (" UDP ");
         Put_Dec (Sport);
         Put (" -> ");
         Put_Dec (Dport);
         New_Line;
      end if;

      --  Filter
      if Dport /= My_Udp_Port then
         return Rcv_Ign;
      end if;
      if Srv_Udp_Port = 0 then
         Srv_Udp_Port := Sport;
      elsif Sport /= Srv_Udp_Port then
         return Rcv_Ign;
      end if;

      Packet_Off := Packet_Off + 8;

      return Rcv_Udp;
   end Handle_Udp;

   function Ip_Hdr_Len (Off : Natural) return Natural is
      Ihl : Unsigned_8;
   begin
      Ihl := Read_BE1 (Off);
      if (Ihl and 16#f0#) /= 16#40# then
         return 0;
      end if;
      return Natural (Ihl and 16#0f#) * 4;
   end Ip_Hdr_Len;

   function Handle_Ip return Rcv_Status
   is
      Hdr_Len : Natural;
      Ip_Src : In_Addr;
      Ip_Dst : In_Addr;
      Frag : Unsigned_16;
      Len : Unsigned_16;
      Proto : Unsigned_8;
      Chksum : Unsigned_16;
      Ip_Off : constant Natural := Packet_Off;
   begin
      --  Sanity check
      if Packet_Len - Packet_Off < 20 then
         return Rcv_Err;
      end if;

      --  Check version and compute header length
      Hdr_Len := Ip_Hdr_Len (Packet_Off);
      if Hdr_Len = 0
        or else Packet_Len - Packet_Off < Hdr_Len
      then
         return Rcv_Err;
      end if;

      Chksum := Ip_Chksum (Hdr_Len);
      if Chksum /= 16#ffff# then
         Put ("IPv4 chksum error, got: ");
         Put_Hex (Chksum);
         New_Line;
         Dump_Pkt (Ip_Off);
         return Rcv_Err;
      end if;

      --  Check if this packet is for me
      Ip_Dst := Read_BE4 (Packet_Off + 16);
      if Ip_Dst /= My_Ip_Addr then
         --  Broadcast are ignored
         return Rcv_Ign;
      end if;

      --  Check if this packet comes from known peer
      Ip_Src := Read_BE4 (Packet_Off + 12);
      if Srv_Ip_Addr = Null_Ip_Addr then
         Srv_Ip_Addr := Ip_Src;
         Srv_Eth_Addr := Null_Eth_Addr;
      elsif Ip_Src /= Srv_Ip_Addr then
         return Rcv_Ign;
      end if;

      --  Check fragment offset
      Frag := Read_BE2 (Packet_Off + 6);
      if (Frag and (Mf_Flag or Frag_Off_Mask)) /= 0 then
         return Rcv_Err;
      end if;

      --  Check length
      Len := Read_BE2 (Packet_Off + 2);
      if Packet_Len - Packet_Off < Natural (Len) then
         return Rcv_Err;
      end if;

      Proto := Read_BE1 (Packet_Off + 9);

      if False then
         Put ("IPv4: ");
         Disp_Ip_Addr (To_Ip_Addr (Ip_Src));
         Put (" -> ");
         Disp_Ip_Addr (To_Ip_Addr (Ip_Dst));
         Put (", proto: ");
         Put_Dec (Proto);
         New_Line;
      end if;

      Packet_Off := Packet_Off + Hdr_Len;

      case Proto is
         when Ip_Proto_Udp =>
            return Handle_Udp (Ip_Off);
         when others =>
            return Rcv_Ign;
      end case;
   end Handle_Ip;

   function Handle_Eth_Packet return Rcv_Status is
      Eth_Type : Unsigned_16;
   begin
      --  Sanity check packet size
      if Packet_Len - Packet_Off < 14 then
         return Rcv_Err;
      end if;

--    if Eth_Addr (Packet (Packet_Off .. Packet_Off + 5)) = Eth_Broadcast then
--       return;
--    end if;

      --  Skip DA and SA.
      Packet_Off := Packet_Off + 12;
      Eth_Type := Read_BE2 (Packet_Off);
      Packet_Off := Packet_Off + 2;

      case Eth_Type is
         when Eth_Type_Ip =>
            --  Put_Line ("IPv4");
            return Handle_Ip;
         when Eth_Type_Arp =>
            --  Put_Line ("Arp");
            return Handle_Arp;
         when others =>
            --  Put ("Unknown: ");
            --  Put_Hex (Eth_Type);
            --  New_Line;
            return Rcv_None;
      end case;
   end Handle_Eth_Packet;

   procedure Resolve_Eth (Ip : In_Addr)
   is
      Time : Unsigned_32;
   begin
      for I in 1 .. 3 loop
         exit when Srv_Eth_Addr /= Null_Eth_Addr;

         --  Send ARP request
         Eth_Send_Init;
         --  Eth frame
         Write_Eth (Broadcast_Eth_Addr);
         Write_Eth (My_Eth_Addr);
         Write_BE2 (Eth_Type_Arp);
         --  Arp header
         Write_BE2 (Arp_Htype);
         Write_BE2 (Arp_Ptype);
         Write_BE1 (Eth_Addr'Length);
         Write_BE1 (Ip_Addr'Length);
         Write_BE2 (Arp_Request);
         --  Arp sender
         Write_Eth (My_Eth_Addr);
         Write_BE4 (My_Ip_Addr);
         --  Arp target
         Write_Eth (Null_Eth_Addr);
         Write_BE4 (Ip);

         Packet_Len := Packet_Off;
         --  Dump_Pkt;
         Eth_Send_Packet;

         --  Wait 1 sec
         Time := Get_Clock + 10;
         loop
            Eth_Rcv_Wait (Time);
            exit when Packet_Len = 0; -- Timeout
            exit when Handle_Eth_Packet = Rcv_Arp; -- Got answer
         end loop;
      end loop;
   end Resolve_Eth;

   procedure Route_To_Server is
   begin
      if In_My_Network (Srv_Ip_Addr) then
         Resolve_Eth (Srv_Ip_Addr);
      elsif Gw_Ip_Addr /= Null_Ip_Addr then
         Resolve_Eth (Gw_Ip_Addr);
      else
         Srv_Eth_Addr := Null_Eth_Addr;
      end if;
   end Route_To_Server;

   procedure Prepare_Ip (Proto : Unsigned_8; Off : out Natural) is
   begin
      --  Send ARP request
      Eth_Send_Init;
      --  Eth frame
      Write_Eth (Srv_Eth_Addr);
      Write_Eth (My_Eth_Addr);
      Write_BE2 (Eth_Type_Ip);

      --  Ip frame
      Off := Packet_Off;
      Write_BE1 (16#45#); --  IHL
      Write_BE1 (0);    --  TOS
      Write_BE2 (20);   --  Total length
      Write_BE2 (0);    --  Id
      Write_BE2 (0);    --  Flags + frag
      Write_BE1 (64);  --  TTL
      Write_BE1 (Proto);
      Write_BE2 (0);    --  Checksum
      Write_BE4 (My_Ip_Addr);
      Write_BE4 (Srv_Ip_Addr);
   end Prepare_Ip;

   procedure Prepare_Udp (Ip_Off : out Natural) is
   begin
      Prepare_Ip (Ip_Proto_Udp, Ip_Off);
      Write_BE2 (My_Udp_Port);
      Write_BE2 (Srv_Udp_Port);
      Write_BE2 (8); --  Length
      Write_BE2 (0); --  Checksum
   end Prepare_Udp;

   procedure Send_Udp (Ip_Off : Natural)
   is
      Ip_Len  : constant Natural := Ip_Hdr_Len (Ip_Off);
      Udp_Off : constant Natural := Ip_Off + Ip_Len;
      Udp_Len : constant Natural := Packet_Off - (Ip_Off + Ip_Len);
   begin
      --  Write UDP length
      Write_BE2 (Unsigned_16 (Udp_Len), Udp_Off + 4);

      --  Write UDP checksum
      Write_2 (not Udp_Checksum (Ip_Off, Udp_Off, Udp_Len), Udp_Off + 6);

      --  Write Ip length
      Write_BE2 (Unsigned_16 (Ip_Len + Udp_Len), Ip_Off + 2);

      --  Write Ip checksum
      Write_2 (not Ip_Chksum (Ip_Len, Ip_Off), Ip_Off + 10);

      Packet_Len := Packet_Off;
      --  Dump_Pkt;
      Eth_Send_Packet;
   end Send_Udp;

end Netproto;
