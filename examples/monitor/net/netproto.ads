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

package Netproto is
   type Rcv_Status is (Rcv_Err, Rcv_Ign, Rcv_Ip, Rcv_Arp, Rcv_Udp, Rcv_None);
   --  Rcv_Err: error detected in the packet
   --  Rcv_Ign: packet ignored

   function Handle_Eth_Packet return Rcv_Status;
   --  To be called when an ethernet packet is received.  Filter and decode
   --  the frame.

   procedure Dump_Pkt (Off : Natural := 0);

   procedure Route_To_Server;
   --  Prepare the route to the server (check if this is possible, get
   --  ARP address).  The Srv_Eth_Addr is set to null_eth_Addr if there is no
   --  route.

   My_Udp_Port : Unsigned_16 := 0;
   Srv_Udp_Port : Unsigned_16 := 0;
   --  UDP ports for outgoing packets.
   --  Only incoming packets whose dport is My_Udp_Port are accept, and sport
   --  is filtered if srv_udp_port is not 0.

   procedure Prepare_Udp (Ip_Off : out Natural);
   --  Prepare an UDP packet: write the ethernet, IP and UDP headers (not
   --  yet completed).

   procedure Send_Udp (Ip_Off : Natural);
   --  Complete the IP and UDP headers and send the packet
end Netproto;
