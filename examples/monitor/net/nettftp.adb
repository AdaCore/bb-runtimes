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
with Netutils; use Netutils;
with Netproto; use Netproto;
with Ethdrv; use Ethdrv;
with Netcfg;
with Memory_Copy;
with Interfaces.C;

--  Implement a tftp server (that accept only write requests).
--  RFC1350

package body Nettftp is
   procedure Get_Next_Data;

   Block_Size : constant := 512;

   Pkt_Len : Natural;
   --  Length in byte of the last tftp data packet

   Tftp_File_Offset : Unsigned_32;
   --  Offset in the file of the last tftp data packet

   Blk_Num : Unsigned_16;
   --  Block number of the last received data packet (also the packet to ack)

   procedure Open is
      Rcv : Rcv_Status;
      Rq : Unsigned_16;
      B : Unsigned_8;
      Mode_Off : Natural;
      Octet : constant Netbuf := (Character'Pos ('o'),
                                  Character'Pos ('c'),
                                  Character'Pos ('t'),
                                  Character'Pos ('e'),
                                  Character'Pos ('t'));
   begin
      My_Udp_Port := 69;

      loop
         --  Accept request from anyone
         Netcfg.Srv_Ip_Addr := Null_Ip_Addr;

         --  Wait for a packet
         Eth_Rcv_Wait;

         Rcv := Handle_Eth_Packet;
         if Rcv = Rcv_Udp then
            if Packet_Len - Packet_Off >= 4 then
               Rq := Read_BE2 (Packet_Off);
               Packet_Off := Packet_Off + 2;
               if Rq = 2 then
                  Put ("Got write request for: ");
                  while Packet_Off < Packet_Len loop
                     B := Packet (Packet_Off);
                     Packet_Off := Packet_Off + 1;
                     exit when B = 0;
                     Put (Character'Val (B));
                  end loop;
                  Put (", mode: ");
                  Mode_Off := Packet_Off;
                  while Packet_Off < Packet_Len loop
                     B := Packet (Packet_Off);
                     Packet_Off := Packet_Off + 1;
                     exit when B = 0;
                     Put (Character'Val (B));
                  end loop;
                  New_Line;
                  if Packet (Mode_Off .. Packet_Off - 2) = Octet then
                     exit;
                  else
                     Put_Line ("not octet mode");
                  end if;
                  Dump_Pkt (Packet_Off);
               else
                  Put_Line ("Unhandled request");
                  Dump_Pkt (Packet_Off);
               end if;
            end if;
         end if;
      end loop;

      Route_To_Server;
      if Netcfg.Srv_Eth_Addr = Null_Eth_Addr then
         Put_Line ("Cannot route to server");
         return;
      end if;

      My_Udp_Port := 49200;
      Pkt_Len := Block_Size;
      Blk_Num := 0;
      Get_Next_Data;
      Tftp_File_Offset := 0;
   end Open;

   procedure Send_Ack is
      Off : Natural;
   begin
      if False then
         Put ("tftp: ack ");
         Put_Hex (Blk_Num);
         New_Line;
      end if;

      Prepare_Udp (Off);

      --  Ack
      Write_BE2 (4);
      Write_BE2 (Blk_Num);

      Send_Udp (Off);
   end Send_Ack;

   procedure Get_Next_Data is
      Rcv : Rcv_Status;
      Rq : Unsigned_16;
   begin
      Send_Ack;

      loop
         --  Wait for a packet
         Eth_Rcv_Wait;

         Rcv := Handle_Eth_Packet;
         if Rcv = Rcv_Udp
           and then Packet_Len - Packet_Off >= 4
         then
            Rq := Read_BE2 (Packet_Off);
            Packet_Off := Packet_Off + 2;
            if Rq = 3 then
               if Read_BE2 (Packet_Off) /= Blk_Num + 1 then
                  Put ("Out of sync, got: ");
                  Put_Hex (Read_BE2 (Packet_Off));
                  Put (", next is ");
                  Put_Hex (Blk_Num + 1);
                  New_Line;
               else
                  Packet_Off := Packet_Off + 2;
                  Pkt_Len := Packet_Len - Packet_Off;
                  if False then
                     Put ("Got ");
                     Put_Dec (Unsigned_16 (Pkt_Len));
                     Put_Line (" bytes");
                  end if;
                  Blk_Num := Blk_Num + 1;
                  return;
               end if;
            else
               Put ("Got unhandled op: ");
               Put_Hex (Rq);
               New_Line;
            end if;
         end if;
      end loop;
   end Get_Next_Data;

   procedure Read (Off : Unsigned_32;
                   Data : Address;
                   Count : in out Natural)
   is
      Res : Natural;
      Pkt_Off : Natural;
   begin
      if Off < Tftp_File_Offset then
         --  Cannot rewind
         Put_Line ("tftp: cannot rewind");
         Count := 0;
         return;
      end if;

      --  Skip data
      while Off >= Tftp_File_Offset + Unsigned_32 (Pkt_Len) loop
         if Pkt_Len < Block_Size then
            Count := 0;
            return;
         end if;
         Tftp_File_Offset := Tftp_File_Offset + Unsigned_32 (Pkt_Len);
         Get_Next_Data;
      end loop;

      Pkt_Off := Natural (Off - Tftp_File_Offset);
      Res := Pkt_Len - Pkt_Off;
      if Res > Count then
         Res := Count;
      end if;
      if False then
         Put ("tftp read: off=");
         Put_Dec (Unsigned_16 (Off));
         Put ("/");
         Put_Dec (Unsigned_16 (Tftp_File_Offset));
         Put (" pkt_off=");
         Put_Dec (Unsigned_16 (Packet_Off));
         Put ("+");
         Put_Dec (Unsigned_16 (Pkt_Off));
         Put (" res=");
         Put_Dec (Unsigned_16 (Res));
         New_Line;
      end if;
      Memory_Copy.memcpy
        (Data, Packet (Packet_Off + Pkt_Off)'Address, C.size_t (Res));
      Count := Res;
   end Read;

   procedure Close is
   begin
      Put_Line ("closing");
      while Pkt_Len = Block_Size loop
         Get_Next_Data;
      end loop;
      Send_Ack;
   end Close;
end Nettftp;
