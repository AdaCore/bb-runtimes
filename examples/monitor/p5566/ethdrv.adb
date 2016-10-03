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

--  Ethernet driver

with Clkdrv;
with System; use System;
with System.Storage_Elements;
with Netcfg; use Netcfg;
with Netutils; use Netutils;
with System.Machine_Code; use System.Machine_Code;
with Dumps; use Dumps;
with Ada.Text_IO; use Ada.Text_IO;
--  with Netproto;

package body Ethdrv is
   FEC_EIR : Unsigned_32;
   for FEC_EIR'Address use System'To_Address (16#fff4_c004#);
   pragma Import (Ada, FEC_EIR);
   pragma Volatile (FEC_EIR);

   EIR_RXF : constant := 16#0200_0000#;
   EIR_RXB : constant := 16#0100_0000#;

   FEC_RDAR : Unsigned_32;
   for FEC_RDAR'Address use System'To_Address (16#fff4_c010#);
   pragma Import (Ada, FEC_RDAR);
   pragma Volatile (FEC_RDAR);

   FEC_TDAR : Unsigned_32;
   for FEC_TDAR'Address use System'To_Address (16#fff4_c014#);
   pragma Import (Ada, FEC_TDAR);
   pragma Volatile (FEC_TDAR);

   FEC_ECR : Unsigned_32;
   for FEC_ECR'Address use System'To_Address (16#fff4_c024#);
   pragma Import (Ada, FEC_ECR);
   pragma Volatile (FEC_ECR);

   FEC_MMFR : Unsigned_32;
   for FEC_MMFR'Address use System'To_Address (16#fff4_c040#);
   pragma Import (Ada, FEC_MMFR);
   pragma Volatile (FEC_MMFR);

   FEC_MSCR : Unsigned_32;
   for FEC_MSCR'Address use System'To_Address (16#fff4_c044#);
   pragma Import (Ada, FEC_MSCR);
   pragma Volatile (FEC_MSCR);

   FEC_RCR : Unsigned_32;
   for FEC_RCR'Address use System'To_Address (16#fff4_c084#);
   pragma Import (Ada, FEC_RCR);
   pragma Volatile (FEC_RCR);

   FEC_TCR : Unsigned_32;
   for FEC_TCR'Address use System'To_Address (16#fff4_c0c4#);
   pragma Import (Ada, FEC_TCR);
   pragma Volatile (FEC_TCR);

   FEC_PALR : Unsigned_32;
   for FEC_PALR'Address use System'To_Address (16#fff4_c0e4#);
   pragma Import (Ada, FEC_PALR);
   pragma Volatile (FEC_PALR);

   FEC_PAUR : Unsigned_32;
   for FEC_PAUR'Address use System'To_Address (16#fff4_c0e8#);
   pragma Import (Ada, FEC_PAUR);
   pragma Volatile (FEC_PAUR);

   FEC_OPD : Unsigned_32;
   for FEC_OPD'Address use System'To_Address (16#fff4_c0ec#);
   pragma Import (Ada, FEC_OPD);
   pragma Volatile (FEC_OPD);

   FEC_IAUR : Unsigned_32;
   for FEC_IAUR'Address use System'To_Address (16#fff4_c118#);
   pragma Import (Ada, FEC_IAUR);
   pragma Volatile (FEC_IAUR);

   FEC_IALR : Unsigned_32;
   for FEC_IALR'Address use System'To_Address (16#fff4_c11c#);
   pragma Import (Ada, FEC_IALR);
   pragma Volatile (FEC_IALR);

   FEC_GAUR : Unsigned_32;
   for FEC_GAUR'Address use System'To_Address (16#fff4_c120#);
   pragma Import (Ada, FEC_GAUR);
   pragma Volatile (FEC_GAUR);

   FEC_GALR : Unsigned_32;
   for FEC_GALR'Address use System'To_Address (16#fff4_c124#);
   pragma Import (Ada, FEC_GALR);
   pragma Volatile (FEC_GALR);

   FEC_TFWR : Unsigned_32;
   for FEC_TFWR'Address use System'To_Address (16#fff4_c144#);
   pragma Import (Ada, FEC_TFWR);
   pragma Volatile (FEC_TFWR);

   FEC_ERDSR : Address;
   for FEC_ERDSR'Address use System'To_Address (16#fff4_c180#);
   pragma Import (Ada, FEC_ERDSR);
   pragma Volatile (FEC_ERDSR);

   FEC_ETDSR : Address;
   for FEC_ETDSR'Address use System'To_Address (16#fff4_c184#);
   pragma Import (Ada, FEC_ETDSR);
   pragma Volatile (FEC_ETDSR);

   FEC_EMRBR : Unsigned_32;
   for FEC_EMRBR'Address use System'To_Address (16#fff4_c188#);
   pragma Import (Ada, FEC_EMRBR);
   pragma Volatile (FEC_EMRBR);

   SIU_PCR : array (0 .. 230) of Unsigned_16;
   for SIU_PCR'Address use System'To_Address (16#c3f9_0040#);
   pragma Import (Ada, SIU_PCR);
   pragma Volatile (SIU_PCR);

   EBI_MCR : Unsigned_32;
   for EBI_MCR'Address use System'To_Address (16#c3f8_4000#);
   pragma Import (Ada, EBI_MCR);
   pragma Volatile (EBI_MCR);

   type TRxbd_Type is record
      Flags : Unsigned_16;
      Len : Unsigned_16;
      Buffer : Address;
   end record;
   pragma Warnings (Off, "* not referenced");
   Flag_E   : constant := 2#1000_0000_0000_0000#;
   Flag_R   : constant := 2#1000_0000_0000_0000#;
   Flag_RO1 : constant := 2#0100_0000_0000_0000#;
   Flag_TO1 : constant := 2#0100_0000_0000_0000#;
   Flag_W   : constant := 2#0010_0000_0000_0000#;
   Flag_RO2 : constant := 2#0001_0000_0000_0000#;
   Flag_TO2 : constant := 2#0001_0000_0000_0000#;
   Flag_L   : constant := 2#0000_1000_0000_0000#;
   Flag_TC  : constant := 2#0000_0100_0000_0000#;
   Flag_ABC : constant := 2#0000_0010_0000_0000#;
   Flag_M   : constant := 2#0000_0001_0000_0000#;
   Flag_BC  : constant := 2#0000_0000_1000_0000#;
   Flag_MC  : constant := 2#0000_0000_0100_0000#;
   Flag_LG  : constant := 2#0000_0000_0010_0000#;
   Flag_NO  : constant := 2#0000_0000_0001_0000#;
   Flag_CR  : constant := 2#0000_0000_0000_0100#;
   Flag_OV  : constant := 2#0000_0000_0000_0010#;
   Flag_TR  : constant := 2#0000_0000_0000_0001#;
   pragma Warnings (On, "* not referenced");

   subtype Rxbd_Range is Natural range 0 .. 7;
   type TRxbd_Array is array (Natural range <>) of TRxbd_Type;
   Rxdesc : TRxbd_Array (Rxbd_Range);
   for Rxdesc'Alignment use 32;
   pragma Volatile (Rxdesc);

   type Netbuf_Array is array (Rxbd_Range) of Netbuf (0 .. 511);
   Rxbuf : Netbuf_Array;
   for Rxbuf'Alignment use 32;
   Rxdesc_Num : Rxbd_Range := 0;

   subtype Txbd_Range is Natural range 0 .. 3;
   Txdesc : TRxbd_Array (Txbd_Range);
   for Txdesc'Alignment use 32;
   pragma Volatile (Txdesc);
   Txdesc_Num : Txbd_Range := 0;

   procedure Cache_Flush (Addr : Address) is
   begin
      Asm ("dcbf 0,%0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end Cache_Flush;

   procedure Flush_Packet is
      use System.Storage_Elements;
      Addr : Address := Packet (0)'Address;
   begin
      while Addr <= Packet (Packet'Last)'Address loop
         Cache_Flush (Addr);
         Addr := Addr + 32;
      end loop;
   end Flush_Packet;

   procedure Flush_Rxbuf (N : Rxbd_Range) is
      use System.Storage_Elements;
      Addr : Address := Rxbuf (N)'Address;
   begin
      while Addr <= Rxbuf (N)(Rxbuf (N)'Last)'Address loop
         Cache_Flush (Addr);
         Addr := Addr + 32;
      end loop;
   end Flush_Rxbuf;

   procedure Cache_Disable is
      Val : Unsigned_32;
   begin
      --  Flush
      for Cset in 0 .. 127 loop
         for Cway in 0 .. 7 loop
            Val := Unsigned_32 (Cway * 16#1_000000# + Cset * 16#2_0# + 16#2#);
            Asm ("mtspr 1016, %0",
                 Inputs => Unsigned_32'Asm_Input ("r", Val),
                 Volatile => True);
         end loop;
      end loop;

      --  Disable
      Asm ("msync; isync; mtspr 1010, %0",
           Inputs => Unsigned_32'Asm_Input ("r", 0), Volatile => True);
   end Cache_Disable;

   procedure Eth_Init is
   begin
      Cache_Disable;

      --  Reset the chip
      FEC_ECR := 1;
      while (FEC_ECR and 3) /= 0 loop
         null;
      end loop;

      SIU_PCR (44) := 2#10_01_11_00_00_00#; -- TX_CLK
      SIU_PCR (45) := 2#10_01_11_00_00_00#; -- CRS
      SIU_PCR (46) := 2#10_01_11_00_00_00#; -- TX_ER
      SIU_PCR (47) := 2#10_01_11_00_00_00#; -- RX_CLK
      SIU_PCR (48) := 2#10_01_11_00_00_00#; -- TXD0
      SIU_PCR (49) := 2#10_01_11_00_00_00#; -- RX_ER
      SIU_PCR (50) := 2#10_01_11_00_00_00#; -- RXD0
      SIU_PCR (51) := 2#10_01_11_00_00_00#; -- TXD3
      SIU_PCR (52) := 2#10_01_11_00_00_00#; -- COL
      SIU_PCR (53) := 2#10_01_11_00_00_00#; -- RX_DV
      SIU_PCR (54) := 2#10_01_11_00_00_00#; -- TX_EN
      SIU_PCR (55) := 2#10_01_11_00_00_00#; -- TXD2
      SIU_PCR (56) := 2#10_01_11_00_00_00#; -- TXD1
      SIU_PCR (57) := 2#10_01_11_00_00_00#; -- RXD1
      SIU_PCR (58) := 2#10_01_11_00_00_00#; -- RXD2
      SIU_PCR (59) := 2#10_01_11_00_00_00#; -- RXD3

      --  Enable FEC_MDC and FEC_MDIO
      SIU_PCR (72) := 2#10_11_11_00_00_00#; -- MDC
      SIU_PCR (73) := 2#10_11_11_00_00_00#; -- MDIO
      EBI_MCR := 16#01#; --  Disable, 16 bits

      --  Clear events
      FEC_EIR := 16#ffff_ffff#;
      --  Set fifo level
      FEC_TFWR := 3;
      --  Individual address
      FEC_PALR := Shift_Left (Unsigned_32 (My_Eth_Addr (1)), 24)
        or Shift_Left (Unsigned_32 (My_Eth_Addr (2)), 16)
        or Shift_Left (Unsigned_32 (My_Eth_Addr (3)), 8)
        or Shift_Left (Unsigned_32 (My_Eth_Addr (4)), 0);
      FEC_PAUR := Shift_Left (Unsigned_32 (My_Eth_Addr (5)), 24)
        or Shift_Left (Unsigned_32 (My_Eth_Addr (6)), 16);
      --  Multicast addresses hash bits
      FEC_GAUR := 0;
      FEC_GALR := 0;
      --  Individual addresses hash bits
      FEC_IAUR := 0;
      FEC_IALR := 0;
      --
      FEC_OPD := 100;
      FEC_RCR := 1518 * 16#1_0000# + 2#100#; --  MII
      FEC_TCR := 2#100#; -- FDEN
      FEC_MSCR := 13 * 2#10#; -- 2.5Mhz at 132Mhz
      --  FEC_FRSR :=
      FEC_EMRBR := Rxbuf (0)'Length;
      FEC_ERDSR := Rxdesc'Address;
      FEC_ETDSR := Txdesc'Address;

      for I in Rxdesc'Range loop
         Rxdesc (I) := (Flag_E, 0, Rxbuf (I)'Address);
         if I = Rxdesc'Last then
            Rxdesc (I).Flags := Flag_E or Flag_W;
         end if;
         Cache_Flush (Rxdesc (I)'Address);
         Flush_Rxbuf (I);
      end loop;
      Rxdesc_Num := 0;

      for I in Txdesc'Range loop
         Txdesc (I) := (0, 0, Packet'Address);
         if I = Txdesc'Last then
            Txdesc (I).Flags := Flag_W;
         end if;
         Cache_Flush (Txdesc (I)'Address);
      end loop;
      Txdesc_Num := 0;

      FEC_ECR := 2;
      FEC_RDAR := 16#01_00_00_00#;

      if False then
         for I in 0 .. 7 loop
            FEC_MMFR := 2#01_10_00001_00000_10_0000000000000000#
              + Unsigned_32 (I * 16#4_0000#);
            while (FEC_EIR and 16#80_0000#) = 0 loop
               null;
            end loop;
            FEC_EIR := 16#80_0000#;
            Put_Line
              ("Reg" & Image1 (Unsigned_32 (I)) & ": " & Image8 (FEC_MMFR));
         end loop;
      end if;
   end Eth_Init;

   procedure Eth_Rcv_Wait (Maxtime : Unsigned_32 := 16#ffffffff#) is
      Chunk_Len : Natural;
      Flags : Unsigned_16;
      Prev_Eir, Eir : Unsigned_32;
   begin
      loop
         if False then
            Put ("eth rcv: wait ");
            Put_Hex (Unsigned_8 (Rxdesc_Num));
            New_Line;
         end if;

         --  Wait until the current descriptor is available
         FEC_EIR := EIR_RXF or EIR_RXB;
         Prev_Eir := 0;
         loop
            exit when (Rxdesc (Rxdesc_Num).Flags and Flag_E) = 0;
            Cache_Flush (Rxdesc (Rxdesc_Num)'Address);
            --  FEC_RDAR := 16#01_00_00_00#;
            loop
               if Clkdrv.Get_Clock > Maxtime then
                  Packet_Len := 0;
                  return;
               end if;
               Eir := FEC_EIR;
               exit when (Eir and EIR_RXF) /= 0;
               if False and then Eir /= Prev_Eir then
                  Put ("EIR: ");
                  Put_Hex (Eir);
                  New_Line;
                  Prev_Eir := Eir;
               end if;
            end loop;
         end loop;

         --  Copy chunks
         Packet_Off := 0;
         loop
            Flags := Rxdesc (Rxdesc_Num).Flags;
            if False then
               Put ("Flags: ");
               Put_Hex (Flags);
               Put (" len: ");
               Put_Hex (Rxdesc (Rxdesc_Num).Len);
               New_Line;
            end if;

            --  Copy chunk
            Chunk_Len := Natural (Rxdesc (Rxdesc_Num).Len);
            if (Flags and Flag_L) /= 0 then
               --  The length of the last buffer is the length of the frame,
               --  not just the length of the last buffer (15.5.1.2)
               Chunk_Len := Chunk_Len - Packet_Off;
            end if;
            Packet (Packet_Off .. Packet_Off + Chunk_Len - 1) :=
              Rxbuf (Rxdesc_Num)(0 .. Chunk_Len - 1);
            Packet_Off := Packet_Off + Chunk_Len;
            Flush_Rxbuf (Rxdesc_Num);

            --  Check flags
            Rxdesc (Rxdesc_Num).Flags := (Flags and Flag_W) or Flag_E;
            Cache_Flush (Rxdesc (Rxdesc_Num)'Address);
            Rxdesc_Num := (Rxdesc_Num + 1) rem Rxdesc'Length;
            exit when (Flags and Flag_L) /= 0;
            --  Wait until the next descriptor is available
            loop
               exit when (Rxdesc (Rxdesc_Num).Flags and Flag_E) = 0;
               Cache_Flush (Rxdesc (Rxdesc_Num)'Address);
            end loop;
         end loop;
         --  Discard invalid packets
         exit when (Flags and (Flag_CR + Flag_OV + Flag_TR)) = 0;
         Put ("err rxdesc: ");
         Put_Hex (Flags);
         New_Line;
      end loop;
      Packet_Len := Packet_Off - 4; --  Remove CRC
      Packet_Off := 0;
   end Eth_Rcv_Wait;

   procedure Eth_Send_Init is
   begin
      Packet_Off := 0;
   end Eth_Send_Init;

   procedure Eth_Send_Packet is
   begin
      --  Put_Line ("Sending");
      --  Netproto.Dump_Pkt;
      Flush_Packet;
      Txdesc (Txdesc_Num).Len := Unsigned_16 (Packet_Len);
      Txdesc (Txdesc_Num).Flags := (Txdesc (Txdesc_Num).Flags and Flag_W)
        or Flag_R + Flag_L + Flag_TC;
      Cache_Flush (Txdesc (Txdesc_Num)'Address);
      Txdesc_Num := (Txdesc_Num + 1) rem Txdesc'Length;
      FEC_TDAR := 16#01_00_00_00#;
      while (FEC_EIR and 16#800_0000#) = 0 loop
         null;
      end loop;
      FEC_EIR := 16#0c00_0000#;
   end Eth_Send_Packet;
end Ethdrv;
