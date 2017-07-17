------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Xilinx Ultrascale+ MPSoC version of this package

with System;

with Interfaces;

package body System.Text_IO is
   use Interfaces;

   Base : constant := 16#FF00_0000#;
   --  UART-1

   CR   : Unsigned_32 with Address => Base + 16#00#, Import, Volatile;
   MR   : Unsigned_32 with Address => Base + 16#00#, Import, Volatile;
   IER  : Unsigned_32 with Address => Base + 16#08#, Import, Volatile;
   IDR  : Unsigned_32 with Address => Base + 16#0C#, Import, Volatile;
   ISR  : Unsigned_32 with Address => Base + 16#14#, Import, Volatile;
   SR   : Unsigned_32 with Address => Base + 16#2c#, Import, Volatile;
   Fifo : Unsigned_32 with Address => Base + 16#30#, Import, Volatile;

   CR_RX_RST : constant := 16#00000001#;
   CR_TX_RST : constant := 16#00000002#;
   CR_RX_EN  : constant := 16#00000004#;
   CR_RX_DIS : constant := 16#00000008#;
   CR_TX_EN  : constant := 16#00000010#;
   CR_TX_DIS : constant := 16#00000020#;
   --  CR bits

   MR_MODE_NORMAL : constant := 0;
   MR_MODE_MASK   : constant := 16#0000_0300#;

   Tx_Full  : constant Unsigned_32 := 2#0001_0000#;
   Rx_Empty : constant Unsigned_32 := 2#0000_0010#;
   --  SR bits

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (Fifo and 16#ff#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Dead : Character with Unreferenced;
   begin
      Initialized := True;

      --  Reset RX and TX
      CR := CR or (CR_TX_RST or CR_RX_RST);
      loop
         exit when (CR and (CR_TX_RST or CR_RX_RST)) = 0;
      end loop;

      --  Disable the EN bits while clearing the DIS bits
      CR := CR and not (CR_TX_DIS or CR_RX_DIS or CR_TX_EN or CR_RX_EN);
      --  Enable RX and TX
      CR := CR or (CR_TX_EN or CR_RX_EN);

      --  Operating mode: normal
      MR := (MR and not MR_MODE_MASK) or MR_MODE_NORMAL;

      --  Disable interrupts
      IDR := 16#3FFF#;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (SR and Rx_Empty) = 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (SR and Tx_Full) = 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character

      Fifo := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
