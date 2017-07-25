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

--  Uart I/O for Raspberry PI 2 using PL011 (aka uart0)

with System;
with Interfaces.Raspberry_Pi;
with System.Machine_Code;

package body System.Text_IO is
   use Interfaces;
   use Interfaces.Raspberry_Pi;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (PL011_Registers.DR and 16#ff#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Interfaces.Raspberry_Pi.PL011_Bits;
      Sel : Unsigned_32;
   begin
      Initialized := True;

      --  Disable UART
      PL011_Registers.CR := 0;

      --  Wait for end of TX and RX.
      if False then
         while (PL011_Registers.FR and FR_BUSY) /= 0 loop
            null;
         end loop;
         while (PL011_Registers.FR and FR_RXFE) = 0 loop
            Sel := PL011_Registers.DR;
         end loop;
      end if;

      --  Use GPIO 14 & 15
      Sel := GPIO_Registers.GPFSEL1;

      --  GPIO14: alt0
      Sel := Sel and not (7 * 2**12);
      Sel := Sel or (4 * 2**12);

      --  GPIO15: alt0
      Sel := Sel and not (7 * 2**15);
      Sel := Sel or (4 * 2**15);

      GPIO_Registers.GPFSEL1 := Sel;

      --  Disable pull-up/down on all GPIOs.
      GPIO_Registers.GPPUD := 0;

      --  Clock pull-up
      for I in 1 .. 150 loop
         System.Machine_Code.Asm ("nop", Volatile => True);
      end loop;

      GPIO_Registers.GPPUDCLK0 := 2**14 + 2**15;

      for I in 1 .. 150 loop
         System.Machine_Code.Asm ("nop", Volatile => True);
      end loop;

      GPIO_Registers.GPPUDCLK0 := 0;

      --  Freq = 46_000_000, baud = 115_200
      --  Freq / baud ~= 26 + 3/64
      PL011_Registers.IBRD := 26;
      PL011_Registers.FBRD := 3;

      --  8n1, FIFO en
      PL011_Registers.LCRH := 16#70#;

      --  Clear interrupts
      PL011_Registers.ICR := 16#3ff#;

      --  Enable
      PL011_Registers.CR := 16#301#;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (PL011_Registers.FR and PL011_Bits.FR_RXFE) = 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (PL011_Registers.FR and PL011_Bits.FR_TXFF) = 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character

      PL011_Registers.DR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
