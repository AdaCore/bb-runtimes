------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Minimal version of Text_IO body for use on SAMV71, using UART1

with Interfaces; use Interfaces;

with Interfaces.SAM.PMC; use Interfaces.SAM.PMC;
with Interfaces.SAM.PIO; use Interfaces.SAM.PIO;
with Interfaces.SAM.UART; use Interfaces.SAM.UART;

with System.SAMV71; use System.SAMV71;
with System.BB.Parameters;

package body System.Text_IO is
   use Interfaces.SAM;

   Baudrate : constant := 115_200;
   --  Bitrate to use

   Clock_Frequency : constant := System.BB.Parameters.Clock_Frequency;

   ---------
   -- Get --
   ---------

   function Get return Character is
      (Character'Val (UART.UART1_Periph.UART_RHR.RXCHR));

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      PA21 : constant := 2 ** 21; --  RX line pio periph A
      PB04 : constant := 2 ** 4; --  TX line pio periph D
   begin
      Initialized := True;

      --  Reset and disable receiver & transmitter
      UART.UART1_Periph.UART_CR.RSTRX := 1;
      UART.UART1_Periph.UART_CR.RSTTX := 1;
      UART.UART1_Periph.UART_CR.RXDIS := 1;
      UART.UART1_Periph.UART_CR.TXDIS := 1;
      UART.UART1_Periph.UART_CR.RSTSTA := 1;

      UART.UART1_Periph.UART_IDR := (RXRDY => 1,
                                     TXRDY => 1,
                                     OVRE => 1,
                                     FRAME => 1,
                                     PARE => 1,
                                     TXEMPTY => 1,
                                     CMP => 1,
                                     others => <>);

      --  Power-up clocks
      PMC.PMC_Periph.PMC_PCR.CMD := 1;
      PMC.PMC_Periph.PMC_PCER0.PID.Val :=
        2 ** UART1_ID + 2 ** PIOB_ID + 2 ** PIOA_ID;

      --  Disable PIO from controlling pins
      PIO.PIOB_Periph.PIO_PDR.Val := PB04;
      PIO.PIOA_Periph.PIO_PDR.Val := PA21;

      PIO.PIOB_Periph.PIO_ODR.Val := PB04;
      PIO.PIOA_Periph.PIO_ODR.Val := PA21;

      PIO.PIOB_Periph.PIO_PUER.Val := PB04;

      PIO.PIOB_Periph.PIO_MDDR.Val := PB04;
      PIO.PIOA_Periph.PIO_MDDR.Val := PA21;

      PIO.PIOB_Periph.PIO_ABCDSR (0).Val :=
        PIO.PIOB_Periph.PIO_ABCDSR (0).Val and PB04;
      PIO.PIOB_Periph.PIO_ABCDSR (1).Val :=
        PIO.PIOB_Periph.PIO_ABCDSR (1).Val and PB04;

      PIO.PIOA_Periph.PIO_ABCDSR (0).Val :=
        PIO.PIOA_Periph.PIO_ABCDSR (0).Val and not PA21;
      PIO.PIOA_Periph.PIO_ABCDSR (1).Val :=
        PIO.PIOA_Periph.PIO_ABCDSR (1).Val and PA21;

      --  Configure Mode
      UART.UART1_Periph.UART_MR := (FILTER => UART.Disabled,
                                    PAR => UART.No,
                                    BRSRCCK => UART.Periph_Clk,
                                    CHMODE => UART.Normal,
                                    others => <>);

      --  Configure baudrate
      UART.UART1_Periph.UART_BRGR.CD :=
      UART_UART_BRGR_CD_Field (Clock_Frequency / (16 * Baudrate));

      --  Enable receiver and transmitter
      UART.UART1_Periph.UART_CR.RXEN := 1;
      UART.UART1_Periph.UART_CR.TXEN := 1;
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      (UART.UART1_Periph.UART_SR.TXRDY = 1);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      (UART.UART1_Periph.UART_SR.RXRDY = 1);

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      UART.UART1_Periph.UART_THR.TXCHR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is (True);
end System.Text_IO;
