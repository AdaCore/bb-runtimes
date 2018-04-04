------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Interfaces.NRF51;      use Interfaces.NRF51;
with Interfaces.NRF51.UART; use Interfaces.NRF51.UART;

package body System.Text_IO is

   ---------
   -- Get --
   ---------

   function Get return Character is
      Ret : constant Character :=
        Character'Val (UART0_Periph.RXD.RXD and 16#FF#);
   begin

      --  Clear the RX event for the character we just received
      UART0_Periph.EVENTS_RXDRDY := 0;

      return Ret;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Unref : Character with Unreferenced;
   begin
      --  Set a 115_200 baudrate
      UART0_Periph.BAUDRATE := 16#01D7E000#;

      --  Set TX io pin
      UART0_Periph.PSELTXD := 24;

      --  Set RX io pin
      UART0_Periph.PSELRXD := 25;

      --  Hardware Flow Control disabled
      UART0_Periph.CONFIG.HWFC := Disabled;

      --  Parity disabled
      UART0_Periph.CONFIG.PARITY := Excluded;

      --  Enable the peripheral
      UART0_Periph.ENABLE.ENABLE := Enabled;

      --  Clear events
      UART0_Periph.EVENTS_RXDRDY := 0;
      UART0_Periph.EVENTS_TXDRDY := 0;

      --  Start TX and RX
      UART0_Periph.TASKS_STARTRX := 1;
      UART0_Periph.TASKS_STARTTX := 1;

      --  Send a first character to start the TXREADY events (See nRF51 Series
      --  Reference Manual Version 3.0 Figure 68: UART transmission)
      Put (ASCII.NUL);

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return UART0_Periph.EVENTS_RXDRDY /= 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return UART0_Periph.EVENTS_TXDRDY /= 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      UART0_Periph.EVENTS_TXDRDY := 0;

      --  Send the character
      UART0_Periph.TXD.TXD := Byte (Character'Pos (C));
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
