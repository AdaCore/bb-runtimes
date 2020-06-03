------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  Minimal version of Text_IO body for use on Unleashed, writes to console

with Interfaces.FE310;      use Interfaces.FE310;
with Interfaces.FE310.UART; use Interfaces.FE310.UART;

package body System.Text_IO is

   UART_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#10010000#);
   --  Unleashed uses the same UART controller as the FE310, but at a different
   --  address.

   Last_RX : Character := ASCII.NUL;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Last_RX;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      --  UART pins are directly accessible, no GPIO function to configure

      UART_Periph.DIV.DIV := UInt16 ((278895001 / 115200)) - 1;
      UART_Periph.TXCTRL.ENABLE := True;
      UART_Periph.RXCTRL.ENABLE := True;

      for I in 1 .. 1_000 loop
         null;
      end loop;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      Reg : constant RXDATA_Register := UART_Periph.RXDATA;
   begin
      if not Reg.EMPTY then
         Last_RX := Character'Val (Reg.DATA);
         return True;
      else
         return False;
      end if;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return not UART_Periph.TXDATA.FULL;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      UART_Periph.TXDATA.DATA := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
