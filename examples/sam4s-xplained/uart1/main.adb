------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
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
with Interfaces; use Interfaces;
with System;
with Sam4s; use Sam4s;
with Pll;
with Leds;

procedure Main is
   procedure Wait is
   begin
      for I in 1 .. 16#3ffff# loop
         null;
      end loop;
   end Wait;
   PB2 : constant := 2 ** 2;
   PB3 : constant := 2 ** 3;

   PA9  : constant := 2 ** 9;
   PA10 : constant := 2 ** 10;

   --  Uart_Ports : constant := PA9 + PA10;
   Uart_Ports : constant := PB2 + PB3;
begin
   Pll.Init;

   Leds.Init;

   --  Disable watchdog
   WDT.WDT_MR := WDT_MR.WDDIS;

   --  Init uart1
   PMC.PMC_PCER0 := 2 ** UART1_ID + 2 ** PIOB_ID;
   PIOB.PDR := Uart_Ports;
   PIOB.ODR := Uart_Ports;
   PIOB.PUER := Uart_Ports;
   PIOB.MDDR := Uart_Ports;
   PIOB.ABCDSR1 := PIOB.ABCDSR1 and not Uart_Ports;
   PIOB.ABCDSR2 := PIOB.ABCDSR2 and not Uart_Ports;

   UART1.UART_BRGR := 120_000_000 / (16 * 115200);
   UART1.UART_MR := UART_MR.CHMODE_NORMAL or UART_MR.PAR_NO;
   UART1.UART_CR := UART_CR.TXEN or UART_CR.RXEN;

   for I in 1 .. 10 loop
      while (UART1.UART_SR and UART_SR.TXRDY) = 0 loop
         null;
      end loop;

      UART1.UART_THR := Character'Pos ('H');
   end loop;

   loop
      null;
   end loop;
end Main;
