------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2014, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the version for Cortex M4 SAM4S targets

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  The SAM4S datasheet defines peripheral indentifiers in Table 11-1
   --  (page 41 of 1100E–ATARM–24-Jul-13). The meaningful number, the ID
   --  starts at 0. Unfortunately, Interrupt_ID 0 is reserved and the SysTick
   --  interrupt (a core interrupt) is handled by the runtime like other
   --  interrupts. So the first interrupt (supply controler) is numbered 2
   --  while its ID is 0 in the manual. The offset of 2 is reflected in
   --  s-bbbosu-stm32f4.adb by the First_IRQ constant.

   Sys_Tick_Interrupt : constant Interrupt_ID := 1;
   SUPC_Interrupt     : constant Interrupt_ID := 2;
   RSTC_Interrupt     : constant Interrupt_ID := 3;
   RTC_Interrupt      : constant Interrupt_ID := 4;
   RTT_Interrupt      : constant Interrupt_ID := 5;
   WDT_Interrupt      : constant Interrupt_ID := 6;
   PMC_Interrupt      : constant Interrupt_ID := 7;
   EEFC0_Interrupt    : constant Interrupt_ID := 8;
   EEFC1_Interrupt    : constant Interrupt_ID := 9;
   UART0_Interrupt    : constant Interrupt_ID := 10;
   UART1_Interrupt    : constant Interrupt_ID := 11;
   SMC_Interrupt      : constant Interrupt_ID := 12;
   PIOA_Interrupt     : constant Interrupt_ID := 13;
   PIOB_Interrupt     : constant Interrupt_ID := 14;
   PIOC_Interrupt     : constant Interrupt_ID := 15;
   USART0_Interrupt   : constant Interrupt_ID := 16;
   USART1_Interrupt   : constant Interrupt_ID := 17;

   GSMCI_Interrupt    : constant Interrupt_ID := 20;
   TWI0_Interrupt     : constant Interrupt_ID := 21;
   TWO1_Interrupt     : constant Interrupt_ID := 22;
   SPI_Interrupt      : constant Interrupt_ID := 23;
   SSC_Interrupt      : constant Interrupt_ID := 24;
   TC0_Interrupt      : constant Interrupt_ID := 25;
   TC1_Interrupt      : constant Interrupt_ID := 26;
   TC2_Interrupt      : constant Interrupt_ID := 27;
   TC3_Interrupt      : constant Interrupt_ID := 28;
   TC4_Interrupt      : constant Interrupt_ID := 29;
   TC5_Interrupt      : constant Interrupt_ID := 30;
   ADC_Interrupt      : constant Interrupt_ID := 31;
   DACC_Interrupt     : constant Interrupt_ID := 32;
   PWM_Interrupt      : constant Interrupt_ID := 33;
   CRCCU_Interrupt    : constant Interrupt_ID := 34;
   ACC_Interrupt      : constant Interrupt_ID := 35;
   UDP_Interrupt      : constant Interrupt_ID := 36;

end Ada.Interrupts.Names;
