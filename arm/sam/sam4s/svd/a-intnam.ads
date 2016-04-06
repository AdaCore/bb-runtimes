--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAM4SD32C.svd

--  This is a version for the Atmel ATSAM4SD32C device: Cortex-M4
--  Microcontroller with 2MB dual-bank Flash, 160KB SRAM, USB, 100 Pins (refer
--  to http://www.atmel.com/devices/SAM4SD32C.aspx for more) MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  The position of the interrupts are documented as starting at 0.
   --  Unfortunately, Interrupt_Id 0 is reserved and the SysTick interrupt (a
   --  core interrupt) is handled by the runtime like other interrupts. So IRQ
   --  0 is numbered 2 while it is at position 0 in the manual. The offset of 2
   --  is reflected in s-bbbosu.adb by the First_IRQ constant.
   Sys_Tick_Interrupt : constant Interrupt_ID := 1;
   SUPC_Interrupt    : constant Interrupt_ID := 2;
   RSTC_Interrupt    : constant Interrupt_ID := 3;
   RTC_Interrupt     : constant Interrupt_ID := 4;
   RTT_Interrupt     : constant Interrupt_ID := 5;
   WDT_Interrupt     : constant Interrupt_ID := 6;
   PMC_Interrupt     : constant Interrupt_ID := 7;
   EEFC0_Interrupt   : constant Interrupt_ID := 8;
   EEFC1_Interrupt   : constant Interrupt_ID := 9;
   UART0_Interrupt   : constant Interrupt_ID := 10;
   UART1_Interrupt   : constant Interrupt_ID := 11;
   SMC_Interrupt     : constant Interrupt_ID := 12;
   PIOA_Interrupt    : constant Interrupt_ID := 13;
   PIOB_Interrupt    : constant Interrupt_ID := 14;
   PIOC_Interrupt    : constant Interrupt_ID := 15;
   USART0_Interrupt  : constant Interrupt_ID := 16;
   USART1_Interrupt  : constant Interrupt_ID := 17;
   HSMCI_Interrupt   : constant Interrupt_ID := 20;
   TWI0_Interrupt    : constant Interrupt_ID := 21;
   TWI1_Interrupt    : constant Interrupt_ID := 22;
   SPI_Interrupt     : constant Interrupt_ID := 23;
   SSC_Interrupt     : constant Interrupt_ID := 24;
   TC0_Interrupt     : constant Interrupt_ID := 25;
   TC1_Interrupt     : constant Interrupt_ID := 26;
   TC2_Interrupt     : constant Interrupt_ID := 27;
   TC3_Interrupt     : constant Interrupt_ID := 28;
   TC4_Interrupt     : constant Interrupt_ID := 29;
   TC5_Interrupt     : constant Interrupt_ID := 30;
   ADC_Interrupt     : constant Interrupt_ID := 31;
   DACC_Interrupt    : constant Interrupt_ID := 32;
   PWM_Interrupt     : constant Interrupt_ID := 33;
   CRCCU_Interrupt   : constant Interrupt_ID := 34;
   ACC_Interrupt     : constant Interrupt_ID := 35;
   UDP_Interrupt     : constant Interrupt_ID := 36;

end Ada.Interrupts.Names;
