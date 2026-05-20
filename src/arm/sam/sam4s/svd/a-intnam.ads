--
--  Copyright (C) 2017, AdaCore
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

   --  System tick
   Sys_Tick_Interrupt : constant Interrupt_ID := -1;
   SUPC_Interrupt    : constant Interrupt_ID := 0;
   RSTC_Interrupt    : constant Interrupt_ID := 1;
   RTC_Interrupt     : constant Interrupt_ID := 2;
   RTT_Interrupt     : constant Interrupt_ID := 3;
   WDT_Interrupt     : constant Interrupt_ID := 4;
   PMC_Interrupt     : constant Interrupt_ID := 5;
   EEFC0_Interrupt   : constant Interrupt_ID := 6;
   EEFC1_Interrupt   : constant Interrupt_ID := 7;
   UART0_Interrupt   : constant Interrupt_ID := 8;
   UART1_Interrupt   : constant Interrupt_ID := 9;
   SMC_Interrupt     : constant Interrupt_ID := 10;
   PIOA_Interrupt    : constant Interrupt_ID := 11;
   PIOB_Interrupt    : constant Interrupt_ID := 12;
   PIOC_Interrupt    : constant Interrupt_ID := 13;
   USART0_Interrupt  : constant Interrupt_ID := 14;
   USART1_Interrupt  : constant Interrupt_ID := 15;
   HSMCI_Interrupt   : constant Interrupt_ID := 18;
   TWI0_Interrupt    : constant Interrupt_ID := 19;
   TWI1_Interrupt    : constant Interrupt_ID := 20;
   SPI_Interrupt     : constant Interrupt_ID := 21;
   SSC_Interrupt     : constant Interrupt_ID := 22;
   TC0_Interrupt     : constant Interrupt_ID := 23;
   TC1_Interrupt     : constant Interrupt_ID := 24;
   TC2_Interrupt     : constant Interrupt_ID := 25;
   TC3_Interrupt     : constant Interrupt_ID := 26;
   TC4_Interrupt     : constant Interrupt_ID := 27;
   TC5_Interrupt     : constant Interrupt_ID := 28;
   ADC_Interrupt     : constant Interrupt_ID := 29;
   DACC_Interrupt    : constant Interrupt_ID := 30;
   PWM_Interrupt     : constant Interrupt_ID := 31;
   CRCCU_Interrupt   : constant Interrupt_ID := 32;
   ACC_Interrupt     : constant Interrupt_ID := 33;
   UDP_Interrupt     : constant Interrupt_ID := 34;

end Ada.Interrupts.Names;
