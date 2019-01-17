--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAM3X8E.svd

--  This is a version for the Atmel ATSAM3X8E device: Cortex-M3 Microcontroller
--  with 2x256 KB Flash, HS USB, 144 Pins (refer to
--  http://www.atmel.com/devices/SAM3X8E.aspx for more) MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt : constant Interrupt_ID := -1;
   PMC_Interrupt     : constant Interrupt_ID := 5;
   EFC0_Interrupt    : constant Interrupt_ID := 6;
   EFC1_Interrupt    : constant Interrupt_ID := 7;
   UART_Interrupt    : constant Interrupt_ID := 8;
   PIOA_Interrupt    : constant Interrupt_ID := 11;
   PIOB_Interrupt    : constant Interrupt_ID := 12;
   PIOC_Interrupt    : constant Interrupt_ID := 13;
   PIOD_Interrupt    : constant Interrupt_ID := 14;
   USART0_Interrupt  : constant Interrupt_ID := 17;
   USART1_Interrupt  : constant Interrupt_ID := 18;
   USART2_Interrupt  : constant Interrupt_ID := 19;
   USART3_Interrupt  : constant Interrupt_ID := 20;
   HSMCI_Interrupt   : constant Interrupt_ID := 21;
   TWI0_Interrupt    : constant Interrupt_ID := 22;
   TWI1_Interrupt    : constant Interrupt_ID := 23;
   SPI0_Interrupt    : constant Interrupt_ID := 24;
   SSC_Interrupt     : constant Interrupt_ID := 26;
   TC0_Interrupt     : constant Interrupt_ID := 27;
   TC1_Interrupt     : constant Interrupt_ID := 28;
   TC2_Interrupt     : constant Interrupt_ID := 29;
   TC3_Interrupt     : constant Interrupt_ID := 30;
   TC4_Interrupt     : constant Interrupt_ID := 31;
   TC5_Interrupt     : constant Interrupt_ID := 32;
   TC6_Interrupt     : constant Interrupt_ID := 33;
   TC7_Interrupt     : constant Interrupt_ID := 34;
   TC8_Interrupt     : constant Interrupt_ID := 35;
   PWM_Interrupt     : constant Interrupt_ID := 36;
   ADC_Interrupt     : constant Interrupt_ID := 37;
   DACC_Interrupt    : constant Interrupt_ID := 38;
   DMAC_Interrupt    : constant Interrupt_ID := 39;
   UOTGHS_Interrupt  : constant Interrupt_ID := 40;
   TRNG_Interrupt    : constant Interrupt_ID := 41;
   EMAC_Interrupt    : constant Interrupt_ID := 42;
   CAN0_Interrupt    : constant Interrupt_ID := 43;
   CAN1_Interrupt    : constant Interrupt_ID := 44;

end Ada.Interrupts.Names;
