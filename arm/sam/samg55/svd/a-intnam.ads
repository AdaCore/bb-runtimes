--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

--  This is a version for the Atmel ATSAMG55J19 Microcontroller MCU
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
   SUPC_Interrupt     : constant Interrupt_ID := 2;
   RSTC_Interrupt     : constant Interrupt_ID := 3;
   RTC_Interrupt      : constant Interrupt_ID := 4;
   RTT_Interrupt      : constant Interrupt_ID := 5;
   WDT_Interrupt      : constant Interrupt_ID := 6;
   PMC_Interrupt      : constant Interrupt_ID := 7;
   EFC_Interrupt      : constant Interrupt_ID := 8;
   SPI7_Interrupt     : constant Interrupt_ID := 9;
   TWI7_Interrupt     : constant Interrupt_ID := 9;
   USART7_Interrupt   : constant Interrupt_ID := 9;
   SPI0_Interrupt     : constant Interrupt_ID := 10;
   USART0_Interrupt   : constant Interrupt_ID := 10;
   SPI1_Interrupt     : constant Interrupt_ID := 11;
   TWI1_Interrupt     : constant Interrupt_ID := 11;
   USART1_Interrupt   : constant Interrupt_ID := 11;
   PIOA_Interrupt     : constant Interrupt_ID := 13;
   PIOB_Interrupt     : constant Interrupt_ID := 14;
   PDMIC0_Interrupt   : constant Interrupt_ID := 15;
   SPI2_Interrupt     : constant Interrupt_ID := 16;
   TWI2_Interrupt     : constant Interrupt_ID := 16;
   USART2_Interrupt   : constant Interrupt_ID := 16;
   MEM2MEM_Interrupt  : constant Interrupt_ID := 17;
   I2SC0_Interrupt    : constant Interrupt_ID := 18;
   I2SC1_Interrupt    : constant Interrupt_ID := 19;
   PDMIC1_Interrupt   : constant Interrupt_ID := 20;
   SPI3_Interrupt     : constant Interrupt_ID := 21;
   TWI3_Interrupt     : constant Interrupt_ID := 21;
   USART3_Interrupt   : constant Interrupt_ID := 21;
   SPI4_Interrupt     : constant Interrupt_ID := 22;
   TWI4_Interrupt     : constant Interrupt_ID := 22;
   USART4_Interrupt   : constant Interrupt_ID := 22;
   SPI5_Interrupt     : constant Interrupt_ID := 23;
   TWI5_Interrupt     : constant Interrupt_ID := 23;
   USART5_Interrupt   : constant Interrupt_ID := 23;
   SPI6_Interrupt     : constant Interrupt_ID := 24;
   TWI6_Interrupt     : constant Interrupt_ID := 24;
   USART6_Interrupt   : constant Interrupt_ID := 24;
   TC0_Interrupt      : constant Interrupt_ID := 25;
   TC1_Interrupt      : constant Interrupt_ID := 26;
   TC2_Interrupt      : constant Interrupt_ID := 27;
   TC3_Interrupt      : constant Interrupt_ID := 28;
   TC4_Interrupt      : constant Interrupt_ID := 29;
   TC5_Interrupt      : constant Interrupt_ID := 30;
   ADC_Interrupt      : constant Interrupt_ID := 31;
   FPU_Interrupt      : constant Interrupt_ID := 32;
   WKUP0_Interrupt    : constant Interrupt_ID := 33;
   WKUP1_Interrupt    : constant Interrupt_ID := 34;
   WKUP2_Interrupt    : constant Interrupt_ID := 35;
   WKUP3_Interrupt    : constant Interrupt_ID := 36;
   WKUP4_Interrupt    : constant Interrupt_ID := 37;
   WKUP5_Interrupt    : constant Interrupt_ID := 38;
   WKUP6_Interrupt    : constant Interrupt_ID := 39;
   WKUP7_Interrupt    : constant Interrupt_ID := 40;
   WKUP8_Interrupt    : constant Interrupt_ID := 41;
   WKUP9_Interrupt    : constant Interrupt_ID := 42;
   WKUP10_Interrupt   : constant Interrupt_ID := 43;
   WKUP11_Interrupt   : constant Interrupt_ID := 44;
   WKUP12_Interrupt   : constant Interrupt_ID := 45;
   WKUP13_Interrupt   : constant Interrupt_ID := 46;
   WKUP14_Interrupt   : constant Interrupt_ID := 47;
   WKUP15_Interrupt   : constant Interrupt_ID := 48;
   UHP_Interrupt      : constant Interrupt_ID := 49;
   UDP_Interrupt      : constant Interrupt_ID := 50;
   CRCCU_Interrupt    : constant Interrupt_ID := 51;

end Ada.Interrupts.Names;
