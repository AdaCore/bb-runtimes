--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

--  This is a version for the Atmel ATSAMG55J19 Microcontroller MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt : constant Interrupt_ID := -1;
   SUPC_Interrupt     : constant Interrupt_ID := 0;
   RSTC_Interrupt     : constant Interrupt_ID := 1;
   RTC_Interrupt      : constant Interrupt_ID := 2;
   RTT_Interrupt      : constant Interrupt_ID := 3;
   WDT_Interrupt      : constant Interrupt_ID := 4;
   PMC_Interrupt      : constant Interrupt_ID := 5;
   EFC_Interrupt      : constant Interrupt_ID := 6;
   SPI7_Interrupt     : constant Interrupt_ID := 7;
   TWI7_Interrupt     : constant Interrupt_ID := 7;
   USART7_Interrupt   : constant Interrupt_ID := 7;
   SPI0_Interrupt     : constant Interrupt_ID := 8;
   USART0_Interrupt   : constant Interrupt_ID := 8;
   SPI1_Interrupt     : constant Interrupt_ID := 9;
   TWI1_Interrupt     : constant Interrupt_ID := 9;
   USART1_Interrupt   : constant Interrupt_ID := 9;
   PIOA_Interrupt     : constant Interrupt_ID := 11;
   PIOB_Interrupt     : constant Interrupt_ID := 12;
   PDMIC0_Interrupt   : constant Interrupt_ID := 13;
   SPI2_Interrupt     : constant Interrupt_ID := 14;
   TWI2_Interrupt     : constant Interrupt_ID := 14;
   USART2_Interrupt   : constant Interrupt_ID := 14;
   MEM2MEM_Interrupt  : constant Interrupt_ID := 15;
   I2SC0_Interrupt    : constant Interrupt_ID := 16;
   I2SC1_Interrupt    : constant Interrupt_ID := 17;
   PDMIC1_Interrupt   : constant Interrupt_ID := 18;
   SPI3_Interrupt     : constant Interrupt_ID := 19;
   TWI3_Interrupt     : constant Interrupt_ID := 19;
   USART3_Interrupt   : constant Interrupt_ID := 19;
   SPI4_Interrupt     : constant Interrupt_ID := 20;
   TWI4_Interrupt     : constant Interrupt_ID := 20;
   USART4_Interrupt   : constant Interrupt_ID := 20;
   SPI5_Interrupt     : constant Interrupt_ID := 21;
   TWI5_Interrupt     : constant Interrupt_ID := 21;
   USART5_Interrupt   : constant Interrupt_ID := 21;
   SPI6_Interrupt     : constant Interrupt_ID := 22;
   TWI6_Interrupt     : constant Interrupt_ID := 22;
   USART6_Interrupt   : constant Interrupt_ID := 22;
   TC0_Interrupt      : constant Interrupt_ID := 23;
   TC1_Interrupt      : constant Interrupt_ID := 24;
   TC2_Interrupt      : constant Interrupt_ID := 25;
   TC3_Interrupt      : constant Interrupt_ID := 26;
   TC4_Interrupt      : constant Interrupt_ID := 27;
   TC5_Interrupt      : constant Interrupt_ID := 28;
   ADC_Interrupt      : constant Interrupt_ID := 29;
   FPU_Interrupt      : constant Interrupt_ID := 30;
   WKUP0_Interrupt    : constant Interrupt_ID := 31;
   WKUP1_Interrupt    : constant Interrupt_ID := 32;
   WKUP2_Interrupt    : constant Interrupt_ID := 33;
   WKUP3_Interrupt    : constant Interrupt_ID := 34;
   WKUP4_Interrupt    : constant Interrupt_ID := 35;
   WKUP5_Interrupt    : constant Interrupt_ID := 36;
   WKUP6_Interrupt    : constant Interrupt_ID := 37;
   WKUP7_Interrupt    : constant Interrupt_ID := 38;
   WKUP8_Interrupt    : constant Interrupt_ID := 39;
   WKUP9_Interrupt    : constant Interrupt_ID := 40;
   WKUP10_Interrupt   : constant Interrupt_ID := 41;
   WKUP11_Interrupt   : constant Interrupt_ID := 42;
   WKUP12_Interrupt   : constant Interrupt_ID := 43;
   WKUP13_Interrupt   : constant Interrupt_ID := 44;
   WKUP14_Interrupt   : constant Interrupt_ID := 45;
   WKUP15_Interrupt   : constant Interrupt_ID := 46;
   UHP_Interrupt      : constant Interrupt_ID := 47;
   UDP_Interrupt      : constant Interrupt_ID := 48;
   CRCCU_Interrupt    : constant Interrupt_ID := 49;

end Ada.Interrupts.Names;
