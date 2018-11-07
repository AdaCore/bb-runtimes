--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from FE310.svd

--  This is a version for the E31 CPU Coreplex, high-performance, 32-bit
--  RV32IMAC core
--     MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt       : constant Interrupt_ID := -1;
   Watchdog_Interrupt       : constant Interrupt_ID := 1;
   RTC_Interrupt            : constant Interrupt_ID := 2;
   UART0_Interrupt          : constant Interrupt_ID := 3;
   UART1_Interrupt          : constant Interrupt_ID := 4;
   QSPI0_Interrupt          : constant Interrupt_ID := 5;
   QSPI1_Interrupt          : constant Interrupt_ID := 6;
   QSPI2_Interrupt          : constant Interrupt_ID := 7;
   GPIO_0_Interrupt         : constant Interrupt_ID := 8;
   GPIO_1_Interrupt         : constant Interrupt_ID := 9;
   GPIO_2_Interrupt         : constant Interrupt_ID := 10;
   GPIO_3_Interrupt         : constant Interrupt_ID := 11;
   GPIO_4_Interrupt         : constant Interrupt_ID := 12;
   GPIO_5_Interrupt         : constant Interrupt_ID := 13;
   GPIO_6_Interrupt         : constant Interrupt_ID := 14;
   GPIO_7_Interrupt         : constant Interrupt_ID := 15;
   GPIO_8_Interrupt         : constant Interrupt_ID := 16;
   GPIO_9_Interrupt         : constant Interrupt_ID := 17;
   GPIO_10_Interrupt        : constant Interrupt_ID := 18;
   GPIO_11_Interrupt        : constant Interrupt_ID := 19;
   GPIO_12_Interrupt        : constant Interrupt_ID := 20;
   GPIO_14_Interrupt        : constant Interrupt_ID := 22;
   GPIO_16_Interrupt        : constant Interrupt_ID := 24;
   GPIO_17_Interrupt        : constant Interrupt_ID := 25;
   GPIO_18_Interrupt        : constant Interrupt_ID := 26;
   GPIO_19_Interrupt        : constant Interrupt_ID := 27;
   GPIO_20_Interrupt        : constant Interrupt_ID := 28;
   GPIO_21_Interrupt        : constant Interrupt_ID := 29;
   GPIO_22_Interrupt        : constant Interrupt_ID := 30;
   GPIO_23_Interrupt        : constant Interrupt_ID := 31;
   GPIO_24_Interrupt        : constant Interrupt_ID := 32;
   GPIO_25_Interrupt        : constant Interrupt_ID := 33;
   GPIO_26_Interrupt        : constant Interrupt_ID := 34;
   GPIO_27_Interrupt        : constant Interrupt_ID := 35;
   GPIO_28_Interrupt        : constant Interrupt_ID := 36;
   GPIO_30_Interrupt        : constant Interrupt_ID := 38;
   GPIO_31_Interrupt        : constant Interrupt_ID := 39;
   PWMO_CMP0_Interrupt      : constant Interrupt_ID := 40;
   PWMO_CMP1_Interrupt      : constant Interrupt_ID := 41;
   PWMO_CMP2_Interrupt      : constant Interrupt_ID := 42;
   PWMO_CMP3_Interrupt      : constant Interrupt_ID := 43;
   PWM1_CMP0_Interrupt      : constant Interrupt_ID := 44;
   PWM1_CMP1_Interrupt      : constant Interrupt_ID := 45;
   PWM1_CMP2_Interrupt      : constant Interrupt_ID := 46;
   PWM1_CMP3_Interrupt      : constant Interrupt_ID := 47;
   PWM2_CMP0_Interrupt      : constant Interrupt_ID := 48;
   PWM2_CMP1_Interrupt      : constant Interrupt_ID := 49;
   PWM2_CMP2_Interrupt      : constant Interrupt_ID := 50;
   PWM2_CMP3_Interrupt      : constant Interrupt_ID := 51;

end Ada.Interrupts.Names;
