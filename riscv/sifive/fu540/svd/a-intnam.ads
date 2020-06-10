--
--  Copyright (C) 2020, AdaCore
--

--  This spec has been automatically generated from FU540.svd

--  This is a version for the E54 CPU Coreplex, high-performance, 64-bit
--  RV64IMAFDC core
--     MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   Watchdog_Interrupt       : constant Interrupt_ID := 1;
   RTC_Interrupt            : constant Interrupt_ID := 2;
   UART0_Interrupt          : constant Interrupt_ID := 3;
   UART1_Interrupt          : constant Interrupt_ID := 4;
   GPIO_0_Interrupt         : constant Interrupt_ID := 7;
   GPIO_1_Interrupt         : constant Interrupt_ID := 8;
   GPIO_2_Interrupt         : constant Interrupt_ID := 9;
   GPIO_3_Interrupt         : constant Interrupt_ID := 10;
   GPIO_4_Interrupt         : constant Interrupt_ID := 11;
   GPIO_5_Interrupt         : constant Interrupt_ID := 12;
   GPIO_6_Interrupt         : constant Interrupt_ID := 13;
   GPIO_7_Interrupt         : constant Interrupt_ID := 14;
   GPIO_8_Interrupt         : constant Interrupt_ID := 15;
   GPIO_9_Interrupt         : constant Interrupt_ID := 16;
   GPIO_10_Interrupt        : constant Interrupt_ID := 17;
   GPIO_11_Interrupt        : constant Interrupt_ID := 18;
   GPIO_12_Interrupt        : constant Interrupt_ID := 19;
   GPIO_14_Interrupt        : constant Interrupt_ID := 21;
   GPIO_15_Interrupt        : constant Interrupt_ID := 22;
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
