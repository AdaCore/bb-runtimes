--
--  Copyright (C) 2020, AdaCore
--

--  This spec has been automatically generated from STM32F401.svd

--  This is a version for the STM32F401 MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt            : constant Interrupt_ID := -1;

   --  PVD through EXTI line detection interrupt
   PVD_Interrupt                 : constant Interrupt_ID := 1;

   --  Tamper and TimeStamp interrupts through the EXTI line
   TAMP_STAMP_Interrupt          : constant Interrupt_ID := 2;

   --  RTC Wakeup interrupt through the EXTI line
   RTC_WKUP_Interrupt            : constant Interrupt_ID := 3;

   --  FLASH global interrupt
   FLASH_Interrupt               : constant Interrupt_ID := 4;

   --  RCC global interrupt
   RCC_Interrupt                 : constant Interrupt_ID := 5;

   --  EXTI Line0 interrupt
   EXTI0_Interrupt               : constant Interrupt_ID := 6;

   --  EXTI Line1 interrupt
   EXTI1_Interrupt               : constant Interrupt_ID := 7;

   --  EXTI Line2 interrupt
   EXTI2_Interrupt               : constant Interrupt_ID := 8;

   --  EXTI Line3 interrupt
   EXTI3_Interrupt               : constant Interrupt_ID := 9;

   --  EXTI Line4 interrupt
   EXTI4_Interrupt               : constant Interrupt_ID := 10;

   --  ADC1 global interrupt
   ADC_Interrupt                 : constant Interrupt_ID := 18;

   --  EXTI Line[9:5] interrupts
   EXTI9_5_Interrupt             : constant Interrupt_ID := 23;

   --  TIM1 Break interrupt and TIM9 global interrupt
   TIM1_BRK_TIM9_Interrupt       : constant Interrupt_ID := 24;

   --  TIM1 Update interrupt and TIM10 global interrupt
   TIM1_UP_TIM10_Interrupt       : constant Interrupt_ID := 25;

   --  TIM1 Trigger and Commutation interrupts and TIM11 global interrupt
   TIM1_TRG_COM_TIM11_Interrupt  : constant Interrupt_ID := 26;

   --  TIM1 Capture Compare interrupt
   TIM1_CC_Interrupt             : constant Interrupt_ID := 27;

   --  TIM2 global interrupt
   TIM2_Interrupt                : constant Interrupt_ID := 28;

   --  TIM3 global interrupt
   TIM3_Interrupt                : constant Interrupt_ID := 29;

   --  I2C1 event interrupt
   I2C1_EV_Interrupt             : constant Interrupt_ID := 31;

   --  I2C1 error interrupt
   I2C1_ER_Interrupt             : constant Interrupt_ID := 32;

   --  I2C2 event interrupt
   I2C2_EV_Interrupt             : constant Interrupt_ID := 33;

   --  I2C2 error interrupt
   I2C2_ER_Interrupt             : constant Interrupt_ID := 34;

   --  SPI1 global interrupt
   SPI1_Interrupt                : constant Interrupt_ID := 35;

   --  SPI2 global interrupt
   SPI2_Interrupt                : constant Interrupt_ID := 36;

   --  EXTI Line[15:10] interrupts
   EXTI15_10_Interrupt           : constant Interrupt_ID := 40;

   --  RTC Alarms (A and B) through EXTI line interrupt
   RTC_Alarm_Interrupt           : constant Interrupt_ID := 41;

   --  USB On-The-Go FS Wakeup through EXTI line interrupt
   OTG_FS_WKUP_Interrupt         : constant Interrupt_ID := 42;

   --  SDIO global interrupt
   SDIO_Interrupt                : constant Interrupt_ID := 49;

   --  SPI3 global interrupt
   SPI3_Interrupt                : constant Interrupt_ID := 51;

   --  USB On The Go FS global interrupt
   OTG_FS_Interrupt              : constant Interrupt_ID := 67;

   --  I2C3 event interrupt
   I2C3_EV_Interrupt             : constant Interrupt_ID := 72;

   --  I2C3 error interrupt
   I2C3_ER_Interrupt             : constant Interrupt_ID := 73;

   --  FPU global interrupt
   FPU_Interrupt                 : constant Interrupt_ID := 81;

   --  SPI4 global interrupt
   SPI4_Interrupt                : constant Interrupt_ID := 84;

end Ada.Interrupts.Names;
