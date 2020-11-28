--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from STM32F103.svd

--  This is a version for the STM32F103 MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt         : constant Interrupt_ID := -1;

   --  Window Watchdog interrupt
   WWDG_Interrupt             : constant Interrupt_ID := 0;

   --  PVD through EXTI line detection interrupt
   PVD_Interrupt              : constant Interrupt_ID := 1;

   --  Tamper interrupt
   TAMPER_Interrupt           : constant Interrupt_ID := 2;

   --  RTC global interrupt
   RTC_Interrupt              : constant Interrupt_ID := 3;

   --  Flash global interrupt
   FLASH_Interrupt            : constant Interrupt_ID := 4;

   --  RCC global interrupt
   RCC_Interrupt              : constant Interrupt_ID := 5;

   --  EXTI Line0 interrupt
   EXTI0_Interrupt            : constant Interrupt_ID := 6;

   --  EXTI Line1 interrupt
   EXTI1_Interrupt            : constant Interrupt_ID := 7;

   --  EXTI Line2 interrupt
   EXTI2_Interrupt            : constant Interrupt_ID := 8;

   --  EXTI Line3 interrupt
   EXTI3_Interrupt            : constant Interrupt_ID := 9;

   --  EXTI Line4 interrupt
   EXTI4_Interrupt            : constant Interrupt_ID := 10;

   --  DMA1 Channel1 global interrupt
   DMA1_Channel1_Interrupt    : constant Interrupt_ID := 11;

   --  DMA1 Channel2 global interrupt
   DMA1_Channel2_Interrupt    : constant Interrupt_ID := 12;

   --  DMA1 Channel3 global interrupt
   DMA1_Channel3_Interrupt    : constant Interrupt_ID := 13;

   --  DMA1 Channel4 global interrupt
   DMA1_Channel4_Interrupt    : constant Interrupt_ID := 14;

   --  DMA1 Channel5 global interrupt
   DMA1_Channel5_Interrupt    : constant Interrupt_ID := 15;

   --  DMA1 Channel6 global interrupt
   DMA1_Channel6_Interrupt    : constant Interrupt_ID := 16;

   --  DMA1 Channel7 global interrupt
   DMA1_Channel7_Interrupt    : constant Interrupt_ID := 17;

   --  ADC1 and ADC2 global interrupt
   ADC1_2_Interrupt           : constant Interrupt_ID := 18;

   --  USB High Priority or CAN TX interrupts
   USB_HP_CAN_TX_Interrupt    : constant Interrupt_ID := 19;

   --  USB Low Priority or CAN RX0 interrupts
   USB_LP_CAN_RX0_Interrupt   : constant Interrupt_ID := 20;

   --  CAN RX1 interrupt
   CAN_RX1_Interrupt          : constant Interrupt_ID := 21;

   --  CAN SCE interrupt
   CAN_SCE_Interrupt          : constant Interrupt_ID := 22;

   --  EXTI Line[9:5] interrupts
   EXTI9_5_Interrupt          : constant Interrupt_ID := 23;

   --  TIM1 Break interrupt
   TIM1_BRK_Interrupt         : constant Interrupt_ID := 24;

   --  TIM1 Update interrupt
   TIM1_UP_Interrupt          : constant Interrupt_ID := 25;

   --  TIM1 Trigger and Commutation interrupts
   TIM1_TRG_COM_Interrupt     : constant Interrupt_ID := 26;

   --  TIM1 Capture Compare interrupt
   TIM1_CC_Interrupt          : constant Interrupt_ID := 27;

   --  TIM2 global interrupt
   TIM2_Interrupt             : constant Interrupt_ID := 28;

   --  TIM3 global interrupt
   TIM3_Interrupt             : constant Interrupt_ID := 29;

   --  TIM4 global interrupt
   TIM4_Interrupt             : constant Interrupt_ID := 30;

   --  I2C1 event interrupt
   I2C1_EV_Interrupt          : constant Interrupt_ID := 31;

   --  I2C1 error interrupt
   I2C1_ER_Interrupt          : constant Interrupt_ID := 32;

   --  I2C2 event interrupt
   I2C2_EV_Interrupt          : constant Interrupt_ID := 33;

   --  I2C2 error interrupt
   I2C2_ER_Interrupt          : constant Interrupt_ID := 34;

   --  SPI1 global interrupt
   SPI1_Interrupt             : constant Interrupt_ID := 35;

   --  SPI2 global interrupt
   SPI2_Interrupt             : constant Interrupt_ID := 36;

   --  USART1 global interrupt
   USART1_Interrupt           : constant Interrupt_ID := 37;

   --  USART2 global interrupt
   USART2_Interrupt           : constant Interrupt_ID := 38;

   --  USART3 global interrupt
   USART3_Interrupt           : constant Interrupt_ID := 39;

   --  EXTI Line[15:10] interrupts
   EXTI15_10_Interrupt        : constant Interrupt_ID := 40;

   --  RTC Alarms through EXTI line interrupt
   RTCAlarm_Interrupt         : constant Interrupt_ID := 41;

   --  TIM8 Break interrupt
   TIM8_BRK_Interrupt         : constant Interrupt_ID := 43;

   --  TIM8 Update interrupt
   TIM8_UP_Interrupt          : constant Interrupt_ID := 44;

   --  TIM8 Trigger and Commutation interrupts
   TIM8_TRG_COM_Interrupt     : constant Interrupt_ID := 45;

   --  TIM8 Capture Compare interrupt
   TIM8_CC_Interrupt          : constant Interrupt_ID := 46;

   --  ADC3 global interrupt
   ADC3_Interrupt             : constant Interrupt_ID := 47;

   --  FSMC global interrupt
   FSMC_Interrupt             : constant Interrupt_ID := 48;

   --  SDIO global interrupt
   SDIO_Interrupt             : constant Interrupt_ID := 49;

   --  TIM5 global interrupt
   TIM5_Interrupt             : constant Interrupt_ID := 50;

   --  SPI3 global interrupt
   SPI3_Interrupt             : constant Interrupt_ID := 51;

   --  UART4 global interrupt
   UART4_Interrupt            : constant Interrupt_ID := 52;

   --  UART5 global interrupt
   UART5_Interrupt            : constant Interrupt_ID := 53;

   --  TIM6 global interrupt
   TIM6_Interrupt             : constant Interrupt_ID := 54;

   --  TIM7 global interrupt
   TIM7_Interrupt             : constant Interrupt_ID := 55;

   --  DMA2 Channel1 global interrupt
   DMA2_Channel1_Interrupt    : constant Interrupt_ID := 56;

   --  DMA2 Channel2 global interrupt
   DMA2_Channel2_Interrupt    : constant Interrupt_ID := 57;

   --  DMA2 Channel3 global interrupt
   DMA2_Channel3_Interrupt    : constant Interrupt_ID := 58;

   --  DMA2 Channel4 and DMA2 Channel5 global interrupt
   DMA2_Channel4_5_Interrupt  : constant Interrupt_ID := 59;

   --  FPU global interrupt
   FPU_Interrupt              : constant Interrupt_ID := 81;

end Ada.Interrupts.Names;
