--
--  Copyright (C) 2020, AdaCore
--

--  This spec has been automatically generated from STM32F0x8.svd

--  This is a version for the STM32F0x8 MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt                     : constant Interrupt_ID := -1;

   --  Window Watchdog interrupt
   WWDG_Interrupt                         : constant Interrupt_ID := 0;

   --  PVD and VDDIO2 supply comparator interrupt
   PVD_Interrupt                          : constant Interrupt_ID := 1;

   --  RTC interrupts
   RTC_Interrupt                          : constant Interrupt_ID := 2;

   --  Flash global interrupt
   FLASH_Interrupt                        : constant Interrupt_ID := 3;

   --  RCC and CRS global interrupts
   RCC_CRS_Interrupt                      : constant Interrupt_ID := 4;

   --  EXTI Line[1:0] interrupts
   EXTI0_1_Interrupt                      : constant Interrupt_ID := 5;

   --  EXTI Line[3:2] interrupts
   EXTI2_3_Interrupt                      : constant Interrupt_ID := 6;

   --  EXTI Line15 and EXTI4 interrupts
   EXTI4_15_Interrupt                     : constant Interrupt_ID := 7;

   --  Touch sensing interrupt
   TSC_Interrupt                          : constant Interrupt_ID := 8;

   --  DMA1 channel 1 interrupt
   DMA1_CH1_Interrupt                     : constant Interrupt_ID := 9;

   --  DMA1 channel 2 and 3 and DMA2 channel 1 and 2 interrupt
   DMA1_CH2_3_DMA2_CH1_2_Interrupt        : constant Interrupt_ID := 10;

   --  DMA1 channel 4, 5, 6 and 7 and DMA2 channel 3, 4 and 5 interrupts
   DMA1_CH4_5_6_7_DMA2_CH3_4_5_Interrupt  : constant Interrupt_ID := 11;

   --  ADC and comparator interrupts
   ADC_COMP_Interrupt                     : constant Interrupt_ID := 12;

   --  TIM1 break, update, trigger and commutation interrupt
   TIM1_BRK_UP_TRG_COM_Interrupt          : constant Interrupt_ID := 13;

   --  TIM1 Capture Compare interrupt
   TIM1_CC_Interrupt                      : constant Interrupt_ID := 14;

   --  TIM2 global interrupt
   TIM2_Interrupt                         : constant Interrupt_ID := 15;

   --  TIM3 global interrupt
   TIM3_Interrupt                         : constant Interrupt_ID := 16;

   --  TIM6 global interrupt and DAC underrun interrupt
   TIM6_DAC_Interrupt                     : constant Interrupt_ID := 17;

   --  TIM7 global interrupt
   TIM7_Interrupt                         : constant Interrupt_ID := 18;

   --  TIM14 global interrupt
   TIM14_Interrupt                        : constant Interrupt_ID := 19;

   --  TIM15 global interrupt
   TIM15_Interrupt                        : constant Interrupt_ID := 20;

   --  TIM16 global interrupt
   TIM16_Interrupt                        : constant Interrupt_ID := 21;

   --  TIM17 global interrupt
   TIM17_Interrupt                        : constant Interrupt_ID := 22;

   --  I2C1 global interrupt
   I2C1_Interrupt                         : constant Interrupt_ID := 23;

   --  I2C2 global interrupt
   I2C2_Interrupt                         : constant Interrupt_ID := 24;

   --  SPI1_global_interrupt
   SPI1_Interrupt                         : constant Interrupt_ID := 25;

   --  SPI2 global interrupt
   SPI2_Interrupt                         : constant Interrupt_ID := 26;

   --  USART1 global interrupt
   USART1_Interrupt                       : constant Interrupt_ID := 27;

   --  USART2 global interrupt
   USART2_Interrupt                       : constant Interrupt_ID := 28;

   --  USART3, USART4, USART5, USART6, USART7, USART8 global interrupt
   USART3_4_5_6_7_8_Interrupt             : constant Interrupt_ID := 29;

   --  CEC and CAN global interrupt
   CEC_CAN_Interrupt                      : constant Interrupt_ID := 30;

   --  USB global interrupt
   USB_Interrupt                          : constant Interrupt_ID := 31;

end Ada.Interrupts.Names;
