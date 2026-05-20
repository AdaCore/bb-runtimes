--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32L562.svd

--  This is a version for the STM32L562 MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt       : constant Interrupt_ID := -1;

   --  Window Watchdog interrupt
   WWDG_Interrupt           : constant Interrupt_ID := 0;

   --  PVD/PVM1/PVM2/PVM3/PVM4 through EXTI
   PVD_PVM_Interrupt        : constant Interrupt_ID := 1;

   --  RTC global interrupts (EXTI line 17)
   RTC_Interrupt            : constant Interrupt_ID := 2;

   --  RTC secure global interrupts (EXTI line 18)
   RTC_S_Interrupt          : constant Interrupt_ID := 3;

   --  TAMPTamper global interrupt (EXTI line 19)
   TAMP_Interrupt           : constant Interrupt_ID := 4;

   --  Tamper secure global interrupt (EXTI line 20)
   TAMP_S_Interrupt         : constant Interrupt_ID := 5;

   --  Flash global interrupt
   FLASH_Interrupt          : constant Interrupt_ID := 6;

   --  Flash memory secure global interrupt
   FLASH_S_Interrupt        : constant Interrupt_ID := 7;

   --  TZIC secure global interrupt
   GTZC_Interrupt           : constant Interrupt_ID := 8;

   --  RCC global interrupt
   RCC_Interrupt            : constant Interrupt_ID := 9;

   --  RCC SECURE GLOBAL INTERRUPT
   RCC_S_Interrupt          : constant Interrupt_ID := 10;

   --  EXTI line0 interrupt
   EXTI0_Interrupt          : constant Interrupt_ID := 11;

   --  EXTI line1 interrupt
   EXTI1_Interrupt          : constant Interrupt_ID := 12;

   --  EXTI line2 interrupt
   EXTI2_Interrupt          : constant Interrupt_ID := 13;

   --  EXTI line3 interrupt
   EXTI3_Interrupt          : constant Interrupt_ID := 14;

   --  EXTI line4 interrupt
   EXTI4_Interrupt          : constant Interrupt_ID := 15;

   --  EXTI line5 interrupt
   EXTI5_Interrupt          : constant Interrupt_ID := 16;

   --  EXTI line6 interrupt
   EXTI6_Interrupt          : constant Interrupt_ID := 17;

   --  EXTI line7 interrupt
   EXTI7_Interrupt          : constant Interrupt_ID := 18;

   --  EXTI line8 interrupt
   EXTI8_Interrupt          : constant Interrupt_ID := 19;

   --  EXTI line9 interrupt
   EXTI9_Interrupt          : constant Interrupt_ID := 20;

   --  EXTI line10 interrupt
   EXTI10_Interrupt         : constant Interrupt_ID := 21;

   --  EXTI line11 interrupt
   EXTI11_Interrupt         : constant Interrupt_ID := 22;

   --  EXTI line12 interrupt
   EXTI12_Interrupt         : constant Interrupt_ID := 23;

   --  EXTI line13 interrupt
   EXTI13_Interrupt         : constant Interrupt_ID := 24;

   --  EXTI line14 interrupt
   EXTI14_Interrupt         : constant Interrupt_ID := 25;

   --  EXTI line15 interrupt
   EXTI15_Interrupt         : constant Interrupt_ID := 26;

   --  DMAMUX overrun interrupt
   DMAMUX1_OVR_Interrupt    : constant Interrupt_ID := 27;

   --  DMAMUX1 secure overRun interrupt
   DMAMUX1_OVR_S_Interrupt  : constant Interrupt_ID := 28;

   --  DMA1 Channel1 global interrupt
   DMA1_Channel1_Interrupt  : constant Interrupt_ID := 29;

   --  DMA1 Channel2 global interrupt
   DMA1_Channel2_Interrupt  : constant Interrupt_ID := 30;

   --  DMA1 Channel3 interrupt
   DMA1_Channel3_Interrupt  : constant Interrupt_ID := 31;

   --  DMA1 Channel4 interrupt
   DMA1_Channel4_Interrupt  : constant Interrupt_ID := 32;

   --  DMA1 Channel5 interrupt
   DMA1_Channel5_Interrupt  : constant Interrupt_ID := 33;

   --  DMA1 Channel6 interrupt
   DMA1_Channel6_Interrupt  : constant Interrupt_ID := 34;

   --  DMA1 Channel 7 interrupt
   DMA1_Channel7_Interrupt  : constant Interrupt_ID := 35;

   --  DMA1_Channel8
   DMA1_Channel8_Interrupt  : constant Interrupt_ID := 36;

   --  ADC1_2 global interrupt
   ADC1_2_Interrupt         : constant Interrupt_ID := 37;

   --  DAC global interrupt
   DAC_Interrupt            : constant Interrupt_ID := 38;

   --  FDCAN1 Interrupt 0
   FDCAN1_IT0_Interrupt     : constant Interrupt_ID := 39;

   --  FDCAN1 Interrupt 1
   FDCAN1_IT1_Interrupt     : constant Interrupt_ID := 40;

   --  TIM1 Break
   TIM1_BRK_Interrupt       : constant Interrupt_ID := 41;

   --  TIM1 Update
   TIM1_UP_Interrupt        : constant Interrupt_ID := 42;

   --  TIM1 Trigger and Commutation
   TIM1_TRG_COM_Interrupt   : constant Interrupt_ID := 43;

   --  TIM1 Capture Compare interrupt
   TIM1_CC_Interrupt        : constant Interrupt_ID := 44;

   --  TIM2 global interrupt
   TIM2_Interrupt           : constant Interrupt_ID := 45;

   --  TIM3 global interrupt
   TIM3_Interrupt           : constant Interrupt_ID := 46;

   --  TIM4 global interrupt
   TIM4_Interrupt           : constant Interrupt_ID := 47;

   --  TIM5 global interrupt
   TIM5_Interrupt           : constant Interrupt_ID := 48;

   --  TIM6 global interrupt
   TIM6_Interrupt           : constant Interrupt_ID := 49;

   --  TIM7 global interrupt
   TIM7_Interrupt           : constant Interrupt_ID := 50;

   --  TIM8 Break Interrupt
   TIM8_BRK_Interrupt       : constant Interrupt_ID := 51;

   --  TIM8 Update Interrupt
   TIM8_UP_Interrupt        : constant Interrupt_ID := 52;

   --  TIM8 Trigger and Commutation Interrupt
   TIM8_TRG_COM_Interrupt   : constant Interrupt_ID := 53;

   --  TIM8 Capture Compare Interrupt
   TIM8_CC_Interrupt        : constant Interrupt_ID := 54;

   --  I2C1 event interrupt
   I2C1_EV_Interrupt        : constant Interrupt_ID := 55;

   --  I2C1 error interrupt
   I2C1_ER_Interrupt        : constant Interrupt_ID := 56;

   --  I2C2 event interrupt
   I2C2_EV_Interrupt        : constant Interrupt_ID := 57;

   --  I2C2 error interrupt
   I2C2_ER_Interrupt        : constant Interrupt_ID := 58;

   --  SPI1 global interrupt
   SPI1_Interrupt           : constant Interrupt_ID := 59;

   --  SPI2 global interrupt
   SPI2_Interrupt           : constant Interrupt_ID := 60;

   --  USART1 global interrupt
   USART1_Interrupt         : constant Interrupt_ID := 61;

   --  USART2 global interrupt
   USART2_Interrupt         : constant Interrupt_ID := 62;

   --  USART3 global interrupt
   USART3_Interrupt         : constant Interrupt_ID := 63;

   --  UART4 global interrupt
   UART4_Interrupt          : constant Interrupt_ID := 64;

   --  UART5 global interrupt
   UART5_Interrupt          : constant Interrupt_ID := 65;

   --  LPUART1 global interrupt
   LPUART1_Interrupt        : constant Interrupt_ID := 66;

   --  LP TIM1 interrupt
   LPTIM1_Interrupt         : constant Interrupt_ID := 67;

   --  LP TIM2 interrupt
   LPTIM2_Interrupt         : constant Interrupt_ID := 68;

   --  TIM15 global interrupt
   TIM15_Interrupt          : constant Interrupt_ID := 69;

   --  TIM16 global interrupt
   TIM16_Interrupt          : constant Interrupt_ID := 70;

   --  TIM17 global interrupt
   TIM17_Interrupt          : constant Interrupt_ID := 71;

   --  COMP1 and COMP2 interrupts
   COMP_Interrupt           : constant Interrupt_ID := 72;

   --  USB FS global interrupt
   USB_FS_Interrupt         : constant Interrupt_ID := 73;

   --  Clock recovery system global interrupt
   CRS_Interrupt            : constant Interrupt_ID := 74;

   --  FMC global interrupt
   FMC_Interrupt            : constant Interrupt_ID := 75;

   --  OCTOSPI1 global interrupt
   OCTOSPI1_Interrupt       : constant Interrupt_ID := 76;

   --  SDMMC1 global interrupt
   SDMMC1_Interrupt         : constant Interrupt_ID := 78;

   --  DMA2_CH1
   DMA2_CH1_Interrupt       : constant Interrupt_ID := 80;

   --  DMA2_CH2
   DMA2_CH2_Interrupt       : constant Interrupt_ID := 81;

   --  FPU global interrupt
   FPU_Interrupt            : constant Interrupt_ID := 81;

   --  DMA2_CH3
   DMA2_CH3_Interrupt       : constant Interrupt_ID := 82;

   --  DMA2_CH4
   DMA2_CH4_Interrupt       : constant Interrupt_ID := 83;

   --  DMA2_CH5
   DMA2_CH5_Interrupt       : constant Interrupt_ID := 84;

   --  DMA2_CH6
   DMA2_CH6_Interrupt       : constant Interrupt_ID := 85;

   --  DMA2_CH7
   DMA2_CH7_Interrupt       : constant Interrupt_ID := 86;

   --  DMA2_CH8
   DMA2_CH8_Interrupt       : constant Interrupt_ID := 87;

   --  I2C3 event interrupt
   I2C3_EV_Interrupt        : constant Interrupt_ID := 88;

   --  I2C3 error interrupt
   I2C3_ER_Interrupt        : constant Interrupt_ID := 89;

   --  SAI1 global interrupt
   SAI1_Interrupt           : constant Interrupt_ID := 90;

   --  SAI2 global interrupt
   SAI2_Interrupt           : constant Interrupt_ID := 91;

   --  TSC global interrupt
   TSC_Interrupt            : constant Interrupt_ID := 92;

   --  AES global interrupts
   AES_Interrupt            : constant Interrupt_ID := 93;

   --  RNG global interrupt
   RNG_Interrupt            : constant Interrupt_ID := 94;

   --  HASH interrupt
   HASH_Interrupt           : constant Interrupt_ID := 96;

   --  PKA global interrupts
   PKA_Interrupt            : constant Interrupt_ID := 97;

   --  LPTIM3
   LPTIM3_Interrupt         : constant Interrupt_ID := 98;

   --  SPI3
   SPI3_Interrupt           : constant Interrupt_ID := 99;

   --  I2C4 error interrupt
   I2C4_ER_Interrupt        : constant Interrupt_ID := 100;

   --  I2C4 event interrupt
   I2C4_EV_Interrupt        : constant Interrupt_ID := 101;

   --  DFSDM1_FLT0 global interrupt
   DFSDM1_FLT0_Interrupt    : constant Interrupt_ID := 102;

   --  DFSDM1_FLT1 global interrupt
   DFSDM1_FLT1_Interrupt    : constant Interrupt_ID := 103;

   --  DFSDM1_FLT2 global interrupt
   DFSDM1_FLT2_Interrupt    : constant Interrupt_ID := 104;

   --  DFSDM1_FLT3 global interrupt
   DFSDM1_FLT3_Interrupt    : constant Interrupt_ID := 105;

   --  UCPD global interrupt
   UCPD1_Interrupt          : constant Interrupt_ID := 106;

   --  ICACHE
   ICACHE_Interrupt         : constant Interrupt_ID := 107;

   --  OTFDEC1 secure global interrupt
   OTFDEC1_Interrupt        : constant Interrupt_ID := 108;

end Ada.Interrupts.Names;
