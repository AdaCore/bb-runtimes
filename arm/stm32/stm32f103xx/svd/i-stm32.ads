--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  STM32F103xx
package Interfaces.STM32 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Base type --
   ---------------

   type UInt32 is new Interfaces.Unsigned_32;
   type UInt16 is new Interfaces.Unsigned_16;
   type Byte is new Interfaces.Unsigned_8;
   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   --------------------
   -- Base addresses --
   --------------------

   FSMC_Base : constant System.Address := System'To_Address (16#A0000000#);
   PWR_Base : constant System.Address := System'To_Address (16#40007000#);
   RCC_Base : constant System.Address := System'To_Address (16#40021000#);
   GPIOA_Base : constant System.Address := System'To_Address (16#40010800#);
   GPIOB_Base : constant System.Address := System'To_Address (16#40010C00#);
   GPIOC_Base : constant System.Address := System'To_Address (16#40011000#);
   GPIOD_Base : constant System.Address := System'To_Address (16#40011400#);
   GPIOE_Base : constant System.Address := System'To_Address (16#40011800#);
   GPIOF_Base : constant System.Address := System'To_Address (16#40011C00#);
   GPIOG_Base : constant System.Address := System'To_Address (16#40012000#);
   AFIO_Base : constant System.Address := System'To_Address (16#40010000#);
   EXTI_Base : constant System.Address := System'To_Address (16#40010400#);
   DMA1_Base : constant System.Address := System'To_Address (16#40020000#);
   DMA2_Base : constant System.Address := System'To_Address (16#40020400#);
   SDIO_Base : constant System.Address := System'To_Address (16#40018000#);
   RTC_Base : constant System.Address := System'To_Address (16#40002800#);
   BKP_Base : constant System.Address := System'To_Address (16#40006C04#);
   IWDG_Base : constant System.Address := System'To_Address (16#40003000#);
   WWDG_Base : constant System.Address := System'To_Address (16#40002C00#);
   TIM1_Base : constant System.Address := System'To_Address (16#40012C00#);
   TIM8_Base : constant System.Address := System'To_Address (16#40013400#);
   TIM2_Base : constant System.Address := System'To_Address (16#40000000#);
   TIM3_Base : constant System.Address := System'To_Address (16#40000400#);
   TIM4_Base : constant System.Address := System'To_Address (16#40000800#);
   TIM5_Base : constant System.Address := System'To_Address (16#40000C00#);
   TIM9_Base : constant System.Address := System'To_Address (16#40014C00#);
   TIM12_Base : constant System.Address := System'To_Address (16#40001800#);
   TIM10_Base : constant System.Address := System'To_Address (16#40015000#);
   TIM11_Base : constant System.Address := System'To_Address (16#40015400#);
   TIM13_Base : constant System.Address := System'To_Address (16#40001C00#);
   TIM14_Base : constant System.Address := System'To_Address (16#40002000#);
   TIM6_Base : constant System.Address := System'To_Address (16#40001000#);
   TIM7_Base : constant System.Address := System'To_Address (16#40001400#);
   I2C1_Base : constant System.Address := System'To_Address (16#40005400#);
   I2C2_Base : constant System.Address := System'To_Address (16#40005800#);
   SPI1_Base : constant System.Address := System'To_Address (16#40013000#);
   SPI2_Base : constant System.Address := System'To_Address (16#40003800#);
   SPI3_Base : constant System.Address := System'To_Address (16#40003C00#);
   USART1_Base : constant System.Address := System'To_Address (16#40013800#);
   USART2_Base : constant System.Address := System'To_Address (16#40004400#);
   USART3_Base : constant System.Address := System'To_Address (16#40004800#);
   ADC1_Base : constant System.Address := System'To_Address (16#40012400#);
   ADC2_Base : constant System.Address := System'To_Address (16#40012800#);
   ADC3_Base : constant System.Address := System'To_Address (16#40013C00#);
   CAN_Base : constant System.Address := System'To_Address (16#40006400#);
   DAC_Base : constant System.Address := System'To_Address (16#40007400#);
   DBG_Base : constant System.Address := System'To_Address (16#E0042000#);
   UART4_Base : constant System.Address := System'To_Address (16#40004C00#);
   UART5_Base : constant System.Address := System'To_Address (16#40005000#);
   CRC_Base : constant System.Address := System'To_Address (16#40023000#);
   FLASH_Base : constant System.Address := System'To_Address (16#40022000#);
   NVIC_Base : constant System.Address := System'To_Address (16#E000E000#);
   USB_Base : constant System.Address := System'To_Address (16#40005C00#);

end Interfaces.STM32;
