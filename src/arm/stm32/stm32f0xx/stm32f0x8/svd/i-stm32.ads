--
--  Copyright (C) 2020, AdaCore
--

--  This spec has been automatically generated from STM32F0x8.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  STM32F0x8
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

   CRC_Base : constant System.Address := System'To_Address (16#40023000#);
   GPIOF_Base : constant System.Address := System'To_Address (16#48001400#);
   GPIOD_Base : constant System.Address := System'To_Address (16#48000C00#);
   GPIOC_Base : constant System.Address := System'To_Address (16#48000800#);
   GPIOB_Base : constant System.Address := System'To_Address (16#48000400#);
   GPIOE_Base : constant System.Address := System'To_Address (16#48001000#);
   GPIOA_Base : constant System.Address := System'To_Address (16#48000000#);
   SPI1_Base : constant System.Address := System'To_Address (16#40013000#);
   SPI2_Base : constant System.Address := System'To_Address (16#40003800#);
   PWR_Base : constant System.Address := System'To_Address (16#40007000#);
   I2C1_Base : constant System.Address := System'To_Address (16#40005400#);
   I2C2_Base : constant System.Address := System'To_Address (16#40005800#);
   IWDG_Base : constant System.Address := System'To_Address (16#40003000#);
   WWDG_Base : constant System.Address := System'To_Address (16#40002C00#);
   TIM1_Base : constant System.Address := System'To_Address (16#40012C00#);
   TIM2_Base : constant System.Address := System'To_Address (16#40000000#);
   TIM3_Base : constant System.Address := System'To_Address (16#40000400#);
   TIM14_Base : constant System.Address := System'To_Address (16#40002000#);
   TIM6_Base : constant System.Address := System'To_Address (16#40001000#);
   TIM7_Base : constant System.Address := System'To_Address (16#40001400#);
   EXTI_Base : constant System.Address := System'To_Address (16#40010400#);
   NVIC_Base : constant System.Address := System'To_Address (16#E000E100#);
   DMA1_Base : constant System.Address := System'To_Address (16#40020000#);
   DMA2_Base : constant System.Address := System'To_Address (16#40020400#);
   RCC_Base : constant System.Address := System'To_Address (16#40021000#);
   SYSCFG_COMP_Base : constant System.Address := System'To_Address (16#40010000#);
   ADC_Base : constant System.Address := System'To_Address (16#40012400#);
   USART1_Base : constant System.Address := System'To_Address (16#40013800#);
   USART2_Base : constant System.Address := System'To_Address (16#40004400#);
   USART3_Base : constant System.Address := System'To_Address (16#40004800#);
   USART4_Base : constant System.Address := System'To_Address (16#40004C00#);
   USART6_Base : constant System.Address := System'To_Address (16#40011400#);
   USART7_Base : constant System.Address := System'To_Address (16#40011800#);
   USART8_Base : constant System.Address := System'To_Address (16#40011C00#);
   USART5_Base : constant System.Address := System'To_Address (16#40005000#);
   RTC_Base : constant System.Address := System'To_Address (16#40002800#);
   TIM15_Base : constant System.Address := System'To_Address (16#40014000#);
   TIM16_Base : constant System.Address := System'To_Address (16#40014400#);
   TIM17_Base : constant System.Address := System'To_Address (16#40014800#);
   TSC_Base : constant System.Address := System'To_Address (16#40024000#);
   CEC_Base : constant System.Address := System'To_Address (16#40007800#);
   Flash_Base : constant System.Address := System'To_Address (16#40022000#);
   DBGMCU_Base : constant System.Address := System'To_Address (16#40015800#);
   USB_Base : constant System.Address := System'To_Address (16#40005C00#);
   CRS_Base : constant System.Address := System'To_Address (16#40006C00#);
   CAN_Base : constant System.Address := System'To_Address (16#40006400#);
   DAC_Base : constant System.Address := System'To_Address (16#40007400#);
   SCB_Base : constant System.Address := System'To_Address (16#E000ED00#);
   STK_Base : constant System.Address := System'To_Address (16#E000E010#);

end Interfaces.STM32;
