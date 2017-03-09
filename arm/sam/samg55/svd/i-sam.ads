--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Atmel ATSAMG55J19 Microcontroller
package Interfaces.SAM is
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

   I2SC0_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   I2SC1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   FLEXCOM5_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   USART5_Base : constant System.Address :=
     System'To_Address (16#40008200#);
   SPI5_Base : constant System.Address :=
     System'To_Address (16#40008400#);
   TWI5_Base : constant System.Address :=
     System'To_Address (16#40008600#);
   FLEXCOM0_Base : constant System.Address :=
     System'To_Address (16#4000C000#);
   USART0_Base : constant System.Address :=
     System'To_Address (16#4000C200#);
   SPI0_Base : constant System.Address :=
     System'To_Address (16#4000C400#);
   TWI0_Base : constant System.Address :=
     System'To_Address (16#4000C600#);
   TC0_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   TC1_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   FLEXCOM3_Base : constant System.Address :=
     System'To_Address (16#40018000#);
   USART3_Base : constant System.Address :=
     System'To_Address (16#40018200#);
   SPI3_Base : constant System.Address :=
     System'To_Address (16#40018400#);
   TWI3_Base : constant System.Address :=
     System'To_Address (16#40018600#);
   FLEXCOM4_Base : constant System.Address :=
     System'To_Address (16#4001C000#);
   USART4_Base : constant System.Address :=
     System'To_Address (16#4001C200#);
   SPI4_Base : constant System.Address :=
     System'To_Address (16#4001C400#);
   TWI4_Base : constant System.Address :=
     System'To_Address (16#4001C600#);
   FLEXCOM1_Base : constant System.Address :=
     System'To_Address (16#40020000#);
   USART1_Base : constant System.Address :=
     System'To_Address (16#40020200#);
   SPI1_Base : constant System.Address :=
     System'To_Address (16#40020400#);
   TWI1_Base : constant System.Address :=
     System'To_Address (16#40020600#);
   FLEXCOM2_Base : constant System.Address :=
     System'To_Address (16#40024000#);
   USART2_Base : constant System.Address :=
     System'To_Address (16#40024200#);
   SPI2_Base : constant System.Address :=
     System'To_Address (16#40024400#);
   TWI2_Base : constant System.Address :=
     System'To_Address (16#40024600#);
   MEM2MEM_Base : constant System.Address :=
     System'To_Address (16#40028000#);
   PDMIC0_Base : constant System.Address :=
     System'To_Address (16#4002C000#);
   PDMIC1_Base : constant System.Address :=
     System'To_Address (16#40030000#);
   FLEXCOM7_Base : constant System.Address :=
     System'To_Address (16#40034000#);
   USART7_Base : constant System.Address :=
     System'To_Address (16#40034200#);
   SPI7_Base : constant System.Address :=
     System'To_Address (16#40034400#);
   TWI7_Base : constant System.Address :=
     System'To_Address (16#40034600#);
   ADC_Base : constant System.Address :=
     System'To_Address (16#40038000#);
   CMCC_Base : constant System.Address :=
     System'To_Address (16#4003C000#);
   FLEXCOM6_Base : constant System.Address :=
     System'To_Address (16#40040000#);
   USART6_Base : constant System.Address :=
     System'To_Address (16#40040200#);
   SPI6_Base : constant System.Address :=
     System'To_Address (16#40040400#);
   TWI6_Base : constant System.Address :=
     System'To_Address (16#40040600#);
   UDP_Base : constant System.Address :=
     System'To_Address (16#40044000#);
   CRCCU_Base : constant System.Address :=
     System'To_Address (16#40048000#);
   UHP_Base : constant System.Address :=
     System'To_Address (16#4004C000#);
   MATRIX_Base : constant System.Address :=
     System'To_Address (16#400E0200#);
   PMC_Base : constant System.Address :=
     System'To_Address (16#400E0400#);
   CHIPID_Base : constant System.Address :=
     System'To_Address (16#400E0740#);
   EFC_Base : constant System.Address :=
     System'To_Address (16#400E0A00#);
   PIOA_Base : constant System.Address :=
     System'To_Address (16#400E0E00#);
   PIOB_Base : constant System.Address :=
     System'To_Address (16#400E1000#);
   RSTC_Base : constant System.Address :=
     System'To_Address (16#400E1400#);
   SUPC_Base : constant System.Address :=
     System'To_Address (16#400E1410#);
   RTT_Base : constant System.Address :=
     System'To_Address (16#400E1430#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#400E1450#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#400E1460#);
   GPBR_Base : constant System.Address :=
     System'To_Address (16#400E1490#);
   FPU_Base : constant System.Address :=
     System'To_Address (16#E000EF34#);
   MPU_Base : constant System.Address :=
     System'To_Address (16#4000D000#);
   NVIC_Base : constant System.Address :=
     System'To_Address (16#E000E100#);
   SCB_Base : constant System.Address :=
     System'To_Address (16#E000E000#);
   SYST_Base : constant System.Address :=
     System'To_Address (16#E000E010#);

end Interfaces.SAM;
