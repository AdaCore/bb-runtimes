--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from ATSAMRH71F20C.svd


with System;

--  Microchip ATSAMRH71F20C Microcontroller
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

   CHIPID_Base : constant System.Address := System'To_Address (16#40100000#);
   FLEXCOM0_Base : constant System.Address := System'To_Address (16#40010000#);
   FLEXCOM1_Base : constant System.Address := System'To_Address (16#40014000#);
   FLEXCOM2_Base : constant System.Address := System'To_Address (16#40018000#);
   FLEXCOM3_Base : constant System.Address := System'To_Address (16#4001C000#);
   FLEXCOM4_Base : constant System.Address := System'To_Address (16#40020000#);
   FLEXCOM5_Base : constant System.Address := System'To_Address (16#40024000#);
   FLEXCOM6_Base : constant System.Address := System'To_Address (16#40028000#);
   FLEXCOM7_Base : constant System.Address := System'To_Address (16#4002C000#);
   FLEXCOM8_Base : constant System.Address := System'To_Address (16#40030000#);
   FLEXCOM9_Base : constant System.Address := System'To_Address (16#40034000#);
   FLEXRAMECC_Base : constant System.Address := System'To_Address (16#40100600#);
   GMAC_Base : constant System.Address := System'To_Address (16#4009C000#);
   HEFC_Base : constant System.Address := System'To_Address (16#40004000#);
   HEMC_Base : constant System.Address := System'To_Address (16#40080000#);
   HSDRAMC_Base : constant System.Address := System'To_Address (16#40082000#);
   HSMC_Base : constant System.Address := System'To_Address (16#40081000#);
   ICM_Base : constant System.Address := System'To_Address (16#4008C000#);
   IP1553_Base : constant System.Address := System'To_Address (16#4003C000#);
   MATRIX0_Base : constant System.Address := System'To_Address (16#40000000#);
   MCAN0_Base : constant System.Address := System'To_Address (16#40058000#);
   MCAN1_Base : constant System.Address := System'To_Address (16#4005C000#);
   NMIC_Base : constant System.Address := System'To_Address (16#400A8000#);
   PIO_Base : constant System.Address := System'To_Address (16#40008000#);
   PMC_Base : constant System.Address := System'To_Address (16#4000C000#);
   PWM0_Base : constant System.Address := System'To_Address (16#40068000#);
   PWM1_Base : constant System.Address := System'To_Address (16#4006C000#);
   QSPI_Base : constant System.Address := System'To_Address (16#40038000#);
   RSTC_Base : constant System.Address := System'To_Address (16#40100200#);
   RSWDT_Base : constant System.Address := System'To_Address (16#40100300#);
   RTC_Base : constant System.Address := System'To_Address (16#40100260#);
   RTT_Base : constant System.Address := System'To_Address (16#40100230#);
   SFR_Base : constant System.Address := System'To_Address (16#400A0000#);
   SHA_Base : constant System.Address := System'To_Address (16#40094000#);
   SPW_Base : constant System.Address := System'To_Address (16#40040000#);
   SUPC_Base : constant System.Address := System'To_Address (16#40100210#);
   TC0_Base : constant System.Address := System'To_Address (16#40070000#);
   TC1_Base : constant System.Address := System'To_Address (16#40074000#);
   TC2_Base : constant System.Address := System'To_Address (16#40078000#);
   TC3_Base : constant System.Address := System'To_Address (16#4007C000#);
   TCMECC_Base : constant System.Address := System'To_Address (16#40100400#);
   TRNG_Base : constant System.Address := System'To_Address (16#40090000#);
   WDT_Base : constant System.Address := System'To_Address (16#40100250#);
   XDMAC_Base : constant System.Address := System'To_Address (16#40098000#);
   LOCKBIT_Base : constant System.Address := System'To_Address (16#0#);
   SCnSCB_Base : constant System.Address := System'To_Address (16#E000E000#);
   SCB_Base : constant System.Address := System'To_Address (16#E000ED00#);
   SysTick_Base : constant System.Address := System'To_Address (16#E000E010#);
   NVIC_Base : constant System.Address := System'To_Address (16#E000E100#);
   MPU_Base : constant System.Address := System'To_Address (16#E000ED90#);
   FPU_Base : constant System.Address := System'To_Address (16#E000EF30#);

end Interfaces.SAM;
