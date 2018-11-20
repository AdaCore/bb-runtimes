--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Microchip ATSAMV71Q21 Microcontroller
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

   ACC_Base : constant System.Address :=
     System'To_Address (16#40044000#);
   AES_Base : constant System.Address :=
     System'To_Address (16#4006C000#);
   AFEC0_Base : constant System.Address :=
     System'To_Address (16#4003C000#);
   AFEC1_Base : constant System.Address :=
     System'To_Address (16#40064000#);
   CHIPID_Base : constant System.Address :=
     System'To_Address (16#400E0940#);
   DACC_Base : constant System.Address :=
     System'To_Address (16#40040000#);
   EFC_Base : constant System.Address :=
     System'To_Address (16#400E0C00#);
   GMAC_Base : constant System.Address :=
     System'To_Address (16#40050000#);
   GPBR_Base : constant System.Address :=
     System'To_Address (16#400E1890#);
   HSMCI_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   ICM_Base : constant System.Address :=
     System'To_Address (16#40048000#);
   ISI_Base : constant System.Address :=
     System'To_Address (16#4004C000#);
   MATRIX_Base : constant System.Address :=
     System'To_Address (16#40088000#);
   MCAN0_Base : constant System.Address :=
     System'To_Address (16#40030000#);
   MCAN1_Base : constant System.Address :=
     System'To_Address (16#40034000#);
   MLB_Base : constant System.Address :=
     System'To_Address (16#40068000#);
   PIOA_Base : constant System.Address :=
     System'To_Address (16#400E0E00#);
   PIOB_Base : constant System.Address :=
     System'To_Address (16#400E1000#);
   PIOC_Base : constant System.Address :=
     System'To_Address (16#400E1200#);
   PIOD_Base : constant System.Address :=
     System'To_Address (16#400E1400#);
   PIOE_Base : constant System.Address :=
     System'To_Address (16#400E1600#);
   PMC_Base : constant System.Address :=
     System'To_Address (16#400E0600#);
   PWM0_Base : constant System.Address :=
     System'To_Address (16#40020000#);
   PWM1_Base : constant System.Address :=
     System'To_Address (16#4005C000#);
   QSPI_Base : constant System.Address :=
     System'To_Address (16#4007C000#);
   RSTC_Base : constant System.Address :=
     System'To_Address (16#400E1800#);
   RSWDT_Base : constant System.Address :=
     System'To_Address (16#400E1900#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#400E1860#);
   RTT_Base : constant System.Address :=
     System'To_Address (16#400E1830#);
   SDRAMC_Base : constant System.Address :=
     System'To_Address (16#40084000#);
   SMC_Base : constant System.Address :=
     System'To_Address (16#40080000#);
   SPI0_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   SPI1_Base : constant System.Address :=
     System'To_Address (16#40058000#);
   SSC_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SUPC_Base : constant System.Address :=
     System'To_Address (16#400E1810#);
   TC0_Base : constant System.Address :=
     System'To_Address (16#4000C000#);
   TC1_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   TC2_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   TC3_Base : constant System.Address :=
     System'To_Address (16#40054000#);
   TRNG_Base : constant System.Address :=
     System'To_Address (16#40070000#);
   TWIHS0_Base : constant System.Address :=
     System'To_Address (16#40018000#);
   TWIHS1_Base : constant System.Address :=
     System'To_Address (16#4001C000#);
   TWIHS2_Base : constant System.Address :=
     System'To_Address (16#40060000#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#400E0800#);
   UART1_Base : constant System.Address :=
     System'To_Address (16#400E0A00#);
   UART2_Base : constant System.Address :=
     System'To_Address (16#400E1A00#);
   UART3_Base : constant System.Address :=
     System'To_Address (16#400E1C00#);
   UART4_Base : constant System.Address :=
     System'To_Address (16#400E1E00#);
   USART0_Base : constant System.Address :=
     System'To_Address (16#40024000#);
   USART1_Base : constant System.Address :=
     System'To_Address (16#40028000#);
   USART2_Base : constant System.Address :=
     System'To_Address (16#4002C000#);
   USBHS_Base : constant System.Address :=
     System'To_Address (16#40038000#);
   UTMI_Base : constant System.Address :=
     System'To_Address (16#400E0400#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#400E1850#);
   XDMAC_Base : constant System.Address :=
     System'To_Address (16#40078000#);
   LOCKBIT_Base : constant System.Address :=
     System'To_Address (16#0#);
   SystemControl_Base : constant System.Address :=
     System'To_Address (16#E000E000#);
   SysTick_Base : constant System.Address :=
     System'To_Address (16#E000E010#);
   NVIC_Base : constant System.Address :=
     System'To_Address (16#E000E100#);
   MPU_Base : constant System.Address :=
     System'To_Address (16#E000ED90#);
   FPU_Base : constant System.Address :=
     System'To_Address (16#E000EF34#);

end Interfaces.SAM;
