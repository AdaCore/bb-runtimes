--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from ATSAM4SD32C.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Atmel ATSAM4SD32C device: Cortex-M4 Microcontroller with 2MB dual-bank
--  Flash, 160KB SRAM, USB, 100 Pins (refer to
--  http://www.atmel.com/devices/SAM4SD32C.aspx for more)
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

   HSMCI_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   SSC_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SPI_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   TC0_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   TC1_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   TWI0_Base : constant System.Address :=
     System'To_Address (16#40018000#);
   TWI1_Base : constant System.Address :=
     System'To_Address (16#4001C000#);
   PWM_Base : constant System.Address :=
     System'To_Address (16#40020000#);
   USART0_Base : constant System.Address :=
     System'To_Address (16#40024000#);
   USART1_Base : constant System.Address :=
     System'To_Address (16#40028000#);
   UDP_Base : constant System.Address :=
     System'To_Address (16#40034000#);
   ADC_Base : constant System.Address :=
     System'To_Address (16#40038000#);
   DACC_Base : constant System.Address :=
     System'To_Address (16#4003C000#);
   ACC_Base : constant System.Address :=
     System'To_Address (16#40040000#);
   CRCCU_Base : constant System.Address :=
     System'To_Address (16#40044000#);
   CMCC_Base : constant System.Address :=
     System'To_Address (16#4007C000#);
   SMC_Base : constant System.Address :=
     System'To_Address (16#400E0000#);
   MATRIX_Base : constant System.Address :=
     System'To_Address (16#400E0200#);
   PMC_Base : constant System.Address :=
     System'To_Address (16#400E0400#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#400E0600#);
   CHIPID_Base : constant System.Address :=
     System'To_Address (16#400E0740#);
   UART1_Base : constant System.Address :=
     System'To_Address (16#400E0800#);
   EFC0_Base : constant System.Address :=
     System'To_Address (16#400E0A00#);
   EFC1_Base : constant System.Address :=
     System'To_Address (16#400E0C00#);
   PIOA_Base : constant System.Address :=
     System'To_Address (16#400E0E00#);
   PIOB_Base : constant System.Address :=
     System'To_Address (16#400E1000#);
   PIOC_Base : constant System.Address :=
     System'To_Address (16#400E1200#);
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

end Interfaces.SAM;
