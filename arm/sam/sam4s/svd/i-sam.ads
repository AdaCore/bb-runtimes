--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAM4SD32C.svd

pragma Ada_2012;

with System;

--  Atmel ATSAM4SD32C device: Cortex-M4 Microcontroller with 2MB dual-bank
--  Flash, 160KB SRAM, USB, 100 Pins (refer to
--  http://www.atmel.com/devices/SAM4SD32C.aspx for more)
package Interfaces.SAM is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

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
