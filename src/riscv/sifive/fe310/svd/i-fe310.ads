--
--  Copyright (C) 2018, AdaCore
--

--  This spec has been automatically generated from FE310.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  E31 CPU Coreplex, high-performance, 32-bit RV32IMAC core
--
package Interfaces.FE310 is
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

   PLIC_Base : constant System.Address :=
     System'To_Address (16#C000000#);
   CLINT_Base : constant System.Address :=
     System'To_Address (16#2000000#);
   GPIO0_Base : constant System.Address :=
     System'To_Address (16#10012000#);
   QSPI0_Base : constant System.Address :=
     System'To_Address (16#10014000#);
   QSPI1_Base : constant System.Address :=
     System'To_Address (16#10024000#);
   QSPI2_Base : constant System.Address :=
     System'To_Address (16#10034000#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#10013000#);
   UART1_Base : constant System.Address :=
     System'To_Address (16#10023000#);
   PWM0_Base : constant System.Address :=
     System'To_Address (16#10015000#);
   PWM1_Base : constant System.Address :=
     System'To_Address (16#10025000#);
   PWM2_Base : constant System.Address :=
     System'To_Address (16#10035000#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#10000000#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#10000040#);
   AON_Base : constant System.Address :=
     System'To_Address (16#10000070#);
   BACKUP_Base : constant System.Address :=
     System'To_Address (16#10000080#);
   PMU_Base : constant System.Address :=
     System'To_Address (16#10000100#);

end Interfaces.FE310;
