--
--  Copyright (C) 2020, AdaCore
--

--  Copyright (c) 2018 Microchip Technology Inc.
--
--  SPDX-License-Identifier: Apache-2.0
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

--  This spec has been automatically generated from ATSAMD21G18AU.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Microchip ATSAMD21G18AU device: Cortex-M0+ Microcontroller with 256KB
--  Flash, 32KB SRAM, 45-pin package
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

   AC_Base : constant System.Address := System'To_Address (16#42004400#);
   ADC_Base : constant System.Address := System'To_Address (16#42004000#);
   DAC_Base : constant System.Address := System'To_Address (16#42004800#);
   DMAC_Base : constant System.Address := System'To_Address (16#41004800#);
   DSU_Base : constant System.Address := System'To_Address (16#41002000#);
   EIC_Base : constant System.Address := System'To_Address (16#40001800#);
   EVSYS_Base : constant System.Address := System'To_Address (16#42000400#);
   GCLK_Base : constant System.Address := System'To_Address (16#40000C00#);
   HMATRIX_Base : constant System.Address := System'To_Address (16#41007000#);
   I2S_Base : constant System.Address := System'To_Address (16#42005000#);
   MTB_Base : constant System.Address := System'To_Address (16#41006000#);
   NVMCTRL_Base : constant System.Address := System'To_Address (16#41004000#);
   PAC0_Base : constant System.Address := System'To_Address (16#40000000#);
   PAC1_Base : constant System.Address := System'To_Address (16#41000000#);
   PAC2_Base : constant System.Address := System'To_Address (16#42000000#);
   PM_Base : constant System.Address := System'To_Address (16#40000400#);
   PORT_Base : constant System.Address := System'To_Address (16#41004400#);
   PORT_IOBUS_Base : constant System.Address := System'To_Address (16#60000000#);
   RTC_Base : constant System.Address := System'To_Address (16#40001400#);
   SERCOM0_Base : constant System.Address := System'To_Address (16#42000800#);
   SERCOM1_Base : constant System.Address := System'To_Address (16#42000C00#);
   SERCOM2_Base : constant System.Address := System'To_Address (16#42001000#);
   SERCOM3_Base : constant System.Address := System'To_Address (16#42001400#);
   SERCOM4_Base : constant System.Address := System'To_Address (16#42001800#);
   SERCOM5_Base : constant System.Address := System'To_Address (16#42001C00#);
   SYSCTRL_Base : constant System.Address := System'To_Address (16#40000800#);
   TC3_Base : constant System.Address := System'To_Address (16#42002C00#);
   TC4_Base : constant System.Address := System'To_Address (16#42003000#);
   TC5_Base : constant System.Address := System'To_Address (16#42003400#);
   TC6_Base : constant System.Address := System'To_Address (16#42003800#);
   TC7_Base : constant System.Address := System'To_Address (16#42003C00#);
   TCC0_Base : constant System.Address := System'To_Address (16#42002000#);
   TCC1_Base : constant System.Address := System'To_Address (16#42002400#);
   TCC2_Base : constant System.Address := System'To_Address (16#42002800#);
   USB_Base : constant System.Address := System'To_Address (16#41005000#);
   WDT_Base : constant System.Address := System'To_Address (16#40001000#);

end Interfaces.SAM;
