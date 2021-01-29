--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

package Interfaces.RP2040 is
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

   XIP_CTRL_Base : constant System.Address := System'To_Address (16#14000000#);
   XIP_SSI_Base : constant System.Address := System'To_Address (16#18000000#);
   SYSINFO_Base : constant System.Address := System'To_Address (16#40000000#);
   SYSCFG_Base : constant System.Address := System'To_Address (16#40004000#);
   CLOCKS_Base : constant System.Address := System'To_Address (16#40008000#);
   RESETS_Base : constant System.Address := System'To_Address (16#4000C000#);
   PSM_Base : constant System.Address := System'To_Address (16#40010000#);
   IO_BANK0_Base : constant System.Address := System'To_Address (16#40014000#);
   IO_QSPI_Base : constant System.Address := System'To_Address (16#40018000#);
   PADS_BANK0_Base : constant System.Address := System'To_Address (16#4001C000#);
   PADS_QSPI_Base : constant System.Address := System'To_Address (16#40020000#);
   XOSC_Base : constant System.Address := System'To_Address (16#40024000#);
   PLL_SYS_Base : constant System.Address := System'To_Address (16#40028000#);
   PLL_USB_Base : constant System.Address := System'To_Address (16#4002C000#);
   BUSCTRL_Base : constant System.Address := System'To_Address (16#40030000#);
   UART0_Base : constant System.Address := System'To_Address (16#40034000#);
   UART1_Base : constant System.Address := System'To_Address (16#40038000#);
   SPI0_Base : constant System.Address := System'To_Address (16#4003C000#);
   SPI1_Base : constant System.Address := System'To_Address (16#40040000#);
   I2C0_Base : constant System.Address := System'To_Address (16#40044000#);
   I2C1_Base : constant System.Address := System'To_Address (16#40048000#);
   ADC_Base : constant System.Address := System'To_Address (16#4004C000#);
   PWM_Base : constant System.Address := System'To_Address (16#40050000#);
   TIMER_Base : constant System.Address := System'To_Address (16#40054000#);
   WATCHDOG_Base : constant System.Address := System'To_Address (16#40058000#);
   RTC_Base : constant System.Address := System'To_Address (16#4005C000#);
   ROSC_Base : constant System.Address := System'To_Address (16#40060000#);
   VREG_AND_CHIP_RESET_Base : constant System.Address := System'To_Address (16#40064000#);
   TBMAN_Base : constant System.Address := System'To_Address (16#4006C000#);
   DMA_Base : constant System.Address := System'To_Address (16#50000000#);
   USBCTRL_REGS_Base : constant System.Address := System'To_Address (16#50110000#);
   PIO0_Base : constant System.Address := System'To_Address (16#50200000#);
   PIO1_Base : constant System.Address := System'To_Address (16#50300000#);
   SIO_Base : constant System.Address := System'To_Address (16#D0000000#);
   PPB_Base : constant System.Address := System'To_Address (16#E0000000#);

end Interfaces.RP2040;
