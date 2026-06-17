--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

package Interfaces.RP2040.CLOCKS is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_GPOUT0_CTRL_AUXSRC_Field is
     (clksrc_pll_sys,
      clksrc_gpin0,
      clksrc_gpin1,
      clksrc_pll_usb,
      rosc_clksrc,
      xosc_clksrc,
      clk_sys,
      clk_usb,
      clk_adc,
      clk_rtc,
      clk_ref)
     with Size => 4;
   for CLK_GPOUT0_CTRL_AUXSRC_Field use
     (clksrc_pll_sys => 0,
      clksrc_gpin0 => 1,
      clksrc_gpin1 => 2,
      clksrc_pll_usb => 3,
      rosc_clksrc => 4,
      xosc_clksrc => 5,
      clk_sys => 6,
      clk_usb => 7,
      clk_adc => 8,
      clk_rtc => 9,
      clk_ref => 10);

   subtype CLK_GPOUT0_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT0_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT0_CTRL_DC50_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT0_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_GPOUT0_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_GPOUT0_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_GPOUT0_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_sys;
      --  unspecified
      Reserved_9_9   : Interfaces.RP2040.Bit := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_GPOUT0_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_GPOUT0_CTRL_ENABLE_Field := 16#0#;
      --  Enables duty cycle correction for odd divisors
      DC50           : CLK_GPOUT0_CTRL_DC50_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_GPOUT0_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_GPOUT0_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT0_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      DC50           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_GPOUT0_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_GPOUT0_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_GPOUT0_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_GPOUT0_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_GPOUT0_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT0_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_GPOUT1_CTRL_AUXSRC_Field is
     (clksrc_pll_sys,
      clksrc_gpin0,
      clksrc_gpin1,
      clksrc_pll_usb,
      rosc_clksrc,
      xosc_clksrc,
      clk_sys,
      clk_usb,
      clk_adc,
      clk_rtc,
      clk_ref)
     with Size => 4;
   for CLK_GPOUT1_CTRL_AUXSRC_Field use
     (clksrc_pll_sys => 0,
      clksrc_gpin0 => 1,
      clksrc_gpin1 => 2,
      clksrc_pll_usb => 3,
      rosc_clksrc => 4,
      xosc_clksrc => 5,
      clk_sys => 6,
      clk_usb => 7,
      clk_adc => 8,
      clk_rtc => 9,
      clk_ref => 10);

   subtype CLK_GPOUT1_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT1_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT1_CTRL_DC50_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT1_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_GPOUT1_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_GPOUT1_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_GPOUT1_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_sys;
      --  unspecified
      Reserved_9_9   : Interfaces.RP2040.Bit := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_GPOUT1_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_GPOUT1_CTRL_ENABLE_Field := 16#0#;
      --  Enables duty cycle correction for odd divisors
      DC50           : CLK_GPOUT1_CTRL_DC50_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_GPOUT1_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_GPOUT1_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT1_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      DC50           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_GPOUT1_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_GPOUT1_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_GPOUT1_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_GPOUT1_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_GPOUT1_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT1_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_GPOUT2_CTRL_AUXSRC_Field is
     (clksrc_pll_sys,
      clksrc_gpin0,
      clksrc_gpin1,
      clksrc_pll_usb,
      rosc_clksrc_ph,
      xosc_clksrc,
      clk_sys,
      clk_usb,
      clk_adc,
      clk_rtc,
      clk_ref)
     with Size => 4;
   for CLK_GPOUT2_CTRL_AUXSRC_Field use
     (clksrc_pll_sys => 0,
      clksrc_gpin0 => 1,
      clksrc_gpin1 => 2,
      clksrc_pll_usb => 3,
      rosc_clksrc_ph => 4,
      xosc_clksrc => 5,
      clk_sys => 6,
      clk_usb => 7,
      clk_adc => 8,
      clk_rtc => 9,
      clk_ref => 10);

   subtype CLK_GPOUT2_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT2_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT2_CTRL_DC50_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT2_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_GPOUT2_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_GPOUT2_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_GPOUT2_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_sys;
      --  unspecified
      Reserved_9_9   : Interfaces.RP2040.Bit := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_GPOUT2_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_GPOUT2_CTRL_ENABLE_Field := 16#0#;
      --  Enables duty cycle correction for odd divisors
      DC50           : CLK_GPOUT2_CTRL_DC50_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_GPOUT2_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_GPOUT2_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT2_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      DC50           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_GPOUT2_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_GPOUT2_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_GPOUT2_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_GPOUT2_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_GPOUT2_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT2_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_GPOUT3_CTRL_AUXSRC_Field is
     (clksrc_pll_sys,
      clksrc_gpin0,
      clksrc_gpin1,
      clksrc_pll_usb,
      rosc_clksrc_ph,
      xosc_clksrc,
      clk_sys,
      clk_usb,
      clk_adc,
      clk_rtc,
      clk_ref)
     with Size => 4;
   for CLK_GPOUT3_CTRL_AUXSRC_Field use
     (clksrc_pll_sys => 0,
      clksrc_gpin0 => 1,
      clksrc_gpin1 => 2,
      clksrc_pll_usb => 3,
      rosc_clksrc_ph => 4,
      xosc_clksrc => 5,
      clk_sys => 6,
      clk_usb => 7,
      clk_adc => 8,
      clk_rtc => 9,
      clk_ref => 10);

   subtype CLK_GPOUT3_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT3_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT3_CTRL_DC50_Field is Interfaces.RP2040.Bit;
   subtype CLK_GPOUT3_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_GPOUT3_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_GPOUT3_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_GPOUT3_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_sys;
      --  unspecified
      Reserved_9_9   : Interfaces.RP2040.Bit := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_GPOUT3_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_GPOUT3_CTRL_ENABLE_Field := 16#0#;
      --  Enables duty cycle correction for odd divisors
      DC50           : CLK_GPOUT3_CTRL_DC50_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_GPOUT3_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_GPOUT3_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT3_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      DC50           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_GPOUT3_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_GPOUT3_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_GPOUT3_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_GPOUT3_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_GPOUT3_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_GPOUT3_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   --  Selects the clock source glitchlessly, can be changed on-the-fly
   type CLK_REF_CTRL_SRC_Field is
     (rosc_clksrc_ph,
      clksrc_clk_ref_aux,
      xosc_clksrc)
     with Size => 2;
   for CLK_REF_CTRL_SRC_Field use
     (rosc_clksrc_ph => 0,
      clksrc_clk_ref_aux => 1,
      xosc_clksrc => 2);

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_REF_CTRL_AUXSRC_Field is
     (clksrc_pll_usb,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 2;
   for CLK_REF_CTRL_AUXSRC_Field use
     (clksrc_pll_usb => 0,
      clksrc_gpin0 => 1,
      clksrc_gpin1 => 2);

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_REF_CTRL_Register is record
      --  Selects the clock source glitchlessly, can be changed on-the-fly
      SRC           : CLK_REF_CTRL_SRC_Field :=
                       Interfaces.RP2040.CLOCKS.rosc_clksrc_ph;
      --  unspecified
      Reserved_2_4  : Interfaces.RP2040.UInt3 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC        : CLK_REF_CTRL_AUXSRC_Field :=
                       Interfaces.RP2040.CLOCKS.clksrc_pll_usb;
      --  unspecified
      Reserved_7_31 : Interfaces.RP2040.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_REF_CTRL_Register use record
      SRC           at 0 range 0 .. 1;
      Reserved_2_4  at 0 range 2 .. 4;
      AUXSRC        at 0 range 5 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype CLK_REF_DIV_INT_Field is Interfaces.RP2040.UInt2;

   --  Clock divisor, can be changed on-the-fly
   type CLK_REF_DIV_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.RP2040.Byte := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT            : CLK_REF_DIV_INT_Field := 16#1#;
      --  unspecified
      Reserved_10_31 : Interfaces.RP2040.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_REF_DIV_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      INT            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Selects the clock source glitchlessly, can be changed on-the-fly
   type CLK_SYS_CTRL_SRC_Field is
     (clk_ref,
      clksrc_clk_sys_aux)
     with Size => 1;
   for CLK_SYS_CTRL_SRC_Field use
     (clk_ref => 0,
      clksrc_clk_sys_aux => 1);

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_SYS_CTRL_AUXSRC_Field is
     (clksrc_pll_sys,
      clksrc_pll_usb,
      rosc_clksrc,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 3;
   for CLK_SYS_CTRL_AUXSRC_Field use
     (clksrc_pll_sys => 0,
      clksrc_pll_usb => 1,
      rosc_clksrc => 2,
      xosc_clksrc => 3,
      clksrc_gpin0 => 4,
      clksrc_gpin1 => 5);

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_SYS_CTRL_Register is record
      --  Selects the clock source glitchlessly, can be changed on-the-fly
      SRC           : CLK_SYS_CTRL_SRC_Field :=
                       Interfaces.RP2040.CLOCKS.clk_ref;
      --  unspecified
      Reserved_1_4  : Interfaces.RP2040.UInt4 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC        : CLK_SYS_CTRL_AUXSRC_Field :=
                       Interfaces.RP2040.CLOCKS.clksrc_pll_sys;
      --  unspecified
      Reserved_8_31 : Interfaces.RP2040.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_SYS_CTRL_Register use record
      SRC           at 0 range 0 .. 0;
      Reserved_1_4  at 0 range 1 .. 4;
      AUXSRC        at 0 range 5 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CLK_SYS_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_SYS_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_SYS_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_SYS_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_SYS_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_SYS_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_PERI_CTRL_AUXSRC_Field is
     (clk_sys,
      clksrc_pll_sys,
      clksrc_pll_usb,
      rosc_clksrc_ph,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 3;
   for CLK_PERI_CTRL_AUXSRC_Field use
     (clk_sys => 0,
      clksrc_pll_sys => 1,
      clksrc_pll_usb => 2,
      rosc_clksrc_ph => 3,
      xosc_clksrc => 4,
      clksrc_gpin0 => 5,
      clksrc_gpin1 => 6);

   subtype CLK_PERI_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_PERI_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_PERI_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_PERI_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clk_sys;
      --  unspecified
      Reserved_8_9   : Interfaces.RP2040.UInt2 := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_PERI_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_PERI_CTRL_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.RP2040.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_PERI_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_USB_CTRL_AUXSRC_Field is
     (clksrc_pll_usb,
      clksrc_pll_sys,
      rosc_clksrc_ph,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 3;
   for CLK_USB_CTRL_AUXSRC_Field use
     (clksrc_pll_usb => 0,
      clksrc_pll_sys => 1,
      rosc_clksrc_ph => 2,
      xosc_clksrc => 3,
      clksrc_gpin0 => 4,
      clksrc_gpin1 => 5);

   subtype CLK_USB_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_USB_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_USB_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_USB_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_USB_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_USB_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_usb;
      --  unspecified
      Reserved_8_9   : Interfaces.RP2040.UInt2 := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_USB_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_USB_CTRL_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : Interfaces.RP2040.UInt4 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_USB_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_USB_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_USB_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_USB_DIV_INT_Field is Interfaces.RP2040.UInt2;

   --  Clock divisor, can be changed on-the-fly
   type CLK_USB_DIV_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.RP2040.Byte := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT            : CLK_USB_DIV_INT_Field := 16#1#;
      --  unspecified
      Reserved_10_31 : Interfaces.RP2040.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_USB_DIV_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      INT            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_ADC_CTRL_AUXSRC_Field is
     (clksrc_pll_usb,
      clksrc_pll_sys,
      rosc_clksrc_ph,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 3;
   for CLK_ADC_CTRL_AUXSRC_Field use
     (clksrc_pll_usb => 0,
      clksrc_pll_sys => 1,
      rosc_clksrc_ph => 2,
      xosc_clksrc => 3,
      clksrc_gpin0 => 4,
      clksrc_gpin1 => 5);

   subtype CLK_ADC_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_ADC_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_ADC_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_ADC_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_ADC_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_ADC_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_usb;
      --  unspecified
      Reserved_8_9   : Interfaces.RP2040.UInt2 := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_ADC_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_ADC_CTRL_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : Interfaces.RP2040.UInt4 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_ADC_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_ADC_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_ADC_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_ADC_DIV_INT_Field is Interfaces.RP2040.UInt2;

   --  Clock divisor, can be changed on-the-fly
   type CLK_ADC_DIV_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.RP2040.Byte := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT            : CLK_ADC_DIV_INT_Field := 16#1#;
      --  unspecified
      Reserved_10_31 : Interfaces.RP2040.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_ADC_DIV_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      INT            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Selects the auxiliary clock source, will glitch when switching
   type CLK_RTC_CTRL_AUXSRC_Field is
     (clksrc_pll_usb,
      clksrc_pll_sys,
      rosc_clksrc_ph,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1)
     with Size => 3;
   for CLK_RTC_CTRL_AUXSRC_Field use
     (clksrc_pll_usb => 0,
      clksrc_pll_sys => 1,
      rosc_clksrc_ph => 2,
      xosc_clksrc => 3,
      clksrc_gpin0 => 4,
      clksrc_gpin1 => 5);

   subtype CLK_RTC_CTRL_KILL_Field is Interfaces.RP2040.Bit;
   subtype CLK_RTC_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_RTC_CTRL_PHASE_Field is Interfaces.RP2040.UInt2;
   subtype CLK_RTC_CTRL_NUDGE_Field is Interfaces.RP2040.Bit;

   --  Clock control, can be changed on-the-fly (except for auxsrc)
   type CLK_RTC_CTRL_Register is record
      --  unspecified
      Reserved_0_4   : Interfaces.RP2040.UInt5 := 16#0#;
      --  Selects the auxiliary clock source, will glitch when switching
      AUXSRC         : CLK_RTC_CTRL_AUXSRC_Field :=
                        Interfaces.RP2040.CLOCKS.clksrc_pll_usb;
      --  unspecified
      Reserved_8_9   : Interfaces.RP2040.UInt2 := 16#0#;
      --  Asynchronously kills the clock generator
      KILL           : CLK_RTC_CTRL_KILL_Field := 16#0#;
      --  Starts and stops the clock generator cleanly
      ENABLE         : CLK_RTC_CTRL_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : Interfaces.RP2040.UInt4 := 16#0#;
      --  This delays the enable signal by up to 3 cycles of the input clock\n
      --  This must be set before the clock is enabled to have any effect
      PHASE          : CLK_RTC_CTRL_PHASE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.RP2040.UInt2 := 16#0#;
      --  An edge on this signal shifts the phase of the output by 1 cycle of
      --  the input clock\n This can be done at any time
      NUDGE          : CLK_RTC_CTRL_NUDGE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_RTC_CTRL_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      AUXSRC         at 0 range 5 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      KILL           at 0 range 10 .. 10;
      ENABLE         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      PHASE          at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      NUDGE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype CLK_RTC_DIV_FRAC_Field is Interfaces.RP2040.Byte;
   subtype CLK_RTC_DIV_INT_Field is Interfaces.RP2040.UInt24;

   --  Clock divisor, can be changed on-the-fly
   type CLK_RTC_DIV_Register is record
      --  Fractional component of the divisor
      FRAC : CLK_RTC_DIV_FRAC_Field := 16#0#;
      --  Integer component of the divisor, 0 -> divide by 2^16
      INT  : CLK_RTC_DIV_INT_Field := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_RTC_DIV_Register use record
      FRAC at 0 range 0 .. 7;
      INT  at 0 range 8 .. 31;
   end record;

   subtype CLK_SYS_RESUS_CTRL_TIMEOUT_Field is Interfaces.RP2040.Byte;
   subtype CLK_SYS_RESUS_CTRL_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype CLK_SYS_RESUS_CTRL_FRCE_Field is Interfaces.RP2040.Bit;
   subtype CLK_SYS_RESUS_CTRL_CLEAR_Field is Interfaces.RP2040.Bit;

   type CLK_SYS_RESUS_CTRL_Register is record
      --  This is expressed as a number of clk_ref cycles\n and must be >= 2x
      --  clk_ref_freq/min_clk_tst_freq
      TIMEOUT        : CLK_SYS_RESUS_CTRL_TIMEOUT_Field := 16#FF#;
      --  Enable resus
      ENABLE         : CLK_SYS_RESUS_CTRL_ENABLE_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : Interfaces.RP2040.UInt3 := 16#0#;
      --  Force a resus, for test purposes only
      FRCE           : CLK_SYS_RESUS_CTRL_FRCE_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  For clearing the resus after the fault that triggered it has been
      --  corrected
      CLEAR          : CLK_SYS_RESUS_CTRL_CLEAR_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : Interfaces.RP2040.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_SYS_RESUS_CTRL_Register use record
      TIMEOUT        at 0 range 0 .. 7;
      ENABLE         at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      FRCE           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      CLEAR          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CLK_SYS_RESUS_STATUS_RESUSSED_Field is Interfaces.RP2040.Bit;

   type CLK_SYS_RESUS_STATUS_Register is record
      --  Read-only. Clock has been resuscitated, correct the error then send
      --  ctrl_clear=1
      RESUSSED      : CLK_SYS_RESUS_STATUS_RESUSSED_Field;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLK_SYS_RESUS_STATUS_Register use record
      RESUSSED      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype FC0_REF_KHZ_FC0_REF_KHZ_Field is Interfaces.RP2040.UInt20;

   --  Reference clock frequency in kHz
   type FC0_REF_KHZ_Register is record
      FC0_REF_KHZ    : FC0_REF_KHZ_FC0_REF_KHZ_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : Interfaces.RP2040.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_REF_KHZ_Register use record
      FC0_REF_KHZ    at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype FC0_MIN_KHZ_FC0_MIN_KHZ_Field is Interfaces.RP2040.UInt25;

   --  Minimum pass frequency in kHz. This is optional. Set to 0 if you are not
   --  using the pass/fail flags
   type FC0_MIN_KHZ_Register is record
      FC0_MIN_KHZ    : FC0_MIN_KHZ_FC0_MIN_KHZ_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : Interfaces.RP2040.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_MIN_KHZ_Register use record
      FC0_MIN_KHZ    at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype FC0_MAX_KHZ_FC0_MAX_KHZ_Field is Interfaces.RP2040.UInt25;

   --  Maximum pass frequency in kHz. This is optional. Set to 0x1ffffff if you
   --  are not using the pass/fail flags
   type FC0_MAX_KHZ_Register is record
      FC0_MAX_KHZ    : FC0_MAX_KHZ_FC0_MAX_KHZ_Field := 16#1FFFFFF#;
      --  unspecified
      Reserved_25_31 : Interfaces.RP2040.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_MAX_KHZ_Register use record
      FC0_MAX_KHZ    at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype FC0_DELAY_FC0_DELAY_Field is Interfaces.RP2040.UInt3;

   --  Delays the start of frequency counting to allow the mux to settle\n
   --  Delay is measured in multiples of the reference clock period
   type FC0_DELAY_Register is record
      FC0_DELAY     : FC0_DELAY_FC0_DELAY_Field := 16#1#;
      --  unspecified
      Reserved_3_31 : Interfaces.RP2040.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_DELAY_Register use record
      FC0_DELAY     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FC0_INTERVAL_FC0_INTERVAL_Field is Interfaces.RP2040.UInt4;

   --  The test interval is 0.98us * 2**interval, but let's call it 1us *
   --  2**interval\n The default gives a test interval of 250us
   type FC0_INTERVAL_Register is record
      FC0_INTERVAL  : FC0_INTERVAL_FC0_INTERVAL_Field := 16#8#;
      --  unspecified
      Reserved_4_31 : Interfaces.RP2040.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_INTERVAL_Register use record
      FC0_INTERVAL  at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   type FC0_SRC_FC0_SRC_Field is
     (NULL_k,
      pll_sys_clksrc_primary,
      pll_usb_clksrc_primary,
      rosc_clksrc,
      rosc_clksrc_ph,
      xosc_clksrc,
      clksrc_gpin0,
      clksrc_gpin1,
      clk_ref,
      clk_sys,
      clk_peri,
      clk_usb,
      clk_adc,
      clk_rtc)
     with Size => 8;
   for FC0_SRC_FC0_SRC_Field use
     (NULL_k => 0,
      pll_sys_clksrc_primary => 1,
      pll_usb_clksrc_primary => 2,
      rosc_clksrc => 3,
      rosc_clksrc_ph => 4,
      xosc_clksrc => 5,
      clksrc_gpin0 => 6,
      clksrc_gpin1 => 7,
      clk_ref => 8,
      clk_sys => 9,
      clk_peri => 10,
      clk_usb => 11,
      clk_adc => 12,
      clk_rtc => 13);

   --  Clock sent to frequency counter, set to 0 when not required\n Writing to
   --  this register initiates the frequency count
   type FC0_SRC_Register is record
      FC0_SRC       : FC0_SRC_FC0_SRC_Field :=
                       Interfaces.RP2040.CLOCKS.NULL_k;
      --  unspecified
      Reserved_8_31 : Interfaces.RP2040.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_SRC_Register use record
      FC0_SRC       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FC0_STATUS_PASS_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_DONE_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_RUNNING_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_WAITING_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_FAIL_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_SLOW_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_FAST_Field is Interfaces.RP2040.Bit;
   subtype FC0_STATUS_DIED_Field is Interfaces.RP2040.Bit;

   --  Frequency counter status
   type FC0_STATUS_Register is record
      --  Read-only. Test passed
      PASS           : FC0_STATUS_PASS_Field;
      --  unspecified
      Reserved_1_3   : Interfaces.RP2040.UInt3;
      --  Read-only. Test complete
      DONE           : FC0_STATUS_DONE_Field;
      --  unspecified
      Reserved_5_7   : Interfaces.RP2040.UInt3;
      --  Read-only. Test running
      RUNNING        : FC0_STATUS_RUNNING_Field;
      --  unspecified
      Reserved_9_11  : Interfaces.RP2040.UInt3;
      --  Read-only. Waiting for test clock to start
      WAITING        : FC0_STATUS_WAITING_Field;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3;
      --  Read-only. Test failed
      FAIL           : FC0_STATUS_FAIL_Field;
      --  unspecified
      Reserved_17_19 : Interfaces.RP2040.UInt3;
      --  Read-only. Test clock slower than expected, only valid when
      --  status_done=1
      SLOW           : FC0_STATUS_SLOW_Field;
      --  unspecified
      Reserved_21_23 : Interfaces.RP2040.UInt3;
      --  Read-only. Test clock faster than expected, only valid when
      --  status_done=1
      FAST           : FC0_STATUS_FAST_Field;
      --  unspecified
      Reserved_25_27 : Interfaces.RP2040.UInt3;
      --  Read-only. Test clock stopped during test
      DIED           : FC0_STATUS_DIED_Field;
      --  unspecified
      Reserved_29_31 : Interfaces.RP2040.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_STATUS_Register use record
      PASS           at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      DONE           at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      RUNNING        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      WAITING        at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      FAIL           at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SLOW           at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      FAST           at 0 range 24 .. 24;
      Reserved_25_27 at 0 range 25 .. 27;
      DIED           at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype FC0_RESULT_FRAC_Field is Interfaces.RP2040.UInt5;
   subtype FC0_RESULT_KHZ_Field is Interfaces.RP2040.UInt25;

   --  Result of frequency measurement, only valid when status_done=1
   type FC0_RESULT_Register is record
      --  Read-only.
      FRAC           : FC0_RESULT_FRAC_Field;
      --  Read-only.
      KHZ            : FC0_RESULT_KHZ_Field;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FC0_RESULT_Register use record
      FRAC           at 0 range 0 .. 4;
      KHZ            at 0 range 5 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype WAKE_EN0_clk_sys_clocks_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_adc_adc_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_adc_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_busctrl_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_busfabric_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_dma_Field is Interfaces.RP2040.Bit;
   --  WAKE_EN0_clk_sys_i2c array element
   subtype WAKE_EN0_clk_sys_i2c_Element is Interfaces.RP2040.Bit;

   --  WAKE_EN0_clk_sys_i2c array
   type WAKE_EN0_clk_sys_i2c_Field_Array is array (0 .. 1)
     of WAKE_EN0_clk_sys_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WAKE_EN0_clk_sys_i2c
   type WAKE_EN0_clk_sys_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_i2c as an array
            Arr : WAKE_EN0_clk_sys_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WAKE_EN0_clk_sys_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WAKE_EN0_clk_sys_io_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_jtag_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_pads_Field is Interfaces.RP2040.Bit;
   --  WAKE_EN0_clk_sys_pio array element
   subtype WAKE_EN0_clk_sys_pio_Element is Interfaces.RP2040.Bit;

   --  WAKE_EN0_clk_sys_pio array
   type WAKE_EN0_clk_sys_pio_Field_Array is array (0 .. 1)
     of WAKE_EN0_clk_sys_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WAKE_EN0_clk_sys_pio
   type WAKE_EN0_clk_sys_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_pio as an array
            Arr : WAKE_EN0_clk_sys_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WAKE_EN0_clk_sys_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WAKE_EN0_clk_sys_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_psm_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_pwm_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_resets_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_rom_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_rosc_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_rtc_rtc_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_rtc_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_sio_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_peri_spi0_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_spi0_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_peri_spi1_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN0_clk_sys_spi1_Field is Interfaces.RP2040.Bit;
   --  WAKE_EN0_clk_sys_sram array element
   subtype WAKE_EN0_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  WAKE_EN0_clk_sys_sram array
   type WAKE_EN0_clk_sys_sram_Field_Array is array (0 .. 3)
     of WAKE_EN0_clk_sys_sram_Element
     with Component_Size => 1, Size => 4;

   --  Type definition for WAKE_EN0_clk_sys_sram
   type WAKE_EN0_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt4;
         when True =>
            --  clk_sys_sram as an array
            Arr : WAKE_EN0_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for WAKE_EN0_clk_sys_sram_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  enable clock in wake mode
   type WAKE_EN0_Register is record
      clk_sys_clocks              : WAKE_EN0_clk_sys_clocks_Field := 16#1#;
      clk_adc_adc                 : WAKE_EN0_clk_adc_adc_Field := 16#1#;
      clk_sys_adc                 : WAKE_EN0_clk_sys_adc_Field := 16#1#;
      clk_sys_busctrl             : WAKE_EN0_clk_sys_busctrl_Field := 16#1#;
      clk_sys_busfabric           : WAKE_EN0_clk_sys_busfabric_Field := 16#1#;
      clk_sys_dma                 : WAKE_EN0_clk_sys_dma_Field := 16#1#;
      clk_sys_i2c                 : WAKE_EN0_clk_sys_i2c_Field :=
                                     (As_Array => False, Val => 16#1#);
      clk_sys_io                  : WAKE_EN0_clk_sys_io_Field := 16#1#;
      clk_sys_jtag                : WAKE_EN0_clk_sys_jtag_Field := 16#1#;
      clk_sys_vreg_and_chip_reset : WAKE_EN0_clk_sys_vreg_and_chip_reset_Field :=
                                     16#1#;
      clk_sys_pads                : WAKE_EN0_clk_sys_pads_Field := 16#1#;
      clk_sys_pio                 : WAKE_EN0_clk_sys_pio_Field :=
                                     (As_Array => False, Val => 16#1#);
      clk_sys_pll_sys             : WAKE_EN0_clk_sys_pll_sys_Field := 16#1#;
      clk_sys_pll_usb             : WAKE_EN0_clk_sys_pll_usb_Field := 16#1#;
      clk_sys_psm                 : WAKE_EN0_clk_sys_psm_Field := 16#1#;
      clk_sys_pwm                 : WAKE_EN0_clk_sys_pwm_Field := 16#1#;
      clk_sys_resets              : WAKE_EN0_clk_sys_resets_Field := 16#1#;
      clk_sys_rom                 : WAKE_EN0_clk_sys_rom_Field := 16#1#;
      clk_sys_rosc                : WAKE_EN0_clk_sys_rosc_Field := 16#1#;
      clk_rtc_rtc                 : WAKE_EN0_clk_rtc_rtc_Field := 16#1#;
      clk_sys_rtc                 : WAKE_EN0_clk_sys_rtc_Field := 16#1#;
      clk_sys_sio                 : WAKE_EN0_clk_sys_sio_Field := 16#1#;
      clk_peri_spi0               : WAKE_EN0_clk_peri_spi0_Field := 16#1#;
      clk_sys_spi0                : WAKE_EN0_clk_sys_spi0_Field := 16#1#;
      clk_peri_spi1               : WAKE_EN0_clk_peri_spi1_Field := 16#1#;
      clk_sys_spi1                : WAKE_EN0_clk_sys_spi1_Field := 16#1#;
      clk_sys_sram                : WAKE_EN0_clk_sys_sram_Field :=
                                     (As_Array => False, Val => 16#1#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKE_EN0_Register use record
      clk_sys_clocks              at 0 range 0 .. 0;
      clk_adc_adc                 at 0 range 1 .. 1;
      clk_sys_adc                 at 0 range 2 .. 2;
      clk_sys_busctrl             at 0 range 3 .. 3;
      clk_sys_busfabric           at 0 range 4 .. 4;
      clk_sys_dma                 at 0 range 5 .. 5;
      clk_sys_i2c                 at 0 range 6 .. 7;
      clk_sys_io                  at 0 range 8 .. 8;
      clk_sys_jtag                at 0 range 9 .. 9;
      clk_sys_vreg_and_chip_reset at 0 range 10 .. 10;
      clk_sys_pads                at 0 range 11 .. 11;
      clk_sys_pio                 at 0 range 12 .. 13;
      clk_sys_pll_sys             at 0 range 14 .. 14;
      clk_sys_pll_usb             at 0 range 15 .. 15;
      clk_sys_psm                 at 0 range 16 .. 16;
      clk_sys_pwm                 at 0 range 17 .. 17;
      clk_sys_resets              at 0 range 18 .. 18;
      clk_sys_rom                 at 0 range 19 .. 19;
      clk_sys_rosc                at 0 range 20 .. 20;
      clk_rtc_rtc                 at 0 range 21 .. 21;
      clk_sys_rtc                 at 0 range 22 .. 22;
      clk_sys_sio                 at 0 range 23 .. 23;
      clk_peri_spi0               at 0 range 24 .. 24;
      clk_sys_spi0                at 0 range 25 .. 25;
      clk_peri_spi1               at 0 range 26 .. 26;
      clk_sys_spi1                at 0 range 27 .. 27;
      clk_sys_sram                at 0 range 28 .. 31;
   end record;

   --  WAKE_EN1_clk_sys_sram array element
   subtype WAKE_EN1_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  WAKE_EN1_clk_sys_sram array
   type WAKE_EN1_clk_sys_sram_Field_Array is array (4 .. 5)
     of WAKE_EN1_clk_sys_sram_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WAKE_EN1_clk_sys_sram
   type WAKE_EN1_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_sram as an array
            Arr : WAKE_EN1_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WAKE_EN1_clk_sys_sram_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WAKE_EN1_clk_sys_syscfg_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_tbman_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_timer_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_peri_uart0_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_uart0_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_peri_uart1_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_uart1_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_usb_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_watchdog_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_xip_Field is Interfaces.RP2040.Bit;
   subtype WAKE_EN1_clk_sys_xosc_Field is Interfaces.RP2040.Bit;

   --  enable clock in wake mode
   type WAKE_EN1_Register is record
      clk_sys_sram     : WAKE_EN1_clk_sys_sram_Field :=
                          (As_Array => False, Val => 16#1#);
      clk_sys_syscfg   : WAKE_EN1_clk_sys_syscfg_Field := 16#1#;
      clk_sys_sysinfo  : WAKE_EN1_clk_sys_sysinfo_Field := 16#1#;
      clk_sys_tbman    : WAKE_EN1_clk_sys_tbman_Field := 16#1#;
      clk_sys_timer    : WAKE_EN1_clk_sys_timer_Field := 16#1#;
      clk_peri_uart0   : WAKE_EN1_clk_peri_uart0_Field := 16#1#;
      clk_sys_uart0    : WAKE_EN1_clk_sys_uart0_Field := 16#1#;
      clk_peri_uart1   : WAKE_EN1_clk_peri_uart1_Field := 16#1#;
      clk_sys_uart1    : WAKE_EN1_clk_sys_uart1_Field := 16#1#;
      clk_sys_usbctrl  : WAKE_EN1_clk_sys_usbctrl_Field := 16#1#;
      clk_usb_usbctrl  : WAKE_EN1_clk_usb_usbctrl_Field := 16#1#;
      clk_sys_watchdog : WAKE_EN1_clk_sys_watchdog_Field := 16#1#;
      clk_sys_xip      : WAKE_EN1_clk_sys_xip_Field := 16#1#;
      clk_sys_xosc     : WAKE_EN1_clk_sys_xosc_Field := 16#1#;
      --  unspecified
      Reserved_15_31   : Interfaces.RP2040.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKE_EN1_Register use record
      clk_sys_sram     at 0 range 0 .. 1;
      clk_sys_syscfg   at 0 range 2 .. 2;
      clk_sys_sysinfo  at 0 range 3 .. 3;
      clk_sys_tbman    at 0 range 4 .. 4;
      clk_sys_timer    at 0 range 5 .. 5;
      clk_peri_uart0   at 0 range 6 .. 6;
      clk_sys_uart0    at 0 range 7 .. 7;
      clk_peri_uart1   at 0 range 8 .. 8;
      clk_sys_uart1    at 0 range 9 .. 9;
      clk_sys_usbctrl  at 0 range 10 .. 10;
      clk_usb_usbctrl  at 0 range 11 .. 11;
      clk_sys_watchdog at 0 range 12 .. 12;
      clk_sys_xip      at 0 range 13 .. 13;
      clk_sys_xosc     at 0 range 14 .. 14;
      Reserved_15_31   at 0 range 15 .. 31;
   end record;

   subtype SLEEP_EN0_clk_sys_clocks_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_adc_adc_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_adc_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_busctrl_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_busfabric_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_dma_Field is Interfaces.RP2040.Bit;
   --  SLEEP_EN0_clk_sys_i2c array element
   subtype SLEEP_EN0_clk_sys_i2c_Element is Interfaces.RP2040.Bit;

   --  SLEEP_EN0_clk_sys_i2c array
   type SLEEP_EN0_clk_sys_i2c_Field_Array is array (0 .. 1)
     of SLEEP_EN0_clk_sys_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for SLEEP_EN0_clk_sys_i2c
   type SLEEP_EN0_clk_sys_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_i2c as an array
            Arr : SLEEP_EN0_clk_sys_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SLEEP_EN0_clk_sys_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype SLEEP_EN0_clk_sys_io_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_jtag_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_vreg_and_chip_reset_Field is
     Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_pads_Field is Interfaces.RP2040.Bit;
   --  SLEEP_EN0_clk_sys_pio array element
   subtype SLEEP_EN0_clk_sys_pio_Element is Interfaces.RP2040.Bit;

   --  SLEEP_EN0_clk_sys_pio array
   type SLEEP_EN0_clk_sys_pio_Field_Array is array (0 .. 1)
     of SLEEP_EN0_clk_sys_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for SLEEP_EN0_clk_sys_pio
   type SLEEP_EN0_clk_sys_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_pio as an array
            Arr : SLEEP_EN0_clk_sys_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SLEEP_EN0_clk_sys_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype SLEEP_EN0_clk_sys_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_psm_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_pwm_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_resets_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_rom_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_rosc_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_rtc_rtc_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_rtc_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_sio_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_peri_spi0_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_spi0_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_peri_spi1_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN0_clk_sys_spi1_Field is Interfaces.RP2040.Bit;
   --  SLEEP_EN0_clk_sys_sram array element
   subtype SLEEP_EN0_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  SLEEP_EN0_clk_sys_sram array
   type SLEEP_EN0_clk_sys_sram_Field_Array is array (0 .. 3)
     of SLEEP_EN0_clk_sys_sram_Element
     with Component_Size => 1, Size => 4;

   --  Type definition for SLEEP_EN0_clk_sys_sram
   type SLEEP_EN0_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt4;
         when True =>
            --  clk_sys_sram as an array
            Arr : SLEEP_EN0_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SLEEP_EN0_clk_sys_sram_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  enable clock in sleep mode
   type SLEEP_EN0_Register is record
      clk_sys_clocks              : SLEEP_EN0_clk_sys_clocks_Field := 16#1#;
      clk_adc_adc                 : SLEEP_EN0_clk_adc_adc_Field := 16#1#;
      clk_sys_adc                 : SLEEP_EN0_clk_sys_adc_Field := 16#1#;
      clk_sys_busctrl             : SLEEP_EN0_clk_sys_busctrl_Field := 16#1#;
      clk_sys_busfabric           : SLEEP_EN0_clk_sys_busfabric_Field :=
                                     16#1#;
      clk_sys_dma                 : SLEEP_EN0_clk_sys_dma_Field := 16#1#;
      clk_sys_i2c                 : SLEEP_EN0_clk_sys_i2c_Field :=
                                     (As_Array => False, Val => 16#1#);
      clk_sys_io                  : SLEEP_EN0_clk_sys_io_Field := 16#1#;
      clk_sys_jtag                : SLEEP_EN0_clk_sys_jtag_Field := 16#1#;
      clk_sys_vreg_and_chip_reset : SLEEP_EN0_clk_sys_vreg_and_chip_reset_Field :=
                                     16#1#;
      clk_sys_pads                : SLEEP_EN0_clk_sys_pads_Field := 16#1#;
      clk_sys_pio                 : SLEEP_EN0_clk_sys_pio_Field :=
                                     (As_Array => False, Val => 16#1#);
      clk_sys_pll_sys             : SLEEP_EN0_clk_sys_pll_sys_Field := 16#1#;
      clk_sys_pll_usb             : SLEEP_EN0_clk_sys_pll_usb_Field := 16#1#;
      clk_sys_psm                 : SLEEP_EN0_clk_sys_psm_Field := 16#1#;
      clk_sys_pwm                 : SLEEP_EN0_clk_sys_pwm_Field := 16#1#;
      clk_sys_resets              : SLEEP_EN0_clk_sys_resets_Field := 16#1#;
      clk_sys_rom                 : SLEEP_EN0_clk_sys_rom_Field := 16#1#;
      clk_sys_rosc                : SLEEP_EN0_clk_sys_rosc_Field := 16#1#;
      clk_rtc_rtc                 : SLEEP_EN0_clk_rtc_rtc_Field := 16#1#;
      clk_sys_rtc                 : SLEEP_EN0_clk_sys_rtc_Field := 16#1#;
      clk_sys_sio                 : SLEEP_EN0_clk_sys_sio_Field := 16#1#;
      clk_peri_spi0               : SLEEP_EN0_clk_peri_spi0_Field := 16#1#;
      clk_sys_spi0                : SLEEP_EN0_clk_sys_spi0_Field := 16#1#;
      clk_peri_spi1               : SLEEP_EN0_clk_peri_spi1_Field := 16#1#;
      clk_sys_spi1                : SLEEP_EN0_clk_sys_spi1_Field := 16#1#;
      clk_sys_sram                : SLEEP_EN0_clk_sys_sram_Field :=
                                     (As_Array => False, Val => 16#1#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLEEP_EN0_Register use record
      clk_sys_clocks              at 0 range 0 .. 0;
      clk_adc_adc                 at 0 range 1 .. 1;
      clk_sys_adc                 at 0 range 2 .. 2;
      clk_sys_busctrl             at 0 range 3 .. 3;
      clk_sys_busfabric           at 0 range 4 .. 4;
      clk_sys_dma                 at 0 range 5 .. 5;
      clk_sys_i2c                 at 0 range 6 .. 7;
      clk_sys_io                  at 0 range 8 .. 8;
      clk_sys_jtag                at 0 range 9 .. 9;
      clk_sys_vreg_and_chip_reset at 0 range 10 .. 10;
      clk_sys_pads                at 0 range 11 .. 11;
      clk_sys_pio                 at 0 range 12 .. 13;
      clk_sys_pll_sys             at 0 range 14 .. 14;
      clk_sys_pll_usb             at 0 range 15 .. 15;
      clk_sys_psm                 at 0 range 16 .. 16;
      clk_sys_pwm                 at 0 range 17 .. 17;
      clk_sys_resets              at 0 range 18 .. 18;
      clk_sys_rom                 at 0 range 19 .. 19;
      clk_sys_rosc                at 0 range 20 .. 20;
      clk_rtc_rtc                 at 0 range 21 .. 21;
      clk_sys_rtc                 at 0 range 22 .. 22;
      clk_sys_sio                 at 0 range 23 .. 23;
      clk_peri_spi0               at 0 range 24 .. 24;
      clk_sys_spi0                at 0 range 25 .. 25;
      clk_peri_spi1               at 0 range 26 .. 26;
      clk_sys_spi1                at 0 range 27 .. 27;
      clk_sys_sram                at 0 range 28 .. 31;
   end record;

   --  SLEEP_EN1_clk_sys_sram array element
   subtype SLEEP_EN1_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  SLEEP_EN1_clk_sys_sram array
   type SLEEP_EN1_clk_sys_sram_Field_Array is array (4 .. 5)
     of SLEEP_EN1_clk_sys_sram_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for SLEEP_EN1_clk_sys_sram
   type SLEEP_EN1_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_sram as an array
            Arr : SLEEP_EN1_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SLEEP_EN1_clk_sys_sram_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype SLEEP_EN1_clk_sys_syscfg_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_tbman_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_timer_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_peri_uart0_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_uart0_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_peri_uart1_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_uart1_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_usb_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_watchdog_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_xip_Field is Interfaces.RP2040.Bit;
   subtype SLEEP_EN1_clk_sys_xosc_Field is Interfaces.RP2040.Bit;

   --  enable clock in sleep mode
   type SLEEP_EN1_Register is record
      clk_sys_sram     : SLEEP_EN1_clk_sys_sram_Field :=
                          (As_Array => False, Val => 16#1#);
      clk_sys_syscfg   : SLEEP_EN1_clk_sys_syscfg_Field := 16#1#;
      clk_sys_sysinfo  : SLEEP_EN1_clk_sys_sysinfo_Field := 16#1#;
      clk_sys_tbman    : SLEEP_EN1_clk_sys_tbman_Field := 16#1#;
      clk_sys_timer    : SLEEP_EN1_clk_sys_timer_Field := 16#1#;
      clk_peri_uart0   : SLEEP_EN1_clk_peri_uart0_Field := 16#1#;
      clk_sys_uart0    : SLEEP_EN1_clk_sys_uart0_Field := 16#1#;
      clk_peri_uart1   : SLEEP_EN1_clk_peri_uart1_Field := 16#1#;
      clk_sys_uart1    : SLEEP_EN1_clk_sys_uart1_Field := 16#1#;
      clk_sys_usbctrl  : SLEEP_EN1_clk_sys_usbctrl_Field := 16#1#;
      clk_usb_usbctrl  : SLEEP_EN1_clk_usb_usbctrl_Field := 16#1#;
      clk_sys_watchdog : SLEEP_EN1_clk_sys_watchdog_Field := 16#1#;
      clk_sys_xip      : SLEEP_EN1_clk_sys_xip_Field := 16#1#;
      clk_sys_xosc     : SLEEP_EN1_clk_sys_xosc_Field := 16#1#;
      --  unspecified
      Reserved_15_31   : Interfaces.RP2040.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLEEP_EN1_Register use record
      clk_sys_sram     at 0 range 0 .. 1;
      clk_sys_syscfg   at 0 range 2 .. 2;
      clk_sys_sysinfo  at 0 range 3 .. 3;
      clk_sys_tbman    at 0 range 4 .. 4;
      clk_sys_timer    at 0 range 5 .. 5;
      clk_peri_uart0   at 0 range 6 .. 6;
      clk_sys_uart0    at 0 range 7 .. 7;
      clk_peri_uart1   at 0 range 8 .. 8;
      clk_sys_uart1    at 0 range 9 .. 9;
      clk_sys_usbctrl  at 0 range 10 .. 10;
      clk_usb_usbctrl  at 0 range 11 .. 11;
      clk_sys_watchdog at 0 range 12 .. 12;
      clk_sys_xip      at 0 range 13 .. 13;
      clk_sys_xosc     at 0 range 14 .. 14;
      Reserved_15_31   at 0 range 15 .. 31;
   end record;

   subtype ENABLED0_clk_sys_clocks_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_adc_adc_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_adc_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_busctrl_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_busfabric_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_dma_Field is Interfaces.RP2040.Bit;
   --  ENABLED0_clk_sys_i2c array element
   subtype ENABLED0_clk_sys_i2c_Element is Interfaces.RP2040.Bit;

   --  ENABLED0_clk_sys_i2c array
   type ENABLED0_clk_sys_i2c_Field_Array is array (0 .. 1)
     of ENABLED0_clk_sys_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for ENABLED0_clk_sys_i2c
   type ENABLED0_clk_sys_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_i2c as an array
            Arr : ENABLED0_clk_sys_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ENABLED0_clk_sys_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype ENABLED0_clk_sys_io_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_jtag_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_pads_Field is Interfaces.RP2040.Bit;
   --  ENABLED0_clk_sys_pio array element
   subtype ENABLED0_clk_sys_pio_Element is Interfaces.RP2040.Bit;

   --  ENABLED0_clk_sys_pio array
   type ENABLED0_clk_sys_pio_Field_Array is array (0 .. 1)
     of ENABLED0_clk_sys_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for ENABLED0_clk_sys_pio
   type ENABLED0_clk_sys_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_pio as an array
            Arr : ENABLED0_clk_sys_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ENABLED0_clk_sys_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype ENABLED0_clk_sys_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_psm_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_pwm_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_resets_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_rom_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_rosc_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_rtc_rtc_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_rtc_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_sio_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_peri_spi0_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_spi0_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_peri_spi1_Field is Interfaces.RP2040.Bit;
   subtype ENABLED0_clk_sys_spi1_Field is Interfaces.RP2040.Bit;
   --  ENABLED0_clk_sys_sram array element
   subtype ENABLED0_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  ENABLED0_clk_sys_sram array
   type ENABLED0_clk_sys_sram_Field_Array is array (0 .. 3)
     of ENABLED0_clk_sys_sram_Element
     with Component_Size => 1, Size => 4;

   --  Type definition for ENABLED0_clk_sys_sram
   type ENABLED0_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt4;
         when True =>
            --  clk_sys_sram as an array
            Arr : ENABLED0_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for ENABLED0_clk_sys_sram_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  indicates the state of the clock enable
   type ENABLED0_Register is record
      --  Read-only.
      clk_sys_clocks              : ENABLED0_clk_sys_clocks_Field;
      --  Read-only.
      clk_adc_adc                 : ENABLED0_clk_adc_adc_Field;
      --  Read-only.
      clk_sys_adc                 : ENABLED0_clk_sys_adc_Field;
      --  Read-only.
      clk_sys_busctrl             : ENABLED0_clk_sys_busctrl_Field;
      --  Read-only.
      clk_sys_busfabric           : ENABLED0_clk_sys_busfabric_Field;
      --  Read-only.
      clk_sys_dma                 : ENABLED0_clk_sys_dma_Field;
      --  Read-only.
      clk_sys_i2c                 : ENABLED0_clk_sys_i2c_Field;
      --  Read-only.
      clk_sys_io                  : ENABLED0_clk_sys_io_Field;
      --  Read-only.
      clk_sys_jtag                : ENABLED0_clk_sys_jtag_Field;
      --  Read-only.
      clk_sys_vreg_and_chip_reset : ENABLED0_clk_sys_vreg_and_chip_reset_Field;
      --  Read-only.
      clk_sys_pads                : ENABLED0_clk_sys_pads_Field;
      --  Read-only.
      clk_sys_pio                 : ENABLED0_clk_sys_pio_Field;
      --  Read-only.
      clk_sys_pll_sys             : ENABLED0_clk_sys_pll_sys_Field;
      --  Read-only.
      clk_sys_pll_usb             : ENABLED0_clk_sys_pll_usb_Field;
      --  Read-only.
      clk_sys_psm                 : ENABLED0_clk_sys_psm_Field;
      --  Read-only.
      clk_sys_pwm                 : ENABLED0_clk_sys_pwm_Field;
      --  Read-only.
      clk_sys_resets              : ENABLED0_clk_sys_resets_Field;
      --  Read-only.
      clk_sys_rom                 : ENABLED0_clk_sys_rom_Field;
      --  Read-only.
      clk_sys_rosc                : ENABLED0_clk_sys_rosc_Field;
      --  Read-only.
      clk_rtc_rtc                 : ENABLED0_clk_rtc_rtc_Field;
      --  Read-only.
      clk_sys_rtc                 : ENABLED0_clk_sys_rtc_Field;
      --  Read-only.
      clk_sys_sio                 : ENABLED0_clk_sys_sio_Field;
      --  Read-only.
      clk_peri_spi0               : ENABLED0_clk_peri_spi0_Field;
      --  Read-only.
      clk_sys_spi0                : ENABLED0_clk_sys_spi0_Field;
      --  Read-only.
      clk_peri_spi1               : ENABLED0_clk_peri_spi1_Field;
      --  Read-only.
      clk_sys_spi1                : ENABLED0_clk_sys_spi1_Field;
      --  Read-only.
      clk_sys_sram                : ENABLED0_clk_sys_sram_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLED0_Register use record
      clk_sys_clocks              at 0 range 0 .. 0;
      clk_adc_adc                 at 0 range 1 .. 1;
      clk_sys_adc                 at 0 range 2 .. 2;
      clk_sys_busctrl             at 0 range 3 .. 3;
      clk_sys_busfabric           at 0 range 4 .. 4;
      clk_sys_dma                 at 0 range 5 .. 5;
      clk_sys_i2c                 at 0 range 6 .. 7;
      clk_sys_io                  at 0 range 8 .. 8;
      clk_sys_jtag                at 0 range 9 .. 9;
      clk_sys_vreg_and_chip_reset at 0 range 10 .. 10;
      clk_sys_pads                at 0 range 11 .. 11;
      clk_sys_pio                 at 0 range 12 .. 13;
      clk_sys_pll_sys             at 0 range 14 .. 14;
      clk_sys_pll_usb             at 0 range 15 .. 15;
      clk_sys_psm                 at 0 range 16 .. 16;
      clk_sys_pwm                 at 0 range 17 .. 17;
      clk_sys_resets              at 0 range 18 .. 18;
      clk_sys_rom                 at 0 range 19 .. 19;
      clk_sys_rosc                at 0 range 20 .. 20;
      clk_rtc_rtc                 at 0 range 21 .. 21;
      clk_sys_rtc                 at 0 range 22 .. 22;
      clk_sys_sio                 at 0 range 23 .. 23;
      clk_peri_spi0               at 0 range 24 .. 24;
      clk_sys_spi0                at 0 range 25 .. 25;
      clk_peri_spi1               at 0 range 26 .. 26;
      clk_sys_spi1                at 0 range 27 .. 27;
      clk_sys_sram                at 0 range 28 .. 31;
   end record;

   --  ENABLED1_clk_sys_sram array element
   subtype ENABLED1_clk_sys_sram_Element is Interfaces.RP2040.Bit;

   --  ENABLED1_clk_sys_sram array
   type ENABLED1_clk_sys_sram_Field_Array is array (4 .. 5)
     of ENABLED1_clk_sys_sram_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for ENABLED1_clk_sys_sram
   type ENABLED1_clk_sys_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  clk_sys_sram as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  clk_sys_sram as an array
            Arr : ENABLED1_clk_sys_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ENABLED1_clk_sys_sram_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype ENABLED1_clk_sys_syscfg_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_tbman_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_timer_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_peri_uart0_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_uart0_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_peri_uart1_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_uart1_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_usb_usbctrl_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_watchdog_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_xip_Field is Interfaces.RP2040.Bit;
   subtype ENABLED1_clk_sys_xosc_Field is Interfaces.RP2040.Bit;

   --  indicates the state of the clock enable
   type ENABLED1_Register is record
      --  Read-only.
      clk_sys_sram     : ENABLED1_clk_sys_sram_Field;
      --  Read-only.
      clk_sys_syscfg   : ENABLED1_clk_sys_syscfg_Field;
      --  Read-only.
      clk_sys_sysinfo  : ENABLED1_clk_sys_sysinfo_Field;
      --  Read-only.
      clk_sys_tbman    : ENABLED1_clk_sys_tbman_Field;
      --  Read-only.
      clk_sys_timer    : ENABLED1_clk_sys_timer_Field;
      --  Read-only.
      clk_peri_uart0   : ENABLED1_clk_peri_uart0_Field;
      --  Read-only.
      clk_sys_uart0    : ENABLED1_clk_sys_uart0_Field;
      --  Read-only.
      clk_peri_uart1   : ENABLED1_clk_peri_uart1_Field;
      --  Read-only.
      clk_sys_uart1    : ENABLED1_clk_sys_uart1_Field;
      --  Read-only.
      clk_sys_usbctrl  : ENABLED1_clk_sys_usbctrl_Field;
      --  Read-only.
      clk_usb_usbctrl  : ENABLED1_clk_usb_usbctrl_Field;
      --  Read-only.
      clk_sys_watchdog : ENABLED1_clk_sys_watchdog_Field;
      --  Read-only.
      clk_sys_xip      : ENABLED1_clk_sys_xip_Field;
      --  Read-only.
      clk_sys_xosc     : ENABLED1_clk_sys_xosc_Field;
      --  unspecified
      Reserved_15_31   : Interfaces.RP2040.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLED1_Register use record
      clk_sys_sram     at 0 range 0 .. 1;
      clk_sys_syscfg   at 0 range 2 .. 2;
      clk_sys_sysinfo  at 0 range 3 .. 3;
      clk_sys_tbman    at 0 range 4 .. 4;
      clk_sys_timer    at 0 range 5 .. 5;
      clk_peri_uart0   at 0 range 6 .. 6;
      clk_sys_uart0    at 0 range 7 .. 7;
      clk_peri_uart1   at 0 range 8 .. 8;
      clk_sys_uart1    at 0 range 9 .. 9;
      clk_sys_usbctrl  at 0 range 10 .. 10;
      clk_usb_usbctrl  at 0 range 11 .. 11;
      clk_sys_watchdog at 0 range 12 .. 12;
      clk_sys_xip      at 0 range 13 .. 13;
      clk_sys_xosc     at 0 range 14 .. 14;
      Reserved_15_31   at 0 range 15 .. 31;
   end record;

   subtype INTR_CLK_SYS_RESUS_Field is Interfaces.RP2040.Bit;

   --  Raw Interrupts
   type INTR_Register is record
      --  Read-only.
      CLK_SYS_RESUS : INTR_CLK_SYS_RESUS_Field;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTR_Register use record
      CLK_SYS_RESUS at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype INTE_CLK_SYS_RESUS_Field is Interfaces.RP2040.Bit;

   --  Interrupt Enable
   type INTE_Register is record
      CLK_SYS_RESUS : INTE_CLK_SYS_RESUS_Field := 16#0#;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTE_Register use record
      CLK_SYS_RESUS at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype INTF_CLK_SYS_RESUS_Field is Interfaces.RP2040.Bit;

   --  Interrupt Force
   type INTF_Register is record
      CLK_SYS_RESUS : INTF_CLK_SYS_RESUS_Field := 16#0#;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTF_Register use record
      CLK_SYS_RESUS at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype INTS_CLK_SYS_RESUS_Field is Interfaces.RP2040.Bit;

   --  Interrupt status after masking & forcing
   type INTS_Register is record
      --  Read-only.
      CLK_SYS_RESUS : INTS_CLK_SYS_RESUS_Field;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTS_Register use record
      CLK_SYS_RESUS at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type CLOCKS_Peripheral is record
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_GPOUT0_CTRL      : aliased CLK_GPOUT0_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_GPOUT0_DIV       : aliased CLK_GPOUT0_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_GPOUT0_SELECTED  : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_GPOUT1_CTRL      : aliased CLK_GPOUT1_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_GPOUT1_DIV       : aliased CLK_GPOUT1_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_GPOUT1_SELECTED  : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_GPOUT2_CTRL      : aliased CLK_GPOUT2_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_GPOUT2_DIV       : aliased CLK_GPOUT2_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_GPOUT2_SELECTED  : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_GPOUT3_CTRL      : aliased CLK_GPOUT3_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_GPOUT3_DIV       : aliased CLK_GPOUT3_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_GPOUT3_SELECTED  : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_REF_CTRL         : aliased CLK_REF_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_REF_DIV          : aliased CLK_REF_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_REF_SELECTED     : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_SYS_CTRL         : aliased CLK_SYS_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_SYS_DIV          : aliased CLK_SYS_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_SYS_SELECTED     : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_PERI_CTRL        : aliased CLK_PERI_CTRL_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_PERI_SELECTED    : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_USB_CTRL         : aliased CLK_USB_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_USB_DIV          : aliased CLK_USB_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_USB_SELECTED     : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_ADC_CTRL         : aliased CLK_ADC_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_ADC_DIV          : aliased CLK_ADC_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_ADC_SELECTED     : aliased Interfaces.RP2040.UInt32;
      --  Clock control, can be changed on-the-fly (except for auxsrc)
      CLK_RTC_CTRL         : aliased CLK_RTC_CTRL_Register;
      --  Clock divisor, can be changed on-the-fly
      CLK_RTC_DIV          : aliased CLK_RTC_DIV_Register;
      --  Indicates which src is currently selected (one-hot)
      CLK_RTC_SELECTED     : aliased Interfaces.RP2040.UInt32;
      CLK_SYS_RESUS_CTRL   : aliased CLK_SYS_RESUS_CTRL_Register;
      CLK_SYS_RESUS_STATUS : aliased CLK_SYS_RESUS_STATUS_Register;
      --  Reference clock frequency in kHz
      FC0_REF_KHZ          : aliased FC0_REF_KHZ_Register;
      --  Minimum pass frequency in kHz. This is optional. Set to 0 if you are
      --  not using the pass/fail flags
      FC0_MIN_KHZ          : aliased FC0_MIN_KHZ_Register;
      --  Maximum pass frequency in kHz. This is optional. Set to 0x1ffffff if
      --  you are not using the pass/fail flags
      FC0_MAX_KHZ          : aliased FC0_MAX_KHZ_Register;
      --  Delays the start of frequency counting to allow the mux to settle\n
      --  Delay is measured in multiples of the reference clock period
      FC0_DELAY            : aliased FC0_DELAY_Register;
      --  The test interval is 0.98us * 2**interval, but let's call it 1us *
      --  2**interval\n The default gives a test interval of 250us
      FC0_INTERVAL         : aliased FC0_INTERVAL_Register;
      --  Clock sent to frequency counter, set to 0 when not required\n Writing
      --  to this register initiates the frequency count
      FC0_SRC              : aliased FC0_SRC_Register;
      --  Frequency counter status
      FC0_STATUS           : aliased FC0_STATUS_Register;
      --  Result of frequency measurement, only valid when status_done=1
      FC0_RESULT           : aliased FC0_RESULT_Register;
      --  enable clock in wake mode
      WAKE_EN0             : aliased WAKE_EN0_Register;
      --  enable clock in wake mode
      WAKE_EN1             : aliased WAKE_EN1_Register;
      --  enable clock in sleep mode
      SLEEP_EN0            : aliased SLEEP_EN0_Register;
      --  enable clock in sleep mode
      SLEEP_EN1            : aliased SLEEP_EN1_Register;
      --  indicates the state of the clock enable
      ENABLED0             : aliased ENABLED0_Register;
      --  indicates the state of the clock enable
      ENABLED1             : aliased ENABLED1_Register;
      --  Raw Interrupts
      INTR                 : aliased INTR_Register;
      --  Interrupt Enable
      INTE                 : aliased INTE_Register;
      --  Interrupt Force
      INTF                 : aliased INTF_Register;
      --  Interrupt status after masking & forcing
      INTS                 : aliased INTS_Register;
   end record
     with Volatile;

   for CLOCKS_Peripheral use record
      CLK_GPOUT0_CTRL      at 16#0# range 0 .. 31;
      CLK_GPOUT0_DIV       at 16#4# range 0 .. 31;
      CLK_GPOUT0_SELECTED  at 16#8# range 0 .. 31;
      CLK_GPOUT1_CTRL      at 16#C# range 0 .. 31;
      CLK_GPOUT1_DIV       at 16#10# range 0 .. 31;
      CLK_GPOUT1_SELECTED  at 16#14# range 0 .. 31;
      CLK_GPOUT2_CTRL      at 16#18# range 0 .. 31;
      CLK_GPOUT2_DIV       at 16#1C# range 0 .. 31;
      CLK_GPOUT2_SELECTED  at 16#20# range 0 .. 31;
      CLK_GPOUT3_CTRL      at 16#24# range 0 .. 31;
      CLK_GPOUT3_DIV       at 16#28# range 0 .. 31;
      CLK_GPOUT3_SELECTED  at 16#2C# range 0 .. 31;
      CLK_REF_CTRL         at 16#30# range 0 .. 31;
      CLK_REF_DIV          at 16#34# range 0 .. 31;
      CLK_REF_SELECTED     at 16#38# range 0 .. 31;
      CLK_SYS_CTRL         at 16#3C# range 0 .. 31;
      CLK_SYS_DIV          at 16#40# range 0 .. 31;
      CLK_SYS_SELECTED     at 16#44# range 0 .. 31;
      CLK_PERI_CTRL        at 16#48# range 0 .. 31;
      CLK_PERI_SELECTED    at 16#50# range 0 .. 31;
      CLK_USB_CTRL         at 16#54# range 0 .. 31;
      CLK_USB_DIV          at 16#58# range 0 .. 31;
      CLK_USB_SELECTED     at 16#5C# range 0 .. 31;
      CLK_ADC_CTRL         at 16#60# range 0 .. 31;
      CLK_ADC_DIV          at 16#64# range 0 .. 31;
      CLK_ADC_SELECTED     at 16#68# range 0 .. 31;
      CLK_RTC_CTRL         at 16#6C# range 0 .. 31;
      CLK_RTC_DIV          at 16#70# range 0 .. 31;
      CLK_RTC_SELECTED     at 16#74# range 0 .. 31;
      CLK_SYS_RESUS_CTRL   at 16#78# range 0 .. 31;
      CLK_SYS_RESUS_STATUS at 16#7C# range 0 .. 31;
      FC0_REF_KHZ          at 16#80# range 0 .. 31;
      FC0_MIN_KHZ          at 16#84# range 0 .. 31;
      FC0_MAX_KHZ          at 16#88# range 0 .. 31;
      FC0_DELAY            at 16#8C# range 0 .. 31;
      FC0_INTERVAL         at 16#90# range 0 .. 31;
      FC0_SRC              at 16#94# range 0 .. 31;
      FC0_STATUS           at 16#98# range 0 .. 31;
      FC0_RESULT           at 16#9C# range 0 .. 31;
      WAKE_EN0             at 16#A0# range 0 .. 31;
      WAKE_EN1             at 16#A4# range 0 .. 31;
      SLEEP_EN0            at 16#A8# range 0 .. 31;
      SLEEP_EN1            at 16#AC# range 0 .. 31;
      ENABLED0             at 16#B0# range 0 .. 31;
      ENABLED1             at 16#B4# range 0 .. 31;
      INTR                 at 16#B8# range 0 .. 31;
      INTE                 at 16#BC# range 0 .. 31;
      INTF                 at 16#C0# range 0 .. 31;
      INTS                 at 16#C4# range 0 .. 31;
   end record;

   CLOCKS_Periph : aliased CLOCKS_Peripheral
     with Import, Address => CLOCKS_Base;

end Interfaces.RP2040.CLOCKS;
