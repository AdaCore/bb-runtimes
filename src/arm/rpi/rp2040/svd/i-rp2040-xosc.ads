--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

--  Controls the crystal oscillator
package Interfaces.RP2040.XOSC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Frequency range. This resets to 0xAA0 and cannot be changed.
   type CTRL_FREQ_RANGE_Field is
     (--  Reset value for the field
      CTRL_FREQ_RANGE_Field_Reset,
      Val_1_15MHZ,
      RESERVED_1,
      RESERVED_2,
      RESERVED_3)
     with Size => 12;
   for CTRL_FREQ_RANGE_Field use
     (CTRL_FREQ_RANGE_Field_Reset => 0,
      Val_1_15MHZ => 2720,
      RESERVED_1 => 2721,
      RESERVED_2 => 2722,
      RESERVED_3 => 2723);

   --  On power-up this field is initialised to DISABLE and the chip runs from
   --  the ROSC.\n If the chip has subsequently been programmed to run from the
   --  XOSC then setting this field to DISABLE may lock-up the chip. If this is
   --  a concern then run the clk_ref from the ROSC and enable the clk_sys
   --  RESUS feature.\n The 12-bit code is intended to give some protection
   --  against accidental writes. An invalid setting will enable the
   --  oscillator.
   type CTRL_ENABLE_Field is
     (--  Reset value for the field
      CTRL_ENABLE_Field_Reset,
      DISABLE,
      ENABLE)
     with Size => 12;
   for CTRL_ENABLE_Field use
     (CTRL_ENABLE_Field_Reset => 0,
      DISABLE => 3358,
      ENABLE => 4011);

   --  Crystal Oscillator Control
   type CTRL_Register is record
      --  Frequency range. This resets to 0xAA0 and cannot be changed.
      FREQ_RANGE     : CTRL_FREQ_RANGE_Field := CTRL_FREQ_RANGE_Field_Reset;
      --  On power-up this field is initialised to DISABLE and the chip runs
      --  from the ROSC.\n If the chip has subsequently been programmed to run
      --  from the XOSC then setting this field to DISABLE may lock-up the
      --  chip. If this is a concern then run the clk_ref from the ROSC and
      --  enable the clk_sys RESUS feature.\n The 12-bit code is intended to
      --  give some protection against accidental writes. An invalid setting
      --  will enable the oscillator.
      ENABLE         : CTRL_ENABLE_Field := CTRL_ENABLE_Field_Reset;
      --  unspecified
      Reserved_24_31 : Interfaces.RP2040.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      FREQ_RANGE     at 0 range 0 .. 11;
      ENABLE         at 0 range 12 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  The current frequency range setting, always reads 0
   type STATUS_FREQ_RANGE_Field is
     (Val_1_15MHZ,
      RESERVED_1,
      RESERVED_2,
      RESERVED_3)
     with Size => 2;
   for STATUS_FREQ_RANGE_Field use
     (Val_1_15MHZ => 0,
      RESERVED_1 => 1,
      RESERVED_2 => 2,
      RESERVED_3 => 3);

   subtype STATUS_ENABLED_Field is Interfaces.RP2040.Bit;
   subtype STATUS_BADWRITE_Field is Interfaces.RP2040.Bit;
   subtype STATUS_STABLE_Field is Interfaces.RP2040.Bit;

   --  Crystal Oscillator Status
   type STATUS_Register is record
      --  Read-only. The current frequency range setting, always reads 0
      FREQ_RANGE     : STATUS_FREQ_RANGE_Field :=
                        Interfaces.RP2040.XOSC.Val_1_15MHZ;
      --  unspecified
      Reserved_2_11  : Interfaces.RP2040.UInt10 := 16#0#;
      --  Read-only. Oscillator is enabled but not necessarily running and
      --  stable, resets to 0
      ENABLED        : STATUS_ENABLED_Field := 16#0#;
      --  unspecified
      Reserved_13_23 : Interfaces.RP2040.UInt11 := 16#0#;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. An invalid value has been written to CTRL_ENABLE or
      --  CTRL_FREQ_RANGE or DORMANT
      BADWRITE       : STATUS_BADWRITE_Field := 16#0#;
      --  unspecified
      Reserved_25_30 : Interfaces.RP2040.UInt6 := 16#0#;
      --  Read-only. Oscillator is running and stable
      STABLE         : STATUS_STABLE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      FREQ_RANGE     at 0 range 0 .. 1;
      Reserved_2_11  at 0 range 2 .. 11;
      ENABLED        at 0 range 12 .. 12;
      Reserved_13_23 at 0 range 13 .. 23;
      BADWRITE       at 0 range 24 .. 24;
      Reserved_25_30 at 0 range 25 .. 30;
      STABLE         at 0 range 31 .. 31;
   end record;

   subtype STARTUP_DELAY_Field is Interfaces.RP2040.UInt14;
   subtype STARTUP_X4_Field is Interfaces.RP2040.Bit;

   --  Controls the startup delay
   type STARTUP_Register is record
      --  in multiples of 256*xtal_period
      DELAY_k        : STARTUP_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_14_19 : Interfaces.RP2040.UInt6 := 16#0#;
      --  Multiplies the startup_delay by 4. This is of little value to the
      --  user given that the delay can be programmed directly
      X4             : STARTUP_X4_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STARTUP_Register use record
      DELAY_k        at 0 range 0 .. 13;
      Reserved_14_19 at 0 range 14 .. 19;
      X4             at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype COUNT_COUNT_Field is Interfaces.RP2040.Byte;

   --  A down counter running at the xosc frequency which counts to zero and
   --  stops.\n To start the counter write a non-zero value.\n Can be used for
   --  short software pauses when setting up time sensitive hardware.
   type COUNT_Register is record
      COUNT         : COUNT_COUNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.RP2040.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_Register use record
      COUNT         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Controls the crystal oscillator
   type XOSC_Peripheral is record
      --  Crystal Oscillator Control
      CTRL    : aliased CTRL_Register;
      --  Crystal Oscillator Status
      STATUS  : aliased STATUS_Register;
      --  Crystal Oscillator pause control\n This is used to save power by
      --  pausing the XOSC\n On power-up this field is initialised to WAKE\n An
      --  invalid write will also select WAKE\n WARNING: stop the PLLs before
      --  selecting dormant mode\n WARNING: setup the irq before selecting
      --  dormant mode
      DORMANT : aliased Interfaces.RP2040.UInt32;
      --  Controls the startup delay
      STARTUP : aliased STARTUP_Register;
      --  A down counter running at the xosc frequency which counts to zero and
      --  stops.\n To start the counter write a non-zero value.\n Can be used
      --  for short software pauses when setting up time sensitive hardware.
      COUNT   : aliased COUNT_Register;
   end record
     with Volatile;

   for XOSC_Peripheral use record
      CTRL    at 16#0# range 0 .. 31;
      STATUS  at 16#4# range 0 .. 31;
      DORMANT at 16#8# range 0 .. 31;
      STARTUP at 16#C# range 0 .. 31;
      COUNT   at 16#1C# range 0 .. 31;
   end record;

   --  Controls the crystal oscillator
   XOSC_Periph : aliased XOSC_Peripheral
     with Import, Address => XOSC_Base;

end Interfaces.RP2040.XOSC;
