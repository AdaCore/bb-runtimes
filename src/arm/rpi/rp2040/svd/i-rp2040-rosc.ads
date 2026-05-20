--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

package Interfaces.RP2040.ROSC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Controls the number of delay stages in the ROSC ring\n LOW uses stages 0
   --  to 7\n MEDIUM uses stages 0 to 5\n HIGH uses stages 0 to 3\n TOOHIGH
   --  uses stages 0 to 1 and should not be used because its frequency exceeds
   --  design specifications\n The clock output will not glitch when changing
   --  the range up one step at a time\n The clock output will glitch when
   --  changing the range down\n Note: the values here are gray coded which is
   --  why HIGH comes before TOOHIGH
   type CTRL_FREQ_RANGE_Field is
     (--  Reset value for the field
      CTRL_FREQ_RANGE_Field_Reset,
      LOW,
      MEDIUM,
      TOOHIGH,
      HIGH)
     with Size => 12;
   for CTRL_FREQ_RANGE_Field use
     (CTRL_FREQ_RANGE_Field_Reset => 2720,
      LOW => 4004,
      MEDIUM => 4005,
      TOOHIGH => 4006,
      HIGH => 4007);

   --  On power-up this field is initialised to ENABLE\n The system clock must
   --  be switched to another source before setting this field to DISABLE
   --  otherwise the chip will lock up\n The 12-bit code is intended to give
   --  some protection against accidental writes. An invalid setting will
   --  enable the oscillator.
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

   --  Ring Oscillator control
   type CTRL_Register is record
      --  Controls the number of delay stages in the ROSC ring\n LOW uses
      --  stages 0 to 7\n MEDIUM uses stages 0 to 5\n HIGH uses stages 0 to 3\n
      --  TOOHIGH uses stages 0 to 1 and should not be used because its
      --  frequency exceeds design specifications\n The clock output will not
      --  glitch when changing the range up one step at a time\n The clock
      --  output will glitch when changing the range down\n Note: the values
      --  here are gray coded which is why HIGH comes before TOOHIGH
      FREQ_RANGE     : CTRL_FREQ_RANGE_Field := CTRL_FREQ_RANGE_Field_Reset;
      --  On power-up this field is initialised to ENABLE\n The system clock
      --  must be switched to another source before setting this field to
      --  DISABLE otherwise the chip will lock up\n The 12-bit code is intended
      --  to give some protection against accidental writes. An invalid setting
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

   subtype FREQA_DS0_Field is Interfaces.RP2040.UInt3;
   subtype FREQA_DS1_Field is Interfaces.RP2040.UInt3;
   subtype FREQA_DS2_Field is Interfaces.RP2040.UInt3;
   subtype FREQA_DS3_Field is Interfaces.RP2040.UInt3;

   --  Set to 0x9696 to apply the settings\n Any other value in this field will
   --  set all drive strengths to 0
   type FREQA_PASSWD_Field is
     (--  Reset value for the field
      FREQA_PASSWD_Field_Reset,
      PASS)
     with Size => 16;
   for FREQA_PASSWD_Field use
     (FREQA_PASSWD_Field_Reset => 0,
      PASS => 38550);

   --  The FREQA & FREQB registers control the frequency by controlling the
   --  drive strength of each stage\n The drive strength has 4 levels
   --  determined by the number of bits set\n Increasing the number of bits set
   --  increases the drive strength and increases the oscillation frequency\n 0
   --  bits set is the default drive strength\n 1 bit set doubles the drive
   --  strength\n 2 bits set triples drive strength\n 3 bits set quadruples
   --  drive strength
   type FREQA_Register is record
      --  Stage 0 drive strength
      DS0            : FREQA_DS0_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 1 drive strength
      DS1            : FREQA_DS1_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 2 drive strength
      DS2            : FREQA_DS2_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 3 drive strength
      DS3            : FREQA_DS3_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.RP2040.Bit := 16#0#;
      --  Set to 0x9696 to apply the settings\n Any other value in this field
      --  will set all drive strengths to 0
      PASSWD         : FREQA_PASSWD_Field := FREQA_PASSWD_Field_Reset;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQA_Register use record
      DS0            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DS1            at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DS2            at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      DS3            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PASSWD         at 0 range 16 .. 31;
   end record;

   subtype FREQB_DS4_Field is Interfaces.RP2040.UInt3;
   subtype FREQB_DS5_Field is Interfaces.RP2040.UInt3;
   subtype FREQB_DS6_Field is Interfaces.RP2040.UInt3;
   subtype FREQB_DS7_Field is Interfaces.RP2040.UInt3;

   --  Set to 0x9696 to apply the settings\n Any other value in this field will
   --  set all drive strengths to 0
   type FREQB_PASSWD_Field is
     (--  Reset value for the field
      FREQB_PASSWD_Field_Reset,
      PASS)
     with Size => 16;
   for FREQB_PASSWD_Field use
     (FREQB_PASSWD_Field_Reset => 0,
      PASS => 38550);

   --  For a detailed description see freqa register
   type FREQB_Register is record
      --  Stage 4 drive strength
      DS4            : FREQB_DS4_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 5 drive strength
      DS5            : FREQB_DS5_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 6 drive strength
      DS6            : FREQB_DS6_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : Interfaces.RP2040.Bit := 16#0#;
      --  Stage 7 drive strength
      DS7            : FREQB_DS7_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.RP2040.Bit := 16#0#;
      --  Set to 0x9696 to apply the settings\n Any other value in this field
      --  will set all drive strengths to 0
      PASSWD         : FREQB_PASSWD_Field := FREQB_PASSWD_Field_Reset;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQB_Register use record
      DS4            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DS5            at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DS6            at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      DS7            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PASSWD         at 0 range 16 .. 31;
   end record;

   --  set to 0xaa0 + div where\n div = 0 divides by 32\n div = 1-31 divides by
   --  div\n any other value sets div=0 and therefore divides by 32\n this
   --  register resets to div=16
   type DIV_DIV_Field is
     (--  Reset value for the field
      DIV_DIV_Field_Reset,
      PASS)
     with Size => 12;
   for DIV_DIV_Field use
     (DIV_DIV_Field_Reset => 0,
      PASS => 2720);

   --  Controls the output divider
   type DIV_Register is record
      --  set to 0xaa0 + div where\n div = 0 divides by 32\n div = 1-31 divides
      --  by div\n any other value sets div=0 and therefore divides by 32\n
      --  this register resets to div=16
      DIV            : DIV_DIV_Field := DIV_DIV_Field_Reset;
      --  unspecified
      Reserved_12_31 : Interfaces.RP2040.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIV_Register use record
      DIV            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype PHASE_SHIFT_Field is Interfaces.RP2040.UInt2;
   subtype PHASE_FLIP_Field is Interfaces.RP2040.Bit;
   subtype PHASE_ENABLE_Field is Interfaces.RP2040.Bit;
   subtype PHASE_PASSWD_Field is Interfaces.RP2040.Byte;

   --  Controls the phase shifted output
   type PHASE_Register is record
      --  phase shift the phase-shifted output by SHIFT input clocks\n this can
      --  be changed on-the-fly\n must be set to 0 before setting div=1
      SHIFT          : PHASE_SHIFT_Field := 16#0#;
      --  invert the phase-shifted output\n this is ignored when div=1
      FLIP           : PHASE_FLIP_Field := 16#0#;
      --  enable the phase-shifted output\n this can be changed on-the-fly
      ENABLE         : PHASE_ENABLE_Field := 16#1#;
      --  set to 0xaa0\n any other value enables the output with shift=0
      PASSWD         : PHASE_PASSWD_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.RP2040.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PHASE_Register use record
      SHIFT          at 0 range 0 .. 1;
      FLIP           at 0 range 2 .. 2;
      ENABLE         at 0 range 3 .. 3;
      PASSWD         at 0 range 4 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype STATUS_ENABLED_Field is Interfaces.RP2040.Bit;
   subtype STATUS_DIV_RUNNING_Field is Interfaces.RP2040.Bit;
   subtype STATUS_BADWRITE_Field is Interfaces.RP2040.Bit;
   subtype STATUS_STABLE_Field is Interfaces.RP2040.Bit;

   --  Ring Oscillator Status
   type STATUS_Register is record
      --  unspecified
      Reserved_0_11  : Interfaces.RP2040.UInt12 := 16#0#;
      --  Read-only. Oscillator is enabled but not necessarily running and
      --  stable\n this resets to 0 but transitions to 1 during chip startup
      ENABLED        : STATUS_ENABLED_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.RP2040.UInt3 := 16#0#;
      --  Read-only. post-divider is running\n this resets to 0 but transitions
      --  to 1 during chip startup
      DIV_RUNNING    : STATUS_DIV_RUNNING_Field := 16#0#;
      --  unspecified
      Reserved_17_23 : Interfaces.RP2040.UInt7 := 16#0#;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. An invalid value has been written to CTRL_ENABLE or
      --  CTRL_FREQ_RANGE or FRFEQA or FREQB or DORMANT
      BADWRITE       : STATUS_BADWRITE_Field := 16#0#;
      --  unspecified
      Reserved_25_30 : Interfaces.RP2040.UInt6 := 16#0#;
      --  Read-only. Oscillator is running and stable
      STABLE         : STATUS_STABLE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      ENABLED        at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DIV_RUNNING    at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      BADWRITE       at 0 range 24 .. 24;
      Reserved_25_30 at 0 range 25 .. 30;
      STABLE         at 0 range 31 .. 31;
   end record;

   subtype RANDOMBIT_RANDOMBIT_Field is Interfaces.RP2040.Bit;

   --  This just reads the state of the oscillator output so randomness is
   --  compromised if the ring oscillator is stopped or run at a harmonic of
   --  the bus frequency
   type RANDOMBIT_Register is record
      --  Read-only.
      RANDOMBIT     : RANDOMBIT_RANDOMBIT_Field;
      --  unspecified
      Reserved_1_31 : Interfaces.RP2040.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RANDOMBIT_Register use record
      RANDOMBIT     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype COUNT_COUNT_Field is Interfaces.RP2040.Byte;

   --  A down counter running at the ROSC frequency which counts to zero and
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

   type ROSC_Peripheral is record
      --  Ring Oscillator control
      CTRL      : aliased CTRL_Register;
      --  The FREQA & FREQB registers control the frequency by controlling the
      --  drive strength of each stage\n The drive strength has 4 levels
      --  determined by the number of bits set\n Increasing the number of bits
      --  set increases the drive strength and increases the oscillation
      --  frequency\n 0 bits set is the default drive strength\n 1 bit set
      --  doubles the drive strength\n 2 bits set triples drive strength\n 3
      --  bits set quadruples drive strength
      FREQA     : aliased FREQA_Register;
      --  For a detailed description see freqa register
      FREQB     : aliased FREQB_Register;
      --  Ring Oscillator pause control\n This is used to save power by pausing
      --  the ROSC\n On power-up this field is initialised to WAKE\n An invalid
      --  write will also select WAKE\n Warning: setup the irq before selecting
      --  dormant mode
      DORMANT   : aliased Interfaces.RP2040.UInt32;
      --  Controls the output divider
      DIV       : aliased DIV_Register;
      --  Controls the phase shifted output
      PHASE     : aliased PHASE_Register;
      --  Ring Oscillator Status
      STATUS    : aliased STATUS_Register;
      --  This just reads the state of the oscillator output so randomness is
      --  compromised if the ring oscillator is stopped or run at a harmonic of
      --  the bus frequency
      RANDOMBIT : aliased RANDOMBIT_Register;
      --  A down counter running at the ROSC frequency which counts to zero and
      --  stops.\n To start the counter write a non-zero value.\n Can be used
      --  for short software pauses when setting up time sensitive hardware.
      COUNT     : aliased COUNT_Register;
   end record
     with Volatile;

   for ROSC_Peripheral use record
      CTRL      at 16#0# range 0 .. 31;
      FREQA     at 16#4# range 0 .. 31;
      FREQB     at 16#8# range 0 .. 31;
      DORMANT   at 16#C# range 0 .. 31;
      DIV       at 16#10# range 0 .. 31;
      PHASE     at 16#14# range 0 .. 31;
      STATUS    at 16#18# range 0 .. 31;
      RANDOMBIT at 16#1C# range 0 .. 31;
      COUNT     at 16#20# range 0 .. 31;
   end record;

   ROSC_Periph : aliased ROSC_Peripheral
     with Import, Address => ROSC_Base;

end Interfaces.RP2040.ROSC;
