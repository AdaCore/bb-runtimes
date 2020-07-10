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

package Interfaces.SAM.PORT is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype WRCONFIG_PINMASK_Field is Interfaces.SAM.UInt16;
   subtype WRCONFIG_PMUXEN_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_INEN_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_PULLEN_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_DRVSTR_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_PMUX_Field is Interfaces.SAM.UInt4;
   subtype WRCONFIG_WRPMUX_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_WRPINCFG_Field is Interfaces.SAM.Bit;
   subtype WRCONFIG_HWSEL_Field is Interfaces.SAM.Bit;

   --  Write Configuration
   type WRCONFIG_Register is record
      --  Write-only. Pin Mask for Multiple Pin Configuration
      PINMASK        : WRCONFIG_PINMASK_Field := 16#0#;
      --  Write-only. Peripheral Multiplexer Enable
      PMUXEN         : WRCONFIG_PMUXEN_Field := 16#0#;
      --  Write-only. Input Enable
      INEN           : WRCONFIG_INEN_Field := 16#0#;
      --  Write-only. Pull Enable
      PULLEN         : WRCONFIG_PULLEN_Field := 16#0#;
      --  unspecified
      --  Reserved_19_21 : Interfaces.SAM.UInt3 := 16#0#;
      --  Write-only. Output Driver Strength Selection
      DRVSTR         : WRCONFIG_DRVSTR_Field := 16#0#;
      --  unspecified
      --  Reserved_23_23 : Interfaces.SAM.Bit := 16#0#;
      --  Write-only. Peripheral Multiplexing
      PMUX           : WRCONFIG_PMUX_Field := 16#0#;
      --  Write-only. Write PMUX
      WRPMUX         : WRCONFIG_WRPMUX_Field := 16#0#;
      --  unspecified
      --  Reserved_29_29 : Interfaces.SAM.Bit := 16#0#;
      --  Write-only. Write PINCFG
      WRPINCFG       : WRCONFIG_WRPINCFG_Field := 16#0#;
      --  Write-only. Half-Word Select
      HWSEL          : WRCONFIG_HWSEL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRCONFIG_Register use record
      PINMASK        at 0 range 0 .. 15;
      PMUXEN         at 0 range 16 .. 16;
      INEN           at 0 range 17 .. 17;
      PULLEN         at 0 range 18 .. 18;
      --  Reserved_19_21 at 0 range 19 .. 21;
      DRVSTR         at 0 range 22 .. 22;
      --  Reserved_23_23 at 0 range 23 .. 23;
      PMUX           at 0 range 24 .. 27;
      WRPMUX         at 0 range 28 .. 28;
      --  Reserved_29_29 at 0 range 29 .. 29;
      WRPINCFG       at 0 range 30 .. 30;
      HWSEL          at 0 range 31 .. 31;
   end record;

   --  Peripheral Multiplexing Even
   type PMUX0_PMUXESelect is
     (--  Peripheral function A selected
      A,
      --  Peripheral function B selected
      B,
      --  Peripheral function C selected
      C,
      --  Peripheral function D selected
      D,
      --  Peripheral function E selected
      E,
      --  Peripheral function F selected
      F,
      --  Peripheral function G selected
      G,
      --  Peripheral function H selected
      H)
     with Size => 4;
   for PMUX0_PMUXESelect use
     (A => 0,
      B => 1,
      C => 2,
      D => 3,
      E => 4,
      F => 5,
      G => 6,
      H => 7);

   --  Peripheral Multiplexing Odd
   type PMUX0_PMUXOSelect is
     (--  Peripheral function A selected
      A,
      --  Peripheral function B selected
      B,
      --  Peripheral function C selected
      C,
      --  Peripheral function D selected
      D,
      --  Peripheral function E selected
      E,
      --  Peripheral function F selected
      F,
      --  Peripheral function G selected
      G,
      --  Peripheral function H selected
      H)
     with Size => 4;
   for PMUX0_PMUXOSelect use
     (A => 0,
      B => 1,
      C => 2,
      D => 3,
      E => 4,
      F => 5,
      G => 6,
      H => 7);

   --  Peripheral Multiplexing n - Group 0
   type PMUX_Register is record
      --  Peripheral Multiplexing Even
      PMUXE : PMUX0_PMUXESelect := Interfaces.SAM.PORT.A;
      --  Peripheral Multiplexing Odd
      PMUXO : PMUX0_PMUXOSelect := Interfaces.SAM.PORT.A;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for PMUX_Register use record
      PMUXE at 0 range 0 .. 3;
      PMUXO at 0 range 4 .. 7;
   end record;

   --  Peripheral Multiplexing n - Group 0
   type PMUX_Registers is array (0 .. 15) of PMUX_Register;

   subtype PINCFG_PMUXEN_Field is Interfaces.SAM.Bit;
   subtype PINCFG_INEN_Field is Interfaces.SAM.Bit;
   subtype PINCFG_PULLEN_Field is Interfaces.SAM.Bit;
   subtype PINCFG_DRVSTR_Field is Interfaces.SAM.Bit;

   --  Pin Configuration n - Group 0
   type PINCFG_Register is record
      --  Peripheral Multiplexer Enable
      PMUXEN       : PINCFG_PMUXEN_Field := 16#0#;
      --  Input Enable
      INEN         : PINCFG_INEN_Field := 16#0#;
      --  Pull Enable
      PULLEN       : PINCFG_PULLEN_Field := 16#0#;
      --  unspecified
      Reserved_3_5 : Interfaces.SAM.UInt3 := 16#0#;
      --  Write-only. Output Driver Strength Selection
      DRVSTR       : PINCFG_DRVSTR_Field := 16#0#;
      --  unspecified
      Reserved_7_7 : Interfaces.SAM.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for PINCFG_Register use record
      PMUXEN       at 0 range 0 .. 0;
      INEN         at 0 range 1 .. 1;
      PULLEN       at 0 range 2 .. 2;
      Reserved_3_5 at 0 range 3 .. 5;
      DRVSTR       at 0 range 6 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
   end record;

   --  Pin Configuration n - Group 0
   type PINCFG_Registers is array (0 .. 31) of PINCFG_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  Port Module
   type PORT_Peripheral is record
      --  Data Direction
      DIR0      : aliased Interfaces.SAM.UInt32;
      --  Data Direction Clear
      DIRCLR0   : aliased Interfaces.SAM.UInt32;
      --  Data Direction Set
      DIRSET0   : aliased Interfaces.SAM.UInt32;
      --  Data Direction Toggle
      DIRTGL0   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value
      OUT0      : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Clear
      OUTCLR0   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Set
      OUTSET0   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Toggle
      OUTTGL0   : aliased Interfaces.SAM.UInt32;
      --  Data Input Value
      IN0       : aliased Interfaces.SAM.UInt32;
      --  Control
      CTRL0     : aliased Interfaces.SAM.UInt32;
      --  Write Configuration
      WRCONFIG0 : aliased WRCONFIG_Register;
      --  Peripheral Multiplexing n - Group 0
      PMUX0     : aliased PMUX_Registers;
      --  Pin Configuration n - Group 0
      PINCFG0   : aliased PINCFG_Registers;
      --  Data Direction
      DIR1      : aliased Interfaces.SAM.UInt32;
      --  Data Direction Clear
      DIRCLR1   : aliased Interfaces.SAM.UInt32;
      --  Data Direction Set
      DIRSET1   : aliased Interfaces.SAM.UInt32;
      --  Data Direction Toggle
      DIRTGL1   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value
      OUT1      : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Clear
      OUTCLR1   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Set
      OUTSET1   : aliased Interfaces.SAM.UInt32;
      --  Data Output Value Toggle
      OUTTGL1   : aliased Interfaces.SAM.UInt32;
      --  Data Input Value
      IN1       : aliased Interfaces.SAM.UInt32;
      --  Control
      CTRL1     : aliased Interfaces.SAM.UInt32;
      --  Write Configuration
      WRCONFIG1 : aliased WRCONFIG_Register;
      --  Peripheral Multiplexing n - Group 1
      PMUX1     : aliased PMUX_Registers;
      --  Pin Configuration n - Group 1
      PINCFG1   : aliased PINCFG_Registers;
   end record
     with Volatile;

   for PORT_Peripheral use record
      DIR0      at 16#0# range 0 .. 31;
      DIRCLR0   at 16#4# range 0 .. 31;
      DIRSET0   at 16#8# range 0 .. 31;
      DIRTGL0   at 16#C# range 0 .. 31;
      OUT0      at 16#10# range 0 .. 31;
      OUTCLR0   at 16#14# range 0 .. 31;
      OUTSET0   at 16#18# range 0 .. 31;
      OUTTGL0   at 16#1C# range 0 .. 31;
      IN0       at 16#20# range 0 .. 31;
      CTRL0     at 16#24# range 0 .. 31;
      WRCONFIG0 at 16#28# range 0 .. 31;
      PMUX0     at 16#30# range 0 .. 127;
      PINCFG0   at 16#40# range 0 .. 255;
      DIR1      at 16#80# range 0 .. 31;
      DIRCLR1   at 16#84# range 0 .. 31;
      DIRSET1   at 16#88# range 0 .. 31;
      DIRTGL1   at 16#8C# range 0 .. 31;
      OUT1      at 16#90# range 0 .. 31;
      OUTCLR1   at 16#94# range 0 .. 31;
      OUTSET1   at 16#98# range 0 .. 31;
      OUTTGL1   at 16#9C# range 0 .. 31;
      IN1       at 16#A0# range 0 .. 31;
      CTRL1     at 16#A4# range 0 .. 31;
      WRCONFIG1 at 16#A8# range 0 .. 31;
      PMUX1     at 16#B0# range 0 .. 127;
      PINCFG1   at 16#C0# range 0 .. 255;
   end record;

   --  Port Module
   PORT_Periph : aliased PORT_Peripheral
     with Import, Address => PORT_Base;

end Interfaces.SAM.PORT;
