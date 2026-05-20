--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

package Interfaces.RP2040.PSM is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype FRCE_ON_rosc_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_xosc_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_clocks_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_resets_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_busfabric_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_rom_Field is Interfaces.RP2040.Bit;
   --  FRCE_ON_sram array element
   subtype FRCE_ON_sram_Element is Interfaces.RP2040.Bit;

   --  FRCE_ON_sram array
   type FRCE_ON_sram_Field_Array is array (0 .. 5) of FRCE_ON_sram_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for FRCE_ON_sram
   type FRCE_ON_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  sram as a value
            Val : Interfaces.RP2040.UInt6;
         when True =>
            --  sram as an array
            Arr : FRCE_ON_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for FRCE_ON_sram_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype FRCE_ON_xip_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype FRCE_ON_sio_Field is Interfaces.RP2040.Bit;
   --  FRCE_ON_proc array element
   subtype FRCE_ON_proc_Element is Interfaces.RP2040.Bit;

   --  FRCE_ON_proc array
   type FRCE_ON_proc_Field_Array is array (0 .. 1) of FRCE_ON_proc_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for FRCE_ON_proc
   type FRCE_ON_proc_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  proc as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  proc as an array
            Arr : FRCE_ON_proc_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FRCE_ON_proc_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Force block out of reset (i.e. power it on)
   type FRCE_ON_Register is record
      rosc                : FRCE_ON_rosc_Field := 16#0#;
      xosc                : FRCE_ON_xosc_Field := 16#0#;
      clocks              : FRCE_ON_clocks_Field := 16#0#;
      resets              : FRCE_ON_resets_Field := 16#0#;
      busfabric           : FRCE_ON_busfabric_Field := 16#0#;
      rom                 : FRCE_ON_rom_Field := 16#0#;
      sram                : FRCE_ON_sram_Field :=
                             (As_Array => False, Val => 16#0#);
      xip                 : FRCE_ON_xip_Field := 16#0#;
      vreg_and_chip_reset : FRCE_ON_vreg_and_chip_reset_Field := 16#0#;
      sio                 : FRCE_ON_sio_Field := 16#0#;
      proc                : FRCE_ON_proc_Field :=
                             (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_31      : Interfaces.RP2040.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRCE_ON_Register use record
      rosc                at 0 range 0 .. 0;
      xosc                at 0 range 1 .. 1;
      clocks              at 0 range 2 .. 2;
      resets              at 0 range 3 .. 3;
      busfabric           at 0 range 4 .. 4;
      rom                 at 0 range 5 .. 5;
      sram                at 0 range 6 .. 11;
      xip                 at 0 range 12 .. 12;
      vreg_and_chip_reset at 0 range 13 .. 13;
      sio                 at 0 range 14 .. 14;
      proc                at 0 range 15 .. 16;
      Reserved_17_31      at 0 range 17 .. 31;
   end record;

   subtype FRCE_OFF_rosc_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_xosc_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_clocks_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_resets_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_busfabric_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_rom_Field is Interfaces.RP2040.Bit;
   --  FRCE_OFF_sram array element
   subtype FRCE_OFF_sram_Element is Interfaces.RP2040.Bit;

   --  FRCE_OFF_sram array
   type FRCE_OFF_sram_Field_Array is array (0 .. 5) of FRCE_OFF_sram_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for FRCE_OFF_sram
   type FRCE_OFF_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  sram as a value
            Val : Interfaces.RP2040.UInt6;
         when True =>
            --  sram as an array
            Arr : FRCE_OFF_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for FRCE_OFF_sram_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype FRCE_OFF_xip_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype FRCE_OFF_sio_Field is Interfaces.RP2040.Bit;
   --  FRCE_OFF_proc array element
   subtype FRCE_OFF_proc_Element is Interfaces.RP2040.Bit;

   --  FRCE_OFF_proc array
   type FRCE_OFF_proc_Field_Array is array (0 .. 1) of FRCE_OFF_proc_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for FRCE_OFF_proc
   type FRCE_OFF_proc_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  proc as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  proc as an array
            Arr : FRCE_OFF_proc_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FRCE_OFF_proc_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Force into reset (i.e. power it off)
   type FRCE_OFF_Register is record
      rosc                : FRCE_OFF_rosc_Field := 16#0#;
      xosc                : FRCE_OFF_xosc_Field := 16#0#;
      clocks              : FRCE_OFF_clocks_Field := 16#0#;
      resets              : FRCE_OFF_resets_Field := 16#0#;
      busfabric           : FRCE_OFF_busfabric_Field := 16#0#;
      rom                 : FRCE_OFF_rom_Field := 16#0#;
      sram                : FRCE_OFF_sram_Field :=
                             (As_Array => False, Val => 16#0#);
      xip                 : FRCE_OFF_xip_Field := 16#0#;
      vreg_and_chip_reset : FRCE_OFF_vreg_and_chip_reset_Field := 16#0#;
      sio                 : FRCE_OFF_sio_Field := 16#0#;
      proc                : FRCE_OFF_proc_Field :=
                             (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_31      : Interfaces.RP2040.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRCE_OFF_Register use record
      rosc                at 0 range 0 .. 0;
      xosc                at 0 range 1 .. 1;
      clocks              at 0 range 2 .. 2;
      resets              at 0 range 3 .. 3;
      busfabric           at 0 range 4 .. 4;
      rom                 at 0 range 5 .. 5;
      sram                at 0 range 6 .. 11;
      xip                 at 0 range 12 .. 12;
      vreg_and_chip_reset at 0 range 13 .. 13;
      sio                 at 0 range 14 .. 14;
      proc                at 0 range 15 .. 16;
      Reserved_17_31      at 0 range 17 .. 31;
   end record;

   subtype WDSEL_rosc_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_xosc_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_clocks_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_resets_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_busfabric_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_rom_Field is Interfaces.RP2040.Bit;
   --  WDSEL_sram array element
   subtype WDSEL_sram_Element is Interfaces.RP2040.Bit;

   --  WDSEL_sram array
   type WDSEL_sram_Field_Array is array (0 .. 5) of WDSEL_sram_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for WDSEL_sram
   type WDSEL_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  sram as a value
            Val : Interfaces.RP2040.UInt6;
         when True =>
            --  sram as an array
            Arr : WDSEL_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for WDSEL_sram_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype WDSEL_xip_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_sio_Field is Interfaces.RP2040.Bit;
   --  WDSEL_proc array element
   subtype WDSEL_proc_Element is Interfaces.RP2040.Bit;

   --  WDSEL_proc array
   type WDSEL_proc_Field_Array is array (0 .. 1) of WDSEL_proc_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WDSEL_proc
   type WDSEL_proc_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  proc as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  proc as an array
            Arr : WDSEL_proc_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WDSEL_proc_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Set to 1 if this peripheral should be reset when the watchdog fires.
   type WDSEL_Register is record
      rosc                : WDSEL_rosc_Field := 16#0#;
      xosc                : WDSEL_xosc_Field := 16#0#;
      clocks              : WDSEL_clocks_Field := 16#0#;
      resets              : WDSEL_resets_Field := 16#0#;
      busfabric           : WDSEL_busfabric_Field := 16#0#;
      rom                 : WDSEL_rom_Field := 16#0#;
      sram                : WDSEL_sram_Field :=
                             (As_Array => False, Val => 16#0#);
      xip                 : WDSEL_xip_Field := 16#0#;
      vreg_and_chip_reset : WDSEL_vreg_and_chip_reset_Field := 16#0#;
      sio                 : WDSEL_sio_Field := 16#0#;
      proc                : WDSEL_proc_Field :=
                             (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_17_31      : Interfaces.RP2040.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDSEL_Register use record
      rosc                at 0 range 0 .. 0;
      xosc                at 0 range 1 .. 1;
      clocks              at 0 range 2 .. 2;
      resets              at 0 range 3 .. 3;
      busfabric           at 0 range 4 .. 4;
      rom                 at 0 range 5 .. 5;
      sram                at 0 range 6 .. 11;
      xip                 at 0 range 12 .. 12;
      vreg_and_chip_reset at 0 range 13 .. 13;
      sio                 at 0 range 14 .. 14;
      proc                at 0 range 15 .. 16;
      Reserved_17_31      at 0 range 17 .. 31;
   end record;

   subtype DONE_rosc_Field is Interfaces.RP2040.Bit;
   subtype DONE_xosc_Field is Interfaces.RP2040.Bit;
   subtype DONE_clocks_Field is Interfaces.RP2040.Bit;
   subtype DONE_resets_Field is Interfaces.RP2040.Bit;
   subtype DONE_busfabric_Field is Interfaces.RP2040.Bit;
   subtype DONE_rom_Field is Interfaces.RP2040.Bit;
   --  DONE_sram array element
   subtype DONE_sram_Element is Interfaces.RP2040.Bit;

   --  DONE_sram array
   type DONE_sram_Field_Array is array (0 .. 5) of DONE_sram_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for DONE_sram
   type DONE_sram_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  sram as a value
            Val : Interfaces.RP2040.UInt6;
         when True =>
            --  sram as an array
            Arr : DONE_sram_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for DONE_sram_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype DONE_xip_Field is Interfaces.RP2040.Bit;
   subtype DONE_vreg_and_chip_reset_Field is Interfaces.RP2040.Bit;
   subtype DONE_sio_Field is Interfaces.RP2040.Bit;
   --  DONE_proc array element
   subtype DONE_proc_Element is Interfaces.RP2040.Bit;

   --  DONE_proc array
   type DONE_proc_Field_Array is array (0 .. 1) of DONE_proc_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for DONE_proc
   type DONE_proc_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  proc as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  proc as an array
            Arr : DONE_proc_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DONE_proc_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Indicates the peripheral's registers are ready to access.
   type DONE_Register is record
      --  Read-only.
      rosc                : DONE_rosc_Field;
      --  Read-only.
      xosc                : DONE_xosc_Field;
      --  Read-only.
      clocks              : DONE_clocks_Field;
      --  Read-only.
      resets              : DONE_resets_Field;
      --  Read-only.
      busfabric           : DONE_busfabric_Field;
      --  Read-only.
      rom                 : DONE_rom_Field;
      --  Read-only.
      sram                : DONE_sram_Field;
      --  Read-only.
      xip                 : DONE_xip_Field;
      --  Read-only.
      vreg_and_chip_reset : DONE_vreg_and_chip_reset_Field;
      --  Read-only.
      sio                 : DONE_sio_Field;
      --  Read-only.
      proc                : DONE_proc_Field;
      --  unspecified
      Reserved_17_31      : Interfaces.RP2040.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DONE_Register use record
      rosc                at 0 range 0 .. 0;
      xosc                at 0 range 1 .. 1;
      clocks              at 0 range 2 .. 2;
      resets              at 0 range 3 .. 3;
      busfabric           at 0 range 4 .. 4;
      rom                 at 0 range 5 .. 5;
      sram                at 0 range 6 .. 11;
      xip                 at 0 range 12 .. 12;
      vreg_and_chip_reset at 0 range 13 .. 13;
      sio                 at 0 range 14 .. 14;
      proc                at 0 range 15 .. 16;
      Reserved_17_31      at 0 range 17 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type PSM_Peripheral is record
      --  Force block out of reset (i.e. power it on)
      FRCE_ON  : aliased FRCE_ON_Register;
      --  Force into reset (i.e. power it off)
      FRCE_OFF : aliased FRCE_OFF_Register;
      --  Set to 1 if this peripheral should be reset when the watchdog fires.
      WDSEL    : aliased WDSEL_Register;
      --  Indicates the peripheral's registers are ready to access.
      DONE     : aliased DONE_Register;
   end record
     with Volatile;

   for PSM_Peripheral use record
      FRCE_ON  at 16#0# range 0 .. 31;
      FRCE_OFF at 16#4# range 0 .. 31;
      WDSEL    at 16#8# range 0 .. 31;
      DONE     at 16#C# range 0 .. 31;
   end record;

   PSM_Periph : aliased PSM_Peripheral
     with Import, Address => PSM_Base;

end Interfaces.RP2040.PSM;
