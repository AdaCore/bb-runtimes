--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.WWDG is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_T_Field is Interfaces.STM32.UInt7;
   subtype CR_WDGA_Field is Interfaces.STM32.Bit;

   --  Control register (WWDG_CR)
   type CR_Register is record
      --  7-bit counter (MSB to LSB)
      T             : CR_T_Field := 16#7F#;
      --  Activation bit
      WDGA          : CR_WDGA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      T             at 0 range 0 .. 6;
      WDGA          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CFR_W_Field is Interfaces.STM32.UInt7;
   subtype CFR_WDGTB_Field is Interfaces.STM32.UInt2;
   subtype CFR_EWI_Field is Interfaces.STM32.Bit;

   --  Configuration register (WWDG_CFR)
   type CFR_Register is record
      --  7-bit window value
      W              : CFR_W_Field := 16#7F#;
      --  Timer Base
      WDGTB          : CFR_WDGTB_Field := 16#0#;
      --  Early Wakeup Interrupt
      EWI            : CFR_EWI_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.STM32.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFR_Register use record
      W              at 0 range 0 .. 6;
      WDGTB          at 0 range 7 .. 8;
      EWI            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SR_EWI_Field is Interfaces.STM32.Bit;

   --  Status register (WWDG_SR)
   type SR_Register is record
      --  Early Wakeup Interrupt
      EWI           : SR_EWI_Field := 16#0#;
      --  unspecified
      Reserved_1_31 : Interfaces.STM32.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      EWI           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Window watchdog
   type WWDG_Peripheral is record
      --  Control register (WWDG_CR)
      CR  : aliased CR_Register;
      --  Configuration register (WWDG_CFR)
      CFR : aliased CFR_Register;
      --  Status register (WWDG_SR)
      SR  : aliased SR_Register;
   end record
     with Volatile;

   for WWDG_Peripheral use record
      CR  at 16#0# range 0 .. 31;
      CFR at 16#4# range 0 .. 31;
      SR  at 16#8# range 0 .. 31;
   end record;

   --  Window watchdog
   WWDG_Periph : aliased WWDG_Peripheral
     with Import, Address => WWDG_Base;

end Interfaces.STM32.WWDG;
