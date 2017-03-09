--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from M2Sxxx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  General-purpose IO peripheral
package Interfaces.SF2.GPIO is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Interrupt Type
   type CONFIG_TYPES_INT_Field is
     (
      --  Level High
      Int_Level_High,
      --  Level Low
      Int_Level_Low,
      --  Edge Positive
      Int_Edge_Positive,
      --  Edge_Negative
      Int_Edge_Negative,
      --  Edge Both
      Int_Edge_Both)
     with Size => 3;
   for CONFIG_TYPES_INT_Field use
     (Int_Level_High => 0,
      Int_Level_Low => 1,
      Int_Edge_Positive => 2,
      Int_Edge_Negative => 3,
      Int_Edge_Both => 4);

   --  No description provided for this register
   type CONFIG_Register is record
      --  Output Reg Enable
      EN_OUT        : Boolean := False;
      --  Input Reg Enable
      EN_IN         : Boolean := False;
      --  Output Buffer Enable
      EN_OE_BUF     : Boolean := False;
      --  Interrupt Enable
      EN_INT        : Boolean := False;
      --  unspecified
      Reserved_4_4  : Interfaces.SF2.Bit := 16#0#;
      --  Interrupt Type
      TYPES_INT     : CONFIG_TYPES_INT_Field :=
                       Interfaces.SF2.GPIO.Int_Level_High;
      --  unspecified
      Reserved_8_31 : Interfaces.SF2.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      EN_OUT        at 0 range 0 .. 0;
      EN_IN         at 0 range 1 .. 1;
      EN_OE_BUF     at 0 range 2 .. 2;
      EN_INT        at 0 range 3 .. 3;
      Reserved_4_4  at 0 range 4 .. 4;
      TYPES_INT     at 0 range 5 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  No description provided for this register
   type CONFIG_Registers is array (0 .. 31) of CONFIG_Register
     with Volatile;

   --  INTR_INT array
   type INTR_INT_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GPIO Interrupt register
   type INTR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INT as a value
            Val : Interfaces.SF2.UInt32;
         when True =>
            --  INT as an array
            Arr : INTR_INT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for INTR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIN_IN array
   type GPIN_IN_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GPIO input register - Read-only for input configured Ports
   type GPIN_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IN as a value
            Val : Interfaces.SF2.UInt32;
         when True =>
            --  IN as an array
            Arr : GPIN_IN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for GPIN_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPOUT_OUT array
   type GPOUT_OUT_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  GPIO Output register - Writeable/Readable for output configured ports.
   --  No action required for input configured ports.
   type GPOUT_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OUT as a value
            Val : Interfaces.SF2.UInt32;
         when True =>
            --  OUT as an array
            Arr : GPOUT_OUT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for GPOUT_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General-purpose IO peripheral
   type GPIO_Peripheral is record
      --  No description provided for this register
      CONFIG : aliased CONFIG_Registers;
      --  GPIO Interrupt register
      INTR   : aliased INTR_Register;
      --  GPIO input register - Read-only for input configured Ports
      GPIN   : aliased GPIN_Register;
      --  GPIO Output register - Writeable/Readable for output configured
      --  ports. No action required for input configured ports.
      GPOUT  : aliased GPOUT_Register;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      CONFIG at 16#0# range 0 .. 1023;
      INTR   at 16#80# range 0 .. 31;
      GPIN   at 16#84# range 0 .. 31;
      GPOUT  at 16#88# range 0 .. 31;
   end record;

   --  General-purpose IO peripheral
   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIO_Base;

end Interfaces.SF2.GPIO;
