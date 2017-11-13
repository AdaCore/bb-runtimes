--
--  Copyright (C) 2018, AdaCore
--

--  This spec has been automatically generated from M1AGL.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  interrupt-generating, programmable decrementing counter
package Interfaces.M1AGL.CoreTimer is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   type Control_Timer_Mode_Field is
     (
      Continuous,
      One_Shot)
     with Size => 1;
   for Control_Timer_Mode_Field use
     (Continuous => 0,
      One_Shot => 1);

   --  PWM Configuration Register.
   type Control_Register is record
      Enable           : Boolean := False;
      Interrupt_Enable : Boolean := False;
      Timer_Mode       : Control_Timer_Mode_Field :=
                          Interfaces.M1AGL.CoreTimer.Continuous;
      --  unspecified
      Reserved_3_31    : Interfaces.M1AGL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Control_Register use record
      Enable           at 0 range 0 .. 0;
      Interrupt_Enable at 0 range 1 .. 1;
      Timer_Mode       at 0 range 2 .. 2;
      Reserved_3_31    at 0 range 3 .. 31;
   end record;

   type Prescale_Value_Field is
     (
      Divide_By_2,
      Divide_By_4,
      Divide_By_8,
      Divide_By_16,
      Divide_By_32,
      Divide_By_64,
      Divide_By_128,
      Divide_By_256,
      Divide_By_512,
      Divide_By_1024)
     with Size => 4;
   for Prescale_Value_Field use
     (Divide_By_2 => 0,
      Divide_By_4 => 1,
      Divide_By_8 => 2,
      Divide_By_16 => 3,
      Divide_By_32 => 4,
      Divide_By_64 => 5,
      Divide_By_128 => 6,
      Divide_By_256 => 7,
      Divide_By_512 => 8,
      Divide_By_1024 => 9);

   --  Clock prescale setting
   type Prescale_Register is record
      Value         : Prescale_Value_Field :=
                       Interfaces.M1AGL.CoreTimer.Divide_By_2;
      --  unspecified
      Reserved_4_31 : Interfaces.M1AGL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Prescale_Register use record
      Value         at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PWM Configuration Register.
   type Raw_Interrupt_Status_Register is record
      Pending       : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.M1AGL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Raw_Interrupt_Status_Register use record
      Pending       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  PWM Configuration Register.
   type Masked_Interrupt_Status_Register is record
      Pending       : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.M1AGL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Masked_Interrupt_Status_Register use record
      Pending       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  interrupt-generating, programmable decrementing counter
   type CoreTimer_Peripheral is record
      --  Counter load value
      Load_Value              : aliased Interfaces.M1AGL.UInt32;
      --  Counter current value
      Current_Value           : aliased Interfaces.M1AGL.UInt32;
      --  PWM Configuration Register.
      Control                 : aliased Control_Register;
      --  Clock prescale setting
      Prescale                : aliased Prescale_Register;
      --  Interrupt Clear. Any write to this register will clear the interrupt.
      Interrupt_Clear         : aliased Interfaces.M1AGL.UInt32;
      --  PWM Configuration Register.
      Raw_Interrupt_Status    : aliased Raw_Interrupt_Status_Register;
      --  PWM Configuration Register.
      Masked_Interrupt_Status : aliased Masked_Interrupt_Status_Register;
   end record
     with Volatile;

   for CoreTimer_Peripheral use record
      Load_Value              at 16#0# range 0 .. 31;
      Current_Value           at 16#4# range 0 .. 31;
      Control                 at 16#8# range 0 .. 31;
      Prescale                at 16#C# range 0 .. 31;
      Interrupt_Clear         at 16#10# range 0 .. 31;
      Raw_Interrupt_Status    at 16#14# range 0 .. 31;
      Masked_Interrupt_Status at 16#18# range 0 .. 31;
   end record;

   --  interrupt-generating, programmable decrementing counter
   CoreTimer_Periph : aliased CoreTimer_Peripheral
     with Import, Address => CoreTimer_Base;

end Interfaces.M1AGL.CoreTimer;
