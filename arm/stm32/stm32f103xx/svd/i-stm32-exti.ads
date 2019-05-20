--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.EXTI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  IMR_MR array element
   subtype IMR_MR_Element is Interfaces.STM32.Bit;

   --  IMR_MR array
   type IMR_MR_Field_Array is array (0 .. 18) of IMR_MR_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for IMR_MR
   type IMR_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  MR as an array
            Arr : IMR_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for IMR_MR_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Interrupt mask register (EXTI_IMR)
   type IMR_Register is record
      --  Interrupt Mask on line 0
      MR             : IMR_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      MR             at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  EMR_MR array element
   subtype EMR_MR_Element is Interfaces.STM32.Bit;

   --  EMR_MR array
   type EMR_MR_Field_Array is array (0 .. 18) of EMR_MR_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for EMR_MR
   type EMR_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  MR as an array
            Arr : EMR_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for EMR_MR_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Event mask register (EXTI_EMR)
   type EMR_Register is record
      --  Event Mask on line 0
      MR             : EMR_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMR_Register use record
      MR             at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RTSR_TR array element
   subtype RTSR_TR_Element is Interfaces.STM32.Bit;

   --  RTSR_TR array
   type RTSR_TR_Field_Array is array (0 .. 18) of RTSR_TR_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for RTSR_TR
   type RTSR_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  TR as an array
            Arr : RTSR_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for RTSR_TR_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Rising Trigger selection register (EXTI_RTSR)
   type RTSR_Register is record
      --  Rising trigger event configuration of line 0
      TR             : RTSR_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR_Register use record
      TR             at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  FTSR_TR array element
   subtype FTSR_TR_Element is Interfaces.STM32.Bit;

   --  FTSR_TR array
   type FTSR_TR_Field_Array is array (0 .. 18) of FTSR_TR_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for FTSR_TR
   type FTSR_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  TR as an array
            Arr : FTSR_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for FTSR_TR_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Falling Trigger selection register (EXTI_FTSR)
   type FTSR_Register is record
      --  Falling trigger event configuration of line 0
      TR             : FTSR_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR_Register use record
      TR             at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  SWIER array element
   subtype SWIER_Element is Interfaces.STM32.Bit;

   --  SWIER array
   type SWIER_Field_Array is array (0 .. 18) of SWIER_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for SWIER
   type SWIER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWIER as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  SWIER as an array
            Arr : SWIER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for SWIER_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Software interrupt event register (EXTI_SWIER)
   type SWIER_Register is record
      --  Software Interrupt on line 0
      SWIER          : SWIER_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER_Register use record
      SWIER          at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  PR array element
   subtype PR_Element is Interfaces.STM32.Bit;

   --  PR array
   type PR_Field_Array is array (0 .. 18) of PR_Element
     with Component_Size => 1, Size => 19;

   --  Type definition for PR
   type PR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PR as a value
            Val : Interfaces.STM32.UInt19;
         when True =>
            --  PR as an array
            Arr : PR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 19;

   for PR_Field use record
      Val at 0 range 0 .. 18;
      Arr at 0 range 0 .. 18;
   end record;

   --  Pending register (EXTI_PR)
   type PR_Register is record
      --  Pending bit 0
      PR             : PR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_31 : Interfaces.STM32.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR_Register use record
      PR             at 0 range 0 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  EXTI
   type EXTI_Peripheral is record
      --  Interrupt mask register (EXTI_IMR)
      IMR   : aliased IMR_Register;
      --  Event mask register (EXTI_EMR)
      EMR   : aliased EMR_Register;
      --  Rising Trigger selection register (EXTI_RTSR)
      RTSR  : aliased RTSR_Register;
      --  Falling Trigger selection register (EXTI_FTSR)
      FTSR  : aliased FTSR_Register;
      --  Software interrupt event register (EXTI_SWIER)
      SWIER : aliased SWIER_Register;
      --  Pending register (EXTI_PR)
      PR    : aliased PR_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      IMR   at 16#0# range 0 .. 31;
      EMR   at 16#4# range 0 .. 31;
      RTSR  at 16#8# range 0 .. 31;
      FTSR  at 16#C# range 0 .. 31;
      SWIER at 16#10# range 0 .. 31;
      PR    at 16#14# range 0 .. 31;
   end record;

   --  EXTI
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

end Interfaces.STM32.EXTI;
