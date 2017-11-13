--
--  Copyright (C) 2018, AdaCore
--

--  This spec has been automatically generated from M1AGL.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Flexible Interrupt Controller for Advanced Microcontroller
--        Bus Architecture (AMBA) Based Systems
--
package Interfaces.M1AGL.CoreInterrupt is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  IRQ_Soft_Interrupt_IRQ array
   type IRQ_Soft_Interrupt_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Soft_Interrupt_IRQ
   type IRQ_Soft_Interrupt_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Soft_Interrupt_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Soft_Interrupt_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Software interrupt trigger IRQs.
   type IRQ_Soft_Interrupt_Register is record
      IRQ           : IRQ_Soft_Interrupt_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Soft_Interrupt_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IRQ_Soft_Interrupt_Clear_IRQ array
   type IRQ_Soft_Interrupt_Clear_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Soft_Interrupt_Clear_IRQ
   type IRQ_Soft_Interrupt_Clear_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Soft_Interrupt_Clear_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Soft_Interrupt_Clear_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Clear soft IRQs.
   type IRQ_Soft_Interrupt_Clear_Register is record
      IRQ           : IRQ_Soft_Interrupt_Clear_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Soft_Interrupt_Clear_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IRQ_Enable_IRQ array
   type IRQ_Enable_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Enable_IRQ
   type IRQ_Enable_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Enable_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Enable_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Enable IRQs.
   type IRQ_Enable_Register is record
      IRQ           : IRQ_Enable_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Enable_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IRQ_Enable_Clear_IRQ array
   type IRQ_Enable_Clear_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Enable_Clear_IRQ
   type IRQ_Enable_Clear_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Enable_Clear_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Enable_Clear_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Disable IRQs.
   type IRQ_Enable_Clear_Register is record
      IRQ           : IRQ_Enable_Clear_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Enable_Clear_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IRQ_Raw_Status_IRQ array
   type IRQ_Raw_Status_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Raw_Status_IRQ
   type IRQ_Raw_Status_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Raw_Status_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Raw_Status_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  IRQ Raw status
   type IRQ_Raw_Status_Register is record
      IRQ           : IRQ_Raw_Status_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Raw_Status_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  IRQ_Status_IRQ array
   type IRQ_Status_IRQ_Field_Array is array (1 .. 8) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for IRQ_Status_IRQ
   type IRQ_Status_IRQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IRQ as a value
            Val : Interfaces.M1AGL.Byte;
         when True =>
            --  IRQ as an array
            Arr : IRQ_Status_IRQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for IRQ_Status_IRQ_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  IRQ status
   type IRQ_Status_Register is record
      IRQ           : IRQ_Status_IRQ_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : Interfaces.M1AGL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IRQ_Status_Register use record
      IRQ           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flexible Interrupt Controller for Advanced Microcontroller Bus
   --  Architecture (AMBA) Based Systems
   type CoreInterrupt_Peripheral is record
      --  Software interrupt trigger IRQs.
      IRQ_Soft_Interrupt       : aliased IRQ_Soft_Interrupt_Register;
      --  Clear soft IRQs.
      IRQ_Soft_Interrupt_Clear : aliased IRQ_Soft_Interrupt_Clear_Register;
      --  Enable IRQs.
      IRQ_Enable               : aliased IRQ_Enable_Register;
      --  Disable IRQs.
      IRQ_Enable_Clear         : aliased IRQ_Enable_Clear_Register;
      --  IRQ Raw status
      IRQ_Raw_Status           : aliased IRQ_Raw_Status_Register;
      --  IRQ status
      IRQ_Status               : aliased IRQ_Status_Register;
   end record
     with Volatile;

   for CoreInterrupt_Peripheral use record
      IRQ_Soft_Interrupt       at 16#18# range 0 .. 31;
      IRQ_Soft_Interrupt_Clear at 16#1C# range 0 .. 31;
      IRQ_Enable               at 16#20# range 0 .. 31;
      IRQ_Enable_Clear         at 16#24# range 0 .. 31;
      IRQ_Raw_Status           at 16#28# range 0 .. 31;
      IRQ_Status               at 16#2C# range 0 .. 31;
   end record;

   --  Flexible Interrupt Controller for Advanced Microcontroller Bus
   --  Architecture (AMBA) Based Systems
   CoreInterrupt_Periph : aliased CoreInterrupt_Peripheral
     with Import, Address => CoreInterrupt_Base;

end Interfaces.M1AGL.CoreInterrupt;
