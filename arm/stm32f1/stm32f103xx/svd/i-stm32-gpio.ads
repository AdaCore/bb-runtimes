--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from STM32F103.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.Bit_Types;
with System;

package Interfaces.STM32.GPIO is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype Cnf_Mode is Interfaces.Bit_Types.UInt4;

   type CRL_Field_Array is array (0 .. 7) of Cnf_Mode
     with Component_Size => 4, Size => 32;

   type CRL_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            Val : Interfaces.Bit_Types.Word;
         when True =>
            Arr : CRL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access;

   type CRH_Field_Array is array (0 .. 7) of Cnf_Mode
     with Component_Size => 4, Size => 32;

   type CRH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            Val : Interfaces.Bit_Types.Word;
         when True =>
            Arr : CRH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access;

   --  IDR array element
   subtype IDR_Element is Interfaces.Bit_Types.Bit;

   --  IDR array
   type IDR_Field_Array is array (0 .. 15) of IDR_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for IDR
   type IDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IDR as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  IDR as an array
            Arr : IDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for IDR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Port input data register (GPIOn_IDR)
   type IDR_Register is record
      --  Read-only. Port input data
      IDR            : IDR_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDR_Register use record
      IDR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  ODR array element
   subtype ODR_Element is Interfaces.Bit_Types.Bit;

   --  ODR array
   type ODR_Field_Array is array (0 .. 15) of ODR_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for ODR
   type ODR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ODR as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  ODR as an array
            Arr : ODR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for ODR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Port output data register (GPIOn_ODR)
   type ODR_Register is record
      --  Port output data
      ODR            : ODR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ODR_Register use record
      ODR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  BSRR_BS array element
   subtype BSRR_BS_Element is Interfaces.Bit_Types.Bit;

   --  BSRR_BS array
   type BSRR_BS_Field_Array is array (0 .. 15) of BSRR_BS_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for BSRR_BS
   type BSRR_BS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BS as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  BS as an array
            Arr : BSRR_BS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BSRR_BS_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  BSRR_BR array element
   subtype BSRR_BR_Element is Interfaces.Bit_Types.Bit;

   --  BSRR_BR array
   type BSRR_BR_Field_Array is array (0 .. 15) of BSRR_BR_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for BSRR_BR
   type BSRR_BR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BR as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  BR as an array
            Arr : BSRR_BR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BSRR_BR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Port bit set/reset register (GPIOn_BSRR)
   type BSRR_Register is record
      --  Write-only. Set bit 0
      BS : BSRR_BS_Field := (As_Array => False, Val => 16#0#);
      --  Write-only. Reset bit 0
      BR : BSRR_BR_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BSRR_Register use record
      BS at 0 range 0 .. 15;
      BR at 0 range 16 .. 31;
   end record;

   --  BRR_BR array element
   subtype BRR_BR_Element is Interfaces.Bit_Types.Bit;

   --  BRR_BR array
   type BRR_BR_Field_Array is array (0 .. 15) of BRR_BR_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for BRR_BR
   type BRR_BR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BR as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  BR as an array
            Arr : BRR_BR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BRR_BR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Port bit reset register (GPIOn_BRR)
   type BRR_Register is record
      --  Write-only. Reset bit 0
      BR             : BRR_BR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BRR_Register use record
      BR             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  LCKR_LCK array element
   subtype LCKR_LCK_Element is Interfaces.Bit_Types.Bit;

   --  LCKR_LCK array
   type LCKR_LCK_Field_Array is array (0 .. 15) of LCKR_LCK_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for LCKR_LCK
   type LCKR_LCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCK as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  LCK as an array
            Arr : LCKR_LCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LCKR_LCK_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   subtype LCKR_LCKK_Field is Interfaces.Bit_Types.Bit;

   --  Port configuration lock register
   type LCKR_Register is record
      --  Port A Lock bit 0
      LCK            : LCKR_LCK_Field := (As_Array => False, Val => 16#0#);
      --  Lock key
      LCKK           : LCKR_LCKK_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : Interfaces.Bit_Types.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCKR_Register use record
      LCK            at 0 range 0 .. 15;
      LCKK           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose I/O
   type GPIO_Peripheral is record
      --  Port configuration register low (GPIOn_CRL)
      CRL  : aliased CRL_Register;
      --  Port configuration register high (GPIOn_CRL)
      CRH  : aliased CRH_Register;
      --  Port input data register (GPIOn_IDR)
      IDR  : aliased IDR_Register;
      --  Port output data register (GPIOn_ODR)
      ODR  : aliased ODR_Register;
      --  Port bit set/reset register (GPIOn_BSRR)
      BSRR : aliased BSRR_Register;
      --  Port bit reset register (GPIOn_BRR)
      BRR  : aliased BRR_Register;
      --  Port configuration lock register
      LCKR : aliased LCKR_Register;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      CRL  at 16#0# range 0 .. 31;
      CRH  at 16#4# range 0 .. 31;
      IDR  at 16#8# range 0 .. 31;
      ODR  at 16#C# range 0 .. 31;
      BSRR at 16#10# range 0 .. 31;
      BRR  at 16#14# range 0 .. 31;
      LCKR at 16#18# range 0 .. 31;
   end record;

   --  General purpose I/O
   GPIOA_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40010800#);

   --  General purpose I/O
   GPIOB_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40010C00#);

   --  General purpose I/O
   GPIOC_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40011000#);

   --  General purpose I/O
   GPIOD_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40011400#);

   --  General purpose I/O
   GPIOE_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40011800#);

   --  General purpose I/O
   GPIOF_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40011C00#);

   --  General purpose I/O
   GPIOG_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#40012000#);

end Interfaces.STM32.GPIO;
