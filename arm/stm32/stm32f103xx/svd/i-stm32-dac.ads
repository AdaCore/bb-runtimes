--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.DAC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_EN1_Field is Interfaces.STM32.Bit;
   subtype CR_BOFF1_Field is Interfaces.STM32.Bit;
   subtype CR_TEN1_Field is Interfaces.STM32.Bit;
   subtype CR_TSEL1_Field is Interfaces.STM32.UInt3;
   subtype CR_WAVE1_Field is Interfaces.STM32.UInt2;
   subtype CR_MAMP1_Field is Interfaces.STM32.UInt4;
   subtype CR_DMAEN1_Field is Interfaces.STM32.Bit;
   subtype CR_EN2_Field is Interfaces.STM32.Bit;
   subtype CR_BOFF2_Field is Interfaces.STM32.Bit;
   subtype CR_TEN2_Field is Interfaces.STM32.Bit;
   subtype CR_TSEL2_Field is Interfaces.STM32.UInt3;
   subtype CR_WAVE2_Field is Interfaces.STM32.UInt2;
   subtype CR_MAMP2_Field is Interfaces.STM32.UInt4;
   subtype CR_DMAEN2_Field is Interfaces.STM32.Bit;

   --  Control register (DAC_CR)
   type CR_Register is record
      --  DAC channel1 enable
      EN1            : CR_EN1_Field := 16#0#;
      --  DAC channel1 output buffer disable
      BOFF1          : CR_BOFF1_Field := 16#0#;
      --  DAC channel1 trigger enable
      TEN1           : CR_TEN1_Field := 16#0#;
      --  DAC channel1 trigger selection
      TSEL1          : CR_TSEL1_Field := 16#0#;
      --  DAC channel1 noise/triangle wave generation enable
      WAVE1          : CR_WAVE1_Field := 16#0#;
      --  DAC channel1 mask/amplitude selector
      MAMP1          : CR_MAMP1_Field := 16#0#;
      --  DAC channel1 DMA enable
      DMAEN1         : CR_DMAEN1_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.STM32.UInt3 := 16#0#;
      --  DAC channel2 enable
      EN2            : CR_EN2_Field := 16#0#;
      --  DAC channel2 output buffer disable
      BOFF2          : CR_BOFF2_Field := 16#0#;
      --  DAC channel2 trigger enable
      TEN2           : CR_TEN2_Field := 16#0#;
      --  DAC channel2 trigger selection
      TSEL2          : CR_TSEL2_Field := 16#0#;
      --  DAC channel2 noise/triangle wave generation enable
      WAVE2          : CR_WAVE2_Field := 16#0#;
      --  DAC channel2 mask/amplitude selector
      MAMP2          : CR_MAMP2_Field := 16#0#;
      --  DAC channel2 DMA enable
      DMAEN2         : CR_DMAEN2_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : Interfaces.STM32.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN1            at 0 range 0 .. 0;
      BOFF1          at 0 range 1 .. 1;
      TEN1           at 0 range 2 .. 2;
      TSEL1          at 0 range 3 .. 5;
      WAVE1          at 0 range 6 .. 7;
      MAMP1          at 0 range 8 .. 11;
      DMAEN1         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      EN2            at 0 range 16 .. 16;
      BOFF2          at 0 range 17 .. 17;
      TEN2           at 0 range 18 .. 18;
      TSEL2          at 0 range 19 .. 21;
      WAVE2          at 0 range 22 .. 23;
      MAMP2          at 0 range 24 .. 27;
      DMAEN2         at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  SWTRIGR_SWTRIG array element
   subtype SWTRIGR_SWTRIG_Element is Interfaces.STM32.Bit;

   --  SWTRIGR_SWTRIG array
   type SWTRIGR_SWTRIG_Field_Array is array (1 .. 2)
     of SWTRIGR_SWTRIG_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for SWTRIGR_SWTRIG
   type SWTRIGR_SWTRIG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWTRIG as a value
            Val : Interfaces.STM32.UInt2;
         when True =>
            --  SWTRIG as an array
            Arr : SWTRIGR_SWTRIG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SWTRIGR_SWTRIG_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DAC software trigger register (DAC_SWTRIGR)
   type SWTRIGR_Register is record
      --  Write-only. DAC channel1 software trigger
      SWTRIG        : SWTRIGR_SWTRIG_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : Interfaces.STM32.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWTRIGR_Register use record
      SWTRIG        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DHR12R1_DACC1DHR_Field is Interfaces.STM32.UInt12;

   --  DAC channel1 12-bit right-aligned data holding register(DAC_DHR12R1)
   type DHR12R1_Register is record
      --  DAC channel1 12-bit right-aligned data
      DACC1DHR       : DHR12R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12R1_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DHR12L1_DACC1DHR_Field is Interfaces.STM32.UInt12;

   --  DAC channel1 12-bit left aligned data holding register (DAC_DHR12L1)
   type DHR12L1_Register is record
      --  unspecified
      Reserved_0_3   : Interfaces.STM32.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data
      DACC1DHR       : DHR12L1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12L1_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DHR8R1_DACC1DHR_Field is Interfaces.STM32.Byte;

   --  DAC channel1 8-bit right aligned data holding register (DAC_DHR8R1)
   type DHR8R1_Register is record
      --  DAC channel1 8-bit right-aligned data
      DACC1DHR      : DHR8R1_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8R1_Register use record
      DACC1DHR      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DHR12R2_DACC2DHR_Field is Interfaces.STM32.UInt12;

   --  DAC channel2 12-bit right aligned data holding register (DAC_DHR12R2)
   type DHR12R2_Register is record
      --  DAC channel2 12-bit right-aligned data
      DACC2DHR       : DHR12R2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12R2_Register use record
      DACC2DHR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DHR12L2_DACC2DHR_Field is Interfaces.STM32.UInt12;

   --  DAC channel2 12-bit left aligned data holding register (DAC_DHR12L2)
   type DHR12L2_Register is record
      --  unspecified
      Reserved_0_3   : Interfaces.STM32.UInt4 := 16#0#;
      --  DAC channel2 12-bit left-aligned data
      DACC2DHR       : DHR12L2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12L2_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC2DHR       at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DHR8R2_DACC2DHR_Field is Interfaces.STM32.Byte;

   --  DAC channel2 8-bit right-aligned data holding register (DAC_DHR8R2)
   type DHR8R2_Register is record
      --  DAC channel2 8-bit right-aligned data
      DACC2DHR      : DHR8R2_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8R2_Register use record
      DACC2DHR      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DHR12RD_DACC1DHR_Field is Interfaces.STM32.UInt12;
   subtype DHR12RD_DACC2DHR_Field is Interfaces.STM32.UInt12;

   --  Dual DAC 12-bit right-aligned data holding register (DAC_DHR12RD), Bits
   --  31:28 Reserved, Bits 15:12 Reserved
   type DHR12RD_Register is record
      --  DAC channel1 12-bit right-aligned data
      DACC1DHR       : DHR12RD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : Interfaces.STM32.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data
      DACC2DHR       : DHR12RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : Interfaces.STM32.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12RD_Register use record
      DACC1DHR       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DACC2DHR       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype DHR12LD_DACC1DHR_Field is Interfaces.STM32.UInt12;
   subtype DHR12LD_DACC2DHR_Field is Interfaces.STM32.UInt12;

   --  DUAL DAC 12-bit left aligned data holding register (DAC_DHR12LD), Bits
   --  19:16 Reserved, Bits 3:0 Reserved
   type DHR12LD_Register is record
      --  unspecified
      Reserved_0_3   : Interfaces.STM32.UInt4 := 16#0#;
      --  DAC channel1 12-bit left-aligned data
      DACC1DHR       : DHR12LD_DACC1DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : Interfaces.STM32.UInt4 := 16#0#;
      --  DAC channel2 12-bit right-aligned data
      DACC2DHR       : DHR12LD_DACC2DHR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR12LD_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      DACC1DHR       at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      DACC2DHR       at 0 range 20 .. 31;
   end record;

   subtype DHR8RD_DACC1DHR_Field is Interfaces.STM32.Byte;
   subtype DHR8RD_DACC2DHR_Field is Interfaces.STM32.Byte;

   --  DUAL DAC 8-bit right aligned data holding register (DAC_DHR8RD), Bits
   --  31:16 Reserved
   type DHR8RD_Register is record
      --  DAC channel1 8-bit right-aligned data
      DACC1DHR       : DHR8RD_DACC1DHR_Field := 16#0#;
      --  DAC channel2 8-bit right-aligned data
      DACC2DHR       : DHR8RD_DACC2DHR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DHR8RD_Register use record
      DACC1DHR       at 0 range 0 .. 7;
      DACC2DHR       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOR1_DACC1DOR_Field is Interfaces.STM32.UInt12;

   --  DAC channel1 data output register (DAC_DOR1)
   type DOR1_Register is record
      --  Read-only. DAC channel1 data output
      DACC1DOR       : DOR1_DACC1DOR_Field;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOR1_Register use record
      DACC1DOR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DOR2_DACC2DOR_Field is Interfaces.STM32.UInt12;

   --  DAC channel2 data output register (DAC_DOR2)
   type DOR2_Register is record
      --  Read-only. DAC channel2 data output
      DACC2DOR       : DOR2_DACC2DOR_Field;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOR2_Register use record
      DACC2DOR       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital to analog converter
   type DAC_Peripheral is record
      --  Control register (DAC_CR)
      CR      : aliased CR_Register;
      --  DAC software trigger register (DAC_SWTRIGR)
      SWTRIGR : aliased SWTRIGR_Register;
      --  DAC channel1 12-bit right-aligned data holding register(DAC_DHR12R1)
      DHR12R1 : aliased DHR12R1_Register;
      --  DAC channel1 12-bit left aligned data holding register (DAC_DHR12L1)
      DHR12L1 : aliased DHR12L1_Register;
      --  DAC channel1 8-bit right aligned data holding register (DAC_DHR8R1)
      DHR8R1  : aliased DHR8R1_Register;
      --  DAC channel2 12-bit right aligned data holding register (DAC_DHR12R2)
      DHR12R2 : aliased DHR12R2_Register;
      --  DAC channel2 12-bit left aligned data holding register (DAC_DHR12L2)
      DHR12L2 : aliased DHR12L2_Register;
      --  DAC channel2 8-bit right-aligned data holding register (DAC_DHR8R2)
      DHR8R2  : aliased DHR8R2_Register;
      --  Dual DAC 12-bit right-aligned data holding register (DAC_DHR12RD),
      --  Bits 31:28 Reserved, Bits 15:12 Reserved
      DHR12RD : aliased DHR12RD_Register;
      --  DUAL DAC 12-bit left aligned data holding register (DAC_DHR12LD),
      --  Bits 19:16 Reserved, Bits 3:0 Reserved
      DHR12LD : aliased DHR12LD_Register;
      --  DUAL DAC 8-bit right aligned data holding register (DAC_DHR8RD), Bits
      --  31:16 Reserved
      DHR8RD  : aliased DHR8RD_Register;
      --  DAC channel1 data output register (DAC_DOR1)
      DOR1    : aliased DOR1_Register;
      --  DAC channel2 data output register (DAC_DOR2)
      DOR2    : aliased DOR2_Register;
   end record
     with Volatile;

   for DAC_Peripheral use record
      CR      at 16#0# range 0 .. 31;
      SWTRIGR at 16#4# range 0 .. 31;
      DHR12R1 at 16#8# range 0 .. 31;
      DHR12L1 at 16#C# range 0 .. 31;
      DHR8R1  at 16#10# range 0 .. 31;
      DHR12R2 at 16#14# range 0 .. 31;
      DHR12L2 at 16#18# range 0 .. 31;
      DHR8R2  at 16#1C# range 0 .. 31;
      DHR12RD at 16#20# range 0 .. 31;
      DHR12LD at 16#24# range 0 .. 31;
      DHR8RD  at 16#28# range 0 .. 31;
      DOR1    at 16#2C# range 0 .. 31;
      DOR2    at 16#30# range 0 .. 31;
   end record;

   --  Digital to analog converter
   DAC_Periph : aliased DAC_Peripheral
     with Import, Address => DAC_Base;

end Interfaces.STM32.DAC;
