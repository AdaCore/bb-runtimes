--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.ADC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype SR_AWD_Field is Interfaces.STM32.Bit;
   subtype SR_EOC_Field is Interfaces.STM32.Bit;
   subtype SR_JEOC_Field is Interfaces.STM32.Bit;
   subtype SR_JSTRT_Field is Interfaces.STM32.Bit;
   subtype SR_STRT_Field is Interfaces.STM32.Bit;

   --  status register
   type SR_Register is record
      --  Analog watchdog flag
      AWD           : SR_AWD_Field := 16#0#;
      --  Regular channel end of conversion
      EOC           : SR_EOC_Field := 16#0#;
      --  Injected channel end of conversion
      JEOC          : SR_JEOC_Field := 16#0#;
      --  Injected channel start flag
      JSTRT         : SR_JSTRT_Field := 16#0#;
      --  Regular channel start flag
      STRT          : SR_STRT_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : Interfaces.STM32.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      AWD           at 0 range 0 .. 0;
      EOC           at 0 range 1 .. 1;
      JEOC          at 0 range 2 .. 2;
      JSTRT         at 0 range 3 .. 3;
      STRT          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype CR1_AWDCH_Field is Interfaces.STM32.UInt5;
   subtype CR1_EOCIE_Field is Interfaces.STM32.Bit;
   subtype CR1_AWDIE_Field is Interfaces.STM32.Bit;
   subtype CR1_JEOCIE_Field is Interfaces.STM32.Bit;
   subtype CR1_SCAN_Field is Interfaces.STM32.Bit;
   subtype CR1_AWDSGL_Field is Interfaces.STM32.Bit;
   subtype CR1_JAUTO_Field is Interfaces.STM32.Bit;
   subtype CR1_DISCEN_Field is Interfaces.STM32.Bit;
   subtype CR1_JDISCEN_Field is Interfaces.STM32.Bit;
   subtype CR1_DISCNUM_Field is Interfaces.STM32.UInt3;
   subtype CR1_DUALMOD_Field is Interfaces.STM32.UInt4;
   subtype CR1_JAWDEN_Field is Interfaces.STM32.Bit;
   subtype CR1_AWDEN_Field is Interfaces.STM32.Bit;

   --  control register 1
   type CR1_Register is record
      --  Analog watchdog channel select bits
      AWDCH          : CR1_AWDCH_Field := 16#0#;
      --  Interrupt enable for EOC
      EOCIE          : CR1_EOCIE_Field := 16#0#;
      --  Analog watchdog interrupt enable
      AWDIE          : CR1_AWDIE_Field := 16#0#;
      --  Interrupt enable for injected channels
      JEOCIE         : CR1_JEOCIE_Field := 16#0#;
      --  Scan mode
      SCAN           : CR1_SCAN_Field := 16#0#;
      --  Enable the watchdog on a single channel in scan mode
      AWDSGL         : CR1_AWDSGL_Field := 16#0#;
      --  Automatic injected group conversion
      JAUTO          : CR1_JAUTO_Field := 16#0#;
      --  Discontinuous mode on regular channels
      DISCEN         : CR1_DISCEN_Field := 16#0#;
      --  Discontinuous mode on injected channels
      JDISCEN        : CR1_JDISCEN_Field := 16#0#;
      --  Discontinuous mode channel count
      DISCNUM        : CR1_DISCNUM_Field := 16#0#;
      --  Dual mode selection
      DUALMOD        : CR1_DUALMOD_Field := 16#0#;
      --  unspecified
      Reserved_20_21 : Interfaces.STM32.UInt2 := 16#0#;
      --  Analog watchdog enable on injected channels
      JAWDEN         : CR1_JAWDEN_Field := 16#0#;
      --  Analog watchdog enable on regular channels
      AWDEN          : CR1_AWDEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      AWDCH          at 0 range 0 .. 4;
      EOCIE          at 0 range 5 .. 5;
      AWDIE          at 0 range 6 .. 6;
      JEOCIE         at 0 range 7 .. 7;
      SCAN           at 0 range 8 .. 8;
      AWDSGL         at 0 range 9 .. 9;
      JAUTO          at 0 range 10 .. 10;
      DISCEN         at 0 range 11 .. 11;
      JDISCEN        at 0 range 12 .. 12;
      DISCNUM        at 0 range 13 .. 15;
      DUALMOD        at 0 range 16 .. 19;
      Reserved_20_21 at 0 range 20 .. 21;
      JAWDEN         at 0 range 22 .. 22;
      AWDEN          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CR2_ADON_Field is Interfaces.STM32.Bit;
   subtype CR2_CONT_Field is Interfaces.STM32.Bit;
   subtype CR2_CAL_Field is Interfaces.STM32.Bit;
   subtype CR2_RSTCAL_Field is Interfaces.STM32.Bit;
   subtype CR2_DMA_Field is Interfaces.STM32.Bit;
   subtype CR2_ALIGN_Field is Interfaces.STM32.Bit;
   subtype CR2_JEXTSEL_Field is Interfaces.STM32.UInt3;
   subtype CR2_JEXTTRIG_Field is Interfaces.STM32.Bit;
   subtype CR2_EXTSEL_Field is Interfaces.STM32.UInt3;
   subtype CR2_EXTTRIG_Field is Interfaces.STM32.Bit;
   subtype CR2_JSWSTART_Field is Interfaces.STM32.Bit;
   subtype CR2_SWSTART_Field is Interfaces.STM32.Bit;
   subtype CR2_TSVREFE_Field is Interfaces.STM32.Bit;

   --  control register 2
   type CR2_Register is record
      --  A/D converter ON / OFF
      ADON           : CR2_ADON_Field := 16#0#;
      --  Continuous conversion
      CONT           : CR2_CONT_Field := 16#0#;
      --  A/D calibration
      CAL            : CR2_CAL_Field := 16#0#;
      --  Reset calibration
      RSTCAL         : CR2_RSTCAL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : Interfaces.STM32.UInt4 := 16#0#;
      --  Direct memory access mode
      DMA            : CR2_DMA_Field := 16#0#;
      --  unspecified
      Reserved_9_10  : Interfaces.STM32.UInt2 := 16#0#;
      --  Data alignment
      ALIGN          : CR2_ALIGN_Field := 16#0#;
      --  External event select for injected group
      JEXTSEL        : CR2_JEXTSEL_Field := 16#0#;
      --  External trigger conversion mode for injected channels
      JEXTTRIG       : CR2_JEXTTRIG_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : Interfaces.STM32.Bit := 16#0#;
      --  External event select for regular group
      EXTSEL         : CR2_EXTSEL_Field := 16#0#;
      --  External trigger conversion mode for regular channels
      EXTTRIG        : CR2_EXTTRIG_Field := 16#0#;
      --  Start conversion of injected channels
      JSWSTART       : CR2_JSWSTART_Field := 16#0#;
      --  Start conversion of regular channels
      SWSTART        : CR2_SWSTART_Field := 16#0#;
      --  Temperature sensor and VREFINT enable
      TSVREFE        : CR2_TSVREFE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      ADON           at 0 range 0 .. 0;
      CONT           at 0 range 1 .. 1;
      CAL            at 0 range 2 .. 2;
      RSTCAL         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DMA            at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      ALIGN          at 0 range 11 .. 11;
      JEXTSEL        at 0 range 12 .. 14;
      JEXTTRIG       at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      EXTSEL         at 0 range 17 .. 19;
      EXTTRIG        at 0 range 20 .. 20;
      JSWSTART       at 0 range 21 .. 21;
      SWSTART        at 0 range 22 .. 22;
      TSVREFE        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SMPR1_SMP array element
   subtype SMPR1_SMP_Element is Interfaces.STM32.UInt3;

   --  SMPR1_SMP array
   type SMPR1_SMP_Field_Array is array (10 .. 17) of SMPR1_SMP_Element
     with Component_Size => 3, Size => 24;

   --  Type definition for SMPR1_SMP
   type SMPR1_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : Interfaces.STM32.UInt24;
         when True =>
            --  SMP as an array
            Arr : SMPR1_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for SMPR1_SMP_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  sample time register 1
   type SMPR1_Register is record
      --  Channel 10 sample time selection
      SMP            : SMPR1_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR1_Register use record
      SMP            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SMPR2_SMP array element
   subtype SMPR2_SMP_Element is Interfaces.STM32.UInt3;

   --  SMPR2_SMP array
   type SMPR2_SMP_Field_Array is array (0 .. 9) of SMPR2_SMP_Element
     with Component_Size => 3, Size => 30;

   --  Type definition for SMPR2_SMP
   type SMPR2_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : Interfaces.STM32.UInt30;
         when True =>
            --  SMP as an array
            Arr : SMPR2_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for SMPR2_SMP_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  sample time register 2
   type SMPR2_Register is record
      --  Channel 0 sample time selection
      SMP            : SMPR2_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : Interfaces.STM32.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR2_Register use record
      SMP            at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype JOFR1_JOFFSET1_Field is Interfaces.STM32.UInt12;

   --  injected channel data offset register x
   type JOFR1_Register is record
      --  Data offset for injected channel x
      JOFFSET1       : JOFR1_JOFFSET1_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JOFR1_Register use record
      JOFFSET1       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype JOFR2_JOFFSET2_Field is Interfaces.STM32.UInt12;

   --  injected channel data offset register x
   type JOFR2_Register is record
      --  Data offset for injected channel x
      JOFFSET2       : JOFR2_JOFFSET2_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JOFR2_Register use record
      JOFFSET2       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype JOFR3_JOFFSET3_Field is Interfaces.STM32.UInt12;

   --  injected channel data offset register x
   type JOFR3_Register is record
      --  Data offset for injected channel x
      JOFFSET3       : JOFR3_JOFFSET3_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JOFR3_Register use record
      JOFFSET3       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype JOFR4_JOFFSET4_Field is Interfaces.STM32.UInt12;

   --  injected channel data offset register x
   type JOFR4_Register is record
      --  Data offset for injected channel x
      JOFFSET4       : JOFR4_JOFFSET4_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JOFR4_Register use record
      JOFFSET4       at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype HTR_HT_Field is Interfaces.STM32.UInt12;

   --  watchdog higher threshold register
   type HTR_Register is record
      --  Analog watchdog higher threshold
      HT             : HTR_HT_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HTR_Register use record
      HT             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype LTR_LT_Field is Interfaces.STM32.UInt12;

   --  watchdog lower threshold register
   type LTR_Register is record
      --  Analog watchdog lower threshold
      LT             : LTR_LT_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LTR_Register use record
      LT             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  SQR1_SQ array element
   subtype SQR1_SQ_Element is Interfaces.STM32.UInt5;

   --  SQR1_SQ array
   type SQR1_SQ_Field_Array is array (13 .. 16) of SQR1_SQ_Element
     with Component_Size => 5, Size => 20;

   --  Type definition for SQR1_SQ
   type SQR1_SQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : Interfaces.STM32.UInt20;
         when True =>
            --  SQ as an array
            Arr : SQR1_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for SQR1_SQ_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   subtype SQR1_L_Field is Interfaces.STM32.UInt4;

   --  regular sequence register 1
   type SQR1_Register is record
      --  13th conversion in regular sequence
      SQ             : SQR1_SQ_Field := (As_Array => False, Val => 16#0#);
      --  Regular channel sequence length
      L              : SQR1_L_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR1_Register use record
      SQ             at 0 range 0 .. 19;
      L              at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  SQR2_SQ array element
   subtype SQR2_SQ_Element is Interfaces.STM32.UInt5;

   --  SQR2_SQ array
   type SQR2_SQ_Field_Array is array (7 .. 12) of SQR2_SQ_Element
     with Component_Size => 5, Size => 30;

   --  Type definition for SQR2_SQ
   type SQR2_SQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : Interfaces.STM32.UInt30;
         when True =>
            --  SQ as an array
            Arr : SQR2_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for SQR2_SQ_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  regular sequence register 2
   type SQR2_Register is record
      --  7th conversion in regular sequence
      SQ             : SQR2_SQ_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : Interfaces.STM32.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR2_Register use record
      SQ             at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  SQR3_SQ array element
   subtype SQR3_SQ_Element is Interfaces.STM32.UInt5;

   --  SQR3_SQ array
   type SQR3_SQ_Field_Array is array (1 .. 6) of SQR3_SQ_Element
     with Component_Size => 5, Size => 30;

   --  Type definition for SQR3_SQ
   type SQR3_SQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SQ as a value
            Val : Interfaces.STM32.UInt30;
         when True =>
            --  SQ as an array
            Arr : SQR3_SQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for SQR3_SQ_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  regular sequence register 3
   type SQR3_Register is record
      --  1st conversion in regular sequence
      SQ             : SQR3_SQ_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : Interfaces.STM32.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR3_Register use record
      SQ             at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  JSQR_JSQ array element
   subtype JSQR_JSQ_Element is Interfaces.STM32.UInt5;

   --  JSQR_JSQ array
   type JSQR_JSQ_Field_Array is array (1 .. 4) of JSQR_JSQ_Element
     with Component_Size => 5, Size => 20;

   --  Type definition for JSQR_JSQ
   type JSQR_JSQ_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  JSQ as a value
            Val : Interfaces.STM32.UInt20;
         when True =>
            --  JSQ as an array
            Arr : JSQR_JSQ_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 20;

   for JSQR_JSQ_Field use record
      Val at 0 range 0 .. 19;
      Arr at 0 range 0 .. 19;
   end record;

   subtype JSQR_JL_Field is Interfaces.STM32.UInt2;

   --  injected sequence register
   type JSQR_Register is record
      --  1st conversion in injected sequence
      JSQ            : JSQR_JSQ_Field := (As_Array => False, Val => 16#0#);
      --  Injected sequence length
      JL             : JSQR_JL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : Interfaces.STM32.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JSQR_Register use record
      JSQ            at 0 range 0 .. 19;
      JL             at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype JDR_JDATA_Field is Interfaces.STM32.UInt16;

   --  injected data register x
   type JDR_Register is record
      --  Read-only. Injected data
      JDATA          : JDR_JDATA_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JDR_Register use record
      JDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR_DATA_Field is Interfaces.STM32.UInt16;
   subtype DR_ADC2DATA_Field is Interfaces.STM32.UInt16;

   --  regular data register
   type DR_Register is record
      --  Read-only. Regular data
      DATA     : DR_DATA_Field;
      --  Read-only. ADC2 data
      ADC2DATA : DR_ADC2DATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DATA     at 0 range 0 .. 15;
      ADC2DATA at 0 range 16 .. 31;
   end record;

   --  control register 1
   type CR1_Register_1 is record
      --  Analog watchdog channel select bits
      AWDCH          : CR1_AWDCH_Field := 16#0#;
      --  Interrupt enable for EOC
      EOCIE          : CR1_EOCIE_Field := 16#0#;
      --  Analog watchdog interrupt enable
      AWDIE          : CR1_AWDIE_Field := 16#0#;
      --  Interrupt enable for injected channels
      JEOCIE         : CR1_JEOCIE_Field := 16#0#;
      --  Scan mode
      SCAN           : CR1_SCAN_Field := 16#0#;
      --  Enable the watchdog on a single channel in scan mode
      AWDSGL         : CR1_AWDSGL_Field := 16#0#;
      --  Automatic injected group conversion
      JAUTO          : CR1_JAUTO_Field := 16#0#;
      --  Discontinuous mode on regular channels
      DISCEN         : CR1_DISCEN_Field := 16#0#;
      --  Discontinuous mode on injected channels
      JDISCEN        : CR1_JDISCEN_Field := 16#0#;
      --  Discontinuous mode channel count
      DISCNUM        : CR1_DISCNUM_Field := 16#0#;
      --  unspecified
      Reserved_16_21 : Interfaces.STM32.UInt6 := 16#0#;
      --  Analog watchdog enable on injected channels
      JAWDEN         : CR1_JAWDEN_Field := 16#0#;
      --  Analog watchdog enable on regular channels
      AWDEN          : CR1_AWDEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register_1 use record
      AWDCH          at 0 range 0 .. 4;
      EOCIE          at 0 range 5 .. 5;
      AWDIE          at 0 range 6 .. 6;
      JEOCIE         at 0 range 7 .. 7;
      SCAN           at 0 range 8 .. 8;
      AWDSGL         at 0 range 9 .. 9;
      JAUTO          at 0 range 10 .. 10;
      DISCEN         at 0 range 11 .. 11;
      JDISCEN        at 0 range 12 .. 12;
      DISCNUM        at 0 range 13 .. 15;
      Reserved_16_21 at 0 range 16 .. 21;
      JAWDEN         at 0 range 22 .. 22;
      AWDEN          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  regular data register
   type DR_Register_1 is record
      --  Read-only. Regular data
      DATA           : DR_DATA_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register_1 use record
      DATA           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Analog to digital converter
   type ADC1_Peripheral is record
      --  status register
      SR    : aliased SR_Register;
      --  control register 1
      CR1   : aliased CR1_Register;
      --  control register 2
      CR2   : aliased CR2_Register;
      --  sample time register 1
      SMPR1 : aliased SMPR1_Register;
      --  sample time register 2
      SMPR2 : aliased SMPR2_Register;
      --  injected channel data offset register x
      JOFR1 : aliased JOFR1_Register;
      --  injected channel data offset register x
      JOFR2 : aliased JOFR2_Register;
      --  injected channel data offset register x
      JOFR3 : aliased JOFR3_Register;
      --  injected channel data offset register x
      JOFR4 : aliased JOFR4_Register;
      --  watchdog higher threshold register
      HTR   : aliased HTR_Register;
      --  watchdog lower threshold register
      LTR   : aliased LTR_Register;
      --  regular sequence register 1
      SQR1  : aliased SQR1_Register;
      --  regular sequence register 2
      SQR2  : aliased SQR2_Register;
      --  regular sequence register 3
      SQR3  : aliased SQR3_Register;
      --  injected sequence register
      JSQR  : aliased JSQR_Register;
      --  injected data register x
      JDR1  : aliased JDR_Register;
      --  injected data register x
      JDR2  : aliased JDR_Register;
      --  injected data register x
      JDR3  : aliased JDR_Register;
      --  injected data register x
      JDR4  : aliased JDR_Register;
      --  regular data register
      DR    : aliased DR_Register;
   end record
     with Volatile;

   for ADC1_Peripheral use record
      SR    at 16#0# range 0 .. 31;
      CR1   at 16#4# range 0 .. 31;
      CR2   at 16#8# range 0 .. 31;
      SMPR1 at 16#C# range 0 .. 31;
      SMPR2 at 16#10# range 0 .. 31;
      JOFR1 at 16#14# range 0 .. 31;
      JOFR2 at 16#18# range 0 .. 31;
      JOFR3 at 16#1C# range 0 .. 31;
      JOFR4 at 16#20# range 0 .. 31;
      HTR   at 16#24# range 0 .. 31;
      LTR   at 16#28# range 0 .. 31;
      SQR1  at 16#2C# range 0 .. 31;
      SQR2  at 16#30# range 0 .. 31;
      SQR3  at 16#34# range 0 .. 31;
      JSQR  at 16#38# range 0 .. 31;
      JDR1  at 16#3C# range 0 .. 31;
      JDR2  at 16#40# range 0 .. 31;
      JDR3  at 16#44# range 0 .. 31;
      JDR4  at 16#48# range 0 .. 31;
      DR    at 16#4C# range 0 .. 31;
   end record;

   --  Analog to digital converter
   ADC1_Periph : aliased ADC1_Peripheral
     with Import, Address => ADC1_Base;

   --  Analog to digital converter
   type ADC_Peripheral is record
      --  status register
      SR    : aliased SR_Register;
      --  control register 1
      CR1   : aliased CR1_Register_1;
      --  control register 2
      CR2   : aliased CR2_Register;
      --  sample time register 1
      SMPR1 : aliased SMPR1_Register;
      --  sample time register 2
      SMPR2 : aliased SMPR2_Register;
      --  injected channel data offset register x
      JOFR1 : aliased JOFR1_Register;
      --  injected channel data offset register x
      JOFR2 : aliased JOFR2_Register;
      --  injected channel data offset register x
      JOFR3 : aliased JOFR3_Register;
      --  injected channel data offset register x
      JOFR4 : aliased JOFR4_Register;
      --  watchdog higher threshold register
      HTR   : aliased HTR_Register;
      --  watchdog lower threshold register
      LTR   : aliased LTR_Register;
      --  regular sequence register 1
      SQR1  : aliased SQR1_Register;
      --  regular sequence register 2
      SQR2  : aliased SQR2_Register;
      --  regular sequence register 3
      SQR3  : aliased SQR3_Register;
      --  injected sequence register
      JSQR  : aliased JSQR_Register;
      --  injected data register x
      JDR1  : aliased JDR_Register;
      --  injected data register x
      JDR2  : aliased JDR_Register;
      --  injected data register x
      JDR3  : aliased JDR_Register;
      --  injected data register x
      JDR4  : aliased JDR_Register;
      --  regular data register
      DR    : aliased DR_Register_1;
   end record
     with Volatile;

   for ADC_Peripheral use record
      SR    at 16#0# range 0 .. 31;
      CR1   at 16#4# range 0 .. 31;
      CR2   at 16#8# range 0 .. 31;
      SMPR1 at 16#C# range 0 .. 31;
      SMPR2 at 16#10# range 0 .. 31;
      JOFR1 at 16#14# range 0 .. 31;
      JOFR2 at 16#18# range 0 .. 31;
      JOFR3 at 16#1C# range 0 .. 31;
      JOFR4 at 16#20# range 0 .. 31;
      HTR   at 16#24# range 0 .. 31;
      LTR   at 16#28# range 0 .. 31;
      SQR1  at 16#2C# range 0 .. 31;
      SQR2  at 16#30# range 0 .. 31;
      SQR3  at 16#34# range 0 .. 31;
      JSQR  at 16#38# range 0 .. 31;
      JDR1  at 16#3C# range 0 .. 31;
      JDR2  at 16#40# range 0 .. 31;
      JDR3  at 16#44# range 0 .. 31;
      JDR4  at 16#48# range 0 .. 31;
      DR    at 16#4C# range 0 .. 31;
   end record;

   --  Analog to digital converter
   ADC2_Periph : aliased ADC_Peripheral
     with Import, Address => ADC2_Base;

   --  Analog to digital converter
   ADC3_Periph : aliased ADC_Peripheral
     with Import, Address => ADC3_Base;

end Interfaces.STM32.ADC;
