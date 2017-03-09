--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from M2Sxxx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  No description provided for this peripheral
package Interfaces.SF2.System_Registers is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  No description provided for this register
   type ESRAM_CR_Register is record
      --  No description provided for this field
      SW_CC_ESRAMFWREMAP  : Boolean := False;
      --  No description provided for this field
      SW_CC_ESRAM1FWREMAP : Boolean := False;
      --  unspecified
      Reserved_2_31       : Interfaces.SF2.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM_CR_Register use record
      SW_CC_ESRAMFWREMAP  at 0 range 0 .. 0;
      SW_CC_ESRAM1FWREMAP at 0 range 1 .. 1;
      Reserved_2_31       at 0 range 2 .. 31;
   end record;

   --  ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM array element
   subtype ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Element is Interfaces.SF2.UInt3;

   --  ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM array
   type ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Field_Array is array (0 .. 1)
     of ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Element
     with Component_Size => 3, Size => 6;

   --  Type definition for ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM
   type ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SW_MAX_LAT_ESRAM as a value
            Val : Interfaces.SF2.UInt6;
         when True =>
            --  SW_MAX_LAT_ESRAM as an array
            Arr : ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  No description provided for this register
   type ESRAM_MAX_LAT_CR_Register is record
      --  No description provided for this field
      SW_MAX_LAT_ESRAM : ESRAM_MAX_LAT_CR_SW_MAX_LAT_ESRAM_Field :=
                          (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_31    : Interfaces.SF2.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM_MAX_LAT_CR_Register use record
      SW_MAX_LAT_ESRAM at 0 range 0 .. 5;
      Reserved_6_31    at 0 range 6 .. 31;
   end record;

   --  No description provided for this register
   type DDR_CR_Register is record
      --  No description provided for this field
      SW_CC_DDRFWREMAP : Boolean := False;
      --  unspecified
      Reserved_1_31    : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDR_CR_Register use record
      SW_CC_DDRFWREMAP at 0 range 0 .. 0;
      Reserved_1_31    at 0 range 1 .. 31;
   end record;

   subtype ENVM_CR_SW_ENVMREMAPSIZE_Field is Interfaces.SF2.UInt5;
   subtype ENVM_CR_NV_FREQRNG_Field is Interfaces.SF2.Byte;

   --  ENVM_CR_NV_DPD array
   type ENVM_CR_NV_DPD_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for ENVM_CR_NV_DPD
   type ENVM_CR_NV_DPD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  NV_DPD as a value
            Val : Interfaces.SF2.UInt2;
         when True =>
            --  NV_DPD as an array
            Arr : ENVM_CR_NV_DPD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for ENVM_CR_NV_DPD_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  No description provided for this register
   type ENVM_CR_Register is record
      --  No description provided for this field
      SW_ENVMREMAPSIZE : ENVM_CR_SW_ENVMREMAPSIZE_Field := 16#0#;
      --  No description provided for this field
      NV_FREQRNG       : ENVM_CR_NV_FREQRNG_Field := 16#0#;
      --  No description provided for this field
      NV_DPD           : ENVM_CR_NV_DPD_Field :=
                          (As_Array => False, Val => 16#0#);
      --  No description provided for this field
      ENVM_PERSIST     : Boolean := False;
      --  No description provided for this field
      ENVM_SENSE_ON    : Boolean := False;
      --  unspecified
      Reserved_17_31   : Interfaces.SF2.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENVM_CR_Register use record
      SW_ENVMREMAPSIZE at 0 range 0 .. 4;
      NV_FREQRNG       at 0 range 5 .. 12;
      NV_DPD           at 0 range 13 .. 14;
      ENVM_PERSIST     at 0 range 15 .. 15;
      ENVM_SENSE_ON    at 0 range 16 .. 16;
      Reserved_17_31   at 0 range 17 .. 31;
   end record;

   subtype ENVM_REMAP_BASE_CR_SW_ENVMREMAPBASE_Field is Interfaces.SF2.UInt18;

   --  No description provided for this register
   type ENVM_REMAP_BASE_CR_Register is record
      --  No description provided for this field
      SW_ENVMREMAPENABLE : Boolean := False;
      --  No description provided for this field
      SW_ENVMREMAPBASE   : ENVM_REMAP_BASE_CR_SW_ENVMREMAPBASE_Field := 16#0#;
      --  unspecified
      Reserved_19_31     : Interfaces.SF2.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENVM_REMAP_BASE_CR_Register use record
      SW_ENVMREMAPENABLE at 0 range 0 .. 0;
      SW_ENVMREMAPBASE   at 0 range 1 .. 18;
      Reserved_19_31     at 0 range 19 .. 31;
   end record;

   subtype ENVM_REMAP_FAB_CR_SW_ENVMFABREMAPBASE_Field is
     Interfaces.SF2.UInt18;

   --  No description provided for this register
   type ENVM_REMAP_FAB_CR_Register is record
      --  No description provided for this field
      SW_ENVMFABREMAPENABLE : Boolean := False;
      --  No description provided for this field
      SW_ENVMFABREMAPBASE   : ENVM_REMAP_FAB_CR_SW_ENVMFABREMAPBASE_Field :=
                               16#0#;
      --  unspecified
      Reserved_19_31        : Interfaces.SF2.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENVM_REMAP_FAB_CR_Register use record
      SW_ENVMFABREMAPENABLE at 0 range 0 .. 0;
      SW_ENVMFABREMAPBASE   at 0 range 1 .. 18;
      Reserved_19_31        at 0 range 19 .. 31;
   end record;

   --  No description provided for this register
   type CC_CR_Register is record
      --  No description provided for this field
      CC_CACHE_ENB    : Boolean := False;
      --  No description provided for this field
      CC_SBUS_WR_MODE : Boolean := False;
      --  No description provided for this field
      CC_CACHE_LOCK   : Boolean := False;
      --  unspecified
      Reserved_3_31   : Interfaces.SF2.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_CR_Register use record
      CC_CACHE_ENB    at 0 range 0 .. 0;
      CC_SBUS_WR_MODE at 0 range 1 .. 1;
      CC_CACHE_LOCK   at 0 range 2 .. 2;
      Reserved_3_31   at 0 range 3 .. 31;
   end record;

   subtype CC_REGION_CR_CC_CACHE_REGION_Field is Interfaces.SF2.UInt4;

   --  No description provided for this register
   type CC_REGION_CR_Register is record
      --  No description provided for this field
      CC_CACHE_REGION : CC_REGION_CR_CC_CACHE_REGION_Field := 16#0#;
      --  unspecified
      Reserved_4_31   : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_REGION_CR_Register use record
      CC_CACHE_REGION at 0 range 0 .. 3;
      Reserved_4_31   at 0 range 4 .. 31;
   end record;

   subtype CC_LOCK_BASE_ADDR_CR_CC_LOCK_BASEADD_Field is Interfaces.SF2.UInt19;

   --  No description provided for this register
   type CC_LOCK_BASE_ADDR_CR_Register is record
      --  No description provided for this field
      CC_LOCK_BASEADD : CC_LOCK_BASE_ADDR_CR_CC_LOCK_BASEADD_Field := 16#0#;
      --  unspecified
      Reserved_19_31  : Interfaces.SF2.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_LOCK_BASE_ADDR_CR_Register use record
      CC_LOCK_BASEADD at 0 range 0 .. 18;
      Reserved_19_31  at 0 range 19 .. 31;
   end record;

   subtype CC_FLUSH_INDX_CR_CC_FLUSH_INDEX_Field is Interfaces.SF2.UInt6;

   --  No description provided for this register
   type CC_FLUSH_INDX_CR_Register is record
      --  No description provided for this field
      CC_FLUSH_INDEX : CC_FLUSH_INDX_CR_CC_FLUSH_INDEX_Field := 16#0#;
      --  unspecified
      Reserved_6_31  : Interfaces.SF2.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_FLUSH_INDX_CR_Register use record
      CC_FLUSH_INDEX at 0 range 0 .. 5;
      Reserved_6_31  at 0 range 6 .. 31;
   end record;

   subtype DDRB_BUF_TIMER_CR_DDRB_TIMER_Field is Interfaces.SF2.UInt10;

   --  No description provided for this register
   type DDRB_BUF_TIMER_CR_Register is record
      --  No description provided for this field
      DDRB_TIMER     : DDRB_BUF_TIMER_CR_DDRB_TIMER_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_BUF_TIMER_CR_Register use record
      DDRB_TIMER     at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DDRB_NB_ADDR_CR_DDRB_NB_ADDR_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type DDRB_NB_ADDR_CR_Register is record
      --  No description provided for this field
      DDRB_NB_ADDR   : DDRB_NB_ADDR_CR_DDRB_NB_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SF2.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_NB_ADDR_CR_Register use record
      DDRB_NB_ADDR   at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DDRB_NB_SIZE_CR_DDRB_NB_SZ_Field is Interfaces.SF2.UInt4;

   --  No description provided for this register
   type DDRB_NB_SIZE_CR_Register is record
      --  No description provided for this field
      DDRB_NB_SZ    : DDRB_NB_SIZE_CR_DDRB_NB_SZ_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_NB_SIZE_CR_Register use record
      DDRB_NB_SZ    at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype DDRB_CR_DDR_DS_MAP_Field is Interfaces.SF2.UInt4;
   subtype DDRB_CR_DDR_HPD_MAP_Field is Interfaces.SF2.UInt4;
   subtype DDRB_CR_DDR_SW_MAP_Field is Interfaces.SF2.UInt4;
   subtype DDRB_CR_DDR_IDC_MAP_Field is Interfaces.SF2.UInt4;

   --  No description provided for this register
   type DDRB_CR_Register is record
      --  No description provided for this field
      DDRB_DS_WEN    : Boolean := False;
      --  No description provided for this field
      DDRB_DS_REN    : Boolean := False;
      --  No description provided for this field
      DDRB_HPD_WEN   : Boolean := False;
      --  No description provided for this field
      DDRB_HPD_REN   : Boolean := False;
      --  No description provided for this field
      DDRB_SW_WEN    : Boolean := False;
      --  No description provided for this field
      DDRB_SW_REN    : Boolean := False;
      --  No description provided for this field
      DDRB_IDC_EN    : Boolean := False;
      --  No description provided for this field
      DDRB_BUF_SZ    : Boolean := False;
      --  No description provided for this field
      DDR_DS_MAP     : DDRB_CR_DDR_DS_MAP_Field := 16#0#;
      --  No description provided for this field
      DDR_HPD_MAP    : DDRB_CR_DDR_HPD_MAP_Field := 16#0#;
      --  No description provided for this field
      DDR_SW_MAP     : DDRB_CR_DDR_SW_MAP_Field := 16#0#;
      --  No description provided for this field
      DDR_IDC_MAP    : DDRB_CR_DDR_IDC_MAP_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.SF2.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_CR_Register use record
      DDRB_DS_WEN    at 0 range 0 .. 0;
      DDRB_DS_REN    at 0 range 1 .. 1;
      DDRB_HPD_WEN   at 0 range 2 .. 2;
      DDRB_HPD_REN   at 0 range 3 .. 3;
      DDRB_SW_WEN    at 0 range 4 .. 4;
      DDRB_SW_REN    at 0 range 5 .. 5;
      DDRB_IDC_EN    at 0 range 6 .. 6;
      DDRB_BUF_SZ    at 0 range 7 .. 7;
      DDR_DS_MAP     at 0 range 8 .. 11;
      DDR_HPD_MAP    at 0 range 12 .. 15;
      DDR_SW_MAP     at 0 range 16 .. 19;
      DDR_IDC_MAP    at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  No description provided for this register
   type EDAC_CR_Register is record
      --  No description provided for this field
      ESRAM0_EDAC_EN : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_EN : Boolean := False;
      --  No description provided for this field
      CC_EDAC_EN     : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_EN : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_EN : Boolean := False;
      --  No description provided for this field
      USB_EDAC_EN    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_EN    : Boolean := False;
      --  unspecified
      Reserved_7_31  : Interfaces.SF2.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EDAC_CR_Register use record
      ESRAM0_EDAC_EN at 0 range 0 .. 0;
      ESRAM1_EDAC_EN at 0 range 1 .. 1;
      CC_EDAC_EN     at 0 range 2 .. 2;
      MAC_EDAC_TX_EN at 0 range 3 .. 3;
      MAC_EDAC_RX_EN at 0 range 4 .. 4;
      USB_EDAC_EN    at 0 range 5 .. 5;
      CAN_EDAC_EN    at 0 range 6 .. 6;
      Reserved_7_31  at 0 range 7 .. 31;
   end record;

   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_IC_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_S_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_GIGE_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_FAB_0_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_FAB_1_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT0_CR_SW_WEIGHT_PDMA_Field is Interfaces.SF2.UInt5;

   --  No description provided for this register
   type MASTER_WEIGHT0_CR_Register is record
      --  No description provided for this field
      SW_WEIGHT_IC    : MASTER_WEIGHT0_CR_SW_WEIGHT_IC_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_S     : MASTER_WEIGHT0_CR_SW_WEIGHT_S_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_GIGE  : MASTER_WEIGHT0_CR_SW_WEIGHT_GIGE_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_FAB_0 : MASTER_WEIGHT0_CR_SW_WEIGHT_FAB_0_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_FAB_1 : MASTER_WEIGHT0_CR_SW_WEIGHT_FAB_1_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_PDMA  : MASTER_WEIGHT0_CR_SW_WEIGHT_PDMA_Field := 16#0#;
      --  unspecified
      Reserved_30_31  : Interfaces.SF2.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASTER_WEIGHT0_CR_Register use record
      SW_WEIGHT_IC    at 0 range 0 .. 4;
      SW_WEIGHT_S     at 0 range 5 .. 9;
      SW_WEIGHT_GIGE  at 0 range 10 .. 14;
      SW_WEIGHT_FAB_0 at 0 range 15 .. 19;
      SW_WEIGHT_FAB_1 at 0 range 20 .. 24;
      SW_WEIGHT_PDMA  at 0 range 25 .. 29;
      Reserved_30_31  at 0 range 30 .. 31;
   end record;

   subtype MASTER_WEIGHT1_CR_SW_WEIGHT_HPDMA_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT1_CR_SW_WEIGHT_USB_Field is Interfaces.SF2.UInt5;
   subtype MASTER_WEIGHT1_CR_SW_WEIGHT_G_Field is Interfaces.SF2.UInt5;

   --  No description provided for this register
   type MASTER_WEIGHT1_CR_Register is record
      --  No description provided for this field
      SW_WEIGHT_HPDMA : MASTER_WEIGHT1_CR_SW_WEIGHT_HPDMA_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_USB   : MASTER_WEIGHT1_CR_SW_WEIGHT_USB_Field := 16#0#;
      --  No description provided for this field
      SW_WEIGHT_G     : MASTER_WEIGHT1_CR_SW_WEIGHT_G_Field := 16#0#;
      --  unspecified
      Reserved_15_31  : Interfaces.SF2.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASTER_WEIGHT1_CR_Register use record
      SW_WEIGHT_HPDMA at 0 range 0 .. 4;
      SW_WEIGHT_USB   at 0 range 5 .. 9;
      SW_WEIGHT_G     at 0 range 10 .. 14;
      Reserved_15_31  at 0 range 15 .. 31;
   end record;

   --  No description provided for this register
   type SOFT_IRQ_CR_Register is record
      --  No description provided for this field
      SOFTINTERRUPT : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SOFT_IRQ_CR_Register use record
      SOFTINTERRUPT at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  No description provided for this register
   type SOFT_RESET_CR_Register is record
      --  No description provided for this field
      ENVM0_SOFTRESET            : Boolean := False;
      --  No description provided for this field
      ENVM1_SOFTRESET            : Boolean := False;
      --  No description provided for this field
      ESRAM0_SOFTRESET           : Boolean := False;
      --  No description provided for this field
      ESRAM1_SOFTRESET           : Boolean := False;
      --  No description provided for this field
      MAC_SOFTRESET              : Boolean := False;
      --  No description provided for this field
      PDMA_SOFTRESET             : Boolean := False;
      --  No description provided for this field
      TIMER_SOFTRESET            : Boolean := False;
      --  No description provided for this field
      MMUART0_SOFTRESET          : Boolean := False;
      --  No description provided for this field
      MMUART1_SOFTRESET          : Boolean := False;
      --  No description provided for this field
      G4SPI0_SOFTRESET           : Boolean := False;
      --  No description provided for this field
      G4SPI1_SOFTRESET           : Boolean := False;
      --  No description provided for this field
      I2C0_SOFTRESET             : Boolean := False;
      --  No description provided for this field
      I2C1_SOFTRESET             : Boolean := False;
      --  No description provided for this field
      CAN_SOFTRESET              : Boolean := False;
      --  No description provided for this field
      USB_SOFTRESET              : Boolean := False;
      --  No description provided for this field
      COMBLK_SOFTRESET           : Boolean := False;
      --  No description provided for this field
      FPGA_SOFTRESET             : Boolean := False;
      --  No description provided for this field
      HPDMA_SOFTRESET            : Boolean := False;
      --  No description provided for this field
      FIC32_0_SOFTRESET          : Boolean := False;
      --  No description provided for this field
      FIC32_1_SOFTRESET          : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_SOFTRESET         : Boolean := False;
      --  No description provided for this field
      MSS_GPOUT_7_0_SOFT_RESET   : Boolean := False;
      --  No description provided for this field
      MSS_GPOUT_15_8_SOFT_RESET  : Boolean := False;
      --  No description provided for this field
      MSS_GPOUT_23_16_SOFT_RESET : Boolean := False;
      --  No description provided for this field
      MSS_GPOUT_31_24_SOFT_RESET : Boolean := False;
      --  No description provided for this field
      MDDR_CTLR_SOFTRESET        : Boolean := False;
      --  No description provided for this field
      MDDR_FIC64_SOFTRESET       : Boolean := False;
      --  unspecified
      Reserved_27_31             : Interfaces.SF2.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SOFT_RESET_CR_Register use record
      ENVM0_SOFTRESET            at 0 range 0 .. 0;
      ENVM1_SOFTRESET            at 0 range 1 .. 1;
      ESRAM0_SOFTRESET           at 0 range 2 .. 2;
      ESRAM1_SOFTRESET           at 0 range 3 .. 3;
      MAC_SOFTRESET              at 0 range 4 .. 4;
      PDMA_SOFTRESET             at 0 range 5 .. 5;
      TIMER_SOFTRESET            at 0 range 6 .. 6;
      MMUART0_SOFTRESET          at 0 range 7 .. 7;
      MMUART1_SOFTRESET          at 0 range 8 .. 8;
      G4SPI0_SOFTRESET           at 0 range 9 .. 9;
      G4SPI1_SOFTRESET           at 0 range 10 .. 10;
      I2C0_SOFTRESET             at 0 range 11 .. 11;
      I2C1_SOFTRESET             at 0 range 12 .. 12;
      CAN_SOFTRESET              at 0 range 13 .. 13;
      USB_SOFTRESET              at 0 range 14 .. 14;
      COMBLK_SOFTRESET           at 0 range 15 .. 15;
      FPGA_SOFTRESET             at 0 range 16 .. 16;
      HPDMA_SOFTRESET            at 0 range 17 .. 17;
      FIC32_0_SOFTRESET          at 0 range 18 .. 18;
      FIC32_1_SOFTRESET          at 0 range 19 .. 19;
      MSS_GPIO_SOFTRESET         at 0 range 20 .. 20;
      MSS_GPOUT_7_0_SOFT_RESET   at 0 range 21 .. 21;
      MSS_GPOUT_15_8_SOFT_RESET  at 0 range 22 .. 22;
      MSS_GPOUT_23_16_SOFT_RESET at 0 range 23 .. 23;
      MSS_GPOUT_31_24_SOFT_RESET at 0 range 24 .. 24;
      MDDR_CTLR_SOFTRESET        at 0 range 25 .. 25;
      MDDR_FIC64_SOFTRESET       at 0 range 26 .. 26;
      Reserved_27_31             at 0 range 27 .. 31;
   end record;

   subtype M3_CR_STCALIB_25_0_Field is Interfaces.SF2.UInt26;
   subtype M3_CR_STCLK_DIVISOR_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type M3_CR_Register is record
      --  No description provided for this field
      STCALIB_25_0   : M3_CR_STCALIB_25_0_Field := 16#0#;
      --  No description provided for this field
      STCLK_DIVISOR  : M3_CR_STCLK_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      M3_MPU_DISABLE : Boolean := False;
      --  unspecified
      Reserved_29_31 : Interfaces.SF2.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for M3_CR_Register use record
      STCALIB_25_0   at 0 range 0 .. 25;
      STCLK_DIVISOR  at 0 range 26 .. 27;
      M3_MPU_DISABLE at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype FAB_IF_CR_SW_FIC_REG_SEL_Field is Interfaces.SF2.UInt6;

   --  No description provided for this register
   type FAB_IF_CR_Register is record
      --  No description provided for this field
      FAB0_AHB_BYPASS : Boolean := False;
      --  No description provided for this field
      FAB1_AHB_BYPASS : Boolean := False;
      --  No description provided for this field
      FAB0_AHB_MODE   : Boolean := False;
      --  No description provided for this field
      FAB1_AHB_MODE   : Boolean := False;
      --  No description provided for this field
      SW_FIC_REG_SEL  : FAB_IF_CR_SW_FIC_REG_SEL_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FAB_IF_CR_Register use record
      FAB0_AHB_BYPASS at 0 range 0 .. 0;
      FAB1_AHB_BYPASS at 0 range 1 .. 1;
      FAB0_AHB_MODE   at 0 range 2 .. 2;
      FAB1_AHB_MODE   at 0 range 3 .. 3;
      SW_FIC_REG_SEL  at 0 range 4 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   --  No description provided for this register
   type LOOPBACK_CR_Register is record
      --  No description provided for this field
      MSS_MMUARTLOOPBACK : Boolean := False;
      --  No description provided for this field
      MSS_SPILOOPBACK    : Boolean := False;
      --  No description provided for this field
      MSS_I2CLOOPBACK    : Boolean := False;
      --  No description provided for this field
      MSS_GPIOLOOPBACK   : Boolean := False;
      --  unspecified
      Reserved_4_31      : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOOPBACK_CR_Register use record
      MSS_MMUARTLOOPBACK at 0 range 0 .. 0;
      MSS_SPILOOPBACK    at 0 range 1 .. 1;
      MSS_I2CLOOPBACK    at 0 range 2 .. 2;
      MSS_GPIOLOOPBACK   at 0 range 3 .. 3;
      Reserved_4_31      at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type GPIO_SYSRESET_SEL_CR_Register is record
      --  No description provided for this field
      MSS_GPIO_7_0_SYSRESET_SEL   : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_15_8_SYSRESET_SEL  : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_23_16_SYSRESET_SEL : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_31_24_SYSRESET_SEL : Boolean := False;
      --  unspecified
      Reserved_4_31               : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_SYSRESET_SEL_CR_Register use record
      MSS_GPIO_7_0_SYSRESET_SEL   at 0 range 0 .. 0;
      MSS_GPIO_15_8_SYSRESET_SEL  at 0 range 1 .. 1;
      MSS_GPIO_23_16_SYSRESET_SEL at 0 range 2 .. 2;
      MSS_GPIO_31_24_SYSRESET_SEL at 0 range 3 .. 3;
      Reserved_4_31               at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type MDDR_CR_Register is record
      --  No description provided for this field
      MDDR_CONFIG_LOCAL : Boolean := False;
      --  No description provided for this field
      SDR_MODE          : Boolean := False;
      --  No description provided for this field
      F_AXI_AHB_MODE    : Boolean := False;
      --  No description provided for this field
      PHY_SELF_REF_EN   : Boolean := False;
      --  unspecified
      Reserved_4_31     : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDDR_CR_Register use record
      MDDR_CONFIG_LOCAL at 0 range 0 .. 0;
      SDR_MODE          at 0 range 1 .. 1;
      F_AXI_AHB_MODE    at 0 range 2 .. 2;
      PHY_SELF_REF_EN   at 0 range 3 .. 3;
      Reserved_4_31     at 0 range 4 .. 31;
   end record;

   subtype USB_IO_INPUT_SEL_CR_USB_IO_INPUT_SEL_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type USB_IO_INPUT_SEL_CR_Register is record
      --  No description provided for this field
      USB_IO_INPUT_SEL : USB_IO_INPUT_SEL_CR_USB_IO_INPUT_SEL_Field := 16#0#;
      --  unspecified
      Reserved_2_31    : Interfaces.SF2.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_IO_INPUT_SEL_CR_Register use record
      USB_IO_INPUT_SEL at 0 range 0 .. 1;
      Reserved_2_31    at 0 range 2 .. 31;
   end record;

   --  No description provided for this register
   type PERIPH_CLK_MUX_SEL_CR_Register is record
      --  No description provided for this field
      SPI0_SCK_FAB_SEL  : Boolean := False;
      --  No description provided for this field
      SPI1_SCK_FAB_SEL  : Boolean := False;
      --  No description provided for this field
      TRACECLK_DIV2_SEL : Boolean := False;
      --  unspecified
      Reserved_3_31     : Interfaces.SF2.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERIPH_CLK_MUX_SEL_CR_Register use record
      SPI0_SCK_FAB_SEL  at 0 range 0 .. 0;
      SPI1_SCK_FAB_SEL  at 0 range 1 .. 1;
      TRACECLK_DIV2_SEL at 0 range 2 .. 2;
      Reserved_3_31     at 0 range 3 .. 31;
   end record;

   --  No description provided for this register
   type WDOG_CR_Register is record
      --  No description provided for this field
      G4_TESTWDOGENABLE : Boolean := False;
      --  No description provided for this field
      G4_TESTWDOGMODE   : Boolean := False;
      --  unspecified
      Reserved_2_31     : Interfaces.SF2.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDOG_CR_Register use record
      G4_TESTWDOGENABLE at 0 range 0 .. 0;
      G4_TESTWDOGMODE   at 0 range 1 .. 1;
      Reserved_2_31     at 0 range 2 .. 31;
   end record;

   subtype MDDR_IO_CALIB_CR_PCODE_Field is Interfaces.SF2.UInt6;
   subtype MDDR_IO_CALIB_CR_NCODE_Field is Interfaces.SF2.UInt6;

   --  No description provided for this register
   type MDDR_IO_CALIB_CR_Register is record
      --  No description provided for this field
      PCODE          : MDDR_IO_CALIB_CR_PCODE_Field := 16#0#;
      --  No description provided for this field
      NCODE          : MDDR_IO_CALIB_CR_NCODE_Field := 16#0#;
      --  No description provided for this field
      CALIB_TRIM     : Boolean := False;
      --  No description provided for this field
      CALIB_START    : Boolean := False;
      --  No description provided for this field
      CALIB_LOCK     : Boolean := False;
      --  unspecified
      Reserved_15_31 : Interfaces.SF2.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDDR_IO_CALIB_CR_Register use record
      PCODE          at 0 range 0 .. 5;
      NCODE          at 0 range 6 .. 11;
      CALIB_TRIM     at 0 range 12 .. 12;
      CALIB_START    at 0 range 13 .. 13;
      CALIB_LOCK     at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype SPARE_OUT_CR_MSS_SPARE_OUT_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type SPARE_OUT_CR_Register is record
      --  No description provided for this field
      MSS_SPARE_OUT  : SPARE_OUT_CR_MSS_SPARE_OUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SF2.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPARE_OUT_CR_Register use record
      MSS_SPARE_OUT  at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  No description provided for this register
   type EDAC_IRQ_ENABLE_CR_Register is record
      --  No description provided for this field
      ESRAM0_EDAC_1E_EN : Boolean := False;
      --  No description provided for this field
      ESRAM0_EDAC_2E_EN : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_1E_EN : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_2E_EN : Boolean := False;
      --  No description provided for this field
      CC_EDAC_1E_EN     : Boolean := False;
      --  No description provided for this field
      CC_EDAC_2E_EN     : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_1E_EN : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_2E_EN : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_1E_EN : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_2E_EN : Boolean := False;
      --  No description provided for this field
      USB_EDAC_1E_EN    : Boolean := False;
      --  No description provided for this field
      USB_EDAC_2E_EN    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_1E_EN    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_2E_EN    : Boolean := False;
      --  No description provided for this field
      MDDR_ECC_INT_EN   : Boolean := False;
      --  unspecified
      Reserved_15_31    : Interfaces.SF2.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EDAC_IRQ_ENABLE_CR_Register use record
      ESRAM0_EDAC_1E_EN at 0 range 0 .. 0;
      ESRAM0_EDAC_2E_EN at 0 range 1 .. 1;
      ESRAM1_EDAC_1E_EN at 0 range 2 .. 2;
      ESRAM1_EDAC_2E_EN at 0 range 3 .. 3;
      CC_EDAC_1E_EN     at 0 range 4 .. 4;
      CC_EDAC_2E_EN     at 0 range 5 .. 5;
      MAC_EDAC_TX_1E_EN at 0 range 6 .. 6;
      MAC_EDAC_TX_2E_EN at 0 range 7 .. 7;
      MAC_EDAC_RX_1E_EN at 0 range 8 .. 8;
      MAC_EDAC_RX_2E_EN at 0 range 9 .. 9;
      USB_EDAC_1E_EN    at 0 range 10 .. 10;
      USB_EDAC_2E_EN    at 0 range 11 .. 11;
      CAN_EDAC_1E_EN    at 0 range 12 .. 12;
      CAN_EDAC_2E_EN    at 0 range 13 .. 13;
      MDDR_ECC_INT_EN   at 0 range 14 .. 14;
      Reserved_15_31    at 0 range 15 .. 31;
   end record;

   --  No description provided for this register
   type USB_CR_Register is record
      --  No description provided for this field
      USB_UTMI_SEL   : Boolean := False;
      --  No description provided for this field
      USB_DDR_SELECT : Boolean := False;
      --  unspecified
      Reserved_2_31  : Interfaces.SF2.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_CR_Register use record
      USB_UTMI_SEL   at 0 range 0 .. 0;
      USB_DDR_SELECT at 0 range 1 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   --  No description provided for this register
   type ESRAM_PIPELINE_CR_Register is record
      --  No description provided for this field
      ESRAM_PIPELINE_ENABLE : Boolean := False;
      --  unspecified
      Reserved_1_31         : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM_PIPELINE_CR_Register use record
      ESRAM_PIPELINE_ENABLE at 0 range 0 .. 0;
      Reserved_1_31         at 0 range 1 .. 31;
   end record;

   subtype MSS_IRQ_ENABLE_CR_SW_INTERRUPT_EN_Field is Interfaces.SF2.UInt7;
   subtype MSS_IRQ_ENABLE_CR_CC_INTERRUPT_EN_Field is Interfaces.SF2.UInt3;
   subtype MSS_IRQ_ENABLE_CR_DDRB_INTERRUPT_EN_Field is Interfaces.SF2.UInt10;

   --  No description provided for this register
   type MSS_IRQ_ENABLE_CR_Register is record
      --  No description provided for this field
      SW_INTERRUPT_EN   : MSS_IRQ_ENABLE_CR_SW_INTERRUPT_EN_Field := 16#0#;
      --  No description provided for this field
      CC_INTERRUPT_EN   : MSS_IRQ_ENABLE_CR_CC_INTERRUPT_EN_Field := 16#0#;
      --  No description provided for this field
      DDRB_INTERRUPT_EN : MSS_IRQ_ENABLE_CR_DDRB_INTERRUPT_EN_Field := 16#0#;
      --  unspecified
      Reserved_20_31    : Interfaces.SF2.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSS_IRQ_ENABLE_CR_Register use record
      SW_INTERRUPT_EN   at 0 range 0 .. 6;
      CC_INTERRUPT_EN   at 0 range 7 .. 9;
      DDRB_INTERRUPT_EN at 0 range 10 .. 19;
      Reserved_20_31    at 0 range 20 .. 31;
   end record;

   --  No description provided for this register
   type RTC_WAKEUP_CR_Register is record
      --  No description provided for this field
      RTC_WAKEUP_M3_EN  : Boolean := False;
      --  No description provided for this field
      RTC_WAKEUP_FAB_EN : Boolean := False;
      --  No description provided for this field
      RTC_WAKEUP_G4C_EN : Boolean := False;
      --  unspecified
      Reserved_3_31     : Interfaces.SF2.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WAKEUP_CR_Register use record
      RTC_WAKEUP_M3_EN  at 0 range 0 .. 0;
      RTC_WAKEUP_FAB_EN at 0 range 1 .. 1;
      RTC_WAKEUP_G4C_EN at 0 range 2 .. 2;
      Reserved_3_31     at 0 range 3 .. 31;
   end record;

   subtype MAC_CR_ETH_LINE_SPEED_Field is Interfaces.SF2.UInt2;
   subtype MAC_CR_ETH_PHY_MODE_Field is Interfaces.SF2.UInt3;
   subtype MAC_CR_RGMII_TXC_DELAY_Field is Interfaces.SF2.UInt4;

   --  No description provided for this register
   type MAC_CR_Register is record
      --  No description provided for this field
      ETH_LINE_SPEED  : MAC_CR_ETH_LINE_SPEED_Field := 16#0#;
      --  No description provided for this field
      ETH_PHY_MODE    : MAC_CR_ETH_PHY_MODE_Field := 16#0#;
      --  No description provided for this field
      RGMII_TXC_DELAY : MAC_CR_RGMII_TXC_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_9_31   : Interfaces.SF2.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_CR_Register use record
      ETH_LINE_SPEED  at 0 range 0 .. 1;
      ETH_PHY_MODE    at 0 range 2 .. 4;
      RGMII_TXC_DELAY at 0 range 5 .. 8;
      Reserved_9_31   at 0 range 9 .. 31;
   end record;

   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVR_Field is
     Interfaces.SF2.UInt6;
   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVF_Field is
     Interfaces.SF2.UInt10;
   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVQ_Field is
     Interfaces.SF2.UInt3;
   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_RANGE_Field is
     Interfaces.SF2.UInt4;
   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_LOCKWIN_Field is
     Interfaces.SF2.UInt3;
   subtype MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_LOCKCNT_Field is
     Interfaces.SF2.UInt4;

   --  No description provided for this register
   type MSSDDR_PLL_STATUS_LOW_CR_Register is record
      --  No description provided for this field
      FACC_PLL_DIVR    : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVR_Field :=
                          16#0#;
      --  No description provided for this field
      FACC_PLL_DIVF    : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVF_Field :=
                          16#0#;
      --  No description provided for this field
      FACC_PLL_DIVQ    : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_DIVQ_Field :=
                          16#0#;
      --  No description provided for this field
      FACC_PLL_RANGE   : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_RANGE_Field :=
                          16#0#;
      --  No description provided for this field
      FACC_PLL_LOCKWIN : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_LOCKWIN_Field :=
                          16#0#;
      --  No description provided for this field
      FACC_PLL_LOCKCNT : MSSDDR_PLL_STATUS_LOW_CR_FACC_PLL_LOCKCNT_Field :=
                          16#0#;
      --  unspecified
      Reserved_30_31   : Interfaces.SF2.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_PLL_STATUS_LOW_CR_Register use record
      FACC_PLL_DIVR    at 0 range 0 .. 5;
      FACC_PLL_DIVF    at 0 range 6 .. 15;
      FACC_PLL_DIVQ    at 0 range 16 .. 18;
      FACC_PLL_RANGE   at 0 range 19 .. 22;
      FACC_PLL_LOCKWIN at 0 range 23 .. 25;
      FACC_PLL_LOCKCNT at 0 range 26 .. 29;
      Reserved_30_31   at 0 range 30 .. 31;
   end record;

   subtype MSSDDR_PLL_STATUS_HIGH_CR_FACC_PLL_SSMD_Field is
     Interfaces.SF2.UInt2;
   subtype MSSDDR_PLL_STATUS_HIGH_CR_FACC_PLL_SSMF_Field is
     Interfaces.SF2.UInt5;

   --  No description provided for this register
   type MSSDDR_PLL_STATUS_HIGH_CR_Register is record
      --  No description provided for this field
      FACC_PLL_BYPASS   : Boolean := False;
      --  No description provided for this field
      FACC_PLL_MODE_1V2 : Boolean := False;
      --  No description provided for this field
      FACC_PLL_MODE_3V3 : Boolean := False;
      --  No description provided for this field
      FACC_PLL_FSE      : Boolean := False;
      --  No description provided for this field
      FACC_PLL_PD       : Boolean := False;
      --  No description provided for this field
      FACC_PLL_SSE      : Boolean := False;
      --  No description provided for this field
      FACC_PLL_SSMD     : MSSDDR_PLL_STATUS_HIGH_CR_FACC_PLL_SSMD_Field :=
                           16#0#;
      --  No description provided for this field
      FACC_PLL_SSMF     : MSSDDR_PLL_STATUS_HIGH_CR_FACC_PLL_SSMF_Field :=
                           16#0#;
      --  unspecified
      Reserved_13_31    : Interfaces.SF2.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_PLL_STATUS_HIGH_CR_Register use record
      FACC_PLL_BYPASS   at 0 range 0 .. 0;
      FACC_PLL_MODE_1V2 at 0 range 1 .. 1;
      FACC_PLL_MODE_3V3 at 0 range 2 .. 2;
      FACC_PLL_FSE      at 0 range 3 .. 3;
      FACC_PLL_PD       at 0 range 4 .. 4;
      FACC_PLL_SSE      at 0 range 5 .. 5;
      FACC_PLL_SSMD     at 0 range 6 .. 7;
      FACC_PLL_SSMF     at 0 range 8 .. 12;
      Reserved_13_31    at 0 range 13 .. 31;
   end record;

   subtype MSSDDR_FACC1_CR_DIVISOR_A_Field is Interfaces.SF2.UInt2;
   subtype MSSDDR_FACC1_CR_APB0_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_APB1_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_FCLK_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_FIC32_0_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_FIC32_1_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_FIC64_DIVISOR_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC1_CR_BASE_DIVISOR_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type MSSDDR_FACC1_CR_Register is record
      --  No description provided for this field
      DIVISOR_A           : MSSDDR_FACC1_CR_DIVISOR_A_Field := 16#0#;
      --  No description provided for this field
      APB0_DIVISOR        : MSSDDR_FACC1_CR_APB0_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      APB1_DIVISOR        : MSSDDR_FACC1_CR_APB1_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      DDR_CLK_EN          : Boolean := False;
      --  No description provided for this field
      FCLK_DIVISOR        : MSSDDR_FACC1_CR_FCLK_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      FACC_GLMUX_SEL      : Boolean := False;
      --  No description provided for this field
      FIC32_0_DIVISOR     : MSSDDR_FACC1_CR_FIC32_0_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      FIC32_1_DIVISOR     : MSSDDR_FACC1_CR_FIC32_1_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      FIC64_DIVISOR       : MSSDDR_FACC1_CR_FIC64_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      BASE_DIVISOR        : MSSDDR_FACC1_CR_BASE_DIVISOR_Field := 16#0#;
      --  No description provided for this field
      PERSIST_CC          : Boolean := False;
      --  No description provided for this field
      CONTROLLER_PLL_INIT : Boolean := False;
      --  No description provided for this field
      FACC_FAB_REF_SEL    : Boolean := False;
      --  unspecified
      Reserved_28_31      : Interfaces.SF2.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_FACC1_CR_Register use record
      DIVISOR_A           at 0 range 0 .. 1;
      APB0_DIVISOR        at 0 range 2 .. 4;
      APB1_DIVISOR        at 0 range 5 .. 7;
      DDR_CLK_EN          at 0 range 8 .. 8;
      FCLK_DIVISOR        at 0 range 9 .. 11;
      FACC_GLMUX_SEL      at 0 range 12 .. 12;
      FIC32_0_DIVISOR     at 0 range 13 .. 15;
      FIC32_1_DIVISOR     at 0 range 16 .. 18;
      FIC64_DIVISOR       at 0 range 19 .. 21;
      BASE_DIVISOR        at 0 range 22 .. 24;
      PERSIST_CC          at 0 range 25 .. 25;
      CONTROLLER_PLL_INIT at 0 range 26 .. 26;
      FACC_FAB_REF_SEL    at 0 range 27 .. 27;
      Reserved_28_31      at 0 range 28 .. 31;
   end record;

   subtype MSSDDR_FACC2_CR_RTC_CLK_SEL_Field is Interfaces.SF2.UInt2;
   subtype MSSDDR_FACC2_CR_FACC_SRC_SEL_Field is Interfaces.SF2.UInt3;
   subtype MSSDDR_FACC2_CR_FACC_STANDBY_SEL_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type MSSDDR_FACC2_CR_Register is record
      --  No description provided for this field
      RTC_CLK_SEL      : MSSDDR_FACC2_CR_RTC_CLK_SEL_Field := 16#0#;
      --  No description provided for this field
      FACC_SRC_SEL     : MSSDDR_FACC2_CR_FACC_SRC_SEL_Field := 16#0#;
      --  No description provided for this field
      FACC_PRE_SRC_SEL : Boolean := False;
      --  No description provided for this field
      FACC_STANDBY_SEL : MSSDDR_FACC2_CR_FACC_STANDBY_SEL_Field := 16#0#;
      --  No description provided for this field
      MSS_25_50MHZ_EN  : Boolean := False;
      --  No description provided for this field
      MSS_1MHZ_EN      : Boolean := False;
      --  No description provided for this field
      MSS_CLK_ENVM_EN  : Boolean := False;
      --  No description provided for this field
      MSS_XTAL_EN      : Boolean := False;
      --  No description provided for this field
      MSS_XTAL_RTC_EN  : Boolean := False;
      --  unspecified
      Reserved_14_31   : Interfaces.SF2.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_FACC2_CR_Register use record
      RTC_CLK_SEL      at 0 range 0 .. 1;
      FACC_SRC_SEL     at 0 range 2 .. 4;
      FACC_PRE_SRC_SEL at 0 range 5 .. 5;
      FACC_STANDBY_SEL at 0 range 6 .. 8;
      MSS_25_50MHZ_EN  at 0 range 9 .. 9;
      MSS_1MHZ_EN      at 0 range 10 .. 10;
      MSS_CLK_ENVM_EN  at 0 range 11 .. 11;
      MSS_XTAL_EN      at 0 range 12 .. 12;
      MSS_XTAL_RTC_EN  at 0 range 13 .. 13;
      Reserved_14_31   at 0 range 14 .. 31;
   end record;

   --  No description provided for this register
   type PLL_LOCK_EN_CR_Register is record
      --  No description provided for this field
      MPLL_LOCK_EN         : Boolean := False;
      --  No description provided for this field
      MPLL_LOCK_LOST_EN    : Boolean := False;
      --  No description provided for this field
      FAB_PLL_LOCK_EN      : Boolean := False;
      --  No description provided for this field
      FAB_PLL_LOCK_LOST_EN : Boolean := False;
      --  unspecified
      Reserved_4_31        : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_LOCK_EN_CR_Register use record
      MPLL_LOCK_EN         at 0 range 0 .. 0;
      MPLL_LOCK_LOST_EN    at 0 range 1 .. 1;
      FAB_PLL_LOCK_EN      at 0 range 2 .. 2;
      FAB_PLL_LOCK_LOST_EN at 0 range 3 .. 3;
      Reserved_4_31        at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type MSSDDR_CLK_CALIB_CR_Register is record
      --  No description provided for this field
      FAB_CALIB_START : Boolean := False;
      --  unspecified
      Reserved_1_31   : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_CLK_CALIB_CR_Register use record
      FAB_CALIB_START at 0 range 0 .. 0;
      Reserved_1_31   at 0 range 1 .. 31;
   end record;

   subtype PLL_DELAY_LINE_SEL_CR_PLL_REF_DEL_SEL_Field is Interfaces.SF2.UInt2;
   subtype PLL_DELAY_LINE_SEL_CR_PLL_FB_DEL_SEL_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type PLL_DELAY_LINE_SEL_CR_Register is record
      --  No description provided for this field
      PLL_REF_DEL_SEL : PLL_DELAY_LINE_SEL_CR_PLL_REF_DEL_SEL_Field := 16#0#;
      --  No description provided for this field
      PLL_FB_DEL_SEL  : PLL_DELAY_LINE_SEL_CR_PLL_FB_DEL_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_31   : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PLL_DELAY_LINE_SEL_CR_Register use record
      PLL_REF_DEL_SEL at 0 range 0 .. 1;
      PLL_FB_DEL_SEL  at 0 range 2 .. 3;
      Reserved_4_31   at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type MAC_STAT_CLRONRD_CR_Register is record
      --  No description provided for this field
      MAC_STAT_CLRONRD : Boolean := False;
      --  unspecified
      Reserved_1_31    : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_STAT_CLRONRD_CR_Register use record
      MAC_STAT_CLRONRD at 0 range 0 .. 0;
      Reserved_1_31    at 0 range 1 .. 31;
   end record;

   --  No description provided for this register
   type RESET_SOURCE_CR_Register is record
      --  No description provided for this field
      PO_RESET_DETECT            : Boolean := False;
      --  No description provided for this field
      CONTROLLER_RESET_DETECT    : Boolean := False;
      --  No description provided for this field
      CONTROLLER_M3_RESET_DETECT : Boolean := False;
      --  No description provided for this field
      SOFT_RESET_DETECT          : Boolean := False;
      --  No description provided for this field
      LOCKUP_RESET_DETECT        : Boolean := False;
      --  No description provided for this field
      WDOG_RESET_DETECT          : Boolean := False;
      --  No description provided for this field
      USER_RESET_DETECT          : Boolean := False;
      --  No description provided for this field
      USER_M3_RESET_DETECT       : Boolean := False;
      --  unspecified
      Reserved_8_31              : Interfaces.SF2.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESET_SOURCE_CR_Register use record
      PO_RESET_DETECT            at 0 range 0 .. 0;
      CONTROLLER_RESET_DETECT    at 0 range 1 .. 1;
      CONTROLLER_M3_RESET_DETECT at 0 range 2 .. 2;
      SOFT_RESET_DETECT          at 0 range 3 .. 3;
      LOCKUP_RESET_DETECT        at 0 range 4 .. 4;
      WDOG_RESET_DETECT          at 0 range 5 .. 5;
      USER_RESET_DETECT          at 0 range 6 .. 6;
      USER_M3_RESET_DETECT       at 0 range 7 .. 7;
      Reserved_8_31              at 0 range 8 .. 31;
   end record;

   subtype CC_ECCERRINDXADR_SR_CC_DECC_ERR_1E_ADD_Field is
     Interfaces.SF2.UInt6;
   subtype CC_ECCERRINDXADR_SR_CC_DECC_ERR_2E_ADD_Field is
     Interfaces.SF2.UInt6;

   --  No description provided for this register
   type CC_ECCERRINDXADR_SR_Register is record
      --  Read-only. No description provided for this field
      CC_DECC_ERR_1E_ADD : CC_ECCERRINDXADR_SR_CC_DECC_ERR_1E_ADD_Field;
      --  Read-only. No description provided for this field
      CC_DECC_ERR_2E_ADD : CC_ECCERRINDXADR_SR_CC_DECC_ERR_2E_ADD_Field;
      --  unspecified
      Reserved_12_31     : Interfaces.SF2.UInt20;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_ECCERRINDXADR_SR_Register use record
      CC_DECC_ERR_1E_ADD at 0 range 0 .. 5;
      CC_DECC_ERR_2E_ADD at 0 range 6 .. 11;
      Reserved_12_31     at 0 range 12 .. 31;
   end record;

   --  No description provided for this register
   type DDRB_BUF_EMPTY_SR_Register is record
      --  Read-only. No description provided for this field
      DDRB_DS_WBEMPTY  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_DS_RBEMPTY  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_SW_WBEMPTY  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_SW_RBEMPTY  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_HPD_WBEMPTY : Boolean;
      --  Read-only. No description provided for this field
      DDRB_HPD_RBEMPTY : Boolean;
      --  Read-only. No description provided for this field
      DDRB_IDC_RBEMPTY : Boolean;
      --  unspecified
      Reserved_7_31    : Interfaces.SF2.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_BUF_EMPTY_SR_Register use record
      DDRB_DS_WBEMPTY  at 0 range 0 .. 0;
      DDRB_DS_RBEMPTY  at 0 range 1 .. 1;
      DDRB_SW_WBEMPTY  at 0 range 2 .. 2;
      DDRB_SW_RBEMPTY  at 0 range 3 .. 3;
      DDRB_HPD_WBEMPTY at 0 range 4 .. 4;
      DDRB_HPD_RBEMPTY at 0 range 5 .. 5;
      DDRB_IDC_RBEMPTY at 0 range 6 .. 6;
      Reserved_7_31    at 0 range 7 .. 31;
   end record;

   --  No description provided for this register
   type DDRB_DSBL_DN_SR_Register is record
      --  Read-only. No description provided for this field
      DDRB_DS_WDSBL_DN  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_DS_RDSBL_DN  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_SW_WDSBL_DN  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_SW_RDSBL_DN  : Boolean;
      --  Read-only. No description provided for this field
      DDRB_HPD_WDSBL_DN : Boolean;
      --  Read-only. No description provided for this field
      DDRB_HPD_RDSBL_DN : Boolean;
      --  Read-only. No description provided for this field
      DDRB_IDC_DSBL_DN  : Boolean;
      --  unspecified
      Reserved_7_31     : Interfaces.SF2.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DDRB_DSBL_DN_SR_Register use record
      DDRB_DS_WDSBL_DN  at 0 range 0 .. 0;
      DDRB_DS_RDSBL_DN  at 0 range 1 .. 1;
      DDRB_SW_WDSBL_DN  at 0 range 2 .. 2;
      DDRB_SW_RDSBL_DN  at 0 range 3 .. 3;
      DDRB_HPD_WDSBL_DN at 0 range 4 .. 4;
      DDRB_HPD_RDSBL_DN at 0 range 5 .. 5;
      DDRB_IDC_DSBL_DN  at 0 range 6 .. 6;
      Reserved_7_31     at 0 range 7 .. 31;
   end record;

   subtype ESRAM0_EDAC_CNT_ESRAM0_EDAC_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype ESRAM0_EDAC_CNT_ESRAM0_EDAC_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type ESRAM0_EDAC_CNT_Register is record
      --  Read-only. No description provided for this field
      ESRAM0_EDAC_CNT_1E : ESRAM0_EDAC_CNT_ESRAM0_EDAC_CNT_1E_Field;
      --  Read-only. No description provided for this field
      ESRAM0_EDAC_CNT_2E : ESRAM0_EDAC_CNT_ESRAM0_EDAC_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM0_EDAC_CNT_Register use record
      ESRAM0_EDAC_CNT_1E at 0 range 0 .. 15;
      ESRAM0_EDAC_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype ESRAM1_EDAC_CNT_ESRAM1_EDAC_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype ESRAM1_EDAC_CNT_ESRAM1_EDAC_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type ESRAM1_EDAC_CNT_Register is record
      --  Read-only. No description provided for this field
      ESRAM1_EDAC_CNT_1E : ESRAM1_EDAC_CNT_ESRAM1_EDAC_CNT_1E_Field;
      --  Read-only. No description provided for this field
      ESRAM1_EDAC_CNT_2E : ESRAM1_EDAC_CNT_ESRAM1_EDAC_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM1_EDAC_CNT_Register use record
      ESRAM1_EDAC_CNT_1E at 0 range 0 .. 15;
      ESRAM1_EDAC_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype CC_EDAC_CNT_CC_EDAC_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype CC_EDAC_CNT_CC_EDAC_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type CC_EDAC_CNT_Register is record
      --  Read-only. No description provided for this field
      CC_EDAC_CNT_1E : CC_EDAC_CNT_CC_EDAC_CNT_1E_Field;
      --  Read-only. No description provided for this field
      CC_EDAC_CNT_2E : CC_EDAC_CNT_CC_EDAC_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_EDAC_CNT_Register use record
      CC_EDAC_CNT_1E at 0 range 0 .. 15;
      CC_EDAC_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype MAC_EDAC_TX_CNT_MAC_EDAC_TX_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype MAC_EDAC_TX_CNT_MAC_EDAC_TX_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type MAC_EDAC_TX_CNT_Register is record
      --  Read-only. No description provided for this field
      MAC_EDAC_TX_CNT_1E : MAC_EDAC_TX_CNT_MAC_EDAC_TX_CNT_1E_Field;
      --  Read-only. No description provided for this field
      MAC_EDAC_TX_CNT_2E : MAC_EDAC_TX_CNT_MAC_EDAC_TX_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_EDAC_TX_CNT_Register use record
      MAC_EDAC_TX_CNT_1E at 0 range 0 .. 15;
      MAC_EDAC_TX_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype MAC_EDAC_RX_CNT_MAC_EDAC_RX_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype MAC_EDAC_RX_CNT_MAC_EDAC_RX_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type MAC_EDAC_RX_CNT_Register is record
      --  Read-only. No description provided for this field
      MAC_EDAC_RX_CNT_1E : MAC_EDAC_RX_CNT_MAC_EDAC_RX_CNT_1E_Field;
      --  Read-only. No description provided for this field
      MAC_EDAC_RX_CNT_2E : MAC_EDAC_RX_CNT_MAC_EDAC_RX_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_EDAC_RX_CNT_Register use record
      MAC_EDAC_RX_CNT_1E at 0 range 0 .. 15;
      MAC_EDAC_RX_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype USB_EDAC_CNT_USB_EDAC_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype USB_EDAC_CNT_USB_EDAC_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type USB_EDAC_CNT_Register is record
      --  Read-only. No description provided for this field
      USB_EDAC_CNT_1E : USB_EDAC_CNT_USB_EDAC_CNT_1E_Field;
      --  Read-only. No description provided for this field
      USB_EDAC_CNT_2E : USB_EDAC_CNT_USB_EDAC_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_EDAC_CNT_Register use record
      USB_EDAC_CNT_1E at 0 range 0 .. 15;
      USB_EDAC_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype CAN_EDAC_CNT_CAN_EDAC_CNT_1E_Field is Interfaces.SF2.UInt16;
   subtype CAN_EDAC_CNT_CAN_EDAC_CNT_2E_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type CAN_EDAC_CNT_Register is record
      --  Read-only. No description provided for this field
      CAN_EDAC_CNT_1E : CAN_EDAC_CNT_CAN_EDAC_CNT_1E_Field;
      --  Read-only. No description provided for this field
      CAN_EDAC_CNT_2E : CAN_EDAC_CNT_CAN_EDAC_CNT_2E_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_EDAC_CNT_Register use record
      CAN_EDAC_CNT_1E at 0 range 0 .. 15;
      CAN_EDAC_CNT_2E at 0 range 16 .. 31;
   end record;

   subtype ESRAM0_EDAC_ADR_ESRAM0_EDAC_1E_AD_Field is Interfaces.SF2.UInt13;
   subtype ESRAM0_EDAC_ADR_ESRAM0_EDAC_2E_AD_Field is Interfaces.SF2.UInt13;

   --  No description provided for this register
   type ESRAM0_EDAC_ADR_Register is record
      --  Read-only. No description provided for this field
      ESRAM0_EDAC_1E_AD : ESRAM0_EDAC_ADR_ESRAM0_EDAC_1E_AD_Field;
      --  Read-only. No description provided for this field
      ESRAM0_EDAC_2E_AD : ESRAM0_EDAC_ADR_ESRAM0_EDAC_2E_AD_Field;
      --  unspecified
      Reserved_26_31    : Interfaces.SF2.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM0_EDAC_ADR_Register use record
      ESRAM0_EDAC_1E_AD at 0 range 0 .. 12;
      ESRAM0_EDAC_2E_AD at 0 range 13 .. 25;
      Reserved_26_31    at 0 range 26 .. 31;
   end record;

   subtype ESRAM1_EDAC_ADR_ESRAM1_EDAC_1E_AD_Field is Interfaces.SF2.UInt13;
   subtype ESRAM1_EDAC_ADR_ESRAM1_EDAC_2E_AD_Field is Interfaces.SF2.UInt13;

   --  No description provided for this register
   type ESRAM1_EDAC_ADR_Register is record
      --  Read-only. No description provided for this field
      ESRAM1_EDAC_1E_AD : ESRAM1_EDAC_ADR_ESRAM1_EDAC_1E_AD_Field;
      --  Read-only. No description provided for this field
      ESRAM1_EDAC_2E_AD : ESRAM1_EDAC_ADR_ESRAM1_EDAC_2E_AD_Field;
      --  unspecified
      Reserved_26_31    : Interfaces.SF2.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESRAM1_EDAC_ADR_Register use record
      ESRAM1_EDAC_1E_AD at 0 range 0 .. 12;
      ESRAM1_EDAC_2E_AD at 0 range 13 .. 25;
      Reserved_26_31    at 0 range 26 .. 31;
   end record;

   subtype MAC_EDAC_RX_ADR_MAC_EDAC_RX_1E_AD_Field is Interfaces.SF2.UInt11;
   subtype MAC_EDAC_RX_ADR_MAC_EDAC_RX_2E_AD_Field is Interfaces.SF2.UInt11;

   --  No description provided for this register
   type MAC_EDAC_RX_ADR_Register is record
      --  Read-only. No description provided for this field
      MAC_EDAC_RX_1E_AD : MAC_EDAC_RX_ADR_MAC_EDAC_RX_1E_AD_Field;
      --  Read-only. No description provided for this field
      MAC_EDAC_RX_2E_AD : MAC_EDAC_RX_ADR_MAC_EDAC_RX_2E_AD_Field;
      --  unspecified
      Reserved_22_31    : Interfaces.SF2.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_EDAC_RX_ADR_Register use record
      MAC_EDAC_RX_1E_AD at 0 range 0 .. 10;
      MAC_EDAC_RX_2E_AD at 0 range 11 .. 21;
      Reserved_22_31    at 0 range 22 .. 31;
   end record;

   subtype MAC_EDAC_TX_ADR_MAC_EDAC_TX_1E_AD_Field is Interfaces.SF2.UInt10;
   subtype MAC_EDAC_TX_ADR_MAC_EDAC_TX_2E_AD_Field is Interfaces.SF2.UInt10;

   --  No description provided for this register
   type MAC_EDAC_TX_ADR_Register is record
      --  Read-only. No description provided for this field
      MAC_EDAC_TX_1E_AD : MAC_EDAC_TX_ADR_MAC_EDAC_TX_1E_AD_Field;
      --  Read-only. No description provided for this field
      MAC_EDAC_TX_2E_AD : MAC_EDAC_TX_ADR_MAC_EDAC_TX_2E_AD_Field;
      --  unspecified
      Reserved_20_31    : Interfaces.SF2.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_EDAC_TX_ADR_Register use record
      MAC_EDAC_TX_1E_AD at 0 range 0 .. 9;
      MAC_EDAC_TX_2E_AD at 0 range 10 .. 19;
      Reserved_20_31    at 0 range 20 .. 31;
   end record;

   subtype CAN_EDAC_ADR_CAN_EDAC_1E_AD_Field is Interfaces.SF2.UInt9;
   subtype CAN_EDAC_ADR_CAN_EDAC_2E_AD_Field is Interfaces.SF2.UInt9;

   --  No description provided for this register
   type CAN_EDAC_ADR_Register is record
      --  Read-only. No description provided for this field
      CAN_EDAC_1E_AD : CAN_EDAC_ADR_CAN_EDAC_1E_AD_Field;
      --  Read-only. No description provided for this field
      CAN_EDAC_2E_AD : CAN_EDAC_ADR_CAN_EDAC_2E_AD_Field;
      --  unspecified
      Reserved_18_31 : Interfaces.SF2.UInt14;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_EDAC_ADR_Register use record
      CAN_EDAC_1E_AD at 0 range 0 .. 8;
      CAN_EDAC_2E_AD at 0 range 9 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype USB_EDAC_ADR_USB_EDAC_1E_AD_Field is Interfaces.SF2.UInt11;
   subtype USB_EDAC_ADR_USB_EDAC_2E_AD_Field is Interfaces.SF2.UInt11;

   --  No description provided for this register
   type USB_EDAC_ADR_Register is record
      --  Read-only. No description provided for this field
      USB_EDAC_1E_AD : USB_EDAC_ADR_USB_EDAC_1E_AD_Field;
      --  Read-only. No description provided for this field
      USB_EDAC_2E_AD : USB_EDAC_ADR_USB_EDAC_2E_AD_Field;
      --  unspecified
      Reserved_22_31 : Interfaces.SF2.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_EDAC_ADR_Register use record
      USB_EDAC_1E_AD at 0 range 0 .. 10;
      USB_EDAC_2E_AD at 0 range 11 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  No description provided for this register
   type MM0_1_2_SECURITY_Register is record
      --  No description provided for this field
      MM0_1_2_MS0_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS0_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS1_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS1_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS2_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS2_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS3_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS3_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS6_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM0_1_2_MS6_ALLOWED_W : Boolean := False;
      --  unspecified
      Reserved_10_31        : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MM0_1_2_SECURITY_Register use record
      MM0_1_2_MS0_ALLOWED_R at 0 range 0 .. 0;
      MM0_1_2_MS0_ALLOWED_W at 0 range 1 .. 1;
      MM0_1_2_MS1_ALLOWED_R at 0 range 2 .. 2;
      MM0_1_2_MS1_ALLOWED_W at 0 range 3 .. 3;
      MM0_1_2_MS2_ALLOWED_R at 0 range 4 .. 4;
      MM0_1_2_MS2_ALLOWED_W at 0 range 5 .. 5;
      MM0_1_2_MS3_ALLOWED_R at 0 range 6 .. 6;
      MM0_1_2_MS3_ALLOWED_W at 0 range 7 .. 7;
      MM0_1_2_MS6_ALLOWED_R at 0 range 8 .. 8;
      MM0_1_2_MS6_ALLOWED_W at 0 range 9 .. 9;
      Reserved_10_31        at 0 range 10 .. 31;
   end record;

   --  No description provided for this register
   type MM4_5_FIC64_SECURITY_Register is record
      --  No description provided for this field
      MM4_5_FIC64_MS0_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS0_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS1_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS1_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS2_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS2_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS3_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS3_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS6_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM4_5_FIC64_MS6_ALLOWED_W : Boolean := False;
      --  unspecified
      Reserved_10_31            : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MM4_5_FIC64_SECURITY_Register use record
      MM4_5_FIC64_MS0_ALLOWED_R at 0 range 0 .. 0;
      MM4_5_FIC64_MS0_ALLOWED_W at 0 range 1 .. 1;
      MM4_5_FIC64_MS1_ALLOWED_R at 0 range 2 .. 2;
      MM4_5_FIC64_MS1_ALLOWED_W at 0 range 3 .. 3;
      MM4_5_FIC64_MS2_ALLOWED_R at 0 range 4 .. 4;
      MM4_5_FIC64_MS2_ALLOWED_W at 0 range 5 .. 5;
      MM4_5_FIC64_MS3_ALLOWED_R at 0 range 6 .. 6;
      MM4_5_FIC64_MS3_ALLOWED_W at 0 range 7 .. 7;
      MM4_5_FIC64_MS6_ALLOWED_R at 0 range 8 .. 8;
      MM4_5_FIC64_MS6_ALLOWED_W at 0 range 9 .. 9;
      Reserved_10_31            at 0 range 10 .. 31;
   end record;

   --  No description provided for this register
   type MM3_6_7_8_SECURITY_Register is record
      --  No description provided for this field
      MM3_6_7_8_MS0_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS0_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS1_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS1_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS2_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS2_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS3_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS3_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS6_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM3_6_7_8_MS6_ALLOWED_W : Boolean := False;
      --  unspecified
      Reserved_10_31          : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MM3_6_7_8_SECURITY_Register use record
      MM3_6_7_8_MS0_ALLOWED_R at 0 range 0 .. 0;
      MM3_6_7_8_MS0_ALLOWED_W at 0 range 1 .. 1;
      MM3_6_7_8_MS1_ALLOWED_R at 0 range 2 .. 2;
      MM3_6_7_8_MS1_ALLOWED_W at 0 range 3 .. 3;
      MM3_6_7_8_MS2_ALLOWED_R at 0 range 4 .. 4;
      MM3_6_7_8_MS2_ALLOWED_W at 0 range 5 .. 5;
      MM3_6_7_8_MS3_ALLOWED_R at 0 range 6 .. 6;
      MM3_6_7_8_MS3_ALLOWED_W at 0 range 7 .. 7;
      MM3_6_7_8_MS6_ALLOWED_R at 0 range 8 .. 8;
      MM3_6_7_8_MS6_ALLOWED_W at 0 range 9 .. 9;
      Reserved_10_31          at 0 range 10 .. 31;
   end record;

   --  No description provided for this register
   type MM9_SECURITY_Register is record
      --  No description provided for this field
      MM9_MS0_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM9_MS0_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM9_MS1_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM9_MS1_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM9_MS2_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM9_MS2_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM9_MS3_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM9_MS3_ALLOWED_W : Boolean := False;
      --  No description provided for this field
      MM9_MS6_ALLOWED_R : Boolean := False;
      --  No description provided for this field
      MM9_MS6_ALLOWED_W : Boolean := False;
      --  unspecified
      Reserved_10_31    : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MM9_SECURITY_Register use record
      MM9_MS0_ALLOWED_R at 0 range 0 .. 0;
      MM9_MS0_ALLOWED_W at 0 range 1 .. 1;
      MM9_MS1_ALLOWED_R at 0 range 2 .. 2;
      MM9_MS1_ALLOWED_W at 0 range 3 .. 3;
      MM9_MS2_ALLOWED_R at 0 range 4 .. 4;
      MM9_MS2_ALLOWED_W at 0 range 5 .. 5;
      MM9_MS3_ALLOWED_R at 0 range 6 .. 6;
      MM9_MS3_ALLOWED_W at 0 range 7 .. 7;
      MM9_MS6_ALLOWED_R at 0 range 8 .. 8;
      MM9_MS6_ALLOWED_W at 0 range 9 .. 9;
      Reserved_10_31    at 0 range 10 .. 31;
   end record;

   subtype M3_SR_CURRPRI_Field is Interfaces.SF2.Byte;

   --  No description provided for this register
   type M3_SR_Register is record
      --  Read-only. No description provided for this field
      CURRPRI       : M3_SR_CURRPRI_Field;
      --  unspecified
      Reserved_8_31 : Interfaces.SF2.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for M3_SR_Register use record
      CURRPRI       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype ETM_COUNT_HIGH_ETMCOUNT_47_32_Field is Interfaces.SF2.UInt16;
   subtype ETM_COUNT_HIGH_ETMINTNUM_Field is Interfaces.SF2.UInt9;
   subtype ETM_COUNT_HIGH_ETMINTSTAT_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type ETM_COUNT_HIGH_Register is record
      --  Read-only. No description provided for this field
      ETMCOUNT_47_32 : ETM_COUNT_HIGH_ETMCOUNT_47_32_Field;
      --  Read-only. No description provided for this field
      ETMINTNUM      : ETM_COUNT_HIGH_ETMINTNUM_Field;
      --  Read-only. No description provided for this field
      ETMINTSTAT     : ETM_COUNT_HIGH_ETMINTSTAT_Field;
      --  unspecified
      Reserved_28_31 : Interfaces.SF2.UInt4;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ETM_COUNT_HIGH_Register use record
      ETMCOUNT_47_32 at 0 range 0 .. 15;
      ETMINTNUM      at 0 range 16 .. 24;
      ETMINTSTAT     at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  No description provided for this register
   type DEVICE_SR_Register is record
      --  Read-only. No description provided for this field
      CORE_UP_SYNC         : Boolean;
      --  Read-only. No description provided for this field
      VIRGIN_PART          : Boolean;
      --  Read-only. No description provided for this field
      FF_IN_PROGRESS_SYNC  : Boolean;
      --  Read-only. No description provided for this field
      WATCHDOG_FREEZE_SYNC : Boolean;
      --  Read-only. No description provided for this field
      FLASH_VALID_SYNC     : Boolean;
      --  Read-only. No description provided for this field
      M3_DISABLE           : Boolean;
      --  Read-only. No description provided for this field
      M3_DEBUG_ENABLE      : Boolean;
      --  unspecified
      Reserved_7_31        : Interfaces.SF2.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICE_SR_Register use record
      CORE_UP_SYNC         at 0 range 0 .. 0;
      VIRGIN_PART          at 0 range 1 .. 1;
      FF_IN_PROGRESS_SYNC  at 0 range 2 .. 2;
      WATCHDOG_FREEZE_SYNC at 0 range 3 .. 3;
      FLASH_VALID_SYNC     at 0 range 4 .. 4;
      M3_DISABLE           at 0 range 5 .. 5;
      M3_DEBUG_ENABLE      at 0 range 6 .. 6;
      Reserved_7_31        at 0 range 7 .. 31;
   end record;

   --  No description provided for this register
   type ENVM_PROTECT_USER_Register is record
      --  No description provided for this field
      NVM0_LOWER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM0_LOWER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0_LOWER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0_LOWER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM0_UPPER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM0_UPPER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0_UPPER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0_UPPER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM1_LOWER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM1_LOWER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1_LOWER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1_LOWER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM1_UPPER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM1_UPPER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1_UPPER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1_UPPER_WRITE_ALLOWED : Boolean := False;
      --  unspecified
      Reserved_16_31           : Interfaces.SF2.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENVM_PROTECT_USER_Register use record
      NVM0_LOWER_M3ACCESS      at 0 range 0 .. 0;
      NVM0_LOWER_FABRIC_ACCESS at 0 range 1 .. 1;
      NVM0_LOWER_OTHERS_ACCESS at 0 range 2 .. 2;
      NVM0_LOWER_WRITE_ALLOWED at 0 range 3 .. 3;
      NVM0_UPPER_M3ACCESS      at 0 range 4 .. 4;
      NVM0_UPPER_FABRIC_ACCESS at 0 range 5 .. 5;
      NVM0_UPPER_OTHERS_ACCESS at 0 range 6 .. 6;
      NVM0_UPPER_WRITE_ALLOWED at 0 range 7 .. 7;
      NVM1_LOWER_M3ACCESS      at 0 range 8 .. 8;
      NVM1_LOWER_FABRIC_ACCESS at 0 range 9 .. 9;
      NVM1_LOWER_OTHERS_ACCESS at 0 range 10 .. 10;
      NVM1_LOWER_WRITE_ALLOWED at 0 range 11 .. 11;
      NVM1_UPPER_M3ACCESS      at 0 range 12 .. 12;
      NVM1_UPPER_FABRIC_ACCESS at 0 range 13 .. 13;
      NVM1_UPPER_OTHERS_ACCESS at 0 range 14 .. 14;
      NVM1_UPPER_WRITE_ALLOWED at 0 range 15 .. 15;
      Reserved_16_31           at 0 range 16 .. 31;
   end record;

   --  No description provided for this register
   type G4C_ENVM_STATUS_Register is record
      --  No description provided for this field
      CODE_SHADOW_EN : Boolean := False;
      --  unspecified
      Reserved_1_31  : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for G4C_ENVM_STATUS_Register use record
      CODE_SHADOW_EN at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   subtype DEVICE_VERSION_IDP_Field is Interfaces.SF2.UInt16;
   subtype DEVICE_VERSION_IDV_Field is Interfaces.SF2.UInt4;

   --  No description provided for this register
   type DEVICE_VERSION_Register is record
      --  Read-only. No description provided for this field
      IDP            : DEVICE_VERSION_IDP_Field;
      --  Read-only. No description provided for this field
      IDV            : DEVICE_VERSION_IDV_Field;
      --  unspecified
      Reserved_20_31 : Interfaces.SF2.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICE_VERSION_Register use record
      IDP            at 0 range 0 .. 15;
      IDV            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  No description provided for this register
   type MSSDDR_PLL_STATUS_Register is record
      --  Read-only. No description provided for this field
      FACC_PLL_LOCK : Boolean;
      --  Read-only. No description provided for this field
      FAB_PLL_LOCK  : Boolean;
      --  Read-only. No description provided for this field
      MPLL_LOCK     : Boolean;
      --  Read-only. No description provided for this field
      RCOSC_DIV2    : Boolean;
      --  unspecified
      Reserved_4_31 : Interfaces.SF2.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_PLL_STATUS_Register use record
      FACC_PLL_LOCK at 0 range 0 .. 0;
      FAB_PLL_LOCK  at 0 range 1 .. 1;
      MPLL_LOCK     at 0 range 2 .. 2;
      RCOSC_DIV2    at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type USB_SR_Register is record
      --  Read-only. No description provided for this field
      POWERDN       : Boolean;
      --  Read-only. No description provided for this field
      LPI_CARKIT_EN : Boolean;
      --  unspecified
      Reserved_2_31 : Interfaces.SF2.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for USB_SR_Register use record
      POWERDN       at 0 range 0 .. 0;
      LPI_CARKIT_EN at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype ENVM_SR_ENVM_BUSY_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type ENVM_SR_Register is record
      --  Read-only. No description provided for this field
      ENVM_BUSY     : ENVM_SR_ENVM_BUSY_Field;
      --  unspecified
      Reserved_2_31 : Interfaces.SF2.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENVM_SR_Register use record
      ENVM_BUSY     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype SPARE_IN_MSS_SPARE_IN_Field is Interfaces.SF2.UInt16;

   --  No description provided for this register
   type SPARE_IN_Register is record
      --  Read-only. No description provided for this field
      MSS_SPARE_IN   : SPARE_IN_MSS_SPARE_IN_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.SF2.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPARE_IN_Register use record
      MSS_SPARE_IN   at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MDDR_IO_CALIB_STATUS_CALIB_NCODE_Field is Interfaces.SF2.UInt6;
   subtype MDDR_IO_CALIB_STATUS_CALIB_PCODE_Field is Interfaces.SF2.UInt6;

   --  No description provided for this register
   type MDDR_IO_CALIB_STATUS_Register is record
      --  Read-only. No description provided for this field
      CALIB_STATUS   : Boolean;
      --  Read-only. No description provided for this field
      CALIB_NCODE    : MDDR_IO_CALIB_STATUS_CALIB_NCODE_Field;
      --  Read-only. No description provided for this field
      CALIB_PCODE    : MDDR_IO_CALIB_STATUS_CALIB_PCODE_Field;
      --  Read-only. No description provided for this field
      CALIB_NCOMP    : Boolean;
      --  Read-only. No description provided for this field
      CALIB_PCOMP    : Boolean;
      --  unspecified
      Reserved_15_31 : Interfaces.SF2.UInt17;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDDR_IO_CALIB_STATUS_Register use record
      CALIB_STATUS   at 0 range 0 .. 0;
      CALIB_NCODE    at 0 range 1 .. 6;
      CALIB_PCODE    at 0 range 7 .. 12;
      CALIB_NCOMP    at 0 range 13 .. 13;
      CALIB_PCOMP    at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  No description provided for this register
   type MSSDDR_CLK_CALIB_STATUS_Register is record
      --  Read-only. No description provided for this field
      FAB_CALIB_FAIL : Boolean;
      --  unspecified
      Reserved_1_31  : Interfaces.SF2.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSSDDR_CLK_CALIB_STATUS_Register use record
      FAB_CALIB_FAIL at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   subtype WDOGLOAD_G4_TESTWDOGLOAD_Field is Interfaces.SF2.UInt26;

   --  No description provided for this register
   type WDOGLOAD_Register is record
      --  No description provided for this field
      G4_TESTWDOGLOAD : WDOGLOAD_G4_TESTWDOGLOAD_Field := 16#0#;
      --  unspecified
      Reserved_26_31  : Interfaces.SF2.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDOGLOAD_Register use record
      G4_TESTWDOGLOAD at 0 range 0 .. 25;
      Reserved_26_31  at 0 range 26 .. 31;
   end record;

   subtype FAB_PROT_SIZE_SW_PROTREGIONSIZE_Field is Interfaces.SF2.UInt5;

   --  No description provided for this register
   type FAB_PROT_SIZE_Register is record
      --  No description provided for this field
      SW_PROTREGIONSIZE : FAB_PROT_SIZE_SW_PROTREGIONSIZE_Field := 16#0#;
      --  unspecified
      Reserved_5_31     : Interfaces.SF2.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FAB_PROT_SIZE_Register use record
      SW_PROTREGIONSIZE at 0 range 0 .. 4;
      Reserved_5_31     at 0 range 5 .. 31;
   end record;

   --  No description provided for this register
   type MSS_GPIO_DEF_Register is record
      --  No description provided for this field
      MSS_GPIO_7_0_DEF   : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_15_8_DEF  : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_23_16_DEF : Boolean := False;
      --  No description provided for this field
      MSS_GPIO_31_24_DEF : Boolean := False;
      --  unspecified
      Reserved_4_31      : Interfaces.SF2.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSS_GPIO_DEF_Register use record
      MSS_GPIO_7_0_DEF   at 0 range 0 .. 0;
      MSS_GPIO_15_8_DEF  at 0 range 1 .. 1;
      MSS_GPIO_23_16_DEF at 0 range 2 .. 2;
      MSS_GPIO_31_24_DEF at 0 range 3 .. 3;
      Reserved_4_31      at 0 range 4 .. 31;
   end record;

   --  No description provided for this register
   type EDAC_SR_Register is record
      --  No description provided for this field
      ESRAM0_EDAC_1E : Boolean := False;
      --  No description provided for this field
      ESRAM0_EDAC_2E : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_1E : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_2E : Boolean := False;
      --  No description provided for this field
      CC_EDAC_1E     : Boolean := False;
      --  No description provided for this field
      CC_EDAC_2E     : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_1E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_2E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_1E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_2E : Boolean := False;
      --  No description provided for this field
      USB_EDAC_1E    : Boolean := False;
      --  No description provided for this field
      USB_EDAC_2E    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_1E    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_2E    : Boolean := False;
      --  unspecified
      Reserved_14_31 : Interfaces.SF2.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EDAC_SR_Register use record
      ESRAM0_EDAC_1E at 0 range 0 .. 0;
      ESRAM0_EDAC_2E at 0 range 1 .. 1;
      ESRAM1_EDAC_1E at 0 range 2 .. 2;
      ESRAM1_EDAC_2E at 0 range 3 .. 3;
      CC_EDAC_1E     at 0 range 4 .. 4;
      CC_EDAC_2E     at 0 range 5 .. 5;
      MAC_EDAC_TX_1E at 0 range 6 .. 6;
      MAC_EDAC_TX_2E at 0 range 7 .. 7;
      MAC_EDAC_RX_1E at 0 range 8 .. 8;
      MAC_EDAC_RX_2E at 0 range 9 .. 9;
      USB_EDAC_1E    at 0 range 10 .. 10;
      USB_EDAC_2E    at 0 range 11 .. 11;
      CAN_EDAC_1E    at 0 range 12 .. 12;
      CAN_EDAC_2E    at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  No description provided for this register
   type MSS_INTERNAL_SR_Register is record
      --  No description provided for this field
      MPLL_LOCK_INT        : Boolean := False;
      --  No description provided for this field
      MPLL_LOCKLOST_INT    : Boolean := False;
      --  No description provided for this field
      FAB_PLL_LOCK_INT     : Boolean := False;
      --  No description provided for this field
      FAB_PLL_LOCKLOST_INT : Boolean := False;
      --  No description provided for this field
      MDDR_IO_CALIB_INT    : Boolean := False;
      --  No description provided for this field
      MDDR_ECC_INT         : Boolean := False;
      --  No description provided for this field
      FIC64_INT            : Boolean := False;
      --  unspecified
      Reserved_7_31        : Interfaces.SF2.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSS_INTERNAL_SR_Register use record
      MPLL_LOCK_INT        at 0 range 0 .. 0;
      MPLL_LOCKLOST_INT    at 0 range 1 .. 1;
      FAB_PLL_LOCK_INT     at 0 range 2 .. 2;
      FAB_PLL_LOCKLOST_INT at 0 range 3 .. 3;
      MDDR_IO_CALIB_INT    at 0 range 4 .. 4;
      MDDR_ECC_INT         at 0 range 5 .. 5;
      FIC64_INT            at 0 range 6 .. 6;
      Reserved_7_31        at 0 range 7 .. 31;
   end record;

   subtype MSS_EXTERNAL_SR_SW_ERRORSTATUS_Field is Interfaces.SF2.UInt7;
   subtype MSS_EXTERNAL_SR_DDRB_RDWR_ERR_REG_Field is Interfaces.SF2.UInt6;
   subtype MSS_EXTERNAL_SR_CC_HRESP_ERR_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type MSS_EXTERNAL_SR_Register is record
      --  No description provided for this field
      SW_ERRORSTATUS    : MSS_EXTERNAL_SR_SW_ERRORSTATUS_Field := 16#0#;
      --  No description provided for this field
      DDRB_RDWR_ERR_REG : MSS_EXTERNAL_SR_DDRB_RDWR_ERR_REG_Field := 16#0#;
      --  No description provided for this field
      DDRB_DS_WR_ERR    : Boolean := False;
      --  No description provided for this field
      DDRB_SW_WR_ERR    : Boolean := False;
      --  No description provided for this field
      DDRB_HPD_WR_ERR   : Boolean := False;
      --  No description provided for this field
      DDRB_LCKOUT       : Boolean := False;
      --  No description provided for this field
      DDRB_LOCK_MID     : Boolean := False;
      --  No description provided for this field
      CC_HRESP_ERR      : MSS_EXTERNAL_SR_CC_HRESP_ERR_Field := 16#0#;
      --  unspecified
      Reserved_21_31    : Interfaces.SF2.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSS_EXTERNAL_SR_Register use record
      SW_ERRORSTATUS    at 0 range 0 .. 6;
      DDRB_RDWR_ERR_REG at 0 range 7 .. 12;
      DDRB_DS_WR_ERR    at 0 range 13 .. 13;
      DDRB_SW_WR_ERR    at 0 range 14 .. 14;
      DDRB_HPD_WR_ERR   at 0 range 15 .. 15;
      DDRB_LCKOUT       at 0 range 16 .. 16;
      DDRB_LOCK_MID     at 0 range 17 .. 17;
      CC_HRESP_ERR      at 0 range 18 .. 20;
      Reserved_21_31    at 0 range 21 .. 31;
   end record;

   --  No description provided for this register
   type WDOGTIMEOUTEVENT_Register is record
      --  No description provided for this field
      WDOGTIMEOUTEVENT : Boolean := False;
      --  unspecified
      Reserved_1_31    : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDOGTIMEOUTEVENT_Register use record
      WDOGTIMEOUTEVENT at 0 range 0 .. 0;
      Reserved_1_31    at 0 range 1 .. 31;
   end record;

   --  No description provided for this register
   type CLR_MSS_COUNTERS_Register is record
      --  No description provided for this field
      CC_IC_MISS_CNTCLR  : Boolean := False;
      --  No description provided for this field
      CC_IC_HIT_CNTCLR   : Boolean := False;
      --  No description provided for this field
      CC_DC_MISS_CNTCLR  : Boolean := False;
      --  No description provided for this field
      CC_DC_HIT_CNTCLR   : Boolean := False;
      --  No description provided for this field
      CC_IC_TRANS_CNTCLR : Boolean := False;
      --  No description provided for this field
      CC_DC_TRANS_CNTCLR : Boolean := False;
      --  unspecified
      Reserved_6_31      : Interfaces.SF2.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLR_MSS_COUNTERS_Register use record
      CC_IC_MISS_CNTCLR  at 0 range 0 .. 0;
      CC_IC_HIT_CNTCLR   at 0 range 1 .. 1;
      CC_DC_MISS_CNTCLR  at 0 range 2 .. 2;
      CC_DC_HIT_CNTCLR   at 0 range 3 .. 3;
      CC_IC_TRANS_CNTCLR at 0 range 4 .. 4;
      CC_DC_TRANS_CNTCLR at 0 range 5 .. 5;
      Reserved_6_31      at 0 range 6 .. 31;
   end record;

   --  No description provided for this register
   type CLR_EDAC_COUNTERS_Register is record
      --  No description provided for this field
      ESRAM0_EDAC_CNTCLR_1E : Boolean := False;
      --  No description provided for this field
      ESRAM0_EDAC_CNTCLR_2E : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_CNTCLR_1E : Boolean := False;
      --  No description provided for this field
      ESRAM1_EDAC_CNTCLR_2E : Boolean := False;
      --  No description provided for this field
      CC_EDAC_CNTCLR_1E     : Boolean := False;
      --  No description provided for this field
      CC_EDAC_CNTCLR_2E     : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_CNTCLR_1E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_TX_CNTCLR_2E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_CNTCLR_1E : Boolean := False;
      --  No description provided for this field
      MAC_EDAC_RX_CNTCLR_2E : Boolean := False;
      --  No description provided for this field
      USB_EDAC_CNTCLR_1E    : Boolean := False;
      --  No description provided for this field
      USB_EDAC_CNTCLR_2E    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_CNTCLR_1E    : Boolean := False;
      --  No description provided for this field
      CAN_EDAC_CNTCLR_2E    : Boolean := False;
      --  unspecified
      Reserved_14_31        : Interfaces.SF2.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLR_EDAC_COUNTERS_Register use record
      ESRAM0_EDAC_CNTCLR_1E at 0 range 0 .. 0;
      ESRAM0_EDAC_CNTCLR_2E at 0 range 1 .. 1;
      ESRAM1_EDAC_CNTCLR_1E at 0 range 2 .. 2;
      ESRAM1_EDAC_CNTCLR_2E at 0 range 3 .. 3;
      CC_EDAC_CNTCLR_1E     at 0 range 4 .. 4;
      CC_EDAC_CNTCLR_2E     at 0 range 5 .. 5;
      MAC_EDAC_TX_CNTCLR_1E at 0 range 6 .. 6;
      MAC_EDAC_TX_CNTCLR_2E at 0 range 7 .. 7;
      MAC_EDAC_RX_CNTCLR_1E at 0 range 8 .. 8;
      MAC_EDAC_RX_CNTCLR_2E at 0 range 9 .. 9;
      USB_EDAC_CNTCLR_1E    at 0 range 10 .. 10;
      USB_EDAC_CNTCLR_2E    at 0 range 11 .. 11;
      CAN_EDAC_CNTCLR_1E    at 0 range 12 .. 12;
      CAN_EDAC_CNTCLR_2E    at 0 range 13 .. 13;
      Reserved_14_31        at 0 range 14 .. 31;
   end record;

   --  No description provided for this register
   type FLUSH_CR_Register is record
      --  No description provided for this field
      CC_FLUSH_CACHE   : Boolean := False;
      --  No description provided for this field
      CC_FLUSH_CHLINE  : Boolean := False;
      --  No description provided for this field
      DDRB_FLSHDS      : Boolean := False;
      --  No description provided for this field
      DDRB_FLSHHPD     : Boolean := False;
      --  No description provided for this field
      DDRB_FLSHSW      : Boolean := False;
      --  No description provided for this field
      DDRB_INVALID_DS  : Boolean := False;
      --  No description provided for this field
      DDRB_INVALID_SW  : Boolean := False;
      --  No description provided for this field
      DDRB_INVALID_HPD : Boolean := False;
      --  No description provided for this field
      DDRB_INVALID_IDC : Boolean := False;
      --  unspecified
      Reserved_9_31    : Interfaces.SF2.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLUSH_CR_Register use record
      CC_FLUSH_CACHE   at 0 range 0 .. 0;
      CC_FLUSH_CHLINE  at 0 range 1 .. 1;
      DDRB_FLSHDS      at 0 range 2 .. 2;
      DDRB_FLSHHPD     at 0 range 3 .. 3;
      DDRB_FLSHSW      at 0 range 4 .. 4;
      DDRB_INVALID_DS  at 0 range 5 .. 5;
      DDRB_INVALID_SW  at 0 range 6 .. 6;
      DDRB_INVALID_HPD at 0 range 7 .. 7;
      DDRB_INVALID_IDC at 0 range 8 .. 8;
      Reserved_9_31    at 0 range 9 .. 31;
   end record;

   --  No description provided for this register
   type MAC_STAT_CLR_CR_Register is record
      --  No description provided for this field
      MAC_STAT_CLR  : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAC_STAT_CLR_CR_Register use record
      MAC_STAT_CLR  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype IOMUXCELL_0_CONFIG_MSS_IOMUXSEL4_0_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_0_CONFIG_MSS_IOMUXSEL5_0_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_0_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_0 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_0 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_0 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_0 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_0 : IOMUXCELL_0_CONFIG_MSS_IOMUXSEL4_0_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_0 : IOMUXCELL_0_CONFIG_MSS_IOMUXSEL5_0_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_0_CONFIG_Register use record
      MSS_IOMUXSEL0_0 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_0 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_0 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_0 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_0 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_0 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_1_CONFIG_MSS_IOMUXSEL4_1_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_1_CONFIG_MSS_IOMUXSEL5_1_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_1_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_1 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_1 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_1 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_1 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_1 : IOMUXCELL_1_CONFIG_MSS_IOMUXSEL4_1_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_1 : IOMUXCELL_1_CONFIG_MSS_IOMUXSEL5_1_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_1_CONFIG_Register use record
      MSS_IOMUXSEL0_1 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_1 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_1 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_1 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_1 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_1 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_2_CONFIG_MSS_IOMUXSEL4_2_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_2_CONFIG_MSS_IOMUXSEL5_2_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_2_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_2 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_2 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_2 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_2 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_2 : IOMUXCELL_2_CONFIG_MSS_IOMUXSEL4_2_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_2 : IOMUXCELL_2_CONFIG_MSS_IOMUXSEL5_2_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_2_CONFIG_Register use record
      MSS_IOMUXSEL0_2 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_2 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_2 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_2 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_2 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_2 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_3_CONFIG_MSS_IOMUXSEL4_3_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_3_CONFIG_MSS_IOMUXSEL5_3_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_3_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_3 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_3 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_3 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_3 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_3 : IOMUXCELL_3_CONFIG_MSS_IOMUXSEL4_3_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_3 : IOMUXCELL_3_CONFIG_MSS_IOMUXSEL5_3_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_3_CONFIG_Register use record
      MSS_IOMUXSEL0_3 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_3 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_3 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_3 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_3 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_3 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_4_CONFIG_MSS_IOMUXSEL4_4_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_4_CONFIG_MSS_IOMUXSEL5_4_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_4_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_4 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_4 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_4 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_4 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_4 : IOMUXCELL_4_CONFIG_MSS_IOMUXSEL4_4_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_4 : IOMUXCELL_4_CONFIG_MSS_IOMUXSEL5_4_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_4_CONFIG_Register use record
      MSS_IOMUXSEL0_4 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_4 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_4 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_4 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_4 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_4 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_5_CONFIG_MSS_IOMUXSEL4_5_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_5_CONFIG_MSS_IOMUXSEL5_5_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_5_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_5 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_5 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_5 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_5 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_5 : IOMUXCELL_5_CONFIG_MSS_IOMUXSEL4_5_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_5 : IOMUXCELL_5_CONFIG_MSS_IOMUXSEL5_5_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_5_CONFIG_Register use record
      MSS_IOMUXSEL0_5 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_5 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_5 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_5 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_5 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_5 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_6_CONFIG_MSS_IOMUXSEL4_6_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_6_CONFIG_MSS_IOMUXSEL5_6_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_6_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_6 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_6 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_6 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_6 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_6 : IOMUXCELL_6_CONFIG_MSS_IOMUXSEL4_6_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_6 : IOMUXCELL_6_CONFIG_MSS_IOMUXSEL5_6_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_6_CONFIG_Register use record
      MSS_IOMUXSEL0_6 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_6 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_6 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_6 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_6 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_6 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_7_CONFIG_MSS_IOMUXSEL4_7_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_7_CONFIG_MSS_IOMUXSEL5_7_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_7_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_7 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_7 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_7 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_7 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_7 : IOMUXCELL_7_CONFIG_MSS_IOMUXSEL4_7_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_7 : IOMUXCELL_7_CONFIG_MSS_IOMUXSEL5_7_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_7_CONFIG_Register use record
      MSS_IOMUXSEL0_7 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_7 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_7 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_7 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_7 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_7 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_8_CONFIG_MSS_IOMUXSEL4_8_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_8_CONFIG_MSS_IOMUXSEL5_8_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_8_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_8 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_8 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_8 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_8 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_8 : IOMUXCELL_8_CONFIG_MSS_IOMUXSEL4_8_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_8 : IOMUXCELL_8_CONFIG_MSS_IOMUXSEL5_8_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_8_CONFIG_Register use record
      MSS_IOMUXSEL0_8 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_8 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_8 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_8 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_8 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_8 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_9_CONFIG_MSS_IOMUXSEL4_9_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_9_CONFIG_MSS_IOMUXSEL5_9_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_9_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_9 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_9 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_9 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_9 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_9 : IOMUXCELL_9_CONFIG_MSS_IOMUXSEL4_9_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_9 : IOMUXCELL_9_CONFIG_MSS_IOMUXSEL5_9_Field := 16#0#;
      --  unspecified
      Reserved_10_31  : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_9_CONFIG_Register use record
      MSS_IOMUXSEL0_9 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_9 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_9 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_9 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_9 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_9 at 0 range 7 .. 9;
      Reserved_10_31  at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_10_CONFIG_MSS_IOMUXSEL4_10_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_10_CONFIG_MSS_IOMUXSEL5_10_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_10_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_10 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_10 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_10 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_10 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_10 : IOMUXCELL_10_CONFIG_MSS_IOMUXSEL4_10_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_10 : IOMUXCELL_10_CONFIG_MSS_IOMUXSEL5_10_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_10_CONFIG_Register use record
      MSS_IOMUXSEL0_10 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_10 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_10 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_10 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_10 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_10 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_11_CONFIG_MSS_IOMUXSEL4_11_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_11_CONFIG_MSS_IOMUXSEL5_11_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_11_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_11 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_11 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_11 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_11 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_11 : IOMUXCELL_11_CONFIG_MSS_IOMUXSEL4_11_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_11 : IOMUXCELL_11_CONFIG_MSS_IOMUXSEL5_11_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_11_CONFIG_Register use record
      MSS_IOMUXSEL0_11 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_11 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_11 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_11 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_11 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_11 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_12_CONFIG_MSS_IOMUXSEL4_12_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_12_CONFIG_MSS_IOMUXSEL5_12_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_12_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_12 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_12 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_12 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_12 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_12 : IOMUXCELL_12_CONFIG_MSS_IOMUXSEL4_12_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_12 : IOMUXCELL_12_CONFIG_MSS_IOMUXSEL5_12_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_12_CONFIG_Register use record
      MSS_IOMUXSEL0_12 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_12 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_12 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_12 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_12 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_12 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_13_CONFIG_MSS_IOMUXSEL4_13_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_13_CONFIG_MSS_IOMUXSEL5_13_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_13_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_13 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_13 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_13 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_13 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_13 : IOMUXCELL_13_CONFIG_MSS_IOMUXSEL4_13_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_13 : IOMUXCELL_13_CONFIG_MSS_IOMUXSEL5_13_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_13_CONFIG_Register use record
      MSS_IOMUXSEL0_13 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_13 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_13 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_13 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_13 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_13 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_14_CONFIG_MSS_IOMUXSEL4_14_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_14_CONFIG_MSS_IOMUXSEL5_14_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_14_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_14 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_14 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_14 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_14 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_14 : IOMUXCELL_14_CONFIG_MSS_IOMUXSEL4_14_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_14 : IOMUXCELL_14_CONFIG_MSS_IOMUXSEL5_14_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_14_CONFIG_Register use record
      MSS_IOMUXSEL0_14 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_14 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_14 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_14 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_14 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_14 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_15_CONFIG_MSS_IOMUXSEL4_15_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_15_CONFIG_MSS_IOMUXSEL5_15_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_15_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_15 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_15 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_15 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_15 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_15 : IOMUXCELL_15_CONFIG_MSS_IOMUXSEL4_15_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_15 : IOMUXCELL_15_CONFIG_MSS_IOMUXSEL5_15_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_15_CONFIG_Register use record
      MSS_IOMUXSEL0_15 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_15 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_15 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_15 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_15 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_15 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_16_CONFIG_MSS_IOMUXSEL4_16_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_16_CONFIG_MSS_IOMUXSEL5_16_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_16_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_16 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_16 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_16 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_16 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_16 : IOMUXCELL_16_CONFIG_MSS_IOMUXSEL4_16_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_16 : IOMUXCELL_16_CONFIG_MSS_IOMUXSEL5_16_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_16_CONFIG_Register use record
      MSS_IOMUXSEL0_16 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_16 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_16 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_16 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_16 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_16 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_17_CONFIG_MSS_IOMUXSEL4_17_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_17_CONFIG_MSS_IOMUXSEL5_17_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_17_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_17 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_17 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_17 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_17 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_17 : IOMUXCELL_17_CONFIG_MSS_IOMUXSEL4_17_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_17 : IOMUXCELL_17_CONFIG_MSS_IOMUXSEL5_17_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_17_CONFIG_Register use record
      MSS_IOMUXSEL0_17 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_17 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_17 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_17 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_17 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_17 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_18_CONFIG_MSS_IOMUXSEL4_18_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_18_CONFIG_MSS_IOMUXSEL5_18_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_18_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_18 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_18 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_18 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_18 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_18 : IOMUXCELL_18_CONFIG_MSS_IOMUXSEL4_18_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_18 : IOMUXCELL_18_CONFIG_MSS_IOMUXSEL5_18_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_18_CONFIG_Register use record
      MSS_IOMUXSEL0_18 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_18 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_18 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_18 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_18 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_18 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_19_CONFIG_MSS_IOMUXSEL4_19_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_19_CONFIG_MSS_IOMUXSEL5_19_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_19_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_19 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_19 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_19 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_19 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_19 : IOMUXCELL_19_CONFIG_MSS_IOMUXSEL4_19_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_19 : IOMUXCELL_19_CONFIG_MSS_IOMUXSEL5_19_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_19_CONFIG_Register use record
      MSS_IOMUXSEL0_19 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_19 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_19 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_19 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_19 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_19 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_20_CONFIG_MSS_IOMUXSEL4_20_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_20_CONFIG_MSS_IOMUXSEL5_20_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_20_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_20 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_20 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_20 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_20 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_20 : IOMUXCELL_20_CONFIG_MSS_IOMUXSEL4_20_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_20 : IOMUXCELL_20_CONFIG_MSS_IOMUXSEL5_20_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_20_CONFIG_Register use record
      MSS_IOMUXSEL0_20 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_20 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_20 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_20 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_20 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_20 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_21_CONFIG_MSS_IOMUXSEL4_21_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_21_CONFIG_MSS_IOMUXSEL5_21_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_21_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_21 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_21 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_21 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_21 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_21 : IOMUXCELL_21_CONFIG_MSS_IOMUXSEL4_21_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_21 : IOMUXCELL_21_CONFIG_MSS_IOMUXSEL5_21_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_21_CONFIG_Register use record
      MSS_IOMUXSEL0_21 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_21 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_21 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_21 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_21 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_21 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_22_CONFIG_MSS_IOMUXSEL4_22_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_22_CONFIG_MSS_IOMUXSEL5_22_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_22_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_22 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_22 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_22 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_22 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_22 : IOMUXCELL_22_CONFIG_MSS_IOMUXSEL4_22_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_22 : IOMUXCELL_22_CONFIG_MSS_IOMUXSEL5_22_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_22_CONFIG_Register use record
      MSS_IOMUXSEL0_22 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_22 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_22 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_22 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_22 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_22 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_23_CONFIG_MSS_IOMUXSEL4_23_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_23_CONFIG_MSS_IOMUXSEL5_23_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_23_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_23 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_23 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_23 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_23 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_23 : IOMUXCELL_23_CONFIG_MSS_IOMUXSEL4_23_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_23 : IOMUXCELL_23_CONFIG_MSS_IOMUXSEL5_23_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_23_CONFIG_Register use record
      MSS_IOMUXSEL0_23 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_23 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_23 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_23 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_23 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_23 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_24_CONFIG_MSS_IOMUXSEL4_24_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_24_CONFIG_MSS_IOMUXSEL5_24_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_24_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_24 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_24 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_24 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_24 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_24 : IOMUXCELL_24_CONFIG_MSS_IOMUXSEL4_24_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_24 : IOMUXCELL_24_CONFIG_MSS_IOMUXSEL5_24_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_24_CONFIG_Register use record
      MSS_IOMUXSEL0_24 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_24 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_24 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_24 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_24 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_24 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_25_CONFIG_MSS_IOMUXSEL4_25_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_25_CONFIG_MSS_IOMUXSEL5_25_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_25_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_25 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_25 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_25 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_25 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_25 : IOMUXCELL_25_CONFIG_MSS_IOMUXSEL4_25_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_25 : IOMUXCELL_25_CONFIG_MSS_IOMUXSEL5_25_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_25_CONFIG_Register use record
      MSS_IOMUXSEL0_25 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_25 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_25 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_25 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_25 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_25 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_26_CONFIG_MSS_IOMUXSEL4_26_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_26_CONFIG_MSS_IOMUXSEL5_26_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_26_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_26 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_26 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_26 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_26 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_26 : IOMUXCELL_26_CONFIG_MSS_IOMUXSEL4_26_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_26 : IOMUXCELL_26_CONFIG_MSS_IOMUXSEL5_26_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_26_CONFIG_Register use record
      MSS_IOMUXSEL0_26 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_26 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_26 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_26 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_26 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_26 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_27_CONFIG_MSS_IOMUXSEL4_27_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_27_CONFIG_MSS_IOMUXSEL5_27_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_27_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_27 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_27 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_27 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_27 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_27 : IOMUXCELL_27_CONFIG_MSS_IOMUXSEL4_27_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_27 : IOMUXCELL_27_CONFIG_MSS_IOMUXSEL5_27_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_27_CONFIG_Register use record
      MSS_IOMUXSEL0_27 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_27 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_27 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_27 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_27 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_27 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_28_CONFIG_MSS_IOMUXSEL4_28_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_28_CONFIG_MSS_IOMUXSEL5_28_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_28_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_28 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_28 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_28 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_28 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_28 : IOMUXCELL_28_CONFIG_MSS_IOMUXSEL4_28_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_28 : IOMUXCELL_28_CONFIG_MSS_IOMUXSEL5_28_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_28_CONFIG_Register use record
      MSS_IOMUXSEL0_28 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_28 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_28 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_28 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_28 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_28 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_29_CONFIG_MSS_IOMUXSEL4_29_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_29_CONFIG_MSS_IOMUXSEL5_29_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_29_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_29 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_29 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_29 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_29 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_29 : IOMUXCELL_29_CONFIG_MSS_IOMUXSEL4_29_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_29 : IOMUXCELL_29_CONFIG_MSS_IOMUXSEL5_29_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_29_CONFIG_Register use record
      MSS_IOMUXSEL0_29 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_29 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_29 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_29 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_29 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_29 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_30_CONFIG_MSS_IOMUXSEL4_30_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_30_CONFIG_MSS_IOMUXSEL5_30_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_30_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_30 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_30 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_30 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_30 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_30 : IOMUXCELL_30_CONFIG_MSS_IOMUXSEL4_30_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_30 : IOMUXCELL_30_CONFIG_MSS_IOMUXSEL5_30_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_30_CONFIG_Register use record
      MSS_IOMUXSEL0_30 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_30 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_30 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_30 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_30 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_30 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_31_CONFIG_MSS_IOMUXSEL4_31_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_31_CONFIG_MSS_IOMUXSEL5_31_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_31_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_31 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_31 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_31 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_31 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_31 : IOMUXCELL_31_CONFIG_MSS_IOMUXSEL4_31_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_31 : IOMUXCELL_31_CONFIG_MSS_IOMUXSEL5_31_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_31_CONFIG_Register use record
      MSS_IOMUXSEL0_31 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_31 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_31 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_31 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_31 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_31 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_32_CONFIG_MSS_IOMUXSEL4_32_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_32_CONFIG_MSS_IOMUXSEL5_32_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_32_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_32 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_32 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_32 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_32 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_32 : IOMUXCELL_32_CONFIG_MSS_IOMUXSEL4_32_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_32 : IOMUXCELL_32_CONFIG_MSS_IOMUXSEL5_32_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_32_CONFIG_Register use record
      MSS_IOMUXSEL0_32 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_32 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_32 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_32 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_32 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_32 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_33_CONFIG_MSS_IOMUXSEL4_33_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_33_CONFIG_MSS_IOMUXSEL5_33_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_33_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_33 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_33 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_33 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_33 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_33 : IOMUXCELL_33_CONFIG_MSS_IOMUXSEL4_33_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_33 : IOMUXCELL_33_CONFIG_MSS_IOMUXSEL5_33_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_33_CONFIG_Register use record
      MSS_IOMUXSEL0_33 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_33 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_33 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_33 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_33 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_33 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_34_CONFIG_MSS_IOMUXSEL4_34_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_34_CONFIG_MSS_IOMUXSEL5_34_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_34_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_34 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_34 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_34 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_34 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_34 : IOMUXCELL_34_CONFIG_MSS_IOMUXSEL4_34_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_34 : IOMUXCELL_34_CONFIG_MSS_IOMUXSEL5_34_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_34_CONFIG_Register use record
      MSS_IOMUXSEL0_34 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_34 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_34 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_34 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_34 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_34 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_35_CONFIG_MSS_IOMUXSEL4_35_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_35_CONFIG_MSS_IOMUXSEL5_35_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_35_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_35 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_35 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_35 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_35 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_35 : IOMUXCELL_35_CONFIG_MSS_IOMUXSEL4_35_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_35 : IOMUXCELL_35_CONFIG_MSS_IOMUXSEL5_35_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_35_CONFIG_Register use record
      MSS_IOMUXSEL0_35 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_35 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_35 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_35 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_35 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_35 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_36_CONFIG_MSS_IOMUXSEL4_36_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_36_CONFIG_MSS_IOMUXSEL5_36_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_36_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_36 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_36 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_36 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_36 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_36 : IOMUXCELL_36_CONFIG_MSS_IOMUXSEL4_36_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_36 : IOMUXCELL_36_CONFIG_MSS_IOMUXSEL5_36_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_36_CONFIG_Register use record
      MSS_IOMUXSEL0_36 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_36 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_36 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_36 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_36 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_36 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_37_CONFIG_MSS_IOMUXSEL4_37_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_37_CONFIG_MSS_IOMUXSEL5_37_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_37_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_37 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_37 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_37 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_37 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_37 : IOMUXCELL_37_CONFIG_MSS_IOMUXSEL4_37_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_37 : IOMUXCELL_37_CONFIG_MSS_IOMUXSEL5_37_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_37_CONFIG_Register use record
      MSS_IOMUXSEL0_37 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_37 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_37 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_37 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_37 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_37 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_38_CONFIG_MSS_IOMUXSEL4_38_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_38_CONFIG_MSS_IOMUXSEL5_38_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_38_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_38 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_38 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_38 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_38 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_38 : IOMUXCELL_38_CONFIG_MSS_IOMUXSEL4_38_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_38 : IOMUXCELL_38_CONFIG_MSS_IOMUXSEL5_38_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_38_CONFIG_Register use record
      MSS_IOMUXSEL0_38 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_38 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_38 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_38 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_38 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_38 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_39_CONFIG_MSS_IOMUXSEL4_39_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_39_CONFIG_MSS_IOMUXSEL5_39_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_39_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_39 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_39 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_39 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_39 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_39 : IOMUXCELL_39_CONFIG_MSS_IOMUXSEL4_39_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_39 : IOMUXCELL_39_CONFIG_MSS_IOMUXSEL5_39_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_39_CONFIG_Register use record
      MSS_IOMUXSEL0_39 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_39 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_39 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_39 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_39 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_39 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_40_CONFIG_MSS_IOMUXSEL4_40_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_40_CONFIG_MSS_IOMUXSEL5_40_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_40_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_40 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_40 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_40 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_40 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_40 : IOMUXCELL_40_CONFIG_MSS_IOMUXSEL4_40_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_40 : IOMUXCELL_40_CONFIG_MSS_IOMUXSEL5_40_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_40_CONFIG_Register use record
      MSS_IOMUXSEL0_40 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_40 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_40 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_40 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_40 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_40 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_41_CONFIG_MSS_IOMUXSEL4_41_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_41_CONFIG_MSS_IOMUXSEL5_41_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_41_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_41 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_41 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_41 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_41 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_41 : IOMUXCELL_41_CONFIG_MSS_IOMUXSEL4_41_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_41 : IOMUXCELL_41_CONFIG_MSS_IOMUXSEL5_41_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_41_CONFIG_Register use record
      MSS_IOMUXSEL0_41 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_41 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_41 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_41 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_41 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_41 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_42_CONFIG_MSS_IOMUXSEL4_42_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_42_CONFIG_MSS_IOMUXSEL5_42_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_42_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_42 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_42 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_42 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_42 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_42 : IOMUXCELL_42_CONFIG_MSS_IOMUXSEL4_42_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_42 : IOMUXCELL_42_CONFIG_MSS_IOMUXSEL5_42_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_42_CONFIG_Register use record
      MSS_IOMUXSEL0_42 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_42 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_42 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_42 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_42 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_42 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_43_CONFIG_MSS_IOMUXSEL4_43_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_43_CONFIG_MSS_IOMUXSEL5_43_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_43_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_43 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_43 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_43 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_43 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_43 : IOMUXCELL_43_CONFIG_MSS_IOMUXSEL4_43_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_43 : IOMUXCELL_43_CONFIG_MSS_IOMUXSEL5_43_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_43_CONFIG_Register use record
      MSS_IOMUXSEL0_43 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_43 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_43 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_43 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_43 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_43 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_44_CONFIG_MSS_IOMUXSEL4_44_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_44_CONFIG_MSS_IOMUXSEL5_44_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_44_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_44 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_44 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_44 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_44 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_44 : IOMUXCELL_44_CONFIG_MSS_IOMUXSEL4_44_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_44 : IOMUXCELL_44_CONFIG_MSS_IOMUXSEL5_44_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_44_CONFIG_Register use record
      MSS_IOMUXSEL0_44 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_44 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_44 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_44 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_44 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_44 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_45_CONFIG_MSS_IOMUXSEL4_45_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_45_CONFIG_MSS_IOMUXSEL5_45_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_45_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_45 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_45 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_45 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_45 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_45 : IOMUXCELL_45_CONFIG_MSS_IOMUXSEL4_45_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_45 : IOMUXCELL_45_CONFIG_MSS_IOMUXSEL5_45_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_45_CONFIG_Register use record
      MSS_IOMUXSEL0_45 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_45 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_45 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_45 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_45 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_45 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_46_CONFIG_MSS_IOMUXSEL4_46_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_46_CONFIG_MSS_IOMUXSEL5_46_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_46_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_46 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_46 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_46 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_46 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_46 : IOMUXCELL_46_CONFIG_MSS_IOMUXSEL4_46_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_46 : IOMUXCELL_46_CONFIG_MSS_IOMUXSEL5_46_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_46_CONFIG_Register use record
      MSS_IOMUXSEL0_46 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_46 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_46 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_46 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_46 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_46 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_47_CONFIG_MSS_IOMUXSEL4_47_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_47_CONFIG_MSS_IOMUXSEL5_47_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_47_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_47 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_47 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_47 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_47 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_47 : IOMUXCELL_47_CONFIG_MSS_IOMUXSEL4_47_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_47 : IOMUXCELL_47_CONFIG_MSS_IOMUXSEL5_47_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_47_CONFIG_Register use record
      MSS_IOMUXSEL0_47 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_47 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_47 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_47 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_47 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_47 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_48_CONFIG_MSS_IOMUXSEL4_48_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_48_CONFIG_MSS_IOMUXSEL5_48_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_48_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_48 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_48 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_48 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_48 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_48 : IOMUXCELL_48_CONFIG_MSS_IOMUXSEL4_48_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_48 : IOMUXCELL_48_CONFIG_MSS_IOMUXSEL5_48_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_48_CONFIG_Register use record
      MSS_IOMUXSEL0_48 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_48 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_48 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_48 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_48 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_48 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_49_CONFIG_MSS_IOMUXSEL4_49_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_49_CONFIG_MSS_IOMUXSEL5_49_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_49_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_49 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_49 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_49 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_49 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_49 : IOMUXCELL_49_CONFIG_MSS_IOMUXSEL4_49_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_49 : IOMUXCELL_49_CONFIG_MSS_IOMUXSEL5_49_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_49_CONFIG_Register use record
      MSS_IOMUXSEL0_49 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_49 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_49 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_49 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_49 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_49 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_50_CONFIG_MSS_IOMUXSEL4_50_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_50_CONFIG_MSS_IOMUXSEL5_50_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_50_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_50 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_50 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_50 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_50 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_50 : IOMUXCELL_50_CONFIG_MSS_IOMUXSEL4_50_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_50 : IOMUXCELL_50_CONFIG_MSS_IOMUXSEL5_50_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_50_CONFIG_Register use record
      MSS_IOMUXSEL0_50 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_50 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_50 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_50 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_50 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_50 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_51_CONFIG_MSS_IOMUXSEL4_51_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_51_CONFIG_MSS_IOMUXSEL5_51_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_51_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_51 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_51 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_51 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_51 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_51 : IOMUXCELL_51_CONFIG_MSS_IOMUXSEL4_51_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_51 : IOMUXCELL_51_CONFIG_MSS_IOMUXSEL5_51_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_51_CONFIG_Register use record
      MSS_IOMUXSEL0_51 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_51 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_51 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_51 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_51 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_51 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_52_CONFIG_MSS_IOMUXSEL4_52_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_52_CONFIG_MSS_IOMUXSEL5_52_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_52_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_52 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_52 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_52 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_52 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_52 : IOMUXCELL_52_CONFIG_MSS_IOMUXSEL4_52_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_52 : IOMUXCELL_52_CONFIG_MSS_IOMUXSEL5_52_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_52_CONFIG_Register use record
      MSS_IOMUXSEL0_52 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_52 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_52 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_52 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_52 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_52 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_53_CONFIG_MSS_IOMUXSEL4_53_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_53_CONFIG_MSS_IOMUXSEL5_53_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_53_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_53 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_53 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_53 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_53 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_53 : IOMUXCELL_53_CONFIG_MSS_IOMUXSEL4_53_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_53 : IOMUXCELL_53_CONFIG_MSS_IOMUXSEL5_53_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_53_CONFIG_Register use record
      MSS_IOMUXSEL0_53 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_53 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_53 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_53 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_53 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_53 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_54_CONFIG_MSS_IOMUXSEL4_54_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_54_CONFIG_MSS_IOMUXSEL5_54_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_54_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_54 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_54 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_54 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_54 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_54 : IOMUXCELL_54_CONFIG_MSS_IOMUXSEL4_54_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_54 : IOMUXCELL_54_CONFIG_MSS_IOMUXSEL5_54_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_54_CONFIG_Register use record
      MSS_IOMUXSEL0_54 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_54 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_54 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_54 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_54 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_54 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_55_CONFIG_MSS_IOMUXSEL4_55_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_55_CONFIG_MSS_IOMUXSEL5_55_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_55_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_55 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_55 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_55 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_55 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_55 : IOMUXCELL_55_CONFIG_MSS_IOMUXSEL4_55_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_55 : IOMUXCELL_55_CONFIG_MSS_IOMUXSEL5_55_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_55_CONFIG_Register use record
      MSS_IOMUXSEL0_55 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_55 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_55 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_55 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_55 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_55 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   subtype IOMUXCELL_56_CONFIG_MSS_IOMUXSEL4_56_Field is Interfaces.SF2.UInt3;
   subtype IOMUXCELL_56_CONFIG_MSS_IOMUXSEL5_56_Field is Interfaces.SF2.UInt3;

   --  No description provided for this register
   type IOMUXCELL_56_CONFIG_Register is record
      --  No description provided for this field
      MSS_IOMUXSEL0_56 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL1_56 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL2_56 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL3_56 : Boolean := False;
      --  No description provided for this field
      MSS_IOMUXSEL4_56 : IOMUXCELL_56_CONFIG_MSS_IOMUXSEL4_56_Field := 16#0#;
      --  No description provided for this field
      MSS_IOMUXSEL5_56 : IOMUXCELL_56_CONFIG_MSS_IOMUXSEL5_56_Field := 16#0#;
      --  unspecified
      Reserved_10_31   : Interfaces.SF2.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IOMUXCELL_56_CONFIG_Register use record
      MSS_IOMUXSEL0_56 at 0 range 0 .. 0;
      MSS_IOMUXSEL1_56 at 0 range 1 .. 1;
      MSS_IOMUXSEL2_56 at 0 range 2 .. 2;
      MSS_IOMUXSEL3_56 at 0 range 3 .. 3;
      MSS_IOMUXSEL4_56 at 0 range 4 .. 6;
      MSS_IOMUXSEL5_56 at 0 range 7 .. 9;
      Reserved_10_31   at 0 range 10 .. 31;
   end record;

   --  No description provided for this register
   type NVM_PROTECT_FACTORY_Register is record
      --  No description provided for this field
      NVM0F_LOWER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM0F_LOWER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0F_LOWER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0F_LOWER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM0F_UPPER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM0F_UPPER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0F_UPPER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM0F_UPPER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM1F_LOWER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM1F_LOWER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1F_LOWER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1F_LOWER_WRITE_ALLOWED : Boolean := False;
      --  No description provided for this field
      NVM1F_UPPER_M3ACCESS      : Boolean := False;
      --  No description provided for this field
      NVM1F_UPPER_FABRIC_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1F_UPPER_OTHERS_ACCESS : Boolean := False;
      --  No description provided for this field
      NVM1F_UPPER_WRITE_ALLOWED : Boolean := False;
      --  unspecified
      Reserved_16_31            : Interfaces.SF2.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVM_PROTECT_FACTORY_Register use record
      NVM0F_LOWER_M3ACCESS      at 0 range 0 .. 0;
      NVM0F_LOWER_FABRIC_ACCESS at 0 range 1 .. 1;
      NVM0F_LOWER_OTHERS_ACCESS at 0 range 2 .. 2;
      NVM0F_LOWER_WRITE_ALLOWED at 0 range 3 .. 3;
      NVM0F_UPPER_M3ACCESS      at 0 range 4 .. 4;
      NVM0F_UPPER_FABRIC_ACCESS at 0 range 5 .. 5;
      NVM0F_UPPER_OTHERS_ACCESS at 0 range 6 .. 6;
      NVM0F_UPPER_WRITE_ALLOWED at 0 range 7 .. 7;
      NVM1F_LOWER_M3ACCESS      at 0 range 8 .. 8;
      NVM1F_LOWER_FABRIC_ACCESS at 0 range 9 .. 9;
      NVM1F_LOWER_OTHERS_ACCESS at 0 range 10 .. 10;
      NVM1F_LOWER_WRITE_ALLOWED at 0 range 11 .. 11;
      NVM1F_UPPER_M3ACCESS      at 0 range 12 .. 12;
      NVM1F_UPPER_FABRIC_ACCESS at 0 range 13 .. 13;
      NVM1F_UPPER_OTHERS_ACCESS at 0 range 14 .. 14;
      NVM1F_UPPER_WRITE_ALLOWED at 0 range 15 .. 15;
      Reserved_16_31            at 0 range 16 .. 31;
   end record;

   subtype DEVICE_STATUS_FIXED_ENVM_BLOCK_SIZE_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type DEVICE_STATUS_FIXED_Register is record
      --  No description provided for this field
      G4_FACTORY_TEST_MODE : Boolean := False;
      --  No description provided for this field
      M3_ALLOWED           : Boolean := False;
      --  No description provided for this field
      CAN_ALLOWED          : Boolean := False;
      --  No description provided for this field
      ENVM1_PRESENT        : Boolean := False;
      --  No description provided for this field
      ENVM_BLOCK_SIZE      : DEVICE_STATUS_FIXED_ENVM_BLOCK_SIZE_Field :=
                              16#0#;
      --  No description provided for this field
      FIC32_1_DISABLE      : Boolean := False;
      --  unspecified
      Reserved_7_31        : Interfaces.SF2.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICE_STATUS_FIXED_Register use record
      G4_FACTORY_TEST_MODE at 0 range 0 .. 0;
      M3_ALLOWED           at 0 range 1 .. 1;
      CAN_ALLOWED          at 0 range 2 .. 2;
      ENVM1_PRESENT        at 0 range 3 .. 3;
      ENVM_BLOCK_SIZE      at 0 range 4 .. 5;
      FIC32_1_DISABLE      at 0 range 6 .. 6;
      Reserved_7_31        at 0 range 7 .. 31;
   end record;

   --  MBIST_ES0_MBIST_ES0_ADDR array element
   subtype MBIST_ES0_MBIST_ES0_ADDR_Element is Interfaces.SF2.UInt13;

   --  MBIST_ES0_MBIST_ES0_ADDR array
   type MBIST_ES0_MBIST_ES0_ADDR_Field_Array is array (0 .. 1)
     of MBIST_ES0_MBIST_ES0_ADDR_Element
     with Component_Size => 13, Size => 26;

   --  Type definition for MBIST_ES0_MBIST_ES0_ADDR
   type MBIST_ES0_MBIST_ES0_ADDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MBIST_ES0_ADDR as a value
            Val : Interfaces.SF2.UInt26;
         when True =>
            --  MBIST_ES0_ADDR as an array
            Arr : MBIST_ES0_MBIST_ES0_ADDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 26;

   for MBIST_ES0_MBIST_ES0_ADDR_Field use record
      Val at 0 range 0 .. 25;
      Arr at 0 range 0 .. 25;
   end record;

   subtype MBIST_ES0_MBIST_ES0_COUNT_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type MBIST_ES0_Register is record
      --  No description provided for this field
      MBIST_ES0_ADDR  : MBIST_ES0_MBIST_ES0_ADDR_Field :=
                         (As_Array => False, Val => 16#0#);
      --  No description provided for this field
      MBIST_ES0_COUNT : MBIST_ES0_MBIST_ES0_COUNT_Field := 16#0#;
      --  unspecified
      Reserved_28_31  : Interfaces.SF2.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MBIST_ES0_Register use record
      MBIST_ES0_ADDR  at 0 range 0 .. 25;
      MBIST_ES0_COUNT at 0 range 26 .. 27;
      Reserved_28_31  at 0 range 28 .. 31;
   end record;

   --  MBIST_ES1_MBIST_ES1_ADDR array element
   subtype MBIST_ES1_MBIST_ES1_ADDR_Element is Interfaces.SF2.UInt13;

   --  MBIST_ES1_MBIST_ES1_ADDR array
   type MBIST_ES1_MBIST_ES1_ADDR_Field_Array is array (0 .. 1)
     of MBIST_ES1_MBIST_ES1_ADDR_Element
     with Component_Size => 13, Size => 26;

   --  Type definition for MBIST_ES1_MBIST_ES1_ADDR
   type MBIST_ES1_MBIST_ES1_ADDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MBIST_ES1_ADDR as a value
            Val : Interfaces.SF2.UInt26;
         when True =>
            --  MBIST_ES1_ADDR as an array
            Arr : MBIST_ES1_MBIST_ES1_ADDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 26;

   for MBIST_ES1_MBIST_ES1_ADDR_Field use record
      Val at 0 range 0 .. 25;
      Arr at 0 range 0 .. 25;
   end record;

   subtype MBIST_ES1_MBIST_ES1_COUNT_Field is Interfaces.SF2.UInt2;

   --  No description provided for this register
   type MBIST_ES1_Register is record
      --  No description provided for this field
      MBIST_ES1_ADDR  : MBIST_ES1_MBIST_ES1_ADDR_Field :=
                         (As_Array => False, Val => 16#0#);
      --  No description provided for this field
      MBIST_ES1_COUNT : MBIST_ES1_MBIST_ES1_COUNT_Field := 16#0#;
      --  unspecified
      Reserved_28_31  : Interfaces.SF2.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MBIST_ES1_Register use record
      MBIST_ES1_ADDR  at 0 range 0 .. 25;
      MBIST_ES1_COUNT at 0 range 26 .. 27;
      Reserved_28_31  at 0 range 28 .. 31;
   end record;

   --  No description provided for this register
   type MSDDR_PLL_STAUS_1_Register is record
      --  No description provided for this field
      PLL_TEST_MODE : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.SF2.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSDDR_PLL_STAUS_1_Register use record
      PLL_TEST_MODE at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR array element
   subtype REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Element is Interfaces.SF2.UInt13;

   --  REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR array
   type REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Field_Array is array (0 .. 1)
     of REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Element
     with Component_Size => 13, Size => 26;

   --  Type definition for REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR
   type REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RED_ESRAM0_ADDR as a value
            Val : Interfaces.SF2.UInt26;
         when True =>
            --  RED_ESRAM0_ADDR as an array
            Arr : REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 26;

   for REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Field use record
      Val at 0 range 0 .. 25;
      Arr at 0 range 0 .. 25;
   end record;

   --  No description provided for this register
   type REDUNDANCY_ESRAM0_Register is record
      --  No description provided for this field
      RED_ESRAM0_ADDR : REDUNDANCY_ESRAM0_RED_ESRAM0_ADDR_Field :=
                         (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_26_31  : Interfaces.SF2.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REDUNDANCY_ESRAM0_Register use record
      RED_ESRAM0_ADDR at 0 range 0 .. 25;
      Reserved_26_31  at 0 range 26 .. 31;
   end record;

   --  REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR array element
   subtype REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Element is Interfaces.SF2.UInt13;

   --  REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR array
   type REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Field_Array is array (0 .. 1)
     of REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Element
     with Component_Size => 13, Size => 26;

   --  Type definition for REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR
   type REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RED_ESRAM1_ADDR as a value
            Val : Interfaces.SF2.UInt26;
         when True =>
            --  RED_ESRAM1_ADDR as an array
            Arr : REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 26;

   for REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Field use record
      Val at 0 range 0 .. 25;
      Arr at 0 range 0 .. 25;
   end record;

   --  No description provided for this register
   type REDUNDANCY_ESRAM1_Register is record
      --  No description provided for this field
      RED_ESRAM1_ADDR : REDUNDANCY_ESRAM1_RED_ESRAM1_ADDR_Field :=
                         (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_26_31  : Interfaces.SF2.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REDUNDANCY_ESRAM1_Register use record
      RED_ESRAM1_ADDR at 0 range 0 .. 25;
      Reserved_26_31  at 0 range 26 .. 31;
   end record;

   --  No description provided for this register
   type SERDESIF_Register is record
      --  No description provided for this field
      SERDESIF0_GEN2 : Boolean := False;
      --  No description provided for this field
      SERDESIF1_GEN2 : Boolean := False;
      --  unspecified
      Reserved_2_31  : Interfaces.SF2.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERDESIF_Register use record
      SERDESIF0_GEN2 at 0 range 0 .. 0;
      SERDESIF1_GEN2 at 0 range 1 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  No description provided for this peripheral
   type System_Registers_Peripheral is record
      --  No description provided for this register
      ESRAM_CR                  : aliased ESRAM_CR_Register;
      --  No description provided for this register
      ESRAM_MAX_LAT_CR          : aliased ESRAM_MAX_LAT_CR_Register;
      --  No description provided for this register
      DDR_CR                    : aliased DDR_CR_Register;
      --  No description provided for this register
      ENVM_CR                   : aliased ENVM_CR_Register;
      --  No description provided for this register
      ENVM_REMAP_BASE_CR        : aliased ENVM_REMAP_BASE_CR_Register;
      --  No description provided for this register
      ENVM_REMAP_FAB_CR         : aliased ENVM_REMAP_FAB_CR_Register;
      --  No description provided for this register
      CC_CR                     : aliased CC_CR_Register;
      --  No description provided for this register
      CC_REGION_CR              : aliased CC_REGION_CR_Register;
      --  No description provided for this register
      CC_LOCK_BASE_ADDR_CR      : aliased CC_LOCK_BASE_ADDR_CR_Register;
      --  No description provided for this register
      CC_FLUSH_INDX_CR          : aliased CC_FLUSH_INDX_CR_Register;
      --  No description provided for this register
      DDRB_BUF_TIMER_CR         : aliased DDRB_BUF_TIMER_CR_Register;
      --  No description provided for this register
      DDRB_NB_ADDR_CR           : aliased DDRB_NB_ADDR_CR_Register;
      --  No description provided for this register
      DDRB_NB_SIZE_CR           : aliased DDRB_NB_SIZE_CR_Register;
      --  No description provided for this register
      DDRB_CR                   : aliased DDRB_CR_Register;
      --  No description provided for this register
      EDAC_CR                   : aliased EDAC_CR_Register;
      --  No description provided for this register
      MASTER_WEIGHT0_CR         : aliased MASTER_WEIGHT0_CR_Register;
      --  No description provided for this register
      MASTER_WEIGHT1_CR         : aliased MASTER_WEIGHT1_CR_Register;
      --  No description provided for this register
      SOFT_IRQ_CR               : aliased SOFT_IRQ_CR_Register;
      --  No description provided for this register
      SOFT_RESET_CR             : aliased SOFT_RESET_CR_Register;
      --  No description provided for this register
      M3_CR                     : aliased M3_CR_Register;
      --  No description provided for this register
      FAB_IF_CR                 : aliased FAB_IF_CR_Register;
      --  No description provided for this register
      LOOPBACK_CR               : aliased LOOPBACK_CR_Register;
      --  No description provided for this register
      GPIO_SYSRESET_SEL_CR      : aliased GPIO_SYSRESET_SEL_CR_Register;
      --  No description provided for this register
      GPIN_SRC_SEL_CR           : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      MDDR_CR                   : aliased MDDR_CR_Register;
      --  No description provided for this register
      USB_IO_INPUT_SEL_CR       : aliased USB_IO_INPUT_SEL_CR_Register;
      --  No description provided for this register
      PERIPH_CLK_MUX_SEL_CR     : aliased PERIPH_CLK_MUX_SEL_CR_Register;
      --  No description provided for this register
      WDOG_CR                   : aliased WDOG_CR_Register;
      --  No description provided for this register
      MDDR_IO_CALIB_CR          : aliased MDDR_IO_CALIB_CR_Register;
      --  No description provided for this register
      SPARE_OUT_CR              : aliased SPARE_OUT_CR_Register;
      --  No description provided for this register
      EDAC_IRQ_ENABLE_CR        : aliased EDAC_IRQ_ENABLE_CR_Register;
      --  No description provided for this register
      USB_CR                    : aliased USB_CR_Register;
      --  No description provided for this register
      ESRAM_PIPELINE_CR         : aliased ESRAM_PIPELINE_CR_Register;
      --  No description provided for this register
      MSS_IRQ_ENABLE_CR         : aliased MSS_IRQ_ENABLE_CR_Register;
      --  No description provided for this register
      RTC_WAKEUP_CR             : aliased RTC_WAKEUP_CR_Register;
      --  No description provided for this register
      MAC_CR                    : aliased MAC_CR_Register;
      --  No description provided for this register
      MSSDDR_PLL_STATUS_LOW_CR  : aliased MSSDDR_PLL_STATUS_LOW_CR_Register;
      --  No description provided for this register
      MSSDDR_PLL_STATUS_HIGH_CR : aliased MSSDDR_PLL_STATUS_HIGH_CR_Register;
      --  No description provided for this register
      MSSDDR_FACC1_CR           : aliased MSSDDR_FACC1_CR_Register;
      --  No description provided for this register
      MSSDDR_FACC2_CR           : aliased MSSDDR_FACC2_CR_Register;
      --  No description provided for this register
      PLL_LOCK_EN_CR            : aliased PLL_LOCK_EN_CR_Register;
      --  No description provided for this register
      MSSDDR_CLK_CALIB_CR       : aliased MSSDDR_CLK_CALIB_CR_Register;
      --  No description provided for this register
      PLL_DELAY_LINE_SEL_CR     : aliased PLL_DELAY_LINE_SEL_CR_Register;
      --  No description provided for this register
      MAC_STAT_CLRONRD_CR       : aliased MAC_STAT_CLRONRD_CR_Register;
      --  No description provided for this register
      RESET_SOURCE_CR           : aliased RESET_SOURCE_CR_Register;
      --  No description provided for this register
      CC_ERRRSPADDRD_SR         : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_ERRRSPADDRI_SR         : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_ERRRSPADDRS_SR         : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_ECCERRINDXADR_SR       : aliased CC_ECCERRINDXADR_SR_Register;
      --  No description provided for this register
      CC_IC_MISS_CNTR_SR        : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_IC_HIT_CNTR_SR         : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_DC_MISS_CNTR_SR        : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_DC_HIT_CNTR_CR         : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_IC_TRANS_CNTR_SR       : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      CC_DC_TRANS_CNTR_SR       : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      DDRB_DS_ERR_ADR_SR        : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      DDRB_HPD_ERR_ADR_SR       : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      DDRB_SW_ERR_ADR_SR        : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      DDRB_BUF_EMPTY_SR         : aliased DDRB_BUF_EMPTY_SR_Register;
      --  No description provided for this register
      DDRB_DSBL_DN_SR           : aliased DDRB_DSBL_DN_SR_Register;
      --  No description provided for this register
      ESRAM0_EDAC_CNT           : aliased ESRAM0_EDAC_CNT_Register;
      --  No description provided for this register
      ESRAM1_EDAC_CNT           : aliased ESRAM1_EDAC_CNT_Register;
      --  No description provided for this register
      CC_EDAC_CNT               : aliased CC_EDAC_CNT_Register;
      --  No description provided for this register
      MAC_EDAC_TX_CNT           : aliased MAC_EDAC_TX_CNT_Register;
      --  No description provided for this register
      MAC_EDAC_RX_CNT           : aliased MAC_EDAC_RX_CNT_Register;
      --  No description provided for this register
      USB_EDAC_CNT              : aliased USB_EDAC_CNT_Register;
      --  No description provided for this register
      CAN_EDAC_CNT              : aliased CAN_EDAC_CNT_Register;
      --  No description provided for this register
      ESRAM0_EDAC_ADR           : aliased ESRAM0_EDAC_ADR_Register;
      --  No description provided for this register
      ESRAM1_EDAC_ADR           : aliased ESRAM1_EDAC_ADR_Register;
      --  No description provided for this register
      MAC_EDAC_RX_ADR           : aliased MAC_EDAC_RX_ADR_Register;
      --  No description provided for this register
      MAC_EDAC_TX_ADR           : aliased MAC_EDAC_TX_ADR_Register;
      --  No description provided for this register
      CAN_EDAC_ADR              : aliased CAN_EDAC_ADR_Register;
      --  No description provided for this register
      USB_EDAC_ADR              : aliased USB_EDAC_ADR_Register;
      --  No description provided for this register
      MM0_1_2_SECURITY          : aliased MM0_1_2_SECURITY_Register;
      --  No description provided for this register
      MM4_5_FIC64_SECURITY      : aliased MM4_5_FIC64_SECURITY_Register;
      --  No description provided for this register
      MM3_6_7_8_SECURITY        : aliased MM3_6_7_8_SECURITY_Register;
      --  No description provided for this register
      MM9_SECURITY              : aliased MM9_SECURITY_Register;
      --  No description provided for this register
      M3_SR                     : aliased M3_SR_Register;
      --  No description provided for this register
      ETM_COUNT_LOW             : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      ETM_COUNT_HIGH            : aliased ETM_COUNT_HIGH_Register;
      --  No description provided for this register
      DEVICE_SR                 : aliased DEVICE_SR_Register;
      --  No description provided for this register
      ENVM_PROTECT_USER         : aliased ENVM_PROTECT_USER_Register;
      --  No description provided for this register
      G4C_ENVM_STATUS           : aliased G4C_ENVM_STATUS_Register;
      --  No description provided for this register
      DEVICE_VERSION            : aliased DEVICE_VERSION_Register;
      --  No description provided for this register
      MSSDDR_PLL_STATUS         : aliased MSSDDR_PLL_STATUS_Register;
      --  No description provided for this register
      USB_SR                    : aliased USB_SR_Register;
      --  No description provided for this register
      ENVM_SR                   : aliased ENVM_SR_Register;
      --  No description provided for this register
      SPARE_IN                  : aliased SPARE_IN_Register;
      --  No description provided for this register
      DDRB_STATUS               : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      MDDR_IO_CALIB_STATUS      : aliased MDDR_IO_CALIB_STATUS_Register;
      --  No description provided for this register
      MSSDDR_CLK_CALIB_STATUS   : aliased MSSDDR_CLK_CALIB_STATUS_Register;
      --  No description provided for this register
      WDOGLOAD                  : aliased WDOGLOAD_Register;
      --  No description provided for this register
      WDOGMVRP                  : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      USERCONFIG0               : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      USERCONFIG1               : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      USERCONFIG2               : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      USERCONFIG3               : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      FAB_PROT_SIZE             : aliased FAB_PROT_SIZE_Register;
      --  No description provided for this register
      FAB_PROT_BASE             : aliased Interfaces.SF2.UInt32;
      --  No description provided for this register
      MSS_GPIO_DEF              : aliased MSS_GPIO_DEF_Register;
      --  No description provided for this register
      EDAC_SR                   : aliased EDAC_SR_Register;
      --  No description provided for this register
      MSS_INTERNAL_SR           : aliased MSS_INTERNAL_SR_Register;
      --  No description provided for this register
      MSS_EXTERNAL_SR           : aliased MSS_EXTERNAL_SR_Register;
      --  No description provided for this register
      WDOGTIMEOUTEVENT          : aliased WDOGTIMEOUTEVENT_Register;
      --  No description provided for this register
      CLR_MSS_COUNTERS          : aliased CLR_MSS_COUNTERS_Register;
      --  No description provided for this register
      CLR_EDAC_COUNTERS         : aliased CLR_EDAC_COUNTERS_Register;
      --  No description provided for this register
      FLUSH_CR                  : aliased FLUSH_CR_Register;
      --  No description provided for this register
      MAC_STAT_CLR_CR           : aliased MAC_STAT_CLR_CR_Register;
      --  No description provided for this register
      IOMUXCELL_0_CONFIG        : aliased IOMUXCELL_0_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_1_CONFIG        : aliased IOMUXCELL_1_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_2_CONFIG        : aliased IOMUXCELL_2_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_3_CONFIG        : aliased IOMUXCELL_3_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_4_CONFIG        : aliased IOMUXCELL_4_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_5_CONFIG        : aliased IOMUXCELL_5_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_6_CONFIG        : aliased IOMUXCELL_6_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_7_CONFIG        : aliased IOMUXCELL_7_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_8_CONFIG        : aliased IOMUXCELL_8_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_9_CONFIG        : aliased IOMUXCELL_9_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_10_CONFIG       : aliased IOMUXCELL_10_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_11_CONFIG       : aliased IOMUXCELL_11_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_12_CONFIG       : aliased IOMUXCELL_12_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_13_CONFIG       : aliased IOMUXCELL_13_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_14_CONFIG       : aliased IOMUXCELL_14_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_15_CONFIG       : aliased IOMUXCELL_15_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_16_CONFIG       : aliased IOMUXCELL_16_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_17_CONFIG       : aliased IOMUXCELL_17_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_18_CONFIG       : aliased IOMUXCELL_18_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_19_CONFIG       : aliased IOMUXCELL_19_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_20_CONFIG       : aliased IOMUXCELL_20_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_21_CONFIG       : aliased IOMUXCELL_21_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_22_CONFIG       : aliased IOMUXCELL_22_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_23_CONFIG       : aliased IOMUXCELL_23_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_24_CONFIG       : aliased IOMUXCELL_24_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_25_CONFIG       : aliased IOMUXCELL_25_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_26_CONFIG       : aliased IOMUXCELL_26_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_27_CONFIG       : aliased IOMUXCELL_27_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_28_CONFIG       : aliased IOMUXCELL_28_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_29_CONFIG       : aliased IOMUXCELL_29_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_30_CONFIG       : aliased IOMUXCELL_30_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_31_CONFIG       : aliased IOMUXCELL_31_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_32_CONFIG       : aliased IOMUXCELL_32_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_33_CONFIG       : aliased IOMUXCELL_33_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_34_CONFIG       : aliased IOMUXCELL_34_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_35_CONFIG       : aliased IOMUXCELL_35_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_36_CONFIG       : aliased IOMUXCELL_36_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_37_CONFIG       : aliased IOMUXCELL_37_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_38_CONFIG       : aliased IOMUXCELL_38_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_39_CONFIG       : aliased IOMUXCELL_39_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_40_CONFIG       : aliased IOMUXCELL_40_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_41_CONFIG       : aliased IOMUXCELL_41_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_42_CONFIG       : aliased IOMUXCELL_42_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_43_CONFIG       : aliased IOMUXCELL_43_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_44_CONFIG       : aliased IOMUXCELL_44_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_45_CONFIG       : aliased IOMUXCELL_45_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_46_CONFIG       : aliased IOMUXCELL_46_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_47_CONFIG       : aliased IOMUXCELL_47_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_48_CONFIG       : aliased IOMUXCELL_48_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_49_CONFIG       : aliased IOMUXCELL_49_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_50_CONFIG       : aliased IOMUXCELL_50_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_51_CONFIG       : aliased IOMUXCELL_51_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_52_CONFIG       : aliased IOMUXCELL_52_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_53_CONFIG       : aliased IOMUXCELL_53_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_54_CONFIG       : aliased IOMUXCELL_54_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_55_CONFIG       : aliased IOMUXCELL_55_CONFIG_Register;
      --  No description provided for this register
      IOMUXCELL_56_CONFIG       : aliased IOMUXCELL_56_CONFIG_Register;
      --  No description provided for this register
      NVM_PROTECT_FACTORY       : aliased NVM_PROTECT_FACTORY_Register;
      --  No description provided for this register
      DEVICE_STATUS_FIXED       : aliased DEVICE_STATUS_FIXED_Register;
      --  No description provided for this register
      MBIST_ES0                 : aliased MBIST_ES0_Register;
      --  No description provided for this register
      MBIST_ES1                 : aliased MBIST_ES1_Register;
      --  No description provided for this register
      MSDDR_PLL_STAUS_1         : aliased MSDDR_PLL_STAUS_1_Register;
      --  No description provided for this register
      REDUNDANCY_ESRAM0         : aliased REDUNDANCY_ESRAM0_Register;
      --  No description provided for this register
      REDUNDANCY_ESRAM1         : aliased REDUNDANCY_ESRAM1_Register;
      --  No description provided for this register
      SERDESIF                  : aliased SERDESIF_Register;
   end record
     with Volatile;

   for System_Registers_Peripheral use record
      ESRAM_CR                  at 16#0# range 0 .. 31;
      ESRAM_MAX_LAT_CR          at 16#4# range 0 .. 31;
      DDR_CR                    at 16#8# range 0 .. 31;
      ENVM_CR                   at 16#C# range 0 .. 31;
      ENVM_REMAP_BASE_CR        at 16#10# range 0 .. 31;
      ENVM_REMAP_FAB_CR         at 16#14# range 0 .. 31;
      CC_CR                     at 16#18# range 0 .. 31;
      CC_REGION_CR              at 16#1C# range 0 .. 31;
      CC_LOCK_BASE_ADDR_CR      at 16#20# range 0 .. 31;
      CC_FLUSH_INDX_CR          at 16#24# range 0 .. 31;
      DDRB_BUF_TIMER_CR         at 16#28# range 0 .. 31;
      DDRB_NB_ADDR_CR           at 16#2C# range 0 .. 31;
      DDRB_NB_SIZE_CR           at 16#30# range 0 .. 31;
      DDRB_CR                   at 16#34# range 0 .. 31;
      EDAC_CR                   at 16#38# range 0 .. 31;
      MASTER_WEIGHT0_CR         at 16#3C# range 0 .. 31;
      MASTER_WEIGHT1_CR         at 16#40# range 0 .. 31;
      SOFT_IRQ_CR               at 16#44# range 0 .. 31;
      SOFT_RESET_CR             at 16#48# range 0 .. 31;
      M3_CR                     at 16#4C# range 0 .. 31;
      FAB_IF_CR                 at 16#50# range 0 .. 31;
      LOOPBACK_CR               at 16#54# range 0 .. 31;
      GPIO_SYSRESET_SEL_CR      at 16#58# range 0 .. 31;
      GPIN_SRC_SEL_CR           at 16#5C# range 0 .. 31;
      MDDR_CR                   at 16#60# range 0 .. 31;
      USB_IO_INPUT_SEL_CR       at 16#64# range 0 .. 31;
      PERIPH_CLK_MUX_SEL_CR     at 16#68# range 0 .. 31;
      WDOG_CR                   at 16#6C# range 0 .. 31;
      MDDR_IO_CALIB_CR          at 16#70# range 0 .. 31;
      SPARE_OUT_CR              at 16#74# range 0 .. 31;
      EDAC_IRQ_ENABLE_CR        at 16#78# range 0 .. 31;
      USB_CR                    at 16#7C# range 0 .. 31;
      ESRAM_PIPELINE_CR         at 16#80# range 0 .. 31;
      MSS_IRQ_ENABLE_CR         at 16#84# range 0 .. 31;
      RTC_WAKEUP_CR             at 16#88# range 0 .. 31;
      MAC_CR                    at 16#8C# range 0 .. 31;
      MSSDDR_PLL_STATUS_LOW_CR  at 16#90# range 0 .. 31;
      MSSDDR_PLL_STATUS_HIGH_CR at 16#94# range 0 .. 31;
      MSSDDR_FACC1_CR           at 16#98# range 0 .. 31;
      MSSDDR_FACC2_CR           at 16#9C# range 0 .. 31;
      PLL_LOCK_EN_CR            at 16#A0# range 0 .. 31;
      MSSDDR_CLK_CALIB_CR       at 16#A4# range 0 .. 31;
      PLL_DELAY_LINE_SEL_CR     at 16#A8# range 0 .. 31;
      MAC_STAT_CLRONRD_CR       at 16#AC# range 0 .. 31;
      RESET_SOURCE_CR           at 16#B0# range 0 .. 31;
      CC_ERRRSPADDRD_SR         at 16#B4# range 0 .. 31;
      CC_ERRRSPADDRI_SR         at 16#B8# range 0 .. 31;
      CC_ERRRSPADDRS_SR         at 16#BC# range 0 .. 31;
      CC_ECCERRINDXADR_SR       at 16#C0# range 0 .. 31;
      CC_IC_MISS_CNTR_SR        at 16#C4# range 0 .. 31;
      CC_IC_HIT_CNTR_SR         at 16#C8# range 0 .. 31;
      CC_DC_MISS_CNTR_SR        at 16#CC# range 0 .. 31;
      CC_DC_HIT_CNTR_CR         at 16#D0# range 0 .. 31;
      CC_IC_TRANS_CNTR_SR       at 16#D4# range 0 .. 31;
      CC_DC_TRANS_CNTR_SR       at 16#D8# range 0 .. 31;
      DDRB_DS_ERR_ADR_SR        at 16#DC# range 0 .. 31;
      DDRB_HPD_ERR_ADR_SR       at 16#E0# range 0 .. 31;
      DDRB_SW_ERR_ADR_SR        at 16#E4# range 0 .. 31;
      DDRB_BUF_EMPTY_SR         at 16#E8# range 0 .. 31;
      DDRB_DSBL_DN_SR           at 16#EC# range 0 .. 31;
      ESRAM0_EDAC_CNT           at 16#F0# range 0 .. 31;
      ESRAM1_EDAC_CNT           at 16#F4# range 0 .. 31;
      CC_EDAC_CNT               at 16#F8# range 0 .. 31;
      MAC_EDAC_TX_CNT           at 16#FC# range 0 .. 31;
      MAC_EDAC_RX_CNT           at 16#100# range 0 .. 31;
      USB_EDAC_CNT              at 16#104# range 0 .. 31;
      CAN_EDAC_CNT              at 16#108# range 0 .. 31;
      ESRAM0_EDAC_ADR           at 16#10C# range 0 .. 31;
      ESRAM1_EDAC_ADR           at 16#110# range 0 .. 31;
      MAC_EDAC_RX_ADR           at 16#114# range 0 .. 31;
      MAC_EDAC_TX_ADR           at 16#118# range 0 .. 31;
      CAN_EDAC_ADR              at 16#11C# range 0 .. 31;
      USB_EDAC_ADR              at 16#120# range 0 .. 31;
      MM0_1_2_SECURITY          at 16#124# range 0 .. 31;
      MM4_5_FIC64_SECURITY      at 16#128# range 0 .. 31;
      MM3_6_7_8_SECURITY        at 16#12C# range 0 .. 31;
      MM9_SECURITY              at 16#130# range 0 .. 31;
      M3_SR                     at 16#134# range 0 .. 31;
      ETM_COUNT_LOW             at 16#138# range 0 .. 31;
      ETM_COUNT_HIGH            at 16#13C# range 0 .. 31;
      DEVICE_SR                 at 16#140# range 0 .. 31;
      ENVM_PROTECT_USER         at 16#144# range 0 .. 31;
      G4C_ENVM_STATUS           at 16#148# range 0 .. 31;
      DEVICE_VERSION            at 16#14C# range 0 .. 31;
      MSSDDR_PLL_STATUS         at 16#150# range 0 .. 31;
      USB_SR                    at 16#154# range 0 .. 31;
      ENVM_SR                   at 16#158# range 0 .. 31;
      SPARE_IN                  at 16#15C# range 0 .. 31;
      DDRB_STATUS               at 16#160# range 0 .. 31;
      MDDR_IO_CALIB_STATUS      at 16#164# range 0 .. 31;
      MSSDDR_CLK_CALIB_STATUS   at 16#168# range 0 .. 31;
      WDOGLOAD                  at 16#16C# range 0 .. 31;
      WDOGMVRP                  at 16#170# range 0 .. 31;
      USERCONFIG0               at 16#174# range 0 .. 31;
      USERCONFIG1               at 16#178# range 0 .. 31;
      USERCONFIG2               at 16#17C# range 0 .. 31;
      USERCONFIG3               at 16#180# range 0 .. 31;
      FAB_PROT_SIZE             at 16#184# range 0 .. 31;
      FAB_PROT_BASE             at 16#188# range 0 .. 31;
      MSS_GPIO_DEF              at 16#18C# range 0 .. 31;
      EDAC_SR                   at 16#190# range 0 .. 31;
      MSS_INTERNAL_SR           at 16#194# range 0 .. 31;
      MSS_EXTERNAL_SR           at 16#198# range 0 .. 31;
      WDOGTIMEOUTEVENT          at 16#19C# range 0 .. 31;
      CLR_MSS_COUNTERS          at 16#1A0# range 0 .. 31;
      CLR_EDAC_COUNTERS         at 16#1A4# range 0 .. 31;
      FLUSH_CR                  at 16#1A8# range 0 .. 31;
      MAC_STAT_CLR_CR           at 16#1AC# range 0 .. 31;
      IOMUXCELL_0_CONFIG        at 16#1B0# range 0 .. 31;
      IOMUXCELL_1_CONFIG        at 16#1B4# range 0 .. 31;
      IOMUXCELL_2_CONFIG        at 16#1B8# range 0 .. 31;
      IOMUXCELL_3_CONFIG        at 16#1BC# range 0 .. 31;
      IOMUXCELL_4_CONFIG        at 16#1C0# range 0 .. 31;
      IOMUXCELL_5_CONFIG        at 16#1C4# range 0 .. 31;
      IOMUXCELL_6_CONFIG        at 16#1C8# range 0 .. 31;
      IOMUXCELL_7_CONFIG        at 16#1CC# range 0 .. 31;
      IOMUXCELL_8_CONFIG        at 16#1D0# range 0 .. 31;
      IOMUXCELL_9_CONFIG        at 16#1D4# range 0 .. 31;
      IOMUXCELL_10_CONFIG       at 16#1D8# range 0 .. 31;
      IOMUXCELL_11_CONFIG       at 16#1DC# range 0 .. 31;
      IOMUXCELL_12_CONFIG       at 16#1E0# range 0 .. 31;
      IOMUXCELL_13_CONFIG       at 16#1E4# range 0 .. 31;
      IOMUXCELL_14_CONFIG       at 16#1E8# range 0 .. 31;
      IOMUXCELL_15_CONFIG       at 16#1EC# range 0 .. 31;
      IOMUXCELL_16_CONFIG       at 16#1F0# range 0 .. 31;
      IOMUXCELL_17_CONFIG       at 16#1F4# range 0 .. 31;
      IOMUXCELL_18_CONFIG       at 16#1F8# range 0 .. 31;
      IOMUXCELL_19_CONFIG       at 16#1FC# range 0 .. 31;
      IOMUXCELL_20_CONFIG       at 16#200# range 0 .. 31;
      IOMUXCELL_21_CONFIG       at 16#204# range 0 .. 31;
      IOMUXCELL_22_CONFIG       at 16#208# range 0 .. 31;
      IOMUXCELL_23_CONFIG       at 16#20C# range 0 .. 31;
      IOMUXCELL_24_CONFIG       at 16#210# range 0 .. 31;
      IOMUXCELL_25_CONFIG       at 16#214# range 0 .. 31;
      IOMUXCELL_26_CONFIG       at 16#218# range 0 .. 31;
      IOMUXCELL_27_CONFIG       at 16#21C# range 0 .. 31;
      IOMUXCELL_28_CONFIG       at 16#220# range 0 .. 31;
      IOMUXCELL_29_CONFIG       at 16#224# range 0 .. 31;
      IOMUXCELL_30_CONFIG       at 16#228# range 0 .. 31;
      IOMUXCELL_31_CONFIG       at 16#22C# range 0 .. 31;
      IOMUXCELL_32_CONFIG       at 16#230# range 0 .. 31;
      IOMUXCELL_33_CONFIG       at 16#234# range 0 .. 31;
      IOMUXCELL_34_CONFIG       at 16#238# range 0 .. 31;
      IOMUXCELL_35_CONFIG       at 16#23C# range 0 .. 31;
      IOMUXCELL_36_CONFIG       at 16#240# range 0 .. 31;
      IOMUXCELL_37_CONFIG       at 16#244# range 0 .. 31;
      IOMUXCELL_38_CONFIG       at 16#248# range 0 .. 31;
      IOMUXCELL_39_CONFIG       at 16#24C# range 0 .. 31;
      IOMUXCELL_40_CONFIG       at 16#250# range 0 .. 31;
      IOMUXCELL_41_CONFIG       at 16#254# range 0 .. 31;
      IOMUXCELL_42_CONFIG       at 16#258# range 0 .. 31;
      IOMUXCELL_43_CONFIG       at 16#25C# range 0 .. 31;
      IOMUXCELL_44_CONFIG       at 16#260# range 0 .. 31;
      IOMUXCELL_45_CONFIG       at 16#264# range 0 .. 31;
      IOMUXCELL_46_CONFIG       at 16#268# range 0 .. 31;
      IOMUXCELL_47_CONFIG       at 16#26C# range 0 .. 31;
      IOMUXCELL_48_CONFIG       at 16#270# range 0 .. 31;
      IOMUXCELL_49_CONFIG       at 16#274# range 0 .. 31;
      IOMUXCELL_50_CONFIG       at 16#278# range 0 .. 31;
      IOMUXCELL_51_CONFIG       at 16#27C# range 0 .. 31;
      IOMUXCELL_52_CONFIG       at 16#280# range 0 .. 31;
      IOMUXCELL_53_CONFIG       at 16#284# range 0 .. 31;
      IOMUXCELL_54_CONFIG       at 16#288# range 0 .. 31;
      IOMUXCELL_55_CONFIG       at 16#28C# range 0 .. 31;
      IOMUXCELL_56_CONFIG       at 16#290# range 0 .. 31;
      NVM_PROTECT_FACTORY       at 16#294# range 0 .. 31;
      DEVICE_STATUS_FIXED       at 16#298# range 0 .. 31;
      MBIST_ES0                 at 16#29C# range 0 .. 31;
      MBIST_ES1                 at 16#2A0# range 0 .. 31;
      MSDDR_PLL_STAUS_1         at 16#2A4# range 0 .. 31;
      REDUNDANCY_ESRAM0         at 16#2A8# range 0 .. 31;
      REDUNDANCY_ESRAM1         at 16#2AC# range 0 .. 31;
      SERDESIF                  at 16#2B0# range 0 .. 31;
   end record;

   --  No description provided for this peripheral
   System_Registers_Periph : aliased System_Registers_Peripheral
     with Import, Address => System_Registers_Base;

end Interfaces.SF2.System_Registers;
