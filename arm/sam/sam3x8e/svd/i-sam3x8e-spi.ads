--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAM3X8E.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.SAM3x8e.SPI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype SPI0_CR_SPIEN_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CR_SPIDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CR_SWRST_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CR_LASTXFER_Field is Interfaces.SAM3x8e.Bit;

   --  Control Register
   type SPI0_CR_Register is record
      --  Write-only. SPI Enable
      SPIEN          : SPI0_CR_SPIEN_Field := 16#0#;
      --  Write-only. SPI Disable
      SPIDIS         : SPI0_CR_SPIDIS_Field := 16#0#;
      --  unspecified
      Reserved_2_6   : Interfaces.SAM3x8e.UInt5 := 16#0#;
      --  Write-only. SPI Software Reset
      SWRST          : SPI0_CR_SWRST_Field := 16#0#;
      --  unspecified
      Reserved_8_23  : Interfaces.SAM3x8e.UInt16 := 16#0#;
      --  Write-only. Last Transfer
      LASTXFER       : SPI0_CR_LASTXFER_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : Interfaces.SAM3x8e.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_CR_Register use record
      SPIEN          at 0 range 0 .. 0;
      SPIDIS         at 0 range 1 .. 1;
      Reserved_2_6   at 0 range 2 .. 6;
      SWRST          at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      LASTXFER       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype SPI0_MR_MSTR_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_PS_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_PCSDEC_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_MODFDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_WDRBT_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_LLB_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_MR_PCS_Field is Interfaces.SAM3x8e.UInt4;
   subtype SPI0_MR_DLYBCS_Field is Interfaces.SAM3x8e.Byte;

   --  Mode Register
   type SPI0_MR_Register is record
      --  Master/Slave Mode
      MSTR           : SPI0_MR_MSTR_Field := 16#0#;
      --  Peripheral Select
      PS             : SPI0_MR_PS_Field := 16#0#;
      --  Chip Select Decode
      PCSDEC         : SPI0_MR_PCSDEC_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Mode Fault Detection
      MODFDIS        : SPI0_MR_MODFDIS_Field := 16#0#;
      --  Wait Data Read Before Transfer
      WDRBT          : SPI0_MR_WDRBT_Field := 16#0#;
      --  unspecified
      Reserved_6_6   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Local Loopback Enable
      LLB            : SPI0_MR_LLB_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : Interfaces.SAM3x8e.Byte := 16#0#;
      --  Peripheral Chip Select
      PCS            : SPI0_MR_PCS_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Delay Between Chip Selects
      DLYBCS         : SPI0_MR_DLYBCS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_MR_Register use record
      MSTR           at 0 range 0 .. 0;
      PS             at 0 range 1 .. 1;
      PCSDEC         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      MODFDIS        at 0 range 4 .. 4;
      WDRBT          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      LLB            at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PCS            at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      DLYBCS         at 0 range 24 .. 31;
   end record;

   subtype SPI0_RDR_RD_Field is Interfaces.SAM3x8e.UInt16;
   subtype SPI0_RDR_PCS_Field is Interfaces.SAM3x8e.UInt4;

   --  Receive Data Register
   type SPI0_RDR_Register is record
      --  Read-only. Receive Data
      RD             : SPI0_RDR_RD_Field;
      --  Read-only. Peripheral Chip Select
      PCS            : SPI0_RDR_PCS_Field;
      --  unspecified
      Reserved_20_31 : Interfaces.SAM3x8e.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_RDR_Register use record
      RD             at 0 range 0 .. 15;
      PCS            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype SPI0_TDR_TD_Field is Interfaces.SAM3x8e.UInt16;
   subtype SPI0_TDR_PCS_Field is Interfaces.SAM3x8e.UInt4;
   subtype SPI0_TDR_LASTXFER_Field is Interfaces.SAM3x8e.Bit;

   --  Transmit Data Register
   type SPI0_TDR_Register is record
      --  Write-only. Transmit Data
      TD             : SPI0_TDR_TD_Field := 16#0#;
      --  Write-only. Peripheral Chip Select
      PCS            : SPI0_TDR_PCS_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Last Transfer
      LASTXFER       : SPI0_TDR_LASTXFER_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : Interfaces.SAM3x8e.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_TDR_Register use record
      TD             at 0 range 0 .. 15;
      PCS            at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      LASTXFER       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype SPI0_SR_RDRF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_TDRE_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_MODF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_OVRES_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_NSSR_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_TXEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_UNDES_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_SR_SPIENS_Field is Interfaces.SAM3x8e.Bit;

   --  Status Register
   type SPI0_SR_Register is record
      --  Read-only. Receive Data Register Full
      RDRF           : SPI0_SR_RDRF_Field;
      --  Read-only. Transmit Data Register Empty
      TDRE           : SPI0_SR_TDRE_Field;
      --  Read-only. Mode Fault Error
      MODF           : SPI0_SR_MODF_Field;
      --  Read-only. Overrun Error Status
      OVRES          : SPI0_SR_OVRES_Field;
      --  unspecified
      Reserved_4_7   : Interfaces.SAM3x8e.UInt4;
      --  Read-only. NSS Rising
      NSSR           : SPI0_SR_NSSR_Field;
      --  Read-only. Transmission Registers Empty
      TXEMPTY        : SPI0_SR_TXEMPTY_Field;
      --  Read-only. Underrun Error Status (Slave Mode Only)
      UNDES          : SPI0_SR_UNDES_Field;
      --  unspecified
      Reserved_11_15 : Interfaces.SAM3x8e.UInt5;
      --  Read-only. SPI Enable Status
      SPIENS         : SPI0_SR_SPIENS_Field;
      --  unspecified
      Reserved_17_31 : Interfaces.SAM3x8e.UInt15;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_SR_Register use record
      RDRF           at 0 range 0 .. 0;
      TDRE           at 0 range 1 .. 1;
      MODF           at 0 range 2 .. 2;
      OVRES          at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      NSSR           at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      UNDES          at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SPIENS         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SPI0_IER_RDRF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_TDRE_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_MODF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_OVRES_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_NSSR_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_TXEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IER_UNDES_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Enable Register
   type SPI0_IER_Register is record
      --  Write-only. Receive Data Register Full Interrupt Enable
      RDRF           : SPI0_IER_RDRF_Field := 16#0#;
      --  Write-only. SPI Transmit Data Register Empty Interrupt Enable
      TDRE           : SPI0_IER_TDRE_Field := 16#0#;
      --  Write-only. Mode Fault Error Interrupt Enable
      MODF           : SPI0_IER_MODF_Field := 16#0#;
      --  Write-only. Overrun Error Interrupt Enable
      OVRES          : SPI0_IER_OVRES_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. NSS Rising Interrupt Enable
      NSSR           : SPI0_IER_NSSR_Field := 16#0#;
      --  Write-only. Transmission Registers Empty Enable
      TXEMPTY        : SPI0_IER_TXEMPTY_Field := 16#0#;
      --  Write-only. Underrun Error Interrupt Enable
      UNDES          : SPI0_IER_UNDES_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : Interfaces.SAM3x8e.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_IER_Register use record
      RDRF           at 0 range 0 .. 0;
      TDRE           at 0 range 1 .. 1;
      MODF           at 0 range 2 .. 2;
      OVRES          at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      NSSR           at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      UNDES          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype SPI0_IDR_RDRF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_TDRE_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_MODF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_OVRES_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_NSSR_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_TXEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IDR_UNDES_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Disable Register
   type SPI0_IDR_Register is record
      --  Write-only. Receive Data Register Full Interrupt Disable
      RDRF           : SPI0_IDR_RDRF_Field := 16#0#;
      --  Write-only. SPI Transmit Data Register Empty Interrupt Disable
      TDRE           : SPI0_IDR_TDRE_Field := 16#0#;
      --  Write-only. Mode Fault Error Interrupt Disable
      MODF           : SPI0_IDR_MODF_Field := 16#0#;
      --  Write-only. Overrun Error Interrupt Disable
      OVRES          : SPI0_IDR_OVRES_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. NSS Rising Interrupt Disable
      NSSR           : SPI0_IDR_NSSR_Field := 16#0#;
      --  Write-only. Transmission Registers Empty Disable
      TXEMPTY        : SPI0_IDR_TXEMPTY_Field := 16#0#;
      --  Write-only. Underrun Error Interrupt Disable
      UNDES          : SPI0_IDR_UNDES_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : Interfaces.SAM3x8e.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_IDR_Register use record
      RDRF           at 0 range 0 .. 0;
      TDRE           at 0 range 1 .. 1;
      MODF           at 0 range 2 .. 2;
      OVRES          at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      NSSR           at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      UNDES          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype SPI0_IMR_RDRF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_TDRE_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_MODF_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_OVRES_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_NSSR_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_TXEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_IMR_UNDES_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Mask Register
   type SPI0_IMR_Register is record
      --  Read-only. Receive Data Register Full Interrupt Mask
      RDRF           : SPI0_IMR_RDRF_Field;
      --  Read-only. SPI Transmit Data Register Empty Interrupt Mask
      TDRE           : SPI0_IMR_TDRE_Field;
      --  Read-only. Mode Fault Error Interrupt Mask
      MODF           : SPI0_IMR_MODF_Field;
      --  Read-only. Overrun Error Interrupt Mask
      OVRES          : SPI0_IMR_OVRES_Field;
      --  unspecified
      Reserved_4_7   : Interfaces.SAM3x8e.UInt4;
      --  Read-only. NSS Rising Interrupt Mask
      NSSR           : SPI0_IMR_NSSR_Field;
      --  Read-only. Transmission Registers Empty Mask
      TXEMPTY        : SPI0_IMR_TXEMPTY_Field;
      --  Read-only. Underrun Error Interrupt Mask
      UNDES          : SPI0_IMR_UNDES_Field;
      --  unspecified
      Reserved_11_31 : Interfaces.SAM3x8e.UInt21;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_IMR_Register use record
      RDRF           at 0 range 0 .. 0;
      TDRE           at 0 range 1 .. 1;
      MODF           at 0 range 2 .. 2;
      OVRES          at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      NSSR           at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      UNDES          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype SPI0_CSR_CPOL_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CSR_NCPHA_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CSR_CSNAAT_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_CSR_CSAAT_Field is Interfaces.SAM3x8e.Bit;

   --  Bits Per Transfer
   type CSR_BITS_Field is
     (--  8 bits for transfer
      Val_8_Bit,
      --  9 bits for transfer
      Val_9_Bit,
      --  10 bits for transfer
      Val_10_Bit,
      --  11 bits for transfer
      Val_11_Bit,
      --  12 bits for transfer
      Val_12_Bit,
      --  13 bits for transfer
      Val_13_Bit,
      --  14 bits for transfer
      Val_14_Bit,
      --  15 bits for transfer
      Val_15_Bit,
      --  16 bits for transfer
      Val_16_Bit)
     with Size => 4;
   for CSR_BITS_Field use
     (Val_8_Bit => 0,
      Val_9_Bit => 1,
      Val_10_Bit => 2,
      Val_11_Bit => 3,
      Val_12_Bit => 4,
      Val_13_Bit => 5,
      Val_14_Bit => 6,
      Val_15_Bit => 7,
      Val_16_Bit => 8);

   subtype SPI0_CSR_SCBR_Field is Interfaces.SAM3x8e.Byte;
   subtype SPI0_CSR_DLYBS_Field is Interfaces.SAM3x8e.Byte;
   subtype SPI0_CSR_DLYBCT_Field is Interfaces.SAM3x8e.Byte;

   --  Chip Select Register
   type SPI0_CSR_Register is record
      --  Clock Polarity
      CPOL   : SPI0_CSR_CPOL_Field := 16#0#;
      --  Clock Phase
      NCPHA  : SPI0_CSR_NCPHA_Field := 16#0#;
      --  Chip Select Not Active After Transfer (Ignored if CSAAT = 1)
      CSNAAT : SPI0_CSR_CSNAAT_Field := 16#0#;
      --  Chip Select Active After Transfer
      CSAAT  : SPI0_CSR_CSAAT_Field := 16#0#;
      --  Bits Per Transfer
      BITS   : CSR_BITS_Field := Interfaces.SAM3x8e.SPI.Val_8_Bit;
      --  Serial Clock Baud Rate
      SCBR   : SPI0_CSR_SCBR_Field := 16#0#;
      --  Delay Before SPCK
      DLYBS  : SPI0_CSR_DLYBS_Field := 16#0#;
      --  Delay Between Consecutive Transfers
      DLYBCT : SPI0_CSR_DLYBCT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_CSR_Register use record
      CPOL   at 0 range 0 .. 0;
      NCPHA  at 0 range 1 .. 1;
      CSNAAT at 0 range 2 .. 2;
      CSAAT  at 0 range 3 .. 3;
      BITS   at 0 range 4 .. 7;
      SCBR   at 0 range 8 .. 15;
      DLYBS  at 0 range 16 .. 23;
      DLYBCT at 0 range 24 .. 31;
   end record;

   --  Chip Select Register
   type SPI0_CSR_Registers is array (0 .. 3) of SPI0_CSR_Register;

   subtype SPI0_WPMR_WPEN_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_WPMR_WPKEY_Field is Interfaces.SAM3x8e.UInt24;

   --  Write Protection Control Register
   type SPI0_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : SPI0_WPMR_WPEN_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Write Protection Key Password
      WPKEY        : SPI0_WPMR_WPKEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype SPI0_WPSR_WPVS_Field is Interfaces.SAM3x8e.Bit;
   subtype SPI0_WPSR_WPVSRC_Field is Interfaces.SAM3x8e.Byte;

   --  Write Protection Status Register
   type SPI0_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : SPI0_WPSR_WPVS_Field;
      --  unspecified
      Reserved_1_7   : Interfaces.SAM3x8e.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : SPI0_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM3x8e.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SPI0_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial Peripheral Interface 0
   type SPI0_Peripheral is record
      --  Control Register
      CR   : aliased SPI0_CR_Register;
      --  Mode Register
      MR   : aliased SPI0_MR_Register;
      --  Receive Data Register
      RDR  : aliased SPI0_RDR_Register;
      --  Transmit Data Register
      TDR  : aliased SPI0_TDR_Register;
      --  Status Register
      SR   : aliased SPI0_SR_Register;
      --  Interrupt Enable Register
      IER  : aliased SPI0_IER_Register;
      --  Interrupt Disable Register
      IDR  : aliased SPI0_IDR_Register;
      --  Interrupt Mask Register
      IMR  : aliased SPI0_IMR_Register;
      --  Chip Select Register
      CSR  : aliased SPI0_CSR_Registers;
      --  Write Protection Control Register
      WPMR : aliased SPI0_WPMR_Register;
      --  Write Protection Status Register
      WPSR : aliased SPI0_WPSR_Register;
   end record
     with Volatile;

   for SPI0_Peripheral use record
      CR   at 16#0# range 0 .. 31;
      MR   at 16#4# range 0 .. 31;
      RDR  at 16#8# range 0 .. 31;
      TDR  at 16#C# range 0 .. 31;
      SR   at 16#10# range 0 .. 31;
      IER  at 16#14# range 0 .. 31;
      IDR  at 16#18# range 0 .. 31;
      IMR  at 16#1C# range 0 .. 31;
      CSR  at 16#30# range 0 .. 127;
      WPMR at 16#E4# range 0 .. 31;
      WPSR at 16#E8# range 0 .. 31;
   end record;

   --  Serial Peripheral Interface 0
   SPI0_Periph : aliased SPI0_Peripheral
     with Import, Address => SPI0_Base;

end Interfaces.SAM3x8e.SPI;
