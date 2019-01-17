--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAM3X8E.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  USB On-The-Go Interface
package Interfaces.SAM3x8e.UOTGHS is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype UOTGHS_DEVCTRL_UADD_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_DEVCTRL_ADDEN_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_DETACH_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_RMWKUP_Field is Interfaces.SAM3x8e.Bit;

   --  Mode Configuration
   type DEVCTRL_SPDCONF_Field is
     (--  The peripheral starts in full-speed mode and performs a high-speed reset to
--  switch to the high-speed mode if the host is high-speed capable.
      Normal,
      --  For a better consumption, if high-speed is not needed.
      Low_Power,
      --  Forced high speed.
      High_Speed,
      --  The peripheral remains in full-speed mode whatever the host speed
--  capability.
      Forced_Fs)
     with Size => 2;
   for DEVCTRL_SPDCONF_Field use
     (Normal => 0,
      Low_Power => 1,
      High_Speed => 2,
      Forced_Fs => 3);

   subtype UOTGHS_DEVCTRL_LS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_TSTJ_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_TSTK_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_TSTPCKT_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVCTRL_OPMODE2_Field is Interfaces.SAM3x8e.Bit;

   --  Device General Control Register
   type UOTGHS_DEVCTRL_Register is record
      --  USB Address
      UADD           : UOTGHS_DEVCTRL_UADD_Field := 16#0#;
      --  Address Enable
      ADDEN          : UOTGHS_DEVCTRL_ADDEN_Field := 16#0#;
      --  Detach
      DETACH         : UOTGHS_DEVCTRL_DETACH_Field := 16#1#;
      --  Remote Wake-Up
      RMWKUP         : UOTGHS_DEVCTRL_RMWKUP_Field := 16#0#;
      --  Mode Configuration
      SPDCONF        : DEVCTRL_SPDCONF_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Normal;
      --  Low-Speed Mode Force
      LS             : UOTGHS_DEVCTRL_LS_Field := 16#0#;
      --  Test mode J
      TSTJ           : UOTGHS_DEVCTRL_TSTJ_Field := 16#0#;
      --  Test mode K
      TSTK           : UOTGHS_DEVCTRL_TSTK_Field := 16#0#;
      --  Test packet mode
      TSTPCKT        : UOTGHS_DEVCTRL_TSTPCKT_Field := 16#0#;
      --  Specific Operational mode
      OPMODE2        : UOTGHS_DEVCTRL_OPMODE2_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : Interfaces.SAM3x8e.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVCTRL_Register use record
      UADD           at 0 range 0 .. 6;
      ADDEN          at 0 range 7 .. 7;
      DETACH         at 0 range 8 .. 8;
      RMWKUP         at 0 range 9 .. 9;
      SPDCONF        at 0 range 10 .. 11;
      LS             at 0 range 12 .. 12;
      TSTJ           at 0 range 13 .. 13;
      TSTK           at 0 range 14 .. 14;
      TSTPCKT        at 0 range 15 .. 15;
      OPMODE2        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype UOTGHS_DEVISR_SUSP_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_MSOF_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_SOF_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_EORST_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_WAKEUP_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_EORSM_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_UPRSM_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVISR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Status Register
   type UOTGHS_DEVISR_Register is record
      --  Read-only. Suspend Interrupt
      SUSP           : UOTGHS_DEVISR_SUSP_Field;
      --  Read-only. Micro Start of Frame Interrupt
      MSOF           : UOTGHS_DEVISR_MSOF_Field;
      --  Read-only. Start of Frame Interrupt
      SOF            : UOTGHS_DEVISR_SOF_Field;
      --  Read-only. End of Reset Interrupt
      EORST          : UOTGHS_DEVISR_EORST_Field;
      --  Read-only. Wake-Up Interrupt
      WAKEUP         : UOTGHS_DEVISR_WAKEUP_Field;
      --  Read-only. End of Resume Interrupt
      EORSM          : UOTGHS_DEVISR_EORSM_Field;
      --  Read-only. Upstream Resume Interrupt
      UPRSM          : UOTGHS_DEVISR_UPRSM_Field;
      --  unspecified
      Reserved_7_11  : Interfaces.SAM3x8e.UInt5;
      --  Read-only. Endpoint 0 Interrupt
      PEP_0          : UOTGHS_DEVISR_PEP_0_Field;
      --  Read-only. Endpoint 1 Interrupt
      PEP_1          : UOTGHS_DEVISR_PEP_1_Field;
      --  Read-only. Endpoint 2 Interrupt
      PEP_2          : UOTGHS_DEVISR_PEP_2_Field;
      --  Read-only. Endpoint 3 Interrupt
      PEP_3          : UOTGHS_DEVISR_PEP_3_Field;
      --  Read-only. Endpoint 4 Interrupt
      PEP_4          : UOTGHS_DEVISR_PEP_4_Field;
      --  Read-only. Endpoint 5 Interrupt
      PEP_5          : UOTGHS_DEVISR_PEP_5_Field;
      --  Read-only. Endpoint 6 Interrupt
      PEP_6          : UOTGHS_DEVISR_PEP_6_Field;
      --  Read-only. Endpoint 7 Interrupt
      PEP_7          : UOTGHS_DEVISR_PEP_7_Field;
      --  Read-only. Endpoint 8 Interrupt
      PEP_8          : UOTGHS_DEVISR_PEP_8_Field;
      --  Read-only. Endpoint 9 Interrupt
      PEP_9          : UOTGHS_DEVISR_PEP_9_Field;
      --  unspecified
      Reserved_22_24 : Interfaces.SAM3x8e.UInt3;
      --  Read-only. DMA Channel 1 Interrupt
      DMA_1          : UOTGHS_DEVISR_DMA_1_Field;
      --  Read-only. DMA Channel 2 Interrupt
      DMA_2          : UOTGHS_DEVISR_DMA_2_Field;
      --  Read-only. DMA Channel 3 Interrupt
      DMA_3          : UOTGHS_DEVISR_DMA_3_Field;
      --  Read-only. DMA Channel 4 Interrupt
      DMA_4          : UOTGHS_DEVISR_DMA_4_Field;
      --  Read-only. DMA Channel 5 Interrupt
      DMA_5          : UOTGHS_DEVISR_DMA_5_Field;
      --  Read-only. DMA Channel 6 Interrupt
      DMA_6          : UOTGHS_DEVISR_DMA_6_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVISR_Register use record
      SUSP           at 0 range 0 .. 0;
      MSOF           at 0 range 1 .. 1;
      SOF            at 0 range 2 .. 2;
      EORST          at 0 range 3 .. 3;
      WAKEUP         at 0 range 4 .. 4;
      EORSM          at 0 range 5 .. 5;
      UPRSM          at 0 range 6 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      PEP_0          at 0 range 12 .. 12;
      PEP_1          at 0 range 13 .. 13;
      PEP_2          at 0 range 14 .. 14;
      PEP_3          at 0 range 15 .. 15;
      PEP_4          at 0 range 16 .. 16;
      PEP_5          at 0 range 17 .. 17;
      PEP_6          at 0 range 18 .. 18;
      PEP_7          at 0 range 19 .. 19;
      PEP_8          at 0 range 20 .. 20;
      PEP_9          at 0 range 21 .. 21;
      Reserved_22_24 at 0 range 22 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_DEVICR_SUSPC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_MSOFC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_SOFC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_EORSTC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_WAKEUPC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_EORSMC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVICR_UPRSMC_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Clear Register
   type UOTGHS_DEVICR_Register is record
      --  Write-only. Suspend Interrupt Clear
      SUSPC         : UOTGHS_DEVICR_SUSPC_Field := 16#0#;
      --  Write-only. Micro Start of Frame Interrupt Clear
      MSOFC         : UOTGHS_DEVICR_MSOFC_Field := 16#0#;
      --  Write-only. Start of Frame Interrupt Clear
      SOFC          : UOTGHS_DEVICR_SOFC_Field := 16#0#;
      --  Write-only. End of Reset Interrupt Clear
      EORSTC        : UOTGHS_DEVICR_EORSTC_Field := 16#0#;
      --  Write-only. Wake-Up Interrupt Clear
      WAKEUPC       : UOTGHS_DEVICR_WAKEUPC_Field := 16#0#;
      --  Write-only. End of Resume Interrupt Clear
      EORSMC        : UOTGHS_DEVICR_EORSMC_Field := 16#0#;
      --  Write-only. Upstream Resume Interrupt Clear
      UPRSMC        : UOTGHS_DEVICR_UPRSMC_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : Interfaces.SAM3x8e.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVICR_Register use record
      SUSPC         at 0 range 0 .. 0;
      MSOFC         at 0 range 1 .. 1;
      SOFC          at 0 range 2 .. 2;
      EORSTC        at 0 range 3 .. 3;
      WAKEUPC       at 0 range 4 .. 4;
      EORSMC        at 0 range 5 .. 5;
      UPRSMC        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype UOTGHS_DEVIFR_SUSPS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_MSOFS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_SOFS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_EORSTS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_WAKEUPS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_EORSMS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_UPRSMS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIFR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Set Register
   type UOTGHS_DEVIFR_Register is record
      --  Write-only. Suspend Interrupt Set
      SUSPS          : UOTGHS_DEVIFR_SUSPS_Field := 16#0#;
      --  Write-only. Micro Start of Frame Interrupt Set
      MSOFS          : UOTGHS_DEVIFR_MSOFS_Field := 16#0#;
      --  Write-only. Start of Frame Interrupt Set
      SOFS           : UOTGHS_DEVIFR_SOFS_Field := 16#0#;
      --  Write-only. End of Reset Interrupt Set
      EORSTS         : UOTGHS_DEVIFR_EORSTS_Field := 16#0#;
      --  Write-only. Wake-Up Interrupt Set
      WAKEUPS        : UOTGHS_DEVIFR_WAKEUPS_Field := 16#0#;
      --  Write-only. End of Resume Interrupt Set
      EORSMS         : UOTGHS_DEVIFR_EORSMS_Field := 16#0#;
      --  Write-only. Upstream Resume Interrupt Set
      UPRSMS         : UOTGHS_DEVIFR_UPRSMS_Field := 16#0#;
      --  unspecified
      Reserved_7_24  : Interfaces.SAM3x8e.UInt18 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Set
      DMA_1          : UOTGHS_DEVIFR_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Set
      DMA_2          : UOTGHS_DEVIFR_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Set
      DMA_3          : UOTGHS_DEVIFR_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Set
      DMA_4          : UOTGHS_DEVIFR_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Set
      DMA_5          : UOTGHS_DEVIFR_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Set
      DMA_6          : UOTGHS_DEVIFR_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVIFR_Register use record
      SUSPS          at 0 range 0 .. 0;
      MSOFS          at 0 range 1 .. 1;
      SOFS           at 0 range 2 .. 2;
      EORSTS         at 0 range 3 .. 3;
      WAKEUPS        at 0 range 4 .. 4;
      EORSMS         at 0 range 5 .. 5;
      UPRSMS         at 0 range 6 .. 6;
      Reserved_7_24  at 0 range 7 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_DEVIMR_SUSPE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_MSOFE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_SOFE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_EORSTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_WAKEUPE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_EORSME_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_UPRSME_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIMR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Mask Register
   type UOTGHS_DEVIMR_Register is record
      --  Read-only. Suspend Interrupt Mask
      SUSPE          : UOTGHS_DEVIMR_SUSPE_Field;
      --  Read-only. Micro Start of Frame Interrupt Mask
      MSOFE          : UOTGHS_DEVIMR_MSOFE_Field;
      --  Read-only. Start of Frame Interrupt Mask
      SOFE           : UOTGHS_DEVIMR_SOFE_Field;
      --  Read-only. End of Reset Interrupt Mask
      EORSTE         : UOTGHS_DEVIMR_EORSTE_Field;
      --  Read-only. Wake-Up Interrupt Mask
      WAKEUPE        : UOTGHS_DEVIMR_WAKEUPE_Field;
      --  Read-only. End of Resume Interrupt Mask
      EORSME         : UOTGHS_DEVIMR_EORSME_Field;
      --  Read-only. Upstream Resume Interrupt Mask
      UPRSME         : UOTGHS_DEVIMR_UPRSME_Field;
      --  unspecified
      Reserved_7_11  : Interfaces.SAM3x8e.UInt5;
      --  Read-only. Endpoint 0 Interrupt Mask
      PEP_0          : UOTGHS_DEVIMR_PEP_0_Field;
      --  Read-only. Endpoint 1 Interrupt Mask
      PEP_1          : UOTGHS_DEVIMR_PEP_1_Field;
      --  Read-only. Endpoint 2 Interrupt Mask
      PEP_2          : UOTGHS_DEVIMR_PEP_2_Field;
      --  Read-only. Endpoint 3 Interrupt Mask
      PEP_3          : UOTGHS_DEVIMR_PEP_3_Field;
      --  Read-only. Endpoint 4 Interrupt Mask
      PEP_4          : UOTGHS_DEVIMR_PEP_4_Field;
      --  Read-only. Endpoint 5 Interrupt Mask
      PEP_5          : UOTGHS_DEVIMR_PEP_5_Field;
      --  Read-only. Endpoint 6 Interrupt Mask
      PEP_6          : UOTGHS_DEVIMR_PEP_6_Field;
      --  Read-only. Endpoint 7 Interrupt Mask
      PEP_7          : UOTGHS_DEVIMR_PEP_7_Field;
      --  Read-only. Endpoint 8 Interrupt Mask
      PEP_8          : UOTGHS_DEVIMR_PEP_8_Field;
      --  Read-only. Endpoint 9 Interrupt Mask
      PEP_9          : UOTGHS_DEVIMR_PEP_9_Field;
      --  unspecified
      Reserved_22_24 : Interfaces.SAM3x8e.UInt3;
      --  Read-only. DMA Channel 1 Interrupt Mask
      DMA_1          : UOTGHS_DEVIMR_DMA_1_Field;
      --  Read-only. DMA Channel 2 Interrupt Mask
      DMA_2          : UOTGHS_DEVIMR_DMA_2_Field;
      --  Read-only. DMA Channel 3 Interrupt Mask
      DMA_3          : UOTGHS_DEVIMR_DMA_3_Field;
      --  Read-only. DMA Channel 4 Interrupt Mask
      DMA_4          : UOTGHS_DEVIMR_DMA_4_Field;
      --  Read-only. DMA Channel 5 Interrupt Mask
      DMA_5          : UOTGHS_DEVIMR_DMA_5_Field;
      --  Read-only. DMA Channel 6 Interrupt Mask
      DMA_6          : UOTGHS_DEVIMR_DMA_6_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVIMR_Register use record
      SUSPE          at 0 range 0 .. 0;
      MSOFE          at 0 range 1 .. 1;
      SOFE           at 0 range 2 .. 2;
      EORSTE         at 0 range 3 .. 3;
      WAKEUPE        at 0 range 4 .. 4;
      EORSME         at 0 range 5 .. 5;
      UPRSME         at 0 range 6 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      PEP_0          at 0 range 12 .. 12;
      PEP_1          at 0 range 13 .. 13;
      PEP_2          at 0 range 14 .. 14;
      PEP_3          at 0 range 15 .. 15;
      PEP_4          at 0 range 16 .. 16;
      PEP_5          at 0 range 17 .. 17;
      PEP_6          at 0 range 18 .. 18;
      PEP_7          at 0 range 19 .. 19;
      PEP_8          at 0 range 20 .. 20;
      PEP_9          at 0 range 21 .. 21;
      Reserved_22_24 at 0 range 22 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_DEVIDR_SUSPEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_MSOFEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_SOFEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_EORSTEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_WAKEUPEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_EORSMEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_UPRSMEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIDR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Disable Register
   type UOTGHS_DEVIDR_Register is record
      --  Write-only. Suspend Interrupt Disable
      SUSPEC         : UOTGHS_DEVIDR_SUSPEC_Field := 16#0#;
      --  Write-only. Micro Start of Frame Interrupt Disable
      MSOFEC         : UOTGHS_DEVIDR_MSOFEC_Field := 16#0#;
      --  Write-only. Start of Frame Interrupt Disable
      SOFEC          : UOTGHS_DEVIDR_SOFEC_Field := 16#0#;
      --  Write-only. End of Reset Interrupt Disable
      EORSTEC        : UOTGHS_DEVIDR_EORSTEC_Field := 16#0#;
      --  Write-only. Wake-Up Interrupt Disable
      WAKEUPEC       : UOTGHS_DEVIDR_WAKEUPEC_Field := 16#0#;
      --  Write-only. End of Resume Interrupt Disable
      EORSMEC        : UOTGHS_DEVIDR_EORSMEC_Field := 16#0#;
      --  Write-only. Upstream Resume Interrupt Disable
      UPRSMEC        : UOTGHS_DEVIDR_UPRSMEC_Field := 16#0#;
      --  unspecified
      Reserved_7_11  : Interfaces.SAM3x8e.UInt5 := 16#0#;
      --  Write-only. Endpoint 0 Interrupt Disable
      PEP_0          : UOTGHS_DEVIDR_PEP_0_Field := 16#0#;
      --  Write-only. Endpoint 1 Interrupt Disable
      PEP_1          : UOTGHS_DEVIDR_PEP_1_Field := 16#0#;
      --  Write-only. Endpoint 2 Interrupt Disable
      PEP_2          : UOTGHS_DEVIDR_PEP_2_Field := 16#0#;
      --  Write-only. Endpoint 3 Interrupt Disable
      PEP_3          : UOTGHS_DEVIDR_PEP_3_Field := 16#0#;
      --  Write-only. Endpoint 4 Interrupt Disable
      PEP_4          : UOTGHS_DEVIDR_PEP_4_Field := 16#0#;
      --  Write-only. Endpoint 5 Interrupt Disable
      PEP_5          : UOTGHS_DEVIDR_PEP_5_Field := 16#0#;
      --  Write-only. Endpoint 6 Interrupt Disable
      PEP_6          : UOTGHS_DEVIDR_PEP_6_Field := 16#0#;
      --  Write-only. Endpoint 7 Interrupt Disable
      PEP_7          : UOTGHS_DEVIDR_PEP_7_Field := 16#0#;
      --  Write-only. Endpoint 8 Interrupt Disable
      PEP_8          : UOTGHS_DEVIDR_PEP_8_Field := 16#0#;
      --  Write-only. Endpoint 9 Interrupt Disable
      PEP_9          : UOTGHS_DEVIDR_PEP_9_Field := 16#0#;
      --  unspecified
      Reserved_22_24 : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Disable
      DMA_1          : UOTGHS_DEVIDR_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Disable
      DMA_2          : UOTGHS_DEVIDR_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Disable
      DMA_3          : UOTGHS_DEVIDR_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Disable
      DMA_4          : UOTGHS_DEVIDR_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Disable
      DMA_5          : UOTGHS_DEVIDR_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Disable
      DMA_6          : UOTGHS_DEVIDR_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVIDR_Register use record
      SUSPEC         at 0 range 0 .. 0;
      MSOFEC         at 0 range 1 .. 1;
      SOFEC          at 0 range 2 .. 2;
      EORSTEC        at 0 range 3 .. 3;
      WAKEUPEC       at 0 range 4 .. 4;
      EORSMEC        at 0 range 5 .. 5;
      UPRSMEC        at 0 range 6 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      PEP_0          at 0 range 12 .. 12;
      PEP_1          at 0 range 13 .. 13;
      PEP_2          at 0 range 14 .. 14;
      PEP_3          at 0 range 15 .. 15;
      PEP_4          at 0 range 16 .. 16;
      PEP_5          at 0 range 17 .. 17;
      PEP_6          at 0 range 18 .. 18;
      PEP_7          at 0 range 19 .. 19;
      PEP_8          at 0 range 20 .. 20;
      PEP_9          at 0 range 21 .. 21;
      Reserved_22_24 at 0 range 22 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_DEVIER_SUSPES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_MSOFES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_SOFES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_EORSTES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_WAKEUPES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_EORSMES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_UPRSMES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVIER_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Device Global Interrupt Enable Register
   type UOTGHS_DEVIER_Register is record
      --  Write-only. Suspend Interrupt Enable
      SUSPES         : UOTGHS_DEVIER_SUSPES_Field := 16#0#;
      --  Write-only. Micro Start of Frame Interrupt Enable
      MSOFES         : UOTGHS_DEVIER_MSOFES_Field := 16#0#;
      --  Write-only. Start of Frame Interrupt Enable
      SOFES          : UOTGHS_DEVIER_SOFES_Field := 16#0#;
      --  Write-only. End of Reset Interrupt Enable
      EORSTES        : UOTGHS_DEVIER_EORSTES_Field := 16#0#;
      --  Write-only. Wake-Up Interrupt Enable
      WAKEUPES       : UOTGHS_DEVIER_WAKEUPES_Field := 16#0#;
      --  Write-only. End of Resume Interrupt Enable
      EORSMES        : UOTGHS_DEVIER_EORSMES_Field := 16#0#;
      --  Write-only. Upstream Resume Interrupt Enable
      UPRSMES        : UOTGHS_DEVIER_UPRSMES_Field := 16#0#;
      --  unspecified
      Reserved_7_11  : Interfaces.SAM3x8e.UInt5 := 16#0#;
      --  Write-only. Endpoint 0 Interrupt Enable
      PEP_0          : UOTGHS_DEVIER_PEP_0_Field := 16#0#;
      --  Write-only. Endpoint 1 Interrupt Enable
      PEP_1          : UOTGHS_DEVIER_PEP_1_Field := 16#0#;
      --  Write-only. Endpoint 2 Interrupt Enable
      PEP_2          : UOTGHS_DEVIER_PEP_2_Field := 16#0#;
      --  Write-only. Endpoint 3 Interrupt Enable
      PEP_3          : UOTGHS_DEVIER_PEP_3_Field := 16#0#;
      --  Write-only. Endpoint 4 Interrupt Enable
      PEP_4          : UOTGHS_DEVIER_PEP_4_Field := 16#0#;
      --  Write-only. Endpoint 5 Interrupt Enable
      PEP_5          : UOTGHS_DEVIER_PEP_5_Field := 16#0#;
      --  Write-only. Endpoint 6 Interrupt Enable
      PEP_6          : UOTGHS_DEVIER_PEP_6_Field := 16#0#;
      --  Write-only. Endpoint 7 Interrupt Enable
      PEP_7          : UOTGHS_DEVIER_PEP_7_Field := 16#0#;
      --  Write-only. Endpoint 8 Interrupt Enable
      PEP_8          : UOTGHS_DEVIER_PEP_8_Field := 16#0#;
      --  Write-only. Endpoint 9 Interrupt Enable
      PEP_9          : UOTGHS_DEVIER_PEP_9_Field := 16#0#;
      --  unspecified
      Reserved_22_24 : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Enable
      DMA_1          : UOTGHS_DEVIER_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Enable
      DMA_2          : UOTGHS_DEVIER_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Enable
      DMA_3          : UOTGHS_DEVIER_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Enable
      DMA_4          : UOTGHS_DEVIER_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Enable
      DMA_5          : UOTGHS_DEVIER_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Enable
      DMA_6          : UOTGHS_DEVIER_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVIER_Register use record
      SUSPES         at 0 range 0 .. 0;
      MSOFES         at 0 range 1 .. 1;
      SOFES          at 0 range 2 .. 2;
      EORSTES        at 0 range 3 .. 3;
      WAKEUPES       at 0 range 4 .. 4;
      EORSMES        at 0 range 5 .. 5;
      UPRSMES        at 0 range 6 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      PEP_0          at 0 range 12 .. 12;
      PEP_1          at 0 range 13 .. 13;
      PEP_2          at 0 range 14 .. 14;
      PEP_3          at 0 range 15 .. 15;
      PEP_4          at 0 range 16 .. 16;
      PEP_5          at 0 range 17 .. 17;
      PEP_6          at 0 range 18 .. 18;
      PEP_7          at 0 range 19 .. 19;
      PEP_8          at 0 range 20 .. 20;
      PEP_9          at 0 range 21 .. 21;
      Reserved_22_24 at 0 range 22 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  UOTGHS_DEVEPT_EPEN array element
   subtype UOTGHS_DEVEPT_EPEN_Element is Interfaces.SAM3x8e.Bit;

   --  UOTGHS_DEVEPT_EPEN array
   type UOTGHS_DEVEPT_EPEN_Field_Array is array (0 .. 8)
     of UOTGHS_DEVEPT_EPEN_Element
     with Component_Size => 1, Size => 9;

   --  Type definition for UOTGHS_DEVEPT_EPEN
   type UOTGHS_DEVEPT_EPEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EPEN as a value
            Val : Interfaces.SAM3x8e.UInt9;
         when True =>
            --  EPEN as an array
            Arr : UOTGHS_DEVEPT_EPEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for UOTGHS_DEVEPT_EPEN_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  UOTGHS_DEVEPT_EPRST array element
   subtype UOTGHS_DEVEPT_EPRST_Element is Interfaces.SAM3x8e.Bit;

   --  UOTGHS_DEVEPT_EPRST array
   type UOTGHS_DEVEPT_EPRST_Field_Array is array (0 .. 8)
     of UOTGHS_DEVEPT_EPRST_Element
     with Component_Size => 1, Size => 9;

   --  Type definition for UOTGHS_DEVEPT_EPRST
   type UOTGHS_DEVEPT_EPRST_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EPRST as a value
            Val : Interfaces.SAM3x8e.UInt9;
         when True =>
            --  EPRST as an array
            Arr : UOTGHS_DEVEPT_EPRST_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for UOTGHS_DEVEPT_EPRST_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  Device Endpoint Register
   type UOTGHS_DEVEPT_Register is record
      --  Endpoint 0 Enable
      EPEN           : UOTGHS_DEVEPT_EPEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_9_15  : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Endpoint 0 Reset
      EPRST          : UOTGHS_DEVEPT_EPRST_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_25_31 : Interfaces.SAM3x8e.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPT_Register use record
      EPEN           at 0 range 0 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      EPRST          at 0 range 16 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype UOTGHS_DEVFNUM_MFNUM_Field is Interfaces.SAM3x8e.UInt3;
   subtype UOTGHS_DEVFNUM_FNUM_Field is Interfaces.SAM3x8e.UInt11;
   subtype UOTGHS_DEVFNUM_FNCERR_Field is Interfaces.SAM3x8e.Bit;

   --  Device Frame Number Register
   type UOTGHS_DEVFNUM_Register is record
      --  Read-only. Micro Frame Number
      MFNUM          : UOTGHS_DEVFNUM_MFNUM_Field;
      --  Read-only. Frame Number
      FNUM           : UOTGHS_DEVFNUM_FNUM_Field;
      --  unspecified
      Reserved_14_14 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Frame Number CRC Error
      FNCERR         : UOTGHS_DEVFNUM_FNCERR_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM3x8e.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVFNUM_Register use record
      MFNUM          at 0 range 0 .. 2;
      FNUM           at 0 range 3 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      FNCERR         at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UOTGHS_DEVEPTCFG_ALLOC_Field is Interfaces.SAM3x8e.Bit;

   --  Endpoint Banks
   type DEVEPTCFG_EPBK_Field is
     (--  Single-bank endpoint
      Val_1_Bank,
      --  Double-bank endpoint
      Val_2_Bank,
      --  Triple-bank endpoint
      Val_3_Bank)
     with Size => 2;
   for DEVEPTCFG_EPBK_Field use
     (Val_1_Bank => 0,
      Val_2_Bank => 1,
      Val_3_Bank => 2);

   --  Endpoint Size
   type DEVEPTCFG_EPSIZE_Field is
     (--  8 bytes
      Val_8_Byte,
      --  16 bytes
      Val_16_Byte,
      --  32 bytes
      Val_32_Byte,
      --  64 bytes
      Val_64_Byte,
      --  128 bytes
      Val_128_Byte,
      --  256 bytes
      Val_256_Byte,
      --  512 bytes
      Val_512_Byte,
      --  1024 bytes
      Val_1024_Byte)
     with Size => 3;
   for DEVEPTCFG_EPSIZE_Field use
     (Val_8_Byte => 0,
      Val_16_Byte => 1,
      Val_32_Byte => 2,
      Val_64_Byte => 3,
      Val_128_Byte => 4,
      Val_256_Byte => 5,
      Val_512_Byte => 6,
      Val_1024_Byte => 7);

   --  Endpoint Direction
   type DEVEPTCFG_EPDIR_Field is
     (--  The endpoint direction is OUT.
      Out_k,
      --  The endpoint direction is IN (nor for control endpoints).
      In_k)
     with Size => 1;
   for DEVEPTCFG_EPDIR_Field use
     (Out_k => 0,
      In_k => 1);

   subtype UOTGHS_DEVEPTCFG_AUTOSW_Field is Interfaces.SAM3x8e.Bit;

   --  Endpoint Type
   type DEVEPTCFG_EPTYPE_Field is
     (--  Control
      Ctrl,
      --  Isochronous
      Iso,
      --  Bulk
      Blk,
      --  Interrupt
      Intrpt)
     with Size => 2;
   for DEVEPTCFG_EPTYPE_Field use
     (Ctrl => 0,
      Iso => 1,
      Blk => 2,
      Intrpt => 3);

   --  Number of transaction per microframe for isochronous endpoint
   type DEVEPTCFG_NBTRANS_Field is
     (--  reserved to endpoint that does not have the high-bandwidth isochronous
--  capability.
      Val_0_Trans,
      --  default value: one transaction per micro-frame.
      Val_1_Trans,
      --  2 transactions per micro-frame. This endpoint should be configured as
--  double-bank.
      Val_2_Trans,
      --  3 transactions per micro-frame. This endpoint should be configured as
--  triple-bank.
      Val_3_Trans)
     with Size => 2;
   for DEVEPTCFG_NBTRANS_Field use
     (Val_0_Trans => 0,
      Val_1_Trans => 1,
      Val_2_Trans => 2,
      Val_3_Trans => 3);

   --  Device Endpoint Configuration Register (n = 0)
   type UOTGHS_DEVEPTCFG_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Endpoint Memory Allocate
      ALLOC          : UOTGHS_DEVEPTCFG_ALLOC_Field := 16#0#;
      --  Endpoint Banks
      EPBK           : DEVEPTCFG_EPBK_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Val_1_Bank;
      --  Endpoint Size
      EPSIZE         : DEVEPTCFG_EPSIZE_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Val_8_Byte;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Endpoint Direction
      EPDIR          : DEVEPTCFG_EPDIR_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Out_k;
      --  Automatic Switch
      AUTOSW         : UOTGHS_DEVEPTCFG_AUTOSW_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Endpoint Type
      EPTYPE         : DEVEPTCFG_EPTYPE_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Ctrl;
      --  Number of transaction per microframe for isochronous endpoint
      NBTRANS        : DEVEPTCFG_NBTRANS_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Val_0_Trans;
      --  unspecified
      Reserved_15_31 : Interfaces.SAM3x8e.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTCFG_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      ALLOC          at 0 range 1 .. 1;
      EPBK           at 0 range 2 .. 3;
      EPSIZE         at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EPDIR          at 0 range 8 .. 8;
      AUTOSW         at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      EPTYPE         at 0 range 11 .. 12;
      NBTRANS        at 0 range 13 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Device Endpoint Configuration Register (n = 0)
   type UOTGHS_DEVEPTCFG_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTCFG_Register;

   subtype UOTGHS_DEVEPTISR_TXINI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_RXOUTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_RXSTPI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_NAKOUTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_NAKINI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_OVERFI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_STALLEDI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_SHORTPACKET_Field is Interfaces.SAM3x8e.Bit;

   --  Data Toggle Sequence
   type DEVEPTISR_DTSEQ_Field is
     (--  Data0 toggle sequence
      Data0,
      --  Data1 toggle sequence
      Data1,
      --  Reserved for high-bandwidth isochronous endpoint
      Data2,
      --  Reserved for high-bandwidth isochronous endpoint
      Mdata)
     with Size => 2;
   for DEVEPTISR_DTSEQ_Field use
     (Data0 => 0,
      Data1 => 1,
      Data2 => 2,
      Mdata => 3);

   --  Number of Busy Banks
   type DEVEPTISR_NBUSYBK_Field is
     (--  0 busy bank (all banks free)
      Val_0_Busy,
      --  1 busy bank
      Val_1_Busy,
      --  2 busy banks
      Val_2_Busy,
      --  3 busy banks
      Val_3_Busy)
     with Size => 2;
   for DEVEPTISR_NBUSYBK_Field use
     (Val_0_Busy => 0,
      Val_1_Busy => 1,
      Val_2_Busy => 2,
      Val_3_Busy => 3);

   --  Current Bank
   type DEVEPTISR_CURRBK_Field is
     (--  Current bank is bank0
      Bank0,
      --  Current bank is bank1
      Bank1,
      --  Current bank is bank2
      Bank2)
     with Size => 2;
   for DEVEPTISR_CURRBK_Field use
     (Bank0 => 0,
      Bank1 => 1,
      Bank2 => 2);

   subtype UOTGHS_DEVEPTISR_RWALL_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_CTRLDIR_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_CFGOK_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTISR_BYCT_Field is Interfaces.SAM3x8e.UInt11;

   --  Device Endpoint Status Register (n = 0)
   type UOTGHS_DEVEPTISR_Register is record
      --  Read-only. Transmitted IN Data Interrupt
      TXINI          : UOTGHS_DEVEPTISR_TXINI_Field;
      --  Read-only. Received OUT Data Interrupt
      RXOUTI         : UOTGHS_DEVEPTISR_RXOUTI_Field;
      --  Read-only. Received SETUP Interrupt
      RXSTPI         : UOTGHS_DEVEPTISR_RXSTPI_Field;
      --  Read-only. NAKed OUT Interrupt
      NAKOUTI        : UOTGHS_DEVEPTISR_NAKOUTI_Field;
      --  Read-only. NAKed IN Interrupt
      NAKINI         : UOTGHS_DEVEPTISR_NAKINI_Field;
      --  Read-only. Overflow Interrupt
      OVERFI         : UOTGHS_DEVEPTISR_OVERFI_Field;
      --  Read-only. STALLed Interrupt
      STALLEDI       : UOTGHS_DEVEPTISR_STALLEDI_Field;
      --  Read-only. Short Packet Interrupt
      SHORTPACKET    : UOTGHS_DEVEPTISR_SHORTPACKET_Field;
      --  Read-only. Data Toggle Sequence
      DTSEQ          : DEVEPTISR_DTSEQ_Field;
      --  unspecified
      Reserved_10_11 : Interfaces.SAM3x8e.UInt2;
      --  Read-only. Number of Busy Banks
      NBUSYBK        : DEVEPTISR_NBUSYBK_Field;
      --  Read-only. Current Bank
      CURRBK         : DEVEPTISR_CURRBK_Field;
      --  Read-only. Read-write Allowed
      RWALL          : UOTGHS_DEVEPTISR_RWALL_Field;
      --  Read-only. Control Direction
      CTRLDIR        : UOTGHS_DEVEPTISR_CTRLDIR_Field;
      --  Read-only. Configuration OK Status
      CFGOK          : UOTGHS_DEVEPTISR_CFGOK_Field;
      --  unspecified
      Reserved_19_19 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Byte Count
      BYCT           : UOTGHS_DEVEPTISR_BYCT_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTISR_Register use record
      TXINI          at 0 range 0 .. 0;
      RXOUTI         at 0 range 1 .. 1;
      RXSTPI         at 0 range 2 .. 2;
      NAKOUTI        at 0 range 3 .. 3;
      NAKINI         at 0 range 4 .. 4;
      OVERFI         at 0 range 5 .. 5;
      STALLEDI       at 0 range 6 .. 6;
      SHORTPACKET    at 0 range 7 .. 7;
      DTSEQ          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      NBUSYBK        at 0 range 12 .. 13;
      CURRBK         at 0 range 14 .. 15;
      RWALL          at 0 range 16 .. 16;
      CTRLDIR        at 0 range 17 .. 17;
      CFGOK          at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      BYCT           at 0 range 20 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Device Endpoint Status Register (n = 0)
   type UOTGHS_DEVEPTISR_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTISR_Register;

   subtype UOTGHS_DEVEPTICR_TXINIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_RXOUTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_RXSTPIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_NAKOUTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_NAKINIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_OVERFIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_STALLEDIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTICR_SHORTPACKETC_Field is Interfaces.SAM3x8e.Bit;

   --  Device Endpoint Clear Register (n = 0)
   type UOTGHS_DEVEPTICR_Register is record
      --  Write-only. Transmitted IN Data Interrupt Clear
      TXINIC        : UOTGHS_DEVEPTICR_TXINIC_Field := 16#0#;
      --  Write-only. Received OUT Data Interrupt Clear
      RXOUTIC       : UOTGHS_DEVEPTICR_RXOUTIC_Field := 16#0#;
      --  Write-only. Received SETUP Interrupt Clear
      RXSTPIC       : UOTGHS_DEVEPTICR_RXSTPIC_Field := 16#0#;
      --  Write-only. NAKed OUT Interrupt Clear
      NAKOUTIC      : UOTGHS_DEVEPTICR_NAKOUTIC_Field := 16#0#;
      --  Write-only. NAKed IN Interrupt Clear
      NAKINIC       : UOTGHS_DEVEPTICR_NAKINIC_Field := 16#0#;
      --  Write-only. Overflow Interrupt Clear
      OVERFIC       : UOTGHS_DEVEPTICR_OVERFIC_Field := 16#0#;
      --  Write-only. STALLed Interrupt Clear
      STALLEDIC     : UOTGHS_DEVEPTICR_STALLEDIC_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Clear
      SHORTPACKETC  : UOTGHS_DEVEPTICR_SHORTPACKETC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM3x8e.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTICR_Register use record
      TXINIC        at 0 range 0 .. 0;
      RXOUTIC       at 0 range 1 .. 1;
      RXSTPIC       at 0 range 2 .. 2;
      NAKOUTIC      at 0 range 3 .. 3;
      NAKINIC       at 0 range 4 .. 4;
      OVERFIC       at 0 range 5 .. 5;
      STALLEDIC     at 0 range 6 .. 6;
      SHORTPACKETC  at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Device Endpoint Clear Register (n = 0)
   type UOTGHS_DEVEPTICR_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTICR_Register;

   subtype UOTGHS_DEVEPTIFR_TXINIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_RXOUTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_RXSTPIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_NAKOUTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_NAKINIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_OVERFIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_STALLEDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_SHORTPACKETS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIFR_NBUSYBKS_Field is Interfaces.SAM3x8e.Bit;

   --  Device Endpoint Set Register (n = 0)
   type UOTGHS_DEVEPTIFR_Register is record
      --  Write-only. Transmitted IN Data Interrupt Set
      TXINIS         : UOTGHS_DEVEPTIFR_TXINIS_Field := 16#0#;
      --  Write-only. Received OUT Data Interrupt Set
      RXOUTIS        : UOTGHS_DEVEPTIFR_RXOUTIS_Field := 16#0#;
      --  Write-only. Received SETUP Interrupt Set
      RXSTPIS        : UOTGHS_DEVEPTIFR_RXSTPIS_Field := 16#0#;
      --  Write-only. NAKed OUT Interrupt Set
      NAKOUTIS       : UOTGHS_DEVEPTIFR_NAKOUTIS_Field := 16#0#;
      --  Write-only. NAKed IN Interrupt Set
      NAKINIS        : UOTGHS_DEVEPTIFR_NAKINIS_Field := 16#0#;
      --  Write-only. Overflow Interrupt Set
      OVERFIS        : UOTGHS_DEVEPTIFR_OVERFIS_Field := 16#0#;
      --  Write-only. STALLed Interrupt Set
      STALLEDIS      : UOTGHS_DEVEPTIFR_STALLEDIS_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Set
      SHORTPACKETS   : UOTGHS_DEVEPTIFR_SHORTPACKETS_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Interrupt Set
      NBUSYBKS       : UOTGHS_DEVEPTIFR_NBUSYBKS_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.SAM3x8e.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTIFR_Register use record
      TXINIS         at 0 range 0 .. 0;
      RXOUTIS        at 0 range 1 .. 1;
      RXSTPIS        at 0 range 2 .. 2;
      NAKOUTIS       at 0 range 3 .. 3;
      NAKINIS        at 0 range 4 .. 4;
      OVERFIS        at 0 range 5 .. 5;
      STALLEDIS      at 0 range 6 .. 6;
      SHORTPACKETS   at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKS       at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  Device Endpoint Set Register (n = 0)
   type UOTGHS_DEVEPTIFR_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTIFR_Register;

   subtype UOTGHS_DEVEPTIMR_TXINE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_RXOUTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_RXSTPE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_NAKOUTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_NAKINE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_OVERFE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_STALLEDE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_SHORTPACKETE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_NBUSYBKE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_KILLBK_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_FIFOCON_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_EPDISHDMA_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_NYETDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_RSTDT_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIMR_STALLRQ_Field is Interfaces.SAM3x8e.Bit;

   --  Device Endpoint Mask Register (n = 0)
   type UOTGHS_DEVEPTIMR_Register is record
      --  Read-only. Transmitted IN Data Interrupt
      TXINE          : UOTGHS_DEVEPTIMR_TXINE_Field;
      --  Read-only. Received OUT Data Interrupt
      RXOUTE         : UOTGHS_DEVEPTIMR_RXOUTE_Field;
      --  Read-only. Received SETUP Interrupt
      RXSTPE         : UOTGHS_DEVEPTIMR_RXSTPE_Field;
      --  Read-only. NAKed OUT Interrupt
      NAKOUTE        : UOTGHS_DEVEPTIMR_NAKOUTE_Field;
      --  Read-only. NAKed IN Interrupt
      NAKINE         : UOTGHS_DEVEPTIMR_NAKINE_Field;
      --  Read-only. Overflow Interrupt
      OVERFE         : UOTGHS_DEVEPTIMR_OVERFE_Field;
      --  Read-only. STALLed Interrupt
      STALLEDE       : UOTGHS_DEVEPTIMR_STALLEDE_Field;
      --  Read-only. Short Packet Interrupt
      SHORTPACKETE   : UOTGHS_DEVEPTIMR_SHORTPACKETE_Field;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4;
      --  Read-only. Number of Busy Banks Interrupt
      NBUSYBKE       : UOTGHS_DEVEPTIMR_NBUSYBKE_Field;
      --  Read-only. Kill IN Bank
      KILLBK         : UOTGHS_DEVEPTIMR_KILLBK_Field;
      --  Read-only. FIFO Control
      FIFOCON        : UOTGHS_DEVEPTIMR_FIFOCON_Field;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Endpoint Interrupts Disable HDMA Request
      EPDISHDMA      : UOTGHS_DEVEPTIMR_EPDISHDMA_Field;
      --  Read-only. NYET Token Disable
      NYETDIS        : UOTGHS_DEVEPTIMR_NYETDIS_Field;
      --  Read-only. Reset Data Toggle
      RSTDT          : UOTGHS_DEVEPTIMR_RSTDT_Field;
      --  Read-only. STALL Request
      STALLRQ        : UOTGHS_DEVEPTIMR_STALLRQ_Field;
      --  unspecified
      Reserved_20_31 : Interfaces.SAM3x8e.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTIMR_Register use record
      TXINE          at 0 range 0 .. 0;
      RXOUTE         at 0 range 1 .. 1;
      RXSTPE         at 0 range 2 .. 2;
      NAKOUTE        at 0 range 3 .. 3;
      NAKINE         at 0 range 4 .. 4;
      OVERFE         at 0 range 5 .. 5;
      STALLEDE       at 0 range 6 .. 6;
      SHORTPACKETE   at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKE       at 0 range 12 .. 12;
      KILLBK         at 0 range 13 .. 13;
      FIFOCON        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      EPDISHDMA      at 0 range 16 .. 16;
      NYETDIS        at 0 range 17 .. 17;
      RSTDT          at 0 range 18 .. 18;
      STALLRQ        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Device Endpoint Mask Register (n = 0)
   type UOTGHS_DEVEPTIMR_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTIMR_Register;

   subtype UOTGHS_DEVEPTIER_TXINES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_RXOUTES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_RXSTPES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_NAKOUTES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_NAKINES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_OVERFES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_STALLEDES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_SHORTPACKETES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_NBUSYBKES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_KILLBKS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_FIFOCONS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_EPDISHDMAS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_NYETDISS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_RSTDTS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIER_STALLRQS_Field is Interfaces.SAM3x8e.Bit;

   --  Device Endpoint Enable Register (n = 0)
   type UOTGHS_DEVEPTIER_Register is record
      --  Write-only. Transmitted IN Data Interrupt Enable
      TXINES         : UOTGHS_DEVEPTIER_TXINES_Field := 16#0#;
      --  Write-only. Received OUT Data Interrupt Enable
      RXOUTES        : UOTGHS_DEVEPTIER_RXOUTES_Field := 16#0#;
      --  Write-only. Received SETUP Interrupt Enable
      RXSTPES        : UOTGHS_DEVEPTIER_RXSTPES_Field := 16#0#;
      --  Write-only. NAKed OUT Interrupt Enable
      NAKOUTES       : UOTGHS_DEVEPTIER_NAKOUTES_Field := 16#0#;
      --  Write-only. NAKed IN Interrupt Enable
      NAKINES        : UOTGHS_DEVEPTIER_NAKINES_Field := 16#0#;
      --  Write-only. Overflow Interrupt Enable
      OVERFES        : UOTGHS_DEVEPTIER_OVERFES_Field := 16#0#;
      --  Write-only. STALLed Interrupt Enable
      STALLEDES      : UOTGHS_DEVEPTIER_STALLEDES_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Enable
      SHORTPACKETES  : UOTGHS_DEVEPTIER_SHORTPACKETES_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Interrupt Enable
      NBUSYBKES      : UOTGHS_DEVEPTIER_NBUSYBKES_Field := 16#0#;
      --  Write-only. Kill IN Bank
      KILLBKS        : UOTGHS_DEVEPTIER_KILLBKS_Field := 16#0#;
      --  Write-only. FIFO Control
      FIFOCONS       : UOTGHS_DEVEPTIER_FIFOCONS_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. Endpoint Interrupts Disable HDMA Request Enable
      EPDISHDMAS     : UOTGHS_DEVEPTIER_EPDISHDMAS_Field := 16#0#;
      --  Write-only. NYET Token Disable Enable
      NYETDISS       : UOTGHS_DEVEPTIER_NYETDISS_Field := 16#0#;
      --  Write-only. Reset Data Toggle Enable
      RSTDTS         : UOTGHS_DEVEPTIER_RSTDTS_Field := 16#0#;
      --  Write-only. STALL Request Enable
      STALLRQS       : UOTGHS_DEVEPTIER_STALLRQS_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : Interfaces.SAM3x8e.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTIER_Register use record
      TXINES         at 0 range 0 .. 0;
      RXOUTES        at 0 range 1 .. 1;
      RXSTPES        at 0 range 2 .. 2;
      NAKOUTES       at 0 range 3 .. 3;
      NAKINES        at 0 range 4 .. 4;
      OVERFES        at 0 range 5 .. 5;
      STALLEDES      at 0 range 6 .. 6;
      SHORTPACKETES  at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKES      at 0 range 12 .. 12;
      KILLBKS        at 0 range 13 .. 13;
      FIFOCONS       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      EPDISHDMAS     at 0 range 16 .. 16;
      NYETDISS       at 0 range 17 .. 17;
      RSTDTS         at 0 range 18 .. 18;
      STALLRQS       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Device Endpoint Enable Register (n = 0)
   type UOTGHS_DEVEPTIER_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTIER_Register;

   subtype UOTGHS_DEVEPTIDR_TXINEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_RXOUTEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_RXSTPEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_NAKOUTEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_NAKINEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_OVERFEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_STALLEDEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_SHORTPACKETEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_NBUSYBKEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_FIFOCONC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_EPDISHDMAC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_NYETDISC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_DEVEPTIDR_STALLRQC_Field is Interfaces.SAM3x8e.Bit;

   --  Device Endpoint Disable Register (n = 0)
   type UOTGHS_DEVEPTIDR_Register is record
      --  Write-only. Transmitted IN Interrupt Clear
      TXINEC         : UOTGHS_DEVEPTIDR_TXINEC_Field := 16#0#;
      --  Write-only. Received OUT Data Interrupt Clear
      RXOUTEC        : UOTGHS_DEVEPTIDR_RXOUTEC_Field := 16#0#;
      --  Write-only. Received SETUP Interrupt Clear
      RXSTPEC        : UOTGHS_DEVEPTIDR_RXSTPEC_Field := 16#0#;
      --  Write-only. NAKed OUT Interrupt Clear
      NAKOUTEC       : UOTGHS_DEVEPTIDR_NAKOUTEC_Field := 16#0#;
      --  Write-only. NAKed IN Interrupt Clear
      NAKINEC        : UOTGHS_DEVEPTIDR_NAKINEC_Field := 16#0#;
      --  Write-only. Overflow Interrupt Clear
      OVERFEC        : UOTGHS_DEVEPTIDR_OVERFEC_Field := 16#0#;
      --  Write-only. STALLed Interrupt Clear
      STALLEDEC      : UOTGHS_DEVEPTIDR_STALLEDEC_Field := 16#0#;
      --  Write-only. Shortpacket Interrupt Clear
      SHORTPACKETEC  : UOTGHS_DEVEPTIDR_SHORTPACKETEC_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Interrupt Clear
      NBUSYBKEC      : UOTGHS_DEVEPTIDR_NBUSYBKEC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. FIFO Control Clear
      FIFOCONC       : UOTGHS_DEVEPTIDR_FIFOCONC_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. Endpoint Interrupts Disable HDMA Request Clear
      EPDISHDMAC     : UOTGHS_DEVEPTIDR_EPDISHDMAC_Field := 16#0#;
      --  Write-only. NYET Token Disable Clear
      NYETDISC       : UOTGHS_DEVEPTIDR_NYETDISC_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. STALL Request Clear
      STALLRQC       : UOTGHS_DEVEPTIDR_STALLRQC_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : Interfaces.SAM3x8e.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_DEVEPTIDR_Register use record
      TXINEC         at 0 range 0 .. 0;
      RXOUTEC        at 0 range 1 .. 1;
      RXSTPEC        at 0 range 2 .. 2;
      NAKOUTEC       at 0 range 3 .. 3;
      NAKINEC        at 0 range 4 .. 4;
      OVERFEC        at 0 range 5 .. 5;
      STALLEDEC      at 0 range 6 .. 6;
      SHORTPACKETEC  at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKEC      at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      FIFOCONC       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      EPDISHDMAC     at 0 range 16 .. 16;
      NYETDISC       at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      STALLRQC       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Device Endpoint Disable Register (n = 0)
   type UOTGHS_DEVEPTIDR_Registers is array (0 .. 9)
     of UOTGHS_DEVEPTIDR_Register;

   subtype DEVDMACONTROL_CHANN_ENB_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_LDNXT_DSC_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_END_TR_EN_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_END_B_EN_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_END_TR_IT_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_END_BUFFIT_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_DESC_LD_IT_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_BURST_LCK_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMACONTROL_BUFF_LENGTH_Field is Interfaces.SAM3x8e.UInt16;

   --  Device DMA Channel Control Register (n = 1)
   type DEVDMACONTROL_Register is record
      --  Channel Enable Command
      CHANN_ENB     : DEVDMACONTROL_CHANN_ENB_Field := 16#0#;
      --  Load Next Channel Transfer Descriptor Enable Command
      LDNXT_DSC     : DEVDMACONTROL_LDNXT_DSC_Field := 16#0#;
      --  End of Transfer Enable Control
      END_TR_EN     : DEVDMACONTROL_END_TR_EN_Field := 16#0#;
      --  End of Buffer Enable Control
      END_B_EN      : DEVDMACONTROL_END_B_EN_Field := 16#0#;
      --  End of Transfer Interrupt Enable
      END_TR_IT     : DEVDMACONTROL_END_TR_IT_Field := 16#0#;
      --  End of Buffer Interrupt Enable
      END_BUFFIT    : DEVDMACONTROL_END_BUFFIT_Field := 16#0#;
      --  Descriptor Loaded Interrupt Enable
      DESC_LD_IT    : DEVDMACONTROL_DESC_LD_IT_Field := 16#0#;
      --  Burst Lock Enable
      BURST_LCK     : DEVDMACONTROL_BURST_LCK_Field := 16#0#;
      --  unspecified
      Reserved_8_15 : Interfaces.SAM3x8e.Byte := 16#0#;
      --  Buffer Byte Length (Write-only)
      BUFF_LENGTH   : DEVDMACONTROL_BUFF_LENGTH_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVDMACONTROL_Register use record
      CHANN_ENB     at 0 range 0 .. 0;
      LDNXT_DSC     at 0 range 1 .. 1;
      END_TR_EN     at 0 range 2 .. 2;
      END_B_EN      at 0 range 3 .. 3;
      END_TR_IT     at 0 range 4 .. 4;
      END_BUFFIT    at 0 range 5 .. 5;
      DESC_LD_IT    at 0 range 6 .. 6;
      BURST_LCK     at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      BUFF_LENGTH   at 0 range 16 .. 31;
   end record;

   subtype DEVDMASTATUS_CHANN_ENB_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMASTATUS_CHANN_ACT_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMASTATUS_END_TR_ST_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMASTATUS_END_BF_ST_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMASTATUS_DESC_LDST_Field is Interfaces.SAM3x8e.Bit;
   subtype DEVDMASTATUS_BUFF_COUNT_Field is Interfaces.SAM3x8e.UInt16;

   --  Device DMA Channel Status Register (n = 1)
   type DEVDMASTATUS_Register is record
      --  Channel Enable Status
      CHANN_ENB     : DEVDMASTATUS_CHANN_ENB_Field := 16#0#;
      --  Channel Active Status
      CHANN_ACT     : DEVDMASTATUS_CHANN_ACT_Field := 16#0#;
      --  unspecified
      Reserved_2_3  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  End of Channel Transfer Status
      END_TR_ST     : DEVDMASTATUS_END_TR_ST_Field := 16#0#;
      --  End of Channel Buffer Status
      END_BF_ST     : DEVDMASTATUS_END_BF_ST_Field := 16#0#;
      --  Descriptor Loaded Status
      DESC_LDST     : DEVDMASTATUS_DESC_LDST_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : Interfaces.SAM3x8e.UInt9 := 16#0#;
      --  Buffer Byte Count
      BUFF_COUNT    : DEVDMASTATUS_BUFF_COUNT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVDMASTATUS_Register use record
      CHANN_ENB     at 0 range 0 .. 0;
      CHANN_ACT     at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      END_TR_ST     at 0 range 4 .. 4;
      END_BF_ST     at 0 range 5 .. 5;
      DESC_LDST     at 0 range 6 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
      BUFF_COUNT    at 0 range 16 .. 31;
   end record;

   subtype UOTGHS_HSTCTRL_SOFE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTCTRL_RESET_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTCTRL_RESUME_Field is Interfaces.SAM3x8e.Bit;

   --  Mode Configuration
   type HSTCTRL_SPDCONF_Field is
     (--  The host starts in full-speed mode and performs a high-speed reset to
--  switch to the high-speed mode if the downstream peripheral is high-speed
--  capable.
      Normal,
      --  For a better consumption, if high-speed is not needed.
      Low_Power,
      --  Forced high speed.
      High_Speed,
      --  The host remains to full-speed mode whatever the peripheral speed
--  capability.
      Forced_Fs)
     with Size => 2;
   for HSTCTRL_SPDCONF_Field use
     (Normal => 0,
      Low_Power => 1,
      High_Speed => 2,
      Forced_Fs => 3);

   --  Host General Control Register
   type UOTGHS_HSTCTRL_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.SAM3x8e.Byte := 16#0#;
      --  Start of Frame Generation Enable
      SOFE           : UOTGHS_HSTCTRL_SOFE_Field := 16#0#;
      --  Send USB Reset
      RESET          : UOTGHS_HSTCTRL_RESET_Field := 16#0#;
      --  Send USB Resume
      RESUME         : UOTGHS_HSTCTRL_RESUME_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Mode Configuration
      SPDCONF        : HSTCTRL_SPDCONF_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Normal;
      --  unspecified
      Reserved_14_31 : Interfaces.SAM3x8e.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTCTRL_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SOFE           at 0 range 8 .. 8;
      RESET          at 0 range 9 .. 9;
      RESUME         at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SPDCONF        at 0 range 12 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype UOTGHS_HSTISR_DCONNI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DDISCI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_RSTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_RSMEDI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_RXRSMI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_HSOFI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_HWUPI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTISR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Status Register
   type UOTGHS_HSTISR_Register is record
      --  Read-only. Device Connection Interrupt
      DCONNI         : UOTGHS_HSTISR_DCONNI_Field;
      --  Read-only. Device Disconnection Interrupt
      DDISCI         : UOTGHS_HSTISR_DDISCI_Field;
      --  Read-only. USB Reset Sent Interrupt
      RSTI           : UOTGHS_HSTISR_RSTI_Field;
      --  Read-only. Downstream Resume Sent Interrupt
      RSMEDI         : UOTGHS_HSTISR_RSMEDI_Field;
      --  Read-only. Upstream Resume Received Interrupt
      RXRSMI         : UOTGHS_HSTISR_RXRSMI_Field;
      --  Read-only. Host Start of Frame Interrupt
      HSOFI          : UOTGHS_HSTISR_HSOFI_Field;
      --  Read-only. Host Wake-Up Interrupt
      HWUPI          : UOTGHS_HSTISR_HWUPI_Field;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit;
      --  Read-only. Pipe 0 Interrupt
      PEP_0          : UOTGHS_HSTISR_PEP_0_Field;
      --  Read-only. Pipe 1 Interrupt
      PEP_1          : UOTGHS_HSTISR_PEP_1_Field;
      --  Read-only. Pipe 2 Interrupt
      PEP_2          : UOTGHS_HSTISR_PEP_2_Field;
      --  Read-only. Pipe 3 Interrupt
      PEP_3          : UOTGHS_HSTISR_PEP_3_Field;
      --  Read-only. Pipe 4 Interrupt
      PEP_4          : UOTGHS_HSTISR_PEP_4_Field;
      --  Read-only. Pipe 5 Interrupt
      PEP_5          : UOTGHS_HSTISR_PEP_5_Field;
      --  Read-only. Pipe 6 Interrupt
      PEP_6          : UOTGHS_HSTISR_PEP_6_Field;
      --  Read-only. Pipe 7 Interrupt
      PEP_7          : UOTGHS_HSTISR_PEP_7_Field;
      --  Read-only. Pipe 8 Interrupt
      PEP_8          : UOTGHS_HSTISR_PEP_8_Field;
      --  Read-only. Pipe 9 Interrupt
      PEP_9          : UOTGHS_HSTISR_PEP_9_Field;
      --  unspecified
      Reserved_18_24 : Interfaces.SAM3x8e.UInt7;
      --  Read-only. DMA Channel 1 Interrupt
      DMA_1          : UOTGHS_HSTISR_DMA_1_Field;
      --  Read-only. DMA Channel 2 Interrupt
      DMA_2          : UOTGHS_HSTISR_DMA_2_Field;
      --  Read-only. DMA Channel 3 Interrupt
      DMA_3          : UOTGHS_HSTISR_DMA_3_Field;
      --  Read-only. DMA Channel 4 Interrupt
      DMA_4          : UOTGHS_HSTISR_DMA_4_Field;
      --  Read-only. DMA Channel 5 Interrupt
      DMA_5          : UOTGHS_HSTISR_DMA_5_Field;
      --  Read-only. DMA Channel 6 Interrupt
      DMA_6          : UOTGHS_HSTISR_DMA_6_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTISR_Register use record
      DCONNI         at 0 range 0 .. 0;
      DDISCI         at 0 range 1 .. 1;
      RSTI           at 0 range 2 .. 2;
      RSMEDI         at 0 range 3 .. 3;
      RXRSMI         at 0 range 4 .. 4;
      HSOFI          at 0 range 5 .. 5;
      HWUPI          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PEP_0          at 0 range 8 .. 8;
      PEP_1          at 0 range 9 .. 9;
      PEP_2          at 0 range 10 .. 10;
      PEP_3          at 0 range 11 .. 11;
      PEP_4          at 0 range 12 .. 12;
      PEP_5          at 0 range 13 .. 13;
      PEP_6          at 0 range 14 .. 14;
      PEP_7          at 0 range 15 .. 15;
      PEP_8          at 0 range 16 .. 16;
      PEP_9          at 0 range 17 .. 17;
      Reserved_18_24 at 0 range 18 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTICR_DCONNIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_DDISCIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_RSTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_RSMEDIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_RXRSMIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_HSOFIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTICR_HWUPIC_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Clear Register
   type UOTGHS_HSTICR_Register is record
      --  Write-only. Device Connection Interrupt Clear
      DCONNIC       : UOTGHS_HSTICR_DCONNIC_Field := 16#0#;
      --  Write-only. Device Disconnection Interrupt Clear
      DDISCIC       : UOTGHS_HSTICR_DDISCIC_Field := 16#0#;
      --  Write-only. USB Reset Sent Interrupt Clear
      RSTIC         : UOTGHS_HSTICR_RSTIC_Field := 16#0#;
      --  Write-only. Downstream Resume Sent Interrupt Clear
      RSMEDIC       : UOTGHS_HSTICR_RSMEDIC_Field := 16#0#;
      --  Write-only. Upstream Resume Received Interrupt Clear
      RXRSMIC       : UOTGHS_HSTICR_RXRSMIC_Field := 16#0#;
      --  Write-only. Host Start of Frame Interrupt Clear
      HSOFIC        : UOTGHS_HSTICR_HSOFIC_Field := 16#0#;
      --  Write-only. Host Wake-Up Interrupt Clear
      HWUPIC        : UOTGHS_HSTICR_HWUPIC_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : Interfaces.SAM3x8e.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTICR_Register use record
      DCONNIC       at 0 range 0 .. 0;
      DDISCIC       at 0 range 1 .. 1;
      RSTIC         at 0 range 2 .. 2;
      RSMEDIC       at 0 range 3 .. 3;
      RXRSMIC       at 0 range 4 .. 4;
      HSOFIC        at 0 range 5 .. 5;
      HWUPIC        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype UOTGHS_HSTIFR_DCONNIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DDISCIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_RSTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_RSMEDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_RXRSMIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_HSOFIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_HWUPIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIFR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Set Register
   type UOTGHS_HSTIFR_Register is record
      --  Write-only. Device Connection Interrupt Set
      DCONNIS        : UOTGHS_HSTIFR_DCONNIS_Field := 16#0#;
      --  Write-only. Device Disconnection Interrupt Set
      DDISCIS        : UOTGHS_HSTIFR_DDISCIS_Field := 16#0#;
      --  Write-only. USB Reset Sent Interrupt Set
      RSTIS          : UOTGHS_HSTIFR_RSTIS_Field := 16#0#;
      --  Write-only. Downstream Resume Sent Interrupt Set
      RSMEDIS        : UOTGHS_HSTIFR_RSMEDIS_Field := 16#0#;
      --  Write-only. Upstream Resume Received Interrupt Set
      RXRSMIS        : UOTGHS_HSTIFR_RXRSMIS_Field := 16#0#;
      --  Write-only. Host Start of Frame Interrupt Set
      HSOFIS         : UOTGHS_HSTIFR_HSOFIS_Field := 16#0#;
      --  Write-only. Host Wake-Up Interrupt Set
      HWUPIS         : UOTGHS_HSTIFR_HWUPIS_Field := 16#0#;
      --  unspecified
      Reserved_7_24  : Interfaces.SAM3x8e.UInt18 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Set
      DMA_1          : UOTGHS_HSTIFR_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Set
      DMA_2          : UOTGHS_HSTIFR_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Set
      DMA_3          : UOTGHS_HSTIFR_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Set
      DMA_4          : UOTGHS_HSTIFR_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Set
      DMA_5          : UOTGHS_HSTIFR_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Set
      DMA_6          : UOTGHS_HSTIFR_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTIFR_Register use record
      DCONNIS        at 0 range 0 .. 0;
      DDISCIS        at 0 range 1 .. 1;
      RSTIS          at 0 range 2 .. 2;
      RSMEDIS        at 0 range 3 .. 3;
      RXRSMIS        at 0 range 4 .. 4;
      HSOFIS         at 0 range 5 .. 5;
      HWUPIS         at 0 range 6 .. 6;
      Reserved_7_24  at 0 range 7 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTIMR_DCONNIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DDISCIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_RSTIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_RSMEDIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_RXRSMIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_HSOFIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_HWUPIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIMR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Mask Register
   type UOTGHS_HSTIMR_Register is record
      --  Read-only. Device Connection Interrupt Enable
      DCONNIE        : UOTGHS_HSTIMR_DCONNIE_Field;
      --  Read-only. Device Disconnection Interrupt Enable
      DDISCIE        : UOTGHS_HSTIMR_DDISCIE_Field;
      --  Read-only. USB Reset Sent Interrupt Enable
      RSTIE          : UOTGHS_HSTIMR_RSTIE_Field;
      --  Read-only. Downstream Resume Sent Interrupt Enable
      RSMEDIE        : UOTGHS_HSTIMR_RSMEDIE_Field;
      --  Read-only. Upstream Resume Received Interrupt Enable
      RXRSMIE        : UOTGHS_HSTIMR_RXRSMIE_Field;
      --  Read-only. Host Start of Frame Interrupt Enable
      HSOFIE         : UOTGHS_HSTIMR_HSOFIE_Field;
      --  Read-only. Host Wake-Up Interrupt Enable
      HWUPIE         : UOTGHS_HSTIMR_HWUPIE_Field;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit;
      --  Read-only. Pipe 0 Interrupt Enable
      PEP_0          : UOTGHS_HSTIMR_PEP_0_Field;
      --  Read-only. Pipe 1 Interrupt Enable
      PEP_1          : UOTGHS_HSTIMR_PEP_1_Field;
      --  Read-only. Pipe 2 Interrupt Enable
      PEP_2          : UOTGHS_HSTIMR_PEP_2_Field;
      --  Read-only. Pipe 3 Interrupt Enable
      PEP_3          : UOTGHS_HSTIMR_PEP_3_Field;
      --  Read-only. Pipe 4 Interrupt Enable
      PEP_4          : UOTGHS_HSTIMR_PEP_4_Field;
      --  Read-only. Pipe 5 Interrupt Enable
      PEP_5          : UOTGHS_HSTIMR_PEP_5_Field;
      --  Read-only. Pipe 6 Interrupt Enable
      PEP_6          : UOTGHS_HSTIMR_PEP_6_Field;
      --  Read-only. Pipe 7 Interrupt Enable
      PEP_7          : UOTGHS_HSTIMR_PEP_7_Field;
      --  Read-only. Pipe 8 Interrupt Enable
      PEP_8          : UOTGHS_HSTIMR_PEP_8_Field;
      --  Read-only. Pipe 9 Interrupt Enable
      PEP_9          : UOTGHS_HSTIMR_PEP_9_Field;
      --  unspecified
      Reserved_18_24 : Interfaces.SAM3x8e.UInt7;
      --  Read-only. DMA Channel 1 Interrupt Enable
      DMA_1          : UOTGHS_HSTIMR_DMA_1_Field;
      --  Read-only. DMA Channel 2 Interrupt Enable
      DMA_2          : UOTGHS_HSTIMR_DMA_2_Field;
      --  Read-only. DMA Channel 3 Interrupt Enable
      DMA_3          : UOTGHS_HSTIMR_DMA_3_Field;
      --  Read-only. DMA Channel 4 Interrupt Enable
      DMA_4          : UOTGHS_HSTIMR_DMA_4_Field;
      --  Read-only. DMA Channel 5 Interrupt Enable
      DMA_5          : UOTGHS_HSTIMR_DMA_5_Field;
      --  Read-only. DMA Channel 6 Interrupt Enable
      DMA_6          : UOTGHS_HSTIMR_DMA_6_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTIMR_Register use record
      DCONNIE        at 0 range 0 .. 0;
      DDISCIE        at 0 range 1 .. 1;
      RSTIE          at 0 range 2 .. 2;
      RSMEDIE        at 0 range 3 .. 3;
      RXRSMIE        at 0 range 4 .. 4;
      HSOFIE         at 0 range 5 .. 5;
      HWUPIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PEP_0          at 0 range 8 .. 8;
      PEP_1          at 0 range 9 .. 9;
      PEP_2          at 0 range 10 .. 10;
      PEP_3          at 0 range 11 .. 11;
      PEP_4          at 0 range 12 .. 12;
      PEP_5          at 0 range 13 .. 13;
      PEP_6          at 0 range 14 .. 14;
      PEP_7          at 0 range 15 .. 15;
      PEP_8          at 0 range 16 .. 16;
      PEP_9          at 0 range 17 .. 17;
      Reserved_18_24 at 0 range 18 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTIDR_DCONNIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DDISCIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_RSTIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_RSMEDIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_RXRSMIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_HSOFIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_HWUPIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIDR_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Disable Register
   type UOTGHS_HSTIDR_Register is record
      --  Write-only. Device Connection Interrupt Disable
      DCONNIEC       : UOTGHS_HSTIDR_DCONNIEC_Field := 16#0#;
      --  Write-only. Device Disconnection Interrupt Disable
      DDISCIEC       : UOTGHS_HSTIDR_DDISCIEC_Field := 16#0#;
      --  Write-only. USB Reset Sent Interrupt Disable
      RSTIEC         : UOTGHS_HSTIDR_RSTIEC_Field := 16#0#;
      --  Write-only. Downstream Resume Sent Interrupt Disable
      RSMEDIEC       : UOTGHS_HSTIDR_RSMEDIEC_Field := 16#0#;
      --  Write-only. Upstream Resume Received Interrupt Disable
      RXRSMIEC       : UOTGHS_HSTIDR_RXRSMIEC_Field := 16#0#;
      --  Write-only. Host Start of Frame Interrupt Disable
      HSOFIEC        : UOTGHS_HSTIDR_HSOFIEC_Field := 16#0#;
      --  Write-only. Host Wake-Up Interrupt Disable
      HWUPIEC        : UOTGHS_HSTIDR_HWUPIEC_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. Pipe 0 Interrupt Disable
      PEP_0          : UOTGHS_HSTIDR_PEP_0_Field := 16#0#;
      --  Write-only. Pipe 1 Interrupt Disable
      PEP_1          : UOTGHS_HSTIDR_PEP_1_Field := 16#0#;
      --  Write-only. Pipe 2 Interrupt Disable
      PEP_2          : UOTGHS_HSTIDR_PEP_2_Field := 16#0#;
      --  Write-only. Pipe 3 Interrupt Disable
      PEP_3          : UOTGHS_HSTIDR_PEP_3_Field := 16#0#;
      --  Write-only. Pipe 4 Interrupt Disable
      PEP_4          : UOTGHS_HSTIDR_PEP_4_Field := 16#0#;
      --  Write-only. Pipe 5 Interrupt Disable
      PEP_5          : UOTGHS_HSTIDR_PEP_5_Field := 16#0#;
      --  Write-only. Pipe 6 Interrupt Disable
      PEP_6          : UOTGHS_HSTIDR_PEP_6_Field := 16#0#;
      --  Write-only. Pipe 7 Interrupt Disable
      PEP_7          : UOTGHS_HSTIDR_PEP_7_Field := 16#0#;
      --  Write-only. Pipe 8 Interrupt Disable
      PEP_8          : UOTGHS_HSTIDR_PEP_8_Field := 16#0#;
      --  Write-only. Pipe 9 Interrupt Disable
      PEP_9          : UOTGHS_HSTIDR_PEP_9_Field := 16#0#;
      --  unspecified
      Reserved_18_24 : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Disable
      DMA_1          : UOTGHS_HSTIDR_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Disable
      DMA_2          : UOTGHS_HSTIDR_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Disable
      DMA_3          : UOTGHS_HSTIDR_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Disable
      DMA_4          : UOTGHS_HSTIDR_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Disable
      DMA_5          : UOTGHS_HSTIDR_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Disable
      DMA_6          : UOTGHS_HSTIDR_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTIDR_Register use record
      DCONNIEC       at 0 range 0 .. 0;
      DDISCIEC       at 0 range 1 .. 1;
      RSTIEC         at 0 range 2 .. 2;
      RSMEDIEC       at 0 range 3 .. 3;
      RXRSMIEC       at 0 range 4 .. 4;
      HSOFIEC        at 0 range 5 .. 5;
      HWUPIEC        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PEP_0          at 0 range 8 .. 8;
      PEP_1          at 0 range 9 .. 9;
      PEP_2          at 0 range 10 .. 10;
      PEP_3          at 0 range 11 .. 11;
      PEP_4          at 0 range 12 .. 12;
      PEP_5          at 0 range 13 .. 13;
      PEP_6          at 0 range 14 .. 14;
      PEP_7          at 0 range 15 .. 15;
      PEP_8          at 0 range 16 .. 16;
      PEP_9          at 0 range 17 .. 17;
      Reserved_18_24 at 0 range 18 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTIER_DCONNIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DDISCIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_RSTIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_RSMEDIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_RXRSMIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_HSOFIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_HWUPIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_0_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_6_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_7_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_8_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_PEP_9_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_1_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_2_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_3_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_4_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_5_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTIER_DMA_6_Field is Interfaces.SAM3x8e.Bit;

   --  Host Global Interrupt Enable Register
   type UOTGHS_HSTIER_Register is record
      --  Write-only. Device Connection Interrupt Enable
      DCONNIES       : UOTGHS_HSTIER_DCONNIES_Field := 16#0#;
      --  Write-only. Device Disconnection Interrupt Enable
      DDISCIES       : UOTGHS_HSTIER_DDISCIES_Field := 16#0#;
      --  Write-only. USB Reset Sent Interrupt Enable
      RSTIES         : UOTGHS_HSTIER_RSTIES_Field := 16#0#;
      --  Write-only. Downstream Resume Sent Interrupt Enable
      RSMEDIES       : UOTGHS_HSTIER_RSMEDIES_Field := 16#0#;
      --  Write-only. Upstream Resume Received Interrupt Enable
      RXRSMIES       : UOTGHS_HSTIER_RXRSMIES_Field := 16#0#;
      --  Write-only. Host Start of Frame Interrupt Enable
      HSOFIES        : UOTGHS_HSTIER_HSOFIES_Field := 16#0#;
      --  Write-only. Host Wake-Up Interrupt Enable
      HWUPIES        : UOTGHS_HSTIER_HWUPIES_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. Pipe 0 Interrupt Enable
      PEP_0          : UOTGHS_HSTIER_PEP_0_Field := 16#0#;
      --  Write-only. Pipe 1 Interrupt Enable
      PEP_1          : UOTGHS_HSTIER_PEP_1_Field := 16#0#;
      --  Write-only. Pipe 2 Interrupt Enable
      PEP_2          : UOTGHS_HSTIER_PEP_2_Field := 16#0#;
      --  Write-only. Pipe 3 Interrupt Enable
      PEP_3          : UOTGHS_HSTIER_PEP_3_Field := 16#0#;
      --  Write-only. Pipe 4 Interrupt Enable
      PEP_4          : UOTGHS_HSTIER_PEP_4_Field := 16#0#;
      --  Write-only. Pipe 5 Interrupt Enable
      PEP_5          : UOTGHS_HSTIER_PEP_5_Field := 16#0#;
      --  Write-only. Pipe 6 Interrupt Enable
      PEP_6          : UOTGHS_HSTIER_PEP_6_Field := 16#0#;
      --  Write-only. Pipe 7 Interrupt Enable
      PEP_7          : UOTGHS_HSTIER_PEP_7_Field := 16#0#;
      --  Write-only. Pipe 8 Interrupt Enable
      PEP_8          : UOTGHS_HSTIER_PEP_8_Field := 16#0#;
      --  Write-only. Pipe 9 Interrupt Enable
      PEP_9          : UOTGHS_HSTIER_PEP_9_Field := 16#0#;
      --  unspecified
      Reserved_18_24 : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Write-only. DMA Channel 1 Interrupt Enable
      DMA_1          : UOTGHS_HSTIER_DMA_1_Field := 16#0#;
      --  Write-only. DMA Channel 2 Interrupt Enable
      DMA_2          : UOTGHS_HSTIER_DMA_2_Field := 16#0#;
      --  Write-only. DMA Channel 3 Interrupt Enable
      DMA_3          : UOTGHS_HSTIER_DMA_3_Field := 16#0#;
      --  Write-only. DMA Channel 4 Interrupt Enable
      DMA_4          : UOTGHS_HSTIER_DMA_4_Field := 16#0#;
      --  Write-only. DMA Channel 5 Interrupt Enable
      DMA_5          : UOTGHS_HSTIER_DMA_5_Field := 16#0#;
      --  Write-only. DMA Channel 6 Interrupt Enable
      DMA_6          : UOTGHS_HSTIER_DMA_6_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTIER_Register use record
      DCONNIES       at 0 range 0 .. 0;
      DDISCIES       at 0 range 1 .. 1;
      RSTIES         at 0 range 2 .. 2;
      RSMEDIES       at 0 range 3 .. 3;
      RXRSMIES       at 0 range 4 .. 4;
      HSOFIES        at 0 range 5 .. 5;
      HWUPIES        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PEP_0          at 0 range 8 .. 8;
      PEP_1          at 0 range 9 .. 9;
      PEP_2          at 0 range 10 .. 10;
      PEP_3          at 0 range 11 .. 11;
      PEP_4          at 0 range 12 .. 12;
      PEP_5          at 0 range 13 .. 13;
      PEP_6          at 0 range 14 .. 14;
      PEP_7          at 0 range 15 .. 15;
      PEP_8          at 0 range 16 .. 16;
      PEP_9          at 0 range 17 .. 17;
      Reserved_18_24 at 0 range 18 .. 24;
      DMA_1          at 0 range 25 .. 25;
      DMA_2          at 0 range 26 .. 26;
      DMA_3          at 0 range 27 .. 27;
      DMA_4          at 0 range 28 .. 28;
      DMA_5          at 0 range 29 .. 29;
      DMA_6          at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  UOTGHS_HSTPIP_PEN array element
   subtype UOTGHS_HSTPIP_PEN_Element is Interfaces.SAM3x8e.Bit;

   --  UOTGHS_HSTPIP_PEN array
   type UOTGHS_HSTPIP_PEN_Field_Array is array (0 .. 8)
     of UOTGHS_HSTPIP_PEN_Element
     with Component_Size => 1, Size => 9;

   --  Type definition for UOTGHS_HSTPIP_PEN
   type UOTGHS_HSTPIP_PEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PEN as a value
            Val : Interfaces.SAM3x8e.UInt9;
         when True =>
            --  PEN as an array
            Arr : UOTGHS_HSTPIP_PEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for UOTGHS_HSTPIP_PEN_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  UOTGHS_HSTPIP_PRST array element
   subtype UOTGHS_HSTPIP_PRST_Element is Interfaces.SAM3x8e.Bit;

   --  UOTGHS_HSTPIP_PRST array
   type UOTGHS_HSTPIP_PRST_Field_Array is array (0 .. 8)
     of UOTGHS_HSTPIP_PRST_Element
     with Component_Size => 1, Size => 9;

   --  Type definition for UOTGHS_HSTPIP_PRST
   type UOTGHS_HSTPIP_PRST_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PRST as a value
            Val : Interfaces.SAM3x8e.UInt9;
         when True =>
            --  PRST as an array
            Arr : UOTGHS_HSTPIP_PRST_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for UOTGHS_HSTPIP_PRST_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  Host Pipe Register
   type UOTGHS_HSTPIP_Register is record
      --  Pipe 0 Enable
      PEN            : UOTGHS_HSTPIP_PEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_9_15  : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Pipe 0 Reset
      PRST           : UOTGHS_HSTPIP_PRST_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_25_31 : Interfaces.SAM3x8e.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIP_Register use record
      PEN            at 0 range 0 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      PRST           at 0 range 16 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype UOTGHS_HSTFNUM_MFNUM_Field is Interfaces.SAM3x8e.UInt3;
   subtype UOTGHS_HSTFNUM_FNUM_Field is Interfaces.SAM3x8e.UInt11;
   subtype UOTGHS_HSTFNUM_FLENHIGH_Field is Interfaces.SAM3x8e.Byte;

   --  Host Frame Number Register
   type UOTGHS_HSTFNUM_Register is record
      --  Micro Frame Number
      MFNUM          : UOTGHS_HSTFNUM_MFNUM_Field := 16#0#;
      --  Frame Number
      FNUM           : UOTGHS_HSTFNUM_FNUM_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Frame Length
      FLENHIGH       : UOTGHS_HSTFNUM_FLENHIGH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM3x8e.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTFNUM_Register use record
      MFNUM          at 0 range 0 .. 2;
      FNUM           at 0 range 3 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      FLENHIGH       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype UOTGHS_HSTADDR1_HSTADDRP0_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR1_HSTADDRP1_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR1_HSTADDRP2_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR1_HSTADDRP3_Field is Interfaces.SAM3x8e.UInt7;

   --  Host Address 1 Register
   type UOTGHS_HSTADDR1_Register is record
      --  USB Host Address
      HSTADDRP0      : UOTGHS_HSTADDR1_HSTADDRP0_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP1      : UOTGHS_HSTADDR1_HSTADDRP1_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP2      : UOTGHS_HSTADDR1_HSTADDRP2_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP3      : UOTGHS_HSTADDR1_HSTADDRP3_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTADDR1_Register use record
      HSTADDRP0      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      HSTADDRP1      at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HSTADDRP2      at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      HSTADDRP3      at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTADDR2_HSTADDRP4_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR2_HSTADDRP5_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR2_HSTADDRP6_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR2_HSTADDRP7_Field is Interfaces.SAM3x8e.UInt7;

   --  Host Address 2 Register
   type UOTGHS_HSTADDR2_Register is record
      --  USB Host Address
      HSTADDRP4      : UOTGHS_HSTADDR2_HSTADDRP4_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP5      : UOTGHS_HSTADDR2_HSTADDRP5_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP6      : UOTGHS_HSTADDR2_HSTADDRP6_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP7      : UOTGHS_HSTADDR2_HSTADDRP7_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTADDR2_Register use record
      HSTADDRP4      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      HSTADDRP5      at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HSTADDRP6      at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      HSTADDRP7      at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype UOTGHS_HSTADDR3_HSTADDRP8_Field is Interfaces.SAM3x8e.UInt7;
   subtype UOTGHS_HSTADDR3_HSTADDRP9_Field is Interfaces.SAM3x8e.UInt7;

   --  Host Address 3 Register
   type UOTGHS_HSTADDR3_Register is record
      --  USB Host Address
      HSTADDRP8      : UOTGHS_HSTADDR3_HSTADDRP8_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  USB Host Address
      HSTADDRP9      : UOTGHS_HSTADDR3_HSTADDRP9_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : Interfaces.SAM3x8e.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTADDR3_Register use record
      HSTADDRP8      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      HSTADDRP9      at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype UOTGHS_HSTPIPCFG_ALLOC_Field is Interfaces.SAM3x8e.Bit;

   --  Pipe Banks
   type HSTPIPCFG_PBK_Field is
     (--  Single-bank pipe
      Val_1_Bank,
      --  Double-bank pipe
      Val_2_Bank,
      --  Triple-bank pipe
      Val_3_Bank)
     with Size => 2;
   for HSTPIPCFG_PBK_Field use
     (Val_1_Bank => 0,
      Val_2_Bank => 1,
      Val_3_Bank => 2);

   --  Pipe Size
   type HSTPIPCFG_PSIZE_Field is
     (--  8 bytes
      Val_8_Byte,
      --  16 bytes
      Val_16_Byte,
      --  32 bytes
      Val_32_Byte,
      --  64 bytes
      Val_64_Byte,
      --  128 bytes
      Val_128_Byte,
      --  256 bytes
      Val_256_Byte,
      --  512 bytes
      Val_512_Byte,
      --  1024 bytes
      Val_1024_Byte)
     with Size => 3;
   for HSTPIPCFG_PSIZE_Field use
     (Val_8_Byte => 0,
      Val_16_Byte => 1,
      Val_32_Byte => 2,
      Val_64_Byte => 3,
      Val_128_Byte => 4,
      Val_256_Byte => 5,
      Val_512_Byte => 6,
      Val_1024_Byte => 7);

   --  Pipe Token
   type HSTPIPCFG_PTOKEN_Field is
     (--  SETUP
      Setup,
      --  IN
      In_k,
      --  OUT
      Out_k)
     with Size => 2;
   for HSTPIPCFG_PTOKEN_Field use
     (Setup => 0,
      In_k => 1,
      Out_k => 2);

   subtype UOTGHS_HSTPIPCFG_AUTOSW_Field is Interfaces.SAM3x8e.Bit;

   --  Pipe Type
   type HSTPIPCFG_PTYPE_Field is
     (--  Control
      Ctrl,
      --  Isochronous
      Iso,
      --  Bulk
      Blk,
      --  Interrupt
      Intrpt)
     with Size => 2;
   for HSTPIPCFG_PTYPE_Field use
     (Ctrl => 0,
      Iso => 1,
      Blk => 2,
      Intrpt => 3);

   subtype UOTGHS_HSTPIPCFG_PEPNUM_Field is Interfaces.SAM3x8e.UInt4;
   subtype UOTGHS_HSTPIPCFG_INTFRQ_Field is Interfaces.SAM3x8e.Byte;

   --  Host Pipe Configuration Register (n = 0)
   type UOTGHS_HSTPIPCFG_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Pipe Memory Allocate
      ALLOC          : UOTGHS_HSTPIPCFG_ALLOC_Field := 16#0#;
      --  Pipe Banks
      PBK            : HSTPIPCFG_PBK_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Val_1_Bank;
      --  Pipe Size
      PSIZE          : HSTPIPCFG_PSIZE_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Val_8_Byte;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Pipe Token
      PTOKEN         : HSTPIPCFG_PTOKEN_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Setup;
      --  Automatic Switch
      AUTOSW         : UOTGHS_HSTPIPCFG_AUTOSW_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Pipe Type
      PTYPE          : HSTPIPCFG_PTYPE_Field :=
                        Interfaces.SAM3x8e.UOTGHS.Ctrl;
      --  unspecified
      Reserved_14_15 : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Pipe Endpoint Number
      PEPNUM         : UOTGHS_HSTPIPCFG_PEPNUM_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Pipe Interrupt Request Frequency
      INTFRQ         : UOTGHS_HSTPIPCFG_INTFRQ_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPCFG_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      ALLOC          at 0 range 1 .. 1;
      PBK            at 0 range 2 .. 3;
      PSIZE          at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PTOKEN         at 0 range 8 .. 9;
      AUTOSW         at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      PTYPE          at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      PEPNUM         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      INTFRQ         at 0 range 24 .. 31;
   end record;

   --  Host Pipe Configuration Register (n = 0)
   type UOTGHS_HSTPIPCFG_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPCFG_Register;

   subtype UOTGHS_HSTPIPISR_RXINI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_TXOUTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_TXSTPI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_PERRI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_NAKEDI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_OVERFI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_RXSTALLDI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_SHORTPACKETI_Field is Interfaces.SAM3x8e.Bit;

   --  Data Toggle Sequence
   type HSTPIPISR_DTSEQ_Field is
     (--  Data0 toggle sequence
      Data0,
      --  Data1 toggle sequence
      Data1)
     with Size => 2;
   for HSTPIPISR_DTSEQ_Field use
     (Data0 => 0,
      Data1 => 1);

   --  Number of Busy Banks
   type HSTPIPISR_NBUSYBK_Field is
     (--  0 busy bank (all banks free)
      Val_0_Busy,
      --  1 busy bank
      Val_1_Busy,
      --  2 busy banks
      Val_2_Busy,
      --  3 busy banks
      Val_3_Busy)
     with Size => 2;
   for HSTPIPISR_NBUSYBK_Field use
     (Val_0_Busy => 0,
      Val_1_Busy => 1,
      Val_2_Busy => 2,
      Val_3_Busy => 3);

   --  Current Bank
   type HSTPIPISR_CURRBK_Field is
     (--  Current bank is bank0
      Bank0,
      --  Current bank is bank1
      Bank1,
      --  Current bank is bank2
      Bank2)
     with Size => 2;
   for HSTPIPISR_CURRBK_Field use
     (Bank0 => 0,
      Bank1 => 1,
      Bank2 => 2);

   subtype UOTGHS_HSTPIPISR_RWALL_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_CFGOK_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPISR_PBYCT_Field is Interfaces.SAM3x8e.UInt11;

   --  Host Pipe Status Register (n = 0)
   type UOTGHS_HSTPIPISR_Register is record
      --  Read-only. Received IN Data Interrupt
      RXINI          : UOTGHS_HSTPIPISR_RXINI_Field;
      --  Read-only. Transmitted OUT Data Interrupt
      TXOUTI         : UOTGHS_HSTPIPISR_TXOUTI_Field;
      --  Read-only. Transmitted SETUP Interrupt
      TXSTPI         : UOTGHS_HSTPIPISR_TXSTPI_Field;
      --  Read-only. Pipe Error Interrupt
      PERRI          : UOTGHS_HSTPIPISR_PERRI_Field;
      --  Read-only. NAKed Interrupt
      NAKEDI         : UOTGHS_HSTPIPISR_NAKEDI_Field;
      --  Read-only. Overflow Interrupt
      OVERFI         : UOTGHS_HSTPIPISR_OVERFI_Field;
      --  Read-only. Received STALLed Interrupt
      RXSTALLDI      : UOTGHS_HSTPIPISR_RXSTALLDI_Field;
      --  Read-only. Short Packet Interrupt
      SHORTPACKETI   : UOTGHS_HSTPIPISR_SHORTPACKETI_Field;
      --  Read-only. Data Toggle Sequence
      DTSEQ          : HSTPIPISR_DTSEQ_Field;
      --  unspecified
      Reserved_10_11 : Interfaces.SAM3x8e.UInt2;
      --  Read-only. Number of Busy Banks
      NBUSYBK        : HSTPIPISR_NBUSYBK_Field;
      --  Read-only. Current Bank
      CURRBK         : HSTPIPISR_CURRBK_Field;
      --  Read-only. Read-write Allowed
      RWALL          : UOTGHS_HSTPIPISR_RWALL_Field;
      --  unspecified
      Reserved_17_17 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Configuration OK Status
      CFGOK          : UOTGHS_HSTPIPISR_CFGOK_Field;
      --  unspecified
      Reserved_19_19 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Pipe Byte Count
      PBYCT          : UOTGHS_HSTPIPISR_PBYCT_Field;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM3x8e.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPISR_Register use record
      RXINI          at 0 range 0 .. 0;
      TXOUTI         at 0 range 1 .. 1;
      TXSTPI         at 0 range 2 .. 2;
      PERRI          at 0 range 3 .. 3;
      NAKEDI         at 0 range 4 .. 4;
      OVERFI         at 0 range 5 .. 5;
      RXSTALLDI      at 0 range 6 .. 6;
      SHORTPACKETI   at 0 range 7 .. 7;
      DTSEQ          at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      NBUSYBK        at 0 range 12 .. 13;
      CURRBK         at 0 range 14 .. 15;
      RWALL          at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      CFGOK          at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      PBYCT          at 0 range 20 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Host Pipe Status Register (n = 0)
   type UOTGHS_HSTPIPISR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPISR_Register;

   subtype UOTGHS_HSTPIPICR_RXINIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_TXOUTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_TXSTPIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_NAKEDIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_OVERFIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_RXSTALLDIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPICR_SHORTPACKETIC_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe Clear Register (n = 0)
   type UOTGHS_HSTPIPICR_Register is record
      --  Write-only. Received IN Data Interrupt Clear
      RXINIC        : UOTGHS_HSTPIPICR_RXINIC_Field := 16#0#;
      --  Write-only. Transmitted OUT Data Interrupt Clear
      TXOUTIC       : UOTGHS_HSTPIPICR_TXOUTIC_Field := 16#0#;
      --  Write-only. Transmitted SETUP Interrupt Clear
      TXSTPIC       : UOTGHS_HSTPIPICR_TXSTPIC_Field := 16#0#;
      --  unspecified
      Reserved_3_3  : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. NAKed Interrupt Clear
      NAKEDIC       : UOTGHS_HSTPIPICR_NAKEDIC_Field := 16#0#;
      --  Write-only. Overflow Interrupt Clear
      OVERFIC       : UOTGHS_HSTPIPICR_OVERFIC_Field := 16#0#;
      --  Write-only. Received STALLed Interrupt Clear
      RXSTALLDIC    : UOTGHS_HSTPIPICR_RXSTALLDIC_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Clear
      SHORTPACKETIC : UOTGHS_HSTPIPICR_SHORTPACKETIC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM3x8e.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPICR_Register use record
      RXINIC        at 0 range 0 .. 0;
      TXOUTIC       at 0 range 1 .. 1;
      TXSTPIC       at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      NAKEDIC       at 0 range 4 .. 4;
      OVERFIC       at 0 range 5 .. 5;
      RXSTALLDIC    at 0 range 6 .. 6;
      SHORTPACKETIC at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Host Pipe Clear Register (n = 0)
   type UOTGHS_HSTPIPICR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPICR_Register;

   subtype UOTGHS_HSTPIPIFR_RXINIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_TXOUTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_TXSTPIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_PERRIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_NAKEDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_OVERFIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_RXSTALLDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_SHORTPACKETIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIFR_NBUSYBKS_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe Set Register (n = 0)
   type UOTGHS_HSTPIPIFR_Register is record
      --  Write-only. Received IN Data Interrupt Set
      RXINIS         : UOTGHS_HSTPIPIFR_RXINIS_Field := 16#0#;
      --  Write-only. Transmitted OUT Data Interrupt Set
      TXOUTIS        : UOTGHS_HSTPIPIFR_TXOUTIS_Field := 16#0#;
      --  Write-only. Transmitted SETUP Interrupt Set
      TXSTPIS        : UOTGHS_HSTPIPIFR_TXSTPIS_Field := 16#0#;
      --  Write-only. Pipe Error Interrupt Set
      PERRIS         : UOTGHS_HSTPIPIFR_PERRIS_Field := 16#0#;
      --  Write-only. NAKed Interrupt Set
      NAKEDIS        : UOTGHS_HSTPIPIFR_NAKEDIS_Field := 16#0#;
      --  Write-only. Overflow Interrupt Set
      OVERFIS        : UOTGHS_HSTPIPIFR_OVERFIS_Field := 16#0#;
      --  Write-only. Received STALLed Interrupt Set
      RXSTALLDIS     : UOTGHS_HSTPIPIFR_RXSTALLDIS_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Set
      SHORTPACKETIS  : UOTGHS_HSTPIPIFR_SHORTPACKETIS_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Set
      NBUSYBKS       : UOTGHS_HSTPIPIFR_NBUSYBKS_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.SAM3x8e.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPIFR_Register use record
      RXINIS         at 0 range 0 .. 0;
      TXOUTIS        at 0 range 1 .. 1;
      TXSTPIS        at 0 range 2 .. 2;
      PERRIS         at 0 range 3 .. 3;
      NAKEDIS        at 0 range 4 .. 4;
      OVERFIS        at 0 range 5 .. 5;
      RXSTALLDIS     at 0 range 6 .. 6;
      SHORTPACKETIS  at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKS       at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  Host Pipe Set Register (n = 0)
   type UOTGHS_HSTPIPIFR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPIFR_Register;

   subtype UOTGHS_HSTPIPIMR_RXINE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_TXOUTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_TXSTPE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_PERRE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_NAKEDE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_OVERFIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_RXSTALLDE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_SHORTPACKETIE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_NBUSYBKE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_FIFOCON_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_PDISHDMA_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_PFREEZE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIMR_RSTDT_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe Mask Register (n = 0)
   type UOTGHS_HSTPIPIMR_Register is record
      --  Read-only. Received IN Data Interrupt Enable
      RXINE          : UOTGHS_HSTPIPIMR_RXINE_Field;
      --  Read-only. Transmitted OUT Data Interrupt Enable
      TXOUTE         : UOTGHS_HSTPIPIMR_TXOUTE_Field;
      --  Read-only. Transmitted SETUP Interrupt Enable
      TXSTPE         : UOTGHS_HSTPIPIMR_TXSTPE_Field;
      --  Read-only. Pipe Error Interrupt Enable
      PERRE          : UOTGHS_HSTPIPIMR_PERRE_Field;
      --  Read-only. NAKed Interrupt Enable
      NAKEDE         : UOTGHS_HSTPIPIMR_NAKEDE_Field;
      --  Read-only. Overflow Interrupt Enable
      OVERFIE        : UOTGHS_HSTPIPIMR_OVERFIE_Field;
      --  Read-only. Received STALLed Interrupt Enable
      RXSTALLDE      : UOTGHS_HSTPIPIMR_RXSTALLDE_Field;
      --  Read-only. Short Packet Interrupt Enable
      SHORTPACKETIE  : UOTGHS_HSTPIPIMR_SHORTPACKETIE_Field;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4;
      --  Read-only. Number of Busy Banks Interrupt Enable
      NBUSYBKE       : UOTGHS_HSTPIPIMR_NBUSYBKE_Field;
      --  unspecified
      Reserved_13_13 : Interfaces.SAM3x8e.Bit;
      --  Read-only. FIFO Control
      FIFOCON        : UOTGHS_HSTPIPIMR_FIFOCON_Field;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit;
      --  Read-only. Pipe Interrupts Disable HDMA Request Enable
      PDISHDMA       : UOTGHS_HSTPIPIMR_PDISHDMA_Field;
      --  Read-only. Pipe Freeze
      PFREEZE        : UOTGHS_HSTPIPIMR_PFREEZE_Field;
      --  Read-only. Reset Data Toggle
      RSTDT          : UOTGHS_HSTPIPIMR_RSTDT_Field;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM3x8e.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPIMR_Register use record
      RXINE          at 0 range 0 .. 0;
      TXOUTE         at 0 range 1 .. 1;
      TXSTPE         at 0 range 2 .. 2;
      PERRE          at 0 range 3 .. 3;
      NAKEDE         at 0 range 4 .. 4;
      OVERFIE        at 0 range 5 .. 5;
      RXSTALLDE      at 0 range 6 .. 6;
      SHORTPACKETIE  at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKE       at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      FIFOCON        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PDISHDMA       at 0 range 16 .. 16;
      PFREEZE        at 0 range 17 .. 17;
      RSTDT          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Host Pipe Mask Register (n = 0)
   type UOTGHS_HSTPIPIMR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPIMR_Register;

   subtype UOTGHS_HSTPIPIER_RXINES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_TXOUTES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_TXSTPES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_PERRES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_NAKEDES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_OVERFIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_RXSTALLDES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_SHORTPACKETIES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_NBUSYBKES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_PDISHDMAS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_PFREEZES_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIER_RSTDTS_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe Enable Register (n = 0)
   type UOTGHS_HSTPIPIER_Register is record
      --  Write-only. Received IN Data Interrupt Enable
      RXINES         : UOTGHS_HSTPIPIER_RXINES_Field := 16#0#;
      --  Write-only. Transmitted OUT Data Interrupt Enable
      TXOUTES        : UOTGHS_HSTPIPIER_TXOUTES_Field := 16#0#;
      --  Write-only. Transmitted SETUP Interrupt Enable
      TXSTPES        : UOTGHS_HSTPIPIER_TXSTPES_Field := 16#0#;
      --  Write-only. Pipe Error Interrupt Enable
      PERRES         : UOTGHS_HSTPIPIER_PERRES_Field := 16#0#;
      --  Write-only. NAKed Interrupt Enable
      NAKEDES        : UOTGHS_HSTPIPIER_NAKEDES_Field := 16#0#;
      --  Write-only. Overflow Interrupt Enable
      OVERFIES       : UOTGHS_HSTPIPIER_OVERFIES_Field := 16#0#;
      --  Write-only. Received STALLed Interrupt Enable
      RXSTALLDES     : UOTGHS_HSTPIPIER_RXSTALLDES_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Enable
      SHORTPACKETIES : UOTGHS_HSTPIPIER_SHORTPACKETIES_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Enable
      NBUSYBKES      : UOTGHS_HSTPIPIER_NBUSYBKES_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Write-only. Pipe Interrupts Disable HDMA Request Enable
      PDISHDMAS      : UOTGHS_HSTPIPIER_PDISHDMAS_Field := 16#0#;
      --  Write-only. Pipe Freeze Enable
      PFREEZES       : UOTGHS_HSTPIPIER_PFREEZES_Field := 16#0#;
      --  Write-only. Reset Data Toggle Enable
      RSTDTS         : UOTGHS_HSTPIPIER_RSTDTS_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM3x8e.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPIER_Register use record
      RXINES         at 0 range 0 .. 0;
      TXOUTES        at 0 range 1 .. 1;
      TXSTPES        at 0 range 2 .. 2;
      PERRES         at 0 range 3 .. 3;
      NAKEDES        at 0 range 4 .. 4;
      OVERFIES       at 0 range 5 .. 5;
      RXSTALLDES     at 0 range 6 .. 6;
      SHORTPACKETIES at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKES      at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      PDISHDMAS      at 0 range 16 .. 16;
      PFREEZES       at 0 range 17 .. 17;
      RSTDTS         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Host Pipe Enable Register (n = 0)
   type UOTGHS_HSTPIPIER_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPIER_Register;

   subtype UOTGHS_HSTPIPIDR_RXINEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_TXOUTEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_TXSTPEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_PERREC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_NAKEDEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_OVERFIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_RXSTALLDEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_SHORTPACKETIEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_NBUSYBKEC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_FIFOCONC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_PDISHDMAC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPIDR_PFREEZEC_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe Disable Register (n = 0)
   type UOTGHS_HSTPIPIDR_Register is record
      --  Write-only. Received IN Data Interrupt Disable
      RXINEC         : UOTGHS_HSTPIPIDR_RXINEC_Field := 16#0#;
      --  Write-only. Transmitted OUT Data Interrupt Disable
      TXOUTEC        : UOTGHS_HSTPIPIDR_TXOUTEC_Field := 16#0#;
      --  Write-only. Transmitted SETUP Interrupt Disable
      TXSTPEC        : UOTGHS_HSTPIPIDR_TXSTPEC_Field := 16#0#;
      --  Write-only. Pipe Error Interrupt Disable
      PERREC         : UOTGHS_HSTPIPIDR_PERREC_Field := 16#0#;
      --  Write-only. NAKed Interrupt Disable
      NAKEDEC        : UOTGHS_HSTPIPIDR_NAKEDEC_Field := 16#0#;
      --  Write-only. Overflow Interrupt Disable
      OVERFIEC       : UOTGHS_HSTPIPIDR_OVERFIEC_Field := 16#0#;
      --  Write-only. Received STALLed Interrupt Disable
      RXSTALLDEC     : UOTGHS_HSTPIPIDR_RXSTALLDEC_Field := 16#0#;
      --  Write-only. Short Packet Interrupt Disable
      SHORTPACKETIEC : UOTGHS_HSTPIPIDR_SHORTPACKETIEC_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  Write-only. Number of Busy Banks Disable
      NBUSYBKEC      : UOTGHS_HSTPIPIDR_NBUSYBKEC_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. FIFO Control Disable
      FIFOCONC       : UOTGHS_HSTPIPIDR_FIFOCONC_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. Pipe Interrupts Disable HDMA Request Disable
      PDISHDMAC      : UOTGHS_HSTPIPIDR_PDISHDMAC_Field := 16#0#;
      --  Write-only. Pipe Freeze Disable
      PFREEZEC       : UOTGHS_HSTPIPIDR_PFREEZEC_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : Interfaces.SAM3x8e.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPIDR_Register use record
      RXINEC         at 0 range 0 .. 0;
      TXOUTEC        at 0 range 1 .. 1;
      TXSTPEC        at 0 range 2 .. 2;
      PERREC         at 0 range 3 .. 3;
      NAKEDEC        at 0 range 4 .. 4;
      OVERFIEC       at 0 range 5 .. 5;
      RXSTALLDEC     at 0 range 6 .. 6;
      SHORTPACKETIEC at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      NBUSYBKEC      at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      FIFOCONC       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PDISHDMAC      at 0 range 16 .. 16;
      PFREEZEC       at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Host Pipe Disable Register (n = 0)
   type UOTGHS_HSTPIPIDR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPIDR_Register;

   subtype UOTGHS_HSTPIPINRQ_INRQ_Field is Interfaces.SAM3x8e.Byte;
   subtype UOTGHS_HSTPIPINRQ_INMODE_Field is Interfaces.SAM3x8e.Bit;

   --  Host Pipe IN Request Register (n = 0)
   type UOTGHS_HSTPIPINRQ_Register is record
      --  IN Request Number before Freeze
      INRQ          : UOTGHS_HSTPIPINRQ_INRQ_Field := 16#0#;
      --  IN Request Mode
      INMODE        : UOTGHS_HSTPIPINRQ_INMODE_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.SAM3x8e.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPINRQ_Register use record
      INRQ          at 0 range 0 .. 7;
      INMODE        at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Host Pipe IN Request Register (n = 0)
   type UOTGHS_HSTPIPINRQ_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPINRQ_Register;

   subtype UOTGHS_HSTPIPERR_DATATGL_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPERR_DATAPID_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPERR_PID_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPERR_TIMEOUT_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPERR_CRC16_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_HSTPIPERR_COUNTER_Field is Interfaces.SAM3x8e.UInt2;

   --  Host Pipe Error Register (n = 0)
   type UOTGHS_HSTPIPERR_Register is record
      --  Data Toggle Error
      DATATGL       : UOTGHS_HSTPIPERR_DATATGL_Field := 16#0#;
      --  Data PID Error
      DATAPID       : UOTGHS_HSTPIPERR_DATAPID_Field := 16#0#;
      --  PID Error
      PID           : UOTGHS_HSTPIPERR_PID_Field := 16#0#;
      --  Time-Out Error
      TIMEOUT       : UOTGHS_HSTPIPERR_TIMEOUT_Field := 16#0#;
      --  CRC16 Error
      CRC16         : UOTGHS_HSTPIPERR_CRC16_Field := 16#0#;
      --  Error Counter
      COUNTER       : UOTGHS_HSTPIPERR_COUNTER_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : Interfaces.SAM3x8e.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_HSTPIPERR_Register use record
      DATATGL       at 0 range 0 .. 0;
      DATAPID       at 0 range 1 .. 1;
      PID           at 0 range 2 .. 2;
      TIMEOUT       at 0 range 3 .. 3;
      CRC16         at 0 range 4 .. 4;
      COUNTER       at 0 range 5 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Host Pipe Error Register (n = 0)
   type UOTGHS_HSTPIPERR_Registers is array (0 .. 9)
     of UOTGHS_HSTPIPERR_Register;

   subtype HSTDMACONTROL_CHANN_ENB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_LDNXT_DSC_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_END_TR_EN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_END_B_EN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_END_TR_IT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_END_BUFFIT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_DESC_LD_IT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_BURST_LCK_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMACONTROL_BUFF_LENGTH_Field is Interfaces.SAM3x8e.UInt16;

   --  Host DMA Channel Control Register (n = 1)
   type HSTDMACONTROL_Register is record
      --  Channel Enable Command
      CHANN_ENB     : HSTDMACONTROL_CHANN_ENB_Field := 16#0#;
      --  Load Next Channel Transfer Descriptor Enable Command
      LDNXT_DSC     : HSTDMACONTROL_LDNXT_DSC_Field := 16#0#;
      --  End of Transfer Enable (Control)
      END_TR_EN     : HSTDMACONTROL_END_TR_EN_Field := 16#0#;
      --  End of Buffer Enable Control
      END_B_EN      : HSTDMACONTROL_END_B_EN_Field := 16#0#;
      --  End of Transfer Interrupt Enable
      END_TR_IT     : HSTDMACONTROL_END_TR_IT_Field := 16#0#;
      --  End of Buffer Interrupt Enable
      END_BUFFIT    : HSTDMACONTROL_END_BUFFIT_Field := 16#0#;
      --  Descriptor Loaded Interrupt Enable
      DESC_LD_IT    : HSTDMACONTROL_DESC_LD_IT_Field := 16#0#;
      --  Burst Lock Enable
      BURST_LCK     : HSTDMACONTROL_BURST_LCK_Field := 16#0#;
      --  unspecified
      Reserved_8_15 : Interfaces.SAM3x8e.Byte := 16#0#;
      --  Buffer Byte Length (Write-only)
      BUFF_LENGTH   : HSTDMACONTROL_BUFF_LENGTH_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSTDMACONTROL_Register use record
      CHANN_ENB     at 0 range 0 .. 0;
      LDNXT_DSC     at 0 range 1 .. 1;
      END_TR_EN     at 0 range 2 .. 2;
      END_B_EN      at 0 range 3 .. 3;
      END_TR_IT     at 0 range 4 .. 4;
      END_BUFFIT    at 0 range 5 .. 5;
      DESC_LD_IT    at 0 range 6 .. 6;
      BURST_LCK     at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      BUFF_LENGTH   at 0 range 16 .. 31;
   end record;

   subtype HSTDMASTATUS_CHANN_ENB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMASTATUS_CHANN_ACT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMASTATUS_END_TR_ST_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMASTATUS_END_BF_ST_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMASTATUS_DESC_LDST_Field is Interfaces.SAM3x8e.Bit;
   subtype HSTDMASTATUS_BUFF_COUNT_Field is Interfaces.SAM3x8e.UInt16;

   --  Host DMA Channel Status Register (n = 1)
   type HSTDMASTATUS_Register is record
      --  Channel Enable Status
      CHANN_ENB     : HSTDMASTATUS_CHANN_ENB_Field := 16#0#;
      --  Channel Active Status
      CHANN_ACT     : HSTDMASTATUS_CHANN_ACT_Field := 16#0#;
      --  unspecified
      Reserved_2_3  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  End of Channel Transfer Status
      END_TR_ST     : HSTDMASTATUS_END_TR_ST_Field := 16#0#;
      --  End of Channel Buffer Status
      END_BF_ST     : HSTDMASTATUS_END_BF_ST_Field := 16#0#;
      --  Descriptor Loaded Status
      DESC_LDST     : HSTDMASTATUS_DESC_LDST_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : Interfaces.SAM3x8e.UInt9 := 16#0#;
      --  Buffer Byte Count
      BUFF_COUNT    : HSTDMASTATUS_BUFF_COUNT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSTDMASTATUS_Register use record
      CHANN_ENB     at 0 range 0 .. 0;
      CHANN_ACT     at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      END_TR_ST     at 0 range 4 .. 4;
      END_BF_ST     at 0 range 5 .. 5;
      DESC_LDST     at 0 range 6 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
      BUFF_COUNT    at 0 range 16 .. 31;
   end record;

   subtype UOTGHS_CTRL_IDTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_VBUSTE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_SRPE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_VBERRE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_BCERRE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_ROLEEXE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_HNPERRE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_STOE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_VBUSHWC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_SRPSEL_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_SRPREQ_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_HNPREQ_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_OTGPADE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_VBUSPO_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_FRZCLK_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_USBE_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_CTRL_TIMVALUE_Field is Interfaces.SAM3x8e.UInt2;
   subtype UOTGHS_CTRL_TIMPAGE_Field is Interfaces.SAM3x8e.UInt2;
   subtype UOTGHS_CTRL_UNLOCK_Field is Interfaces.SAM3x8e.Bit;

   --  UOTGID Pin Enable
   type CTRL_UIDE_Field is
     (--  The USB mode (device/host) is selected from the UIMOD bit.
      Uimod,
      --  The USB mode (device/host) is selected from the UOTGID input pin.
      Uotgid)
     with Size => 1;
   for CTRL_UIDE_Field use
     (Uimod => 0,
      Uotgid => 1);

   --  UOTGHS Mode
   type CTRL_UIMOD_Field is
     (--  The module is in USB host mode.
      Host,
      --  The module is in USB device mode.
      Device)
     with Size => 1;
   for CTRL_UIMOD_Field use
     (Host => 0,
      Device => 1);

   --  General Control Register
   type UOTGHS_CTRL_Register is record
      --  ID Transition Interrupt Enable
      IDTE           : UOTGHS_CTRL_IDTE_Field := 16#0#;
      --  VBus Transition Interrupt Enable
      VBUSTE         : UOTGHS_CTRL_VBUSTE_Field := 16#0#;
      --  SRP Interrupt Enable
      SRPE           : UOTGHS_CTRL_SRPE_Field := 16#0#;
      --  VBus Error Interrupt Enable
      VBERRE         : UOTGHS_CTRL_VBERRE_Field := 16#0#;
      --  B-Connection Error Interrupt Enable
      BCERRE         : UOTGHS_CTRL_BCERRE_Field := 16#0#;
      --  Role Exchange Interrupt Enable
      ROLEEXE        : UOTGHS_CTRL_ROLEEXE_Field := 16#0#;
      --  HNP Error Interrupt Enable
      HNPERRE        : UOTGHS_CTRL_HNPERRE_Field := 16#0#;
      --  Suspend Time-Out Interrupt Enable
      STOE           : UOTGHS_CTRL_STOE_Field := 16#0#;
      --  VBus Hardware Control
      VBUSHWC        : UOTGHS_CTRL_VBUSHWC_Field := 16#0#;
      --  SRP Selection
      SRPSEL         : UOTGHS_CTRL_SRPSEL_Field := 16#0#;
      --  SRP Request
      SRPREQ         : UOTGHS_CTRL_SRPREQ_Field := 16#0#;
      --  HNP Request
      HNPREQ         : UOTGHS_CTRL_HNPREQ_Field := 16#0#;
      --  OTG Pad Enable
      OTGPADE        : UOTGHS_CTRL_OTGPADE_Field := 16#0#;
      --  VBus Polarity Off
      VBUSPO         : UOTGHS_CTRL_VBUSPO_Field := 16#0#;
      --  Freeze USB Clock
      FRZCLK         : UOTGHS_CTRL_FRZCLK_Field := 16#1#;
      --  UOTGHS Enable
      USBE           : UOTGHS_CTRL_USBE_Field := 16#0#;
      --  Timer Value
      TIMVALUE       : UOTGHS_CTRL_TIMVALUE_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Timer Page
      TIMPAGE        : UOTGHS_CTRL_TIMPAGE_Field := 16#0#;
      --  Timer Access Unlock
      UNLOCK         : UOTGHS_CTRL_UNLOCK_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : Interfaces.SAM3x8e.Bit := 16#0#;
      --  UOTGID Pin Enable
      UIDE           : CTRL_UIDE_Field := Interfaces.SAM3x8e.UOTGHS.Uotgid;
      --  UOTGHS Mode
      UIMOD          : CTRL_UIMOD_Field := Interfaces.SAM3x8e.UOTGHS.Device;
      --  unspecified
      Reserved_26_31 : Interfaces.SAM3x8e.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_CTRL_Register use record
      IDTE           at 0 range 0 .. 0;
      VBUSTE         at 0 range 1 .. 1;
      SRPE           at 0 range 2 .. 2;
      VBERRE         at 0 range 3 .. 3;
      BCERRE         at 0 range 4 .. 4;
      ROLEEXE        at 0 range 5 .. 5;
      HNPERRE        at 0 range 6 .. 6;
      STOE           at 0 range 7 .. 7;
      VBUSHWC        at 0 range 8 .. 8;
      SRPSEL         at 0 range 9 .. 9;
      SRPREQ         at 0 range 10 .. 10;
      HNPREQ         at 0 range 11 .. 11;
      OTGPADE        at 0 range 12 .. 12;
      VBUSPO         at 0 range 13 .. 13;
      FRZCLK         at 0 range 14 .. 14;
      USBE           at 0 range 15 .. 15;
      TIMVALUE       at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      TIMPAGE        at 0 range 20 .. 21;
      UNLOCK         at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      UIDE           at 0 range 24 .. 24;
      UIMOD          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype UOTGHS_SR_IDTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_VBUSTI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_SRPI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_VBERRI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_BCERRI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_ROLEEXI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_HNPERRI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_STOI_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_VBUSRQ_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_ID_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SR_VBUS_Field is Interfaces.SAM3x8e.Bit;

   --  Speed Status
   type SR_SPEED_Field is
     (--  Full-Speed mode
      Full_Speed,
      --  High-Speed mode
      High_Speed,
      --  Low-Speed mode
      Low_Speed)
     with Size => 2;
   for SR_SPEED_Field use
     (Full_Speed => 0,
      High_Speed => 1,
      Low_Speed => 2);

   subtype UOTGHS_SR_CLKUSABLE_Field is Interfaces.SAM3x8e.Bit;

   --  General Status Register
   type UOTGHS_SR_Register is record
      --  Read-only. ID Transition Interrupt
      IDTI           : UOTGHS_SR_IDTI_Field;
      --  Read-only. VBus Transition Interrupt
      VBUSTI         : UOTGHS_SR_VBUSTI_Field;
      --  Read-only. SRP Interrupt
      SRPI           : UOTGHS_SR_SRPI_Field;
      --  Read-only. VBus Error Interrupt
      VBERRI         : UOTGHS_SR_VBERRI_Field;
      --  Read-only. B-Connection Error Interrupt
      BCERRI         : UOTGHS_SR_BCERRI_Field;
      --  Read-only. Role Exchange Interrupt
      ROLEEXI        : UOTGHS_SR_ROLEEXI_Field;
      --  Read-only. HNP Error Interrupt
      HNPERRI        : UOTGHS_SR_HNPERRI_Field;
      --  Read-only. Suspend Time-Out Interrupt
      STOI           : UOTGHS_SR_STOI_Field;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM3x8e.Bit;
      --  Read-only. VBus Request
      VBUSRQ         : UOTGHS_SR_VBUSRQ_Field;
      --  Read-only. UOTGID Pin State
      ID             : UOTGHS_SR_ID_Field;
      --  Read-only. VBus Level
      VBUS           : UOTGHS_SR_VBUS_Field;
      --  Read-only. Speed Status
      SPEED          : SR_SPEED_Field;
      --  Read-only. UTMI Clock Usable
      CLKUSABLE      : UOTGHS_SR_CLKUSABLE_Field;
      --  unspecified
      Reserved_15_31 : Interfaces.SAM3x8e.UInt17;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_SR_Register use record
      IDTI           at 0 range 0 .. 0;
      VBUSTI         at 0 range 1 .. 1;
      SRPI           at 0 range 2 .. 2;
      VBERRI         at 0 range 3 .. 3;
      BCERRI         at 0 range 4 .. 4;
      ROLEEXI        at 0 range 5 .. 5;
      HNPERRI        at 0 range 6 .. 6;
      STOI           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      VBUSRQ         at 0 range 9 .. 9;
      ID             at 0 range 10 .. 10;
      VBUS           at 0 range 11 .. 11;
      SPEED          at 0 range 12 .. 13;
      CLKUSABLE      at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype UOTGHS_SCR_IDTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_VBUSTIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_SRPIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_VBERRIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_BCERRIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_ROLEEXIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_HNPERRIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_STOIC_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SCR_VBUSRQC_Field is Interfaces.SAM3x8e.Bit;

   --  General Status Clear Register
   type UOTGHS_SCR_Register is record
      --  Write-only. ID Transition Interrupt Clear
      IDTIC          : UOTGHS_SCR_IDTIC_Field := 16#0#;
      --  Write-only. VBus Transition Interrupt Clear
      VBUSTIC        : UOTGHS_SCR_VBUSTIC_Field := 16#0#;
      --  Write-only. SRP Interrupt Clear
      SRPIC          : UOTGHS_SCR_SRPIC_Field := 16#0#;
      --  Write-only. VBus Error Interrupt Clear
      VBERRIC        : UOTGHS_SCR_VBERRIC_Field := 16#0#;
      --  Write-only. B-Connection Error Interrupt Clear
      BCERRIC        : UOTGHS_SCR_BCERRIC_Field := 16#0#;
      --  Write-only. Role Exchange Interrupt Clear
      ROLEEXIC       : UOTGHS_SCR_ROLEEXIC_Field := 16#0#;
      --  Write-only. HNP Error Interrupt Clear
      HNPERRIC       : UOTGHS_SCR_HNPERRIC_Field := 16#0#;
      --  Write-only. Suspend Time-Out Interrupt Clear
      STOIC          : UOTGHS_SCR_STOIC_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. VBus Request Clear
      VBUSRQC        : UOTGHS_SCR_VBUSRQC_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.SAM3x8e.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_SCR_Register use record
      IDTIC          at 0 range 0 .. 0;
      VBUSTIC        at 0 range 1 .. 1;
      SRPIC          at 0 range 2 .. 2;
      VBERRIC        at 0 range 3 .. 3;
      BCERRIC        at 0 range 4 .. 4;
      ROLEEXIC       at 0 range 5 .. 5;
      HNPERRIC       at 0 range 6 .. 6;
      STOIC          at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      VBUSRQC        at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype UOTGHS_SFR_IDTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_VBUSTIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_SRPIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_VBERRIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_BCERRIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_ROLEEXIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_HNPERRIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_STOIS_Field is Interfaces.SAM3x8e.Bit;
   subtype UOTGHS_SFR_VBUSRQS_Field is Interfaces.SAM3x8e.Bit;

   --  General Status Set Register
   type UOTGHS_SFR_Register is record
      --  Write-only. ID Transition Interrupt Set
      IDTIS          : UOTGHS_SFR_IDTIS_Field := 16#0#;
      --  Write-only. VBus Transition Interrupt Set
      VBUSTIS        : UOTGHS_SFR_VBUSTIS_Field := 16#0#;
      --  Write-only. SRP Interrupt Set
      SRPIS          : UOTGHS_SFR_SRPIS_Field := 16#0#;
      --  Write-only. VBus Error Interrupt Set
      VBERRIS        : UOTGHS_SFR_VBERRIS_Field := 16#0#;
      --  Write-only. B-Connection Error Interrupt Set
      BCERRIS        : UOTGHS_SFR_BCERRIS_Field := 16#0#;
      --  Write-only. Role Exchange Interrupt Set
      ROLEEXIS       : UOTGHS_SFR_ROLEEXIS_Field := 16#0#;
      --  Write-only. HNP Error Interrupt Set
      HNPERRIS       : UOTGHS_SFR_HNPERRIS_Field := 16#0#;
      --  Write-only. Suspend Time-Out Interrupt Set
      STOIS          : UOTGHS_SFR_STOIS_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM3x8e.Bit := 16#0#;
      --  Write-only. VBus Request Set
      VBUSRQS        : UOTGHS_SFR_VBUSRQS_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.SAM3x8e.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_SFR_Register use record
      IDTIS          at 0 range 0 .. 0;
      VBUSTIS        at 0 range 1 .. 1;
      SRPIS          at 0 range 2 .. 2;
      VBERRIS        at 0 range 3 .. 3;
      BCERRIS        at 0 range 4 .. 4;
      ROLEEXIS       at 0 range 5 .. 5;
      HNPERRIS       at 0 range 6 .. 6;
      STOIS          at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      VBUSRQS        at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Dual Role Device State
   type FSM_DRDSTATE_Field is
     (--  This is the start state for A-devices (when the ID pin is 0)
      A_Idlestate,
      --  In this state, the A-device waits for the voltage on VBus to rise above the
--  A-device VBus Valid threshold (4.4 V).
      A_Wait_Vrise,
      --  In this state, the A-device waits for the B-device to signal a connection.
      A_Wait_Bcon,
      --  In this state, the A-device that operates in Host mode is operational.
      A_Host,
      --  The A-device operating as a host is in the suspend mode.
      A_Suspend,
      --  The A-device operates as a peripheral.
      A_Peripheral,
      --  In this state, the A-device waits for the voltage on VBus to drop below the
--  A-device Session Valid threshold (1.4 V).
      A_Wait_Vfall,
      --  In this state, the A-device waits for recovery of the over-current
--  condition that caused it to enter this state.
      A_Vbus_Err,
      --  In this state, the A-device waits for the data USB line to discharge (100
--  us).
      A_Wait_Discharge,
      --  This is the start state for B-device (when the ID pin is 1).
      B_Idle,
      --  In this state, the B-device acts as the peripheral.
      B_Peripheral,
      --  In this state, the B-device is in suspend mode and waits until 3 ms before
--  initiating the HNP protocol if requested.
      B_Wait_Begin_Hnp,
      --  In this state, the B-device waits for the data USB line to discharge (100
--  us) before becoming Host.
      B_Wait_Discharge,
      --  In this state, the B-device waits for the A-device to signal a connect
--  before becoming B-Host.
      B_Wait_Acon,
      --  In this state, the B-device acts as the Host.
      B_Host,
      --  In this state, the B-device attempts to start a session using the SRP
--  protocol.
      B_Srp_Init)
     with Size => 4;
   for FSM_DRDSTATE_Field use
     (A_Idlestate => 0,
      A_Wait_Vrise => 1,
      A_Wait_Bcon => 2,
      A_Host => 3,
      A_Suspend => 4,
      A_Peripheral => 5,
      A_Wait_Vfall => 6,
      A_Vbus_Err => 7,
      A_Wait_Discharge => 8,
      B_Idle => 9,
      B_Peripheral => 10,
      B_Wait_Begin_Hnp => 11,
      B_Wait_Discharge => 12,
      B_Wait_Acon => 13,
      B_Host => 14,
      B_Srp_Init => 15);

   --  General Finite State Machine Register
   type UOTGHS_FSM_Register is record
      --  Read-only. Dual Role Device State
      DRDSTATE      : FSM_DRDSTATE_Field;
      --  unspecified
      Reserved_4_31 : Interfaces.SAM3x8e.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UOTGHS_FSM_Register use record
      DRDSTATE      at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB On-The-Go Interface
   type UOTGHS_Peripheral is record
      --  Device General Control Register
      DEVCTRL        : aliased UOTGHS_DEVCTRL_Register;
      --  Device Global Interrupt Status Register
      DEVISR         : aliased UOTGHS_DEVISR_Register;
      --  Device Global Interrupt Clear Register
      DEVICR         : aliased UOTGHS_DEVICR_Register;
      --  Device Global Interrupt Set Register
      DEVIFR         : aliased UOTGHS_DEVIFR_Register;
      --  Device Global Interrupt Mask Register
      DEVIMR         : aliased UOTGHS_DEVIMR_Register;
      --  Device Global Interrupt Disable Register
      DEVIDR         : aliased UOTGHS_DEVIDR_Register;
      --  Device Global Interrupt Enable Register
      DEVIER         : aliased UOTGHS_DEVIER_Register;
      --  Device Endpoint Register
      DEVEPT         : aliased UOTGHS_DEVEPT_Register;
      --  Device Frame Number Register
      DEVFNUM        : aliased UOTGHS_DEVFNUM_Register;
      --  Device Endpoint Configuration Register (n = 0)
      DEVEPTCFG      : aliased UOTGHS_DEVEPTCFG_Registers;
      --  Device Endpoint Status Register (n = 0)
      DEVEPTISR      : aliased UOTGHS_DEVEPTISR_Registers;
      --  Device Endpoint Clear Register (n = 0)
      DEVEPTICR      : aliased UOTGHS_DEVEPTICR_Registers;
      --  Device Endpoint Set Register (n = 0)
      DEVEPTIFR      : aliased UOTGHS_DEVEPTIFR_Registers;
      --  Device Endpoint Mask Register (n = 0)
      DEVEPTIMR      : aliased UOTGHS_DEVEPTIMR_Registers;
      --  Device Endpoint Enable Register (n = 0)
      DEVEPTIER      : aliased UOTGHS_DEVEPTIER_Registers;
      --  Device Endpoint Disable Register (n = 0)
      DEVEPTIDR      : aliased UOTGHS_DEVEPTIDR_Registers;
      --  Device DMA Channel Next Descriptor Address Register (n = 1)
      DEVDMANXTDSC1  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 1)
      DEVDMAADDRESS1 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 1)
      DEVDMACONTROL1 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 1)
      DEVDMASTATUS1  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 2)
      DEVDMANXTDSC2  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 2)
      DEVDMAADDRESS2 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 2)
      DEVDMACONTROL2 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 2)
      DEVDMASTATUS2  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 3)
      DEVDMANXTDSC3  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 3)
      DEVDMAADDRESS3 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 3)
      DEVDMACONTROL3 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 3)
      DEVDMASTATUS3  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 4)
      DEVDMANXTDSC4  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 4)
      DEVDMAADDRESS4 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 4)
      DEVDMACONTROL4 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 4)
      DEVDMASTATUS4  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 5)
      DEVDMANXTDSC5  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 5)
      DEVDMAADDRESS5 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 5)
      DEVDMACONTROL5 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 5)
      DEVDMASTATUS5  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 6)
      DEVDMANXTDSC6  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 6)
      DEVDMAADDRESS6 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 6)
      DEVDMACONTROL6 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 6)
      DEVDMASTATUS6  : aliased DEVDMASTATUS_Register;
      --  Device DMA Channel Next Descriptor Address Register (n = 7)
      DEVDMANXTDSC7  : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Address Register (n = 7)
      DEVDMAADDRESS7 : aliased Interfaces.SAM3x8e.UInt32;
      --  Device DMA Channel Control Register (n = 7)
      DEVDMACONTROL7 : aliased DEVDMACONTROL_Register;
      --  Device DMA Channel Status Register (n = 7)
      DEVDMASTATUS7  : aliased DEVDMASTATUS_Register;
      --  Host General Control Register
      HSTCTRL        : aliased UOTGHS_HSTCTRL_Register;
      --  Host Global Interrupt Status Register
      HSTISR         : aliased UOTGHS_HSTISR_Register;
      --  Host Global Interrupt Clear Register
      HSTICR         : aliased UOTGHS_HSTICR_Register;
      --  Host Global Interrupt Set Register
      HSTIFR         : aliased UOTGHS_HSTIFR_Register;
      --  Host Global Interrupt Mask Register
      HSTIMR         : aliased UOTGHS_HSTIMR_Register;
      --  Host Global Interrupt Disable Register
      HSTIDR         : aliased UOTGHS_HSTIDR_Register;
      --  Host Global Interrupt Enable Register
      HSTIER         : aliased UOTGHS_HSTIER_Register;
      --  Host Pipe Register
      HSTPIP         : aliased UOTGHS_HSTPIP_Register;
      --  Host Frame Number Register
      HSTFNUM        : aliased UOTGHS_HSTFNUM_Register;
      --  Host Address 1 Register
      HSTADDR1       : aliased UOTGHS_HSTADDR1_Register;
      --  Host Address 2 Register
      HSTADDR2       : aliased UOTGHS_HSTADDR2_Register;
      --  Host Address 3 Register
      HSTADDR3       : aliased UOTGHS_HSTADDR3_Register;
      --  Host Pipe Configuration Register (n = 0)
      HSTPIPCFG      : aliased UOTGHS_HSTPIPCFG_Registers;
      --  Host Pipe Status Register (n = 0)
      HSTPIPISR      : aliased UOTGHS_HSTPIPISR_Registers;
      --  Host Pipe Clear Register (n = 0)
      HSTPIPICR      : aliased UOTGHS_HSTPIPICR_Registers;
      --  Host Pipe Set Register (n = 0)
      HSTPIPIFR      : aliased UOTGHS_HSTPIPIFR_Registers;
      --  Host Pipe Mask Register (n = 0)
      HSTPIPIMR      : aliased UOTGHS_HSTPIPIMR_Registers;
      --  Host Pipe Enable Register (n = 0)
      HSTPIPIER      : aliased UOTGHS_HSTPIPIER_Registers;
      --  Host Pipe Disable Register (n = 0)
      HSTPIPIDR      : aliased UOTGHS_HSTPIPIDR_Registers;
      --  Host Pipe IN Request Register (n = 0)
      HSTPIPINRQ     : aliased UOTGHS_HSTPIPINRQ_Registers;
      --  Host Pipe Error Register (n = 0)
      HSTPIPERR      : aliased UOTGHS_HSTPIPERR_Registers;
      --  Host DMA Channel Next Descriptor Address Register (n = 1)
      HSTDMANXTDSC1  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 1)
      HSTDMAADDRESS1 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 1)
      HSTDMACONTROL1 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 1)
      HSTDMASTATUS1  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 2)
      HSTDMANXTDSC2  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 2)
      HSTDMAADDRESS2 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 2)
      HSTDMACONTROL2 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 2)
      HSTDMASTATUS2  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 3)
      HSTDMANXTDSC3  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 3)
      HSTDMAADDRESS3 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 3)
      HSTDMACONTROL3 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 3)
      HSTDMASTATUS3  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 4)
      HSTDMANXTDSC4  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 4)
      HSTDMAADDRESS4 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 4)
      HSTDMACONTROL4 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 4)
      HSTDMASTATUS4  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 5)
      HSTDMANXTDSC5  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 5)
      HSTDMAADDRESS5 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 5)
      HSTDMACONTROL5 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 5)
      HSTDMASTATUS5  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 6)
      HSTDMANXTDSC6  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 6)
      HSTDMAADDRESS6 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 6)
      HSTDMACONTROL6 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 6)
      HSTDMASTATUS6  : aliased HSTDMASTATUS_Register;
      --  Host DMA Channel Next Descriptor Address Register (n = 7)
      HSTDMANXTDSC7  : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Address Register (n = 7)
      HSTDMAADDRESS7 : aliased Interfaces.SAM3x8e.UInt32;
      --  Host DMA Channel Control Register (n = 7)
      HSTDMACONTROL7 : aliased HSTDMACONTROL_Register;
      --  Host DMA Channel Status Register (n = 7)
      HSTDMASTATUS7  : aliased HSTDMASTATUS_Register;
      --  General Control Register
      CTRL           : aliased UOTGHS_CTRL_Register;
      --  General Status Register
      SR             : aliased UOTGHS_SR_Register;
      --  General Status Clear Register
      SCR            : aliased UOTGHS_SCR_Register;
      --  General Status Set Register
      SFR            : aliased UOTGHS_SFR_Register;
      --  General Finite State Machine Register
      FSM            : aliased UOTGHS_FSM_Register;
   end record
     with Volatile;

   for UOTGHS_Peripheral use record
      DEVCTRL        at 16#0# range 0 .. 31;
      DEVISR         at 16#4# range 0 .. 31;
      DEVICR         at 16#8# range 0 .. 31;
      DEVIFR         at 16#C# range 0 .. 31;
      DEVIMR         at 16#10# range 0 .. 31;
      DEVIDR         at 16#14# range 0 .. 31;
      DEVIER         at 16#18# range 0 .. 31;
      DEVEPT         at 16#1C# range 0 .. 31;
      DEVFNUM        at 16#20# range 0 .. 31;
      DEVEPTCFG      at 16#100# range 0 .. 319;
      DEVEPTISR      at 16#130# range 0 .. 319;
      DEVEPTICR      at 16#160# range 0 .. 319;
      DEVEPTIFR      at 16#190# range 0 .. 319;
      DEVEPTIMR      at 16#1C0# range 0 .. 319;
      DEVEPTIER      at 16#1F0# range 0 .. 319;
      DEVEPTIDR      at 16#220# range 0 .. 319;
      DEVDMANXTDSC1  at 16#310# range 0 .. 31;
      DEVDMAADDRESS1 at 16#314# range 0 .. 31;
      DEVDMACONTROL1 at 16#318# range 0 .. 31;
      DEVDMASTATUS1  at 16#31C# range 0 .. 31;
      DEVDMANXTDSC2  at 16#320# range 0 .. 31;
      DEVDMAADDRESS2 at 16#324# range 0 .. 31;
      DEVDMACONTROL2 at 16#328# range 0 .. 31;
      DEVDMASTATUS2  at 16#32C# range 0 .. 31;
      DEVDMANXTDSC3  at 16#330# range 0 .. 31;
      DEVDMAADDRESS3 at 16#334# range 0 .. 31;
      DEVDMACONTROL3 at 16#338# range 0 .. 31;
      DEVDMASTATUS3  at 16#33C# range 0 .. 31;
      DEVDMANXTDSC4  at 16#340# range 0 .. 31;
      DEVDMAADDRESS4 at 16#344# range 0 .. 31;
      DEVDMACONTROL4 at 16#348# range 0 .. 31;
      DEVDMASTATUS4  at 16#34C# range 0 .. 31;
      DEVDMANXTDSC5  at 16#350# range 0 .. 31;
      DEVDMAADDRESS5 at 16#354# range 0 .. 31;
      DEVDMACONTROL5 at 16#358# range 0 .. 31;
      DEVDMASTATUS5  at 16#35C# range 0 .. 31;
      DEVDMANXTDSC6  at 16#360# range 0 .. 31;
      DEVDMAADDRESS6 at 16#364# range 0 .. 31;
      DEVDMACONTROL6 at 16#368# range 0 .. 31;
      DEVDMASTATUS6  at 16#36C# range 0 .. 31;
      DEVDMANXTDSC7  at 16#370# range 0 .. 31;
      DEVDMAADDRESS7 at 16#374# range 0 .. 31;
      DEVDMACONTROL7 at 16#378# range 0 .. 31;
      DEVDMASTATUS7  at 16#37C# range 0 .. 31;
      HSTCTRL        at 16#400# range 0 .. 31;
      HSTISR         at 16#404# range 0 .. 31;
      HSTICR         at 16#408# range 0 .. 31;
      HSTIFR         at 16#40C# range 0 .. 31;
      HSTIMR         at 16#410# range 0 .. 31;
      HSTIDR         at 16#414# range 0 .. 31;
      HSTIER         at 16#418# range 0 .. 31;
      HSTPIP         at 16#41C# range 0 .. 31;
      HSTFNUM        at 16#420# range 0 .. 31;
      HSTADDR1       at 16#424# range 0 .. 31;
      HSTADDR2       at 16#428# range 0 .. 31;
      HSTADDR3       at 16#42C# range 0 .. 31;
      HSTPIPCFG      at 16#500# range 0 .. 319;
      HSTPIPISR      at 16#530# range 0 .. 319;
      HSTPIPICR      at 16#560# range 0 .. 319;
      HSTPIPIFR      at 16#590# range 0 .. 319;
      HSTPIPIMR      at 16#5C0# range 0 .. 319;
      HSTPIPIER      at 16#5F0# range 0 .. 319;
      HSTPIPIDR      at 16#620# range 0 .. 319;
      HSTPIPINRQ     at 16#650# range 0 .. 319;
      HSTPIPERR      at 16#680# range 0 .. 319;
      HSTDMANXTDSC1  at 16#710# range 0 .. 31;
      HSTDMAADDRESS1 at 16#714# range 0 .. 31;
      HSTDMACONTROL1 at 16#718# range 0 .. 31;
      HSTDMASTATUS1  at 16#71C# range 0 .. 31;
      HSTDMANXTDSC2  at 16#720# range 0 .. 31;
      HSTDMAADDRESS2 at 16#724# range 0 .. 31;
      HSTDMACONTROL2 at 16#728# range 0 .. 31;
      HSTDMASTATUS2  at 16#72C# range 0 .. 31;
      HSTDMANXTDSC3  at 16#730# range 0 .. 31;
      HSTDMAADDRESS3 at 16#734# range 0 .. 31;
      HSTDMACONTROL3 at 16#738# range 0 .. 31;
      HSTDMASTATUS3  at 16#73C# range 0 .. 31;
      HSTDMANXTDSC4  at 16#740# range 0 .. 31;
      HSTDMAADDRESS4 at 16#744# range 0 .. 31;
      HSTDMACONTROL4 at 16#748# range 0 .. 31;
      HSTDMASTATUS4  at 16#74C# range 0 .. 31;
      HSTDMANXTDSC5  at 16#750# range 0 .. 31;
      HSTDMAADDRESS5 at 16#754# range 0 .. 31;
      HSTDMACONTROL5 at 16#758# range 0 .. 31;
      HSTDMASTATUS5  at 16#75C# range 0 .. 31;
      HSTDMANXTDSC6  at 16#760# range 0 .. 31;
      HSTDMAADDRESS6 at 16#764# range 0 .. 31;
      HSTDMACONTROL6 at 16#768# range 0 .. 31;
      HSTDMASTATUS6  at 16#76C# range 0 .. 31;
      HSTDMANXTDSC7  at 16#770# range 0 .. 31;
      HSTDMAADDRESS7 at 16#774# range 0 .. 31;
      HSTDMACONTROL7 at 16#778# range 0 .. 31;
      HSTDMASTATUS7  at 16#77C# range 0 .. 31;
      CTRL           at 16#800# range 0 .. 31;
      SR             at 16#804# range 0 .. 31;
      SCR            at 16#808# range 0 .. 31;
      SFR            at 16#80C# range 0 .. 31;
      FSM            at 16#82C# range 0 .. 31;
   end record;

   --  USB On-The-Go Interface
   UOTGHS_Periph : aliased UOTGHS_Peripheral
     with Import, Address => UOTGHS_Base;

end Interfaces.SAM3x8e.UOTGHS;
