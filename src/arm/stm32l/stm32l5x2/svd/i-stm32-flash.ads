--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32L562.svd


with System;

package Interfaces.STM32.Flash is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Access control register
   type ACR_Register is record
      --  Latency
      LATENCY        : Interfaces.STM32.UInt4 := 16#0#;
      --  unspecified
      Reserved_4_12  : Interfaces.STM32.UInt9 := 16#0#;
      --  Flash Power-down mode during Low-power run mode
      RUN_PD         : Boolean := False;
      --  Flash Power-down mode during Low-power sleep mode
      SLEEP_PD       : Boolean := False;
      --  LVEN
      LVEN           : Boolean := False;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR_Register use record
      LATENCY        at 0 range 0 .. 3;
      Reserved_4_12  at 0 range 4 .. 12;
      RUN_PD         at 0 range 13 .. 13;
      SLEEP_PD       at 0 range 14 .. 14;
      LVEN           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Flash status register
   type NSSR_Register is record
      --  NSEOP
      NSEOP          : Boolean := False;
      --  NSOPERR
      NSOPERR        : Boolean := False;
      --  unspecified
      Reserved_2_2   : Interfaces.STM32.Bit := 16#0#;
      --  NSPROGERR
      NSPROGERR      : Boolean := False;
      --  NSWRPERR
      NSWRPERR       : Boolean := False;
      --  NSPGAERR
      NSPGAERR       : Boolean := False;
      --  NSSIZERR
      NSSIZERR       : Boolean := False;
      --  NSPGSERR
      NSPGSERR       : Boolean := False;
      --  unspecified
      Reserved_8_12  : Interfaces.STM32.UInt5 := 16#0#;
      --  OPTWERR
      OPTWERR        : Boolean := False;
      --  unspecified
      Reserved_14_14 : Interfaces.STM32.Bit := 16#0#;
      --  OPTVERR
      OPTVERR        : Boolean := False;
      --  Read-only. NSBusy
      NSBSY          : Boolean := False;
      --  unspecified
      Reserved_17_31 : Interfaces.STM32.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NSSR_Register use record
      NSEOP          at 0 range 0 .. 0;
      NSOPERR        at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      NSPROGERR      at 0 range 3 .. 3;
      NSWRPERR       at 0 range 4 .. 4;
      NSPGAERR       at 0 range 5 .. 5;
      NSSIZERR       at 0 range 6 .. 6;
      NSPGSERR       at 0 range 7 .. 7;
      Reserved_8_12  at 0 range 8 .. 12;
      OPTWERR        at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      OPTVERR        at 0 range 15 .. 15;
      NSBSY          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Flash status register
   type SECSR_Register is record
      --  SECEOP
      SECEOP         : Boolean := False;
      --  SECOPERR
      SECOPERR       : Boolean := False;
      --  unspecified
      Reserved_2_2   : Interfaces.STM32.Bit := 16#0#;
      --  SECPROGERR
      SECPROGERR     : Boolean := False;
      --  SECWRPERR
      SECWRPERR      : Boolean := False;
      --  SECPGAERR
      SECPGAERR      : Boolean := False;
      --  SECSIZERR
      SECSIZERR      : Boolean := False;
      --  SECPGSERR
      SECPGSERR      : Boolean := False;
      --  unspecified
      Reserved_8_13  : Interfaces.STM32.UInt6 := 16#0#;
      --  Secure read protection error
      SECRDERR       : Boolean := False;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  Read-only. SECBusy
      SECBSY         : Boolean := False;
      --  unspecified
      Reserved_17_31 : Interfaces.STM32.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECSR_Register use record
      SECEOP         at 0 range 0 .. 0;
      SECOPERR       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SECPROGERR     at 0 range 3 .. 3;
      SECWRPERR      at 0 range 4 .. 4;
      SECPGAERR      at 0 range 5 .. 5;
      SECSIZERR      at 0 range 6 .. 6;
      SECPGSERR      at 0 range 7 .. 7;
      Reserved_8_13  at 0 range 8 .. 13;
      SECRDERR       at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SECBSY         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Flash non-secure control register
   type NSCR_Register is record
      --  NSPG
      NSPG           : Boolean := False;
      --  NSPER
      NSPER          : Boolean := False;
      --  NSMER1
      NSMER1         : Boolean := False;
      --  NSPNB
      NSPNB          : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_10_10 : Interfaces.STM32.Bit := 16#0#;
      --  NSBKER
      NSBKER         : Boolean := False;
      --  unspecified
      Reserved_12_14 : Interfaces.STM32.UInt3 := 16#0#;
      --  NSMER2
      NSMER2         : Boolean := False;
      --  Options modification start
      NSSTRT         : Boolean := False;
      --  Options modification start
      OPTSTRT        : Boolean := False;
      --  unspecified
      Reserved_18_23 : Interfaces.STM32.UInt6 := 16#0#;
      --  NSEOPIE
      NSEOPIE        : Boolean := False;
      --  NSERRIE
      NSERRIE        : Boolean := False;
      --  unspecified
      Reserved_26_26 : Interfaces.STM32.Bit := 16#0#;
      --  Force the option byte loading
      OBL_LAUNCH     : Boolean := False;
      --  unspecified
      Reserved_28_29 : Interfaces.STM32.UInt2 := 16#0#;
      --  Options Lock
      OPTLOCK        : Boolean := True;
      --  NSLOCK
      NSLOCK         : Boolean := True;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NSCR_Register use record
      NSPG           at 0 range 0 .. 0;
      NSPER          at 0 range 1 .. 1;
      NSMER1         at 0 range 2 .. 2;
      NSPNB          at 0 range 3 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      NSBKER         at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      NSMER2         at 0 range 15 .. 15;
      NSSTRT         at 0 range 16 .. 16;
      OPTSTRT        at 0 range 17 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      NSEOPIE        at 0 range 24 .. 24;
      NSERRIE        at 0 range 25 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      OBL_LAUNCH     at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      OPTLOCK        at 0 range 30 .. 30;
      NSLOCK         at 0 range 31 .. 31;
   end record;

   --  Flash secure control register
   type SECCR_Register is record
      --  SECPG
      SECPG          : Boolean := False;
      --  SECPER
      SECPER         : Boolean := False;
      --  SECMER1
      SECMER1        : Boolean := False;
      --  SECPNB
      SECPNB         : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_10_10 : Interfaces.STM32.Bit := 16#0#;
      --  SECBKER
      SECBKER        : Boolean := False;
      --  unspecified
      Reserved_12_14 : Interfaces.STM32.UInt3 := 16#0#;
      --  SECMER2
      SECMER2        : Boolean := False;
      --  SECSTRT
      SECSTRT        : Boolean := False;
      --  unspecified
      Reserved_17_23 : Interfaces.STM32.UInt7 := 16#0#;
      --  SECEOPIE
      SECEOPIE       : Boolean := False;
      --  SECERRIE
      SECERRIE       : Boolean := False;
      --  SECRDERRIE
      SECRDERRIE     : Boolean := False;
      --  unspecified
      Reserved_27_28 : Interfaces.STM32.UInt2 := 16#0#;
      --  SECINV
      SECINV         : Boolean := False;
      --  unspecified
      Reserved_30_30 : Interfaces.STM32.Bit := 16#0#;
      --  SECLOCK
      SECLOCK        : Boolean := True;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECCR_Register use record
      SECPG          at 0 range 0 .. 0;
      SECPER         at 0 range 1 .. 1;
      SECMER1        at 0 range 2 .. 2;
      SECPNB         at 0 range 3 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      SECBKER        at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      SECMER2        at 0 range 15 .. 15;
      SECSTRT        at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      SECEOPIE       at 0 range 24 .. 24;
      SECERRIE       at 0 range 25 .. 25;
      SECRDERRIE     at 0 range 26 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      SECINV         at 0 range 29 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SECLOCK        at 0 range 31 .. 31;
   end record;

   --  Flash ECC register
   type ECCR_Register is record
      --  Read-only. ECC fail address
      ADDR_ECC       : Interfaces.STM32.UInt19 := 16#0#;
      --  unspecified
      Reserved_19_20 : Interfaces.STM32.UInt2 := 16#0#;
      --  Read-only. BK_ECC
      BK_ECC         : Boolean := False;
      --  Read-only. SYSF_ECC
      SYSF_ECC       : Boolean := False;
      --  unspecified
      Reserved_23_23 : Interfaces.STM32.Bit := 16#0#;
      --  ECC correction interrupt enable
      ECCIE          : Boolean := False;
      --  unspecified
      Reserved_25_27 : Interfaces.STM32.UInt3 := 16#0#;
      --  ECCC2
      ECCC2          : Boolean := False;
      --  ECCD2
      ECCD2          : Boolean := False;
      --  ECC correction
      ECCC           : Boolean := False;
      --  ECC detection
      ECCD           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ECCR_Register use record
      ADDR_ECC       at 0 range 0 .. 18;
      Reserved_19_20 at 0 range 19 .. 20;
      BK_ECC         at 0 range 21 .. 21;
      SYSF_ECC       at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      ECCIE          at 0 range 24 .. 24;
      Reserved_25_27 at 0 range 25 .. 27;
      ECCC2          at 0 range 28 .. 28;
      ECCD2          at 0 range 29 .. 29;
      ECCC           at 0 range 30 .. 30;
      ECCD           at 0 range 31 .. 31;
   end record;

   --  Flash option register
   type OPTR_Register is record
      --  Read protection level
      RDP            : Interfaces.STM32.Byte := 16#0#;
      --  BOR reset Level
      BOR_LEV        : Interfaces.STM32.UInt3 := 16#0#;
      --  unspecified
      Reserved_11_11 : Interfaces.STM32.Bit := 16#0#;
      --  nRST_STOP
      nRST_STOP      : Boolean := False;
      --  nRST_STDBY
      nRST_STDBY     : Boolean := False;
      --  nRST_SHDW
      nRST_SHDW      : Boolean := False;
      --  unspecified
      Reserved_15_15 : Interfaces.STM32.Bit := 16#0#;
      --  Independent watchdog selection
      IWDG_SW        : Boolean := False;
      --  Independent watchdog counter freeze in Stop mode
      IWDG_STOP      : Boolean := False;
      --  Independent watchdog counter freeze in Standby mode
      IWDG_STDBY     : Boolean := False;
      --  Window watchdog selection
      WWDG_SW        : Boolean := False;
      --  SWAP_BANK
      SWAP_BANK      : Boolean := False;
      --  DB256K
      DB256K         : Boolean := False;
      --  DBANK
      DBANK          : Boolean := False;
      --  unspecified
      Reserved_23_23 : Interfaces.STM32.Bit := 16#0#;
      --  SRAM2 parity check enable
      SRAM2_PE       : Boolean := False;
      --  SRAM2 Erase when system reset
      SRAM2_RST      : Boolean := False;
      --  nSWBOOT0
      nSWBOOT0       : Boolean := False;
      --  nBOOT0
      nBOOT0         : Boolean := False;
      --  PA15_PUPEN
      PA15_PUPEN     : Boolean := False;
      --  unspecified
      Reserved_29_30 : Interfaces.STM32.UInt2 := 16#0#;
      --  TZEN
      TZEN           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTR_Register use record
      RDP            at 0 range 0 .. 7;
      BOR_LEV        at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      nRST_STOP      at 0 range 12 .. 12;
      nRST_STDBY     at 0 range 13 .. 13;
      nRST_SHDW      at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      IWDG_SW        at 0 range 16 .. 16;
      IWDG_STOP      at 0 range 17 .. 17;
      IWDG_STDBY     at 0 range 18 .. 18;
      WWDG_SW        at 0 range 19 .. 19;
      SWAP_BANK      at 0 range 20 .. 20;
      DB256K         at 0 range 21 .. 21;
      DBANK          at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SRAM2_PE       at 0 range 24 .. 24;
      SRAM2_RST      at 0 range 25 .. 25;
      nSWBOOT0       at 0 range 26 .. 26;
      nBOOT0         at 0 range 27 .. 27;
      PA15_PUPEN     at 0 range 28 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      TZEN           at 0 range 31 .. 31;
   end record;

   --  Flash non-secure boot address 0 register
   type NSBOOTADD0R_Register is record
      --  unspecified
      Reserved_0_6 : Interfaces.STM32.UInt7 := 16#F#;
      --  Write-only. NSBOOTADD0
      NSBOOTADD0   : Interfaces.STM32.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NSBOOTADD0R_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      NSBOOTADD0   at 0 range 7 .. 31;
   end record;

   --  Flash non-secure boot address 1 register
   type NSBOOTADD1R_Register is record
      --  unspecified
      Reserved_0_6 : Interfaces.STM32.UInt7 := 16#F#;
      --  Write-only. NSBOOTADD1
      NSBOOTADD1   : Interfaces.STM32.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NSBOOTADD1R_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      NSBOOTADD1   at 0 range 7 .. 31;
   end record;

   --  FFlash secure boot address 0 register
   type SECBOOTADD0R_Register is record
      --  BOOT_LOCK
      BOOT_LOCK    : Boolean := False;
      --  unspecified
      Reserved_1_6 : Interfaces.STM32.UInt6 := 16#0#;
      --  Write-only. SECBOOTADD0
      SECBOOTADD0  : Interfaces.STM32.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECBOOTADD0R_Register use record
      BOOT_LOCK    at 0 range 0 .. 0;
      Reserved_1_6 at 0 range 1 .. 6;
      SECBOOTADD0  at 0 range 7 .. 31;
   end record;

   --  Flash bank 1 secure watermak1 register
   type SECWM1R1_Register is record
      --  SECWM1_PSTRT
      SECWM1_PSTRT   : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  SECWM1_PEND
      SECWM1_PEND    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECWM1R1_Register use record
      SECWM1_PSTRT   at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      SECWM1_PEND    at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Flash secure watermak1 register 2
   type SECWM1R2_Register is record
      --  PCROP1_PSTRT
      PCROP1_PSTRT   : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_14  : Interfaces.STM32.Byte := 16#1E#;
      --  PCROP1EN
      PCROP1EN       : Boolean := False;
      --  HDP1_PEND
      HDP1_PEND      : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_30 : Interfaces.STM32.Byte := 16#1E#;
      --  HDP1EN
      HDP1EN         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECWM1R2_Register use record
      PCROP1_PSTRT   at 0 range 0 .. 6;
      Reserved_7_14  at 0 range 7 .. 14;
      PCROP1EN       at 0 range 15 .. 15;
      HDP1_PEND      at 0 range 16 .. 22;
      Reserved_23_30 at 0 range 23 .. 30;
      HDP1EN         at 0 range 31 .. 31;
   end record;

   --  Flash Bank 1 WRP area A address register
   type WRP1AR_Register is record
      --  WRP1A_PSTRT
      WRP1A_PSTRT    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  WRP1A_PEND
      WRP1A_PEND     : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRP1AR_Register use record
      WRP1A_PSTRT    at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      WRP1A_PEND     at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Flash Bank 1 WRP area B address register
   type WRP1BR_Register is record
      --  WRP1B_PSTRT
      WRP1B_PSTRT    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  WRP1B_PEND
      WRP1B_PEND     : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRP1BR_Register use record
      WRP1B_PSTRT    at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      WRP1B_PEND     at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Flash secure watermak2 register
   type SECWM2R1_Register is record
      --  SECWM2_PSTRT
      SECWM2_PSTRT   : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  SECWM2_PEND
      SECWM2_PEND    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECWM2R1_Register use record
      SECWM2_PSTRT   at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      SECWM2_PEND    at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Flash secure watermak2 register2
   type SECWM2R2_Register is record
      --  PCROP2_PSTRT
      PCROP2_PSTRT   : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_14  : Interfaces.STM32.Byte := 16#1E#;
      --  PCROP2EN
      PCROP2EN       : Boolean := False;
      --  HDP2_PEND
      HDP2_PEND      : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_30 : Interfaces.STM32.Byte := 16#1E#;
      --  HDP2EN
      HDP2EN         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECWM2R2_Register use record
      PCROP2_PSTRT   at 0 range 0 .. 6;
      Reserved_7_14  at 0 range 7 .. 14;
      PCROP2EN       at 0 range 15 .. 15;
      HDP2_PEND      at 0 range 16 .. 22;
      Reserved_23_30 at 0 range 23 .. 30;
      HDP2EN         at 0 range 31 .. 31;
   end record;

   --  Flash WPR2 area A address register
   type WRP2AR_Register is record
      --  WRP2A_PSTRT
      WRP2A_PSTRT    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  WRP2A_PEND
      WRP2A_PEND     : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRP2AR_Register use record
      WRP2A_PSTRT    at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      WRP2A_PEND     at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Flash WPR2 area B address register
   type WRP2BR_Register is record
      --  WRP2B_PSTRT
      WRP2B_PSTRT    : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_7_15  : Interfaces.STM32.UInt9 := 16#1FE#;
      --  WRP2B_PEND
      WRP2B_PEND     : Interfaces.STM32.UInt7 := 16#0#;
      --  unspecified
      Reserved_23_31 : Interfaces.STM32.UInt9 := 16#1FE#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WRP2BR_Register use record
      WRP2B_PSTRT    at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      WRP2B_PEND     at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FLASH secure HDP control register
   type SECHDPCR_Register is record
      --  HDP1_ACCDIS
      HDP1_ACCDIS   : Boolean := False;
      --  HDP2_ACCDIS
      HDP2_ACCDIS   : Boolean := False;
      --  unspecified
      Reserved_2_31 : Interfaces.STM32.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECHDPCR_Register use record
      HDP1_ACCDIS   at 0 range 0 .. 0;
      HDP2_ACCDIS   at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Power privilege configuration register
   type PRIVCFGR_Register is record
      --  PRIV
      PRIV          : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.STM32.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRIVCFGR_Register use record
      PRIV          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flash
   type Flash_Peripheral is record
      --  Access control register
      ACR          : aliased ACR_Register;
      --  Power down key register
      PDKEYR       : aliased Interfaces.STM32.UInt32;
      --  Flash non-secure key register
      NSKEYR       : aliased Interfaces.STM32.UInt32;
      --  Flash secure key register
      SECKEYR      : aliased Interfaces.STM32.UInt32;
      --  Flash option key register
      OPTKEYR      : aliased Interfaces.STM32.UInt32;
      --  Flash low voltage key register
      LVEKEYR      : aliased Interfaces.STM32.UInt32;
      --  Flash status register
      NSSR         : aliased NSSR_Register;
      --  Flash status register
      SECSR        : aliased SECSR_Register;
      --  Flash non-secure control register
      NSCR         : aliased NSCR_Register;
      --  Flash secure control register
      SECCR        : aliased SECCR_Register;
      --  Flash ECC register
      ECCR         : aliased ECCR_Register;
      --  Flash option register
      OPTR         : aliased OPTR_Register;
      --  Flash non-secure boot address 0 register
      NSBOOTADD0R  : aliased NSBOOTADD0R_Register;
      --  Flash non-secure boot address 1 register
      NSBOOTADD1R  : aliased NSBOOTADD1R_Register;
      --  FFlash secure boot address 0 register
      SECBOOTADD0R : aliased SECBOOTADD0R_Register;
      --  Flash bank 1 secure watermak1 register
      SECWM1R1     : aliased SECWM1R1_Register;
      --  Flash secure watermak1 register 2
      SECWM1R2     : aliased SECWM1R2_Register;
      --  Flash Bank 1 WRP area A address register
      WRP1AR       : aliased WRP1AR_Register;
      --  Flash Bank 1 WRP area B address register
      WRP1BR       : aliased WRP1BR_Register;
      --  Flash secure watermak2 register
      SECWM2R1     : aliased SECWM2R1_Register;
      --  Flash secure watermak2 register2
      SECWM2R2     : aliased SECWM2R2_Register;
      --  Flash WPR2 area A address register
      WRP2AR       : aliased WRP2AR_Register;
      --  Flash WPR2 area B address register
      WRP2BR       : aliased WRP2BR_Register;
      --  FLASH secure block based bank 1 register
      SECBB1R1     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 1 register
      SECBB1R2     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 1 register
      SECBB1R3     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 1 register
      SECBB1R4     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 2 register
      SECBB2R1     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 2 register
      SECBB2R2     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 2 register
      SECBB2R3     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure block based bank 2 register
      SECBB2R4     : aliased Interfaces.STM32.UInt32;
      --  FLASH secure HDP control register
      SECHDPCR     : aliased SECHDPCR_Register;
      --  Power privilege configuration register
      PRIVCFGR     : aliased PRIVCFGR_Register;
   end record
     with Volatile;

   for Flash_Peripheral use record
      ACR          at 16#0# range 0 .. 31;
      PDKEYR       at 16#4# range 0 .. 31;
      NSKEYR       at 16#8# range 0 .. 31;
      SECKEYR      at 16#C# range 0 .. 31;
      OPTKEYR      at 16#10# range 0 .. 31;
      LVEKEYR      at 16#14# range 0 .. 31;
      NSSR         at 16#20# range 0 .. 31;
      SECSR        at 16#24# range 0 .. 31;
      NSCR         at 16#28# range 0 .. 31;
      SECCR        at 16#2C# range 0 .. 31;
      ECCR         at 16#30# range 0 .. 31;
      OPTR         at 16#40# range 0 .. 31;
      NSBOOTADD0R  at 16#44# range 0 .. 31;
      NSBOOTADD1R  at 16#48# range 0 .. 31;
      SECBOOTADD0R at 16#4C# range 0 .. 31;
      SECWM1R1     at 16#50# range 0 .. 31;
      SECWM1R2     at 16#54# range 0 .. 31;
      WRP1AR       at 16#58# range 0 .. 31;
      WRP1BR       at 16#5C# range 0 .. 31;
      SECWM2R1     at 16#60# range 0 .. 31;
      SECWM2R2     at 16#64# range 0 .. 31;
      WRP2AR       at 16#68# range 0 .. 31;
      WRP2BR       at 16#6C# range 0 .. 31;
      SECBB1R1     at 16#80# range 0 .. 31;
      SECBB1R2     at 16#84# range 0 .. 31;
      SECBB1R3     at 16#88# range 0 .. 31;
      SECBB1R4     at 16#8C# range 0 .. 31;
      SECBB2R1     at 16#A0# range 0 .. 31;
      SECBB2R2     at 16#A4# range 0 .. 31;
      SECBB2R3     at 16#A8# range 0 .. 31;
      SECBB2R4     at 16#AC# range 0 .. 31;
      SECHDPCR     at 16#C0# range 0 .. 31;
      PRIVCFGR     at 16#C4# range 0 .. 31;
   end record;

   --  Flash
   FLASH_Periph : aliased Flash_Peripheral
     with Import, Address => FLASH_Base;

   --  Flash
   SEC_FLASH_Periph : aliased Flash_Peripheral
     with Import, Address => SEC_FLASH_Base;

end Interfaces.STM32.Flash;
