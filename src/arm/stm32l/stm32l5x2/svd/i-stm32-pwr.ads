--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32L562.svd


with System;

package Interfaces.STM32.PWR is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Power control register 1
   type CR1_Register is record
      --  Low-power mode selection
      LPMS           : Interfaces.STM32.UInt3 := 16#0#;
      --  unspecified
      Reserved_3_7   : Interfaces.STM32.UInt5 := 16#0#;
      --  Disable backup domain write protection
      DBP            : Boolean := False;
      --  Voltage scaling range selection
      VOS            : Interfaces.STM32.UInt2 := 16#2#;
      --  unspecified
      Reserved_11_13 : Interfaces.STM32.UInt3 := 16#0#;
      --  Low-power run
      LPR            : Boolean := False;
      --  unspecified
      Reserved_15_31 : Interfaces.STM32.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      LPMS           at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      DBP            at 0 range 8 .. 8;
      VOS            at 0 range 9 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      LPR            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  CR2_PVME array
   type CR2_PVME_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CR2_PVME
   type CR2_PVME_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PVME as a value
            Val : Interfaces.STM32.UInt4;
         when True =>
            --  PVME as an array
            Arr : CR2_PVME_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CR2_PVME_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Power control register 2
   type CR2_Register is record
      --  Power voltage detector enable
      PVDE           : Boolean := False;
      --  Power voltage detector level selection
      PLS            : Interfaces.STM32.UInt3 := 16#0#;
      --  Peripheral voltage monitoring 1 enable: VDDUSB vs. 1.2V
      PVME           : CR2_PVME_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_8   : Interfaces.STM32.Bit := 16#0#;
      --  VDDIO2 Independent I/Os supply valid
      IOSV           : Boolean := False;
      --  VDDUSB USB supply valid
      USV            : Boolean := False;
      --  unspecified
      Reserved_11_31 : Interfaces.STM32.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      PVDE           at 0 range 0 .. 0;
      PLS            at 0 range 1 .. 3;
      PVME           at 0 range 4 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      IOSV           at 0 range 9 .. 9;
      USV            at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CR3_EWUP array
   type CR3_EWUP_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for CR3_EWUP
   type CR3_EWUP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EWUP as a value
            Val : Interfaces.STM32.UInt5;
         when True =>
            --  EWUP as an array
            Arr : CR3_EWUP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for CR3_EWUP_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Power control register 3
   type CR3_Register is record
      --  Enable Wakeup pin WKUP1
      EWUP           : CR3_EWUP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  SRAM2 retention in Standby mode
      RRS            : Interfaces.STM32.UInt2 := 16#0#;
      --  Apply pull-up and pull-down configuration
      APC            : Boolean := False;
      --  ULPMEN
      ULPMEN         : Boolean := False;
      --  unspecified
      Reserved_12_12 : Interfaces.STM32.Bit := 16#0#;
      --  UCPD_STDBY
      UCPD_STDBY     : Boolean := False;
      --  UCPD_DBDIS
      UCPD_DBDIS     : Boolean := False;
      --  unspecified
      Reserved_15_31 : Interfaces.STM32.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR3_Register use record
      EWUP           at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      RRS            at 0 range 8 .. 9;
      APC            at 0 range 10 .. 10;
      ULPMEN         at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      UCPD_STDBY     at 0 range 13 .. 13;
      UCPD_DBDIS     at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  CR4_WUPP array
   type CR4_WUPP_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for CR4_WUPP
   type CR4_WUPP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUPP as a value
            Val : Interfaces.STM32.UInt5;
         when True =>
            --  WUPP as an array
            Arr : CR4_WUPP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for CR4_WUPP_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Power control register 4
   type CR4_Register is record
      --  Wakeup pin WKUP1 polarity
      WUPP           : CR4_WUPP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  VBAT battery charging enable
      VBE            : Boolean := False;
      --  VBAT battery charging resistor selection
      VBRS           : Boolean := False;
      --  unspecified
      Reserved_10_11 : Interfaces.STM32.UInt2 := 16#0#;
      --  SMPSBYP
      SMPSBYP        : Boolean := False;
      --  EXTSMPSEN
      EXTSMPSEN      : Boolean := False;
      --  SMPSFSTEN
      SMPSFSTEN      : Boolean := False;
      --  SMPSLPEN
      SMPSLPEN       : Boolean := False;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR4_Register use record
      WUPP           at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      VBE            at 0 range 8 .. 8;
      VBRS           at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      SMPSBYP        at 0 range 12 .. 12;
      EXTSMPSEN      at 0 range 13 .. 13;
      SMPSFSTEN      at 0 range 14 .. 14;
      SMPSLPEN       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SR1_WUF array
   type SR1_WUF_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for SR1_WUF
   type SR1_WUF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUF as a value
            Val : Interfaces.STM32.UInt5;
         when True =>
            --  WUF as an array
            Arr : SR1_WUF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for SR1_WUF_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Power status register 1
   type SR1_Register is record
      --  Read-only. Wakeup flag 1
      WUF            : SR1_WUF_Field;
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3;
      --  Read-only. Standby flag
      SBF            : Boolean;
      --  unspecified
      Reserved_9_11  : Interfaces.STM32.UInt3;
      --  Read-only. SMPSBYPRDY
      SMPSBYPRDY     : Boolean;
      --  Read-only. EXTSMPSRDY
      EXTSMPSRDY     : Boolean;
      --  unspecified
      Reserved_14_14 : Interfaces.STM32.Bit;
      --  Read-only. SMPSHPRDY
      SMPSHPRDY      : Boolean;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR1_Register use record
      WUF            at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SBF            at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      SMPSBYPRDY     at 0 range 12 .. 12;
      EXTSMPSRDY     at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      SMPSHPRDY      at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SR2_PVMO array
   type SR2_PVMO_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for SR2_PVMO
   type SR2_PVMO_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PVMO as a value
            Val : Interfaces.STM32.UInt4;
         when True =>
            --  PVMO as an array
            Arr : SR2_PVMO_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SR2_PVMO_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Power status register 2
   type SR2_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.STM32.Byte;
      --  Read-only. Low-power regulator started
      REGLPS         : Boolean;
      --  Read-only. Low-power regulator flag
      REGLPF         : Boolean;
      --  Read-only. Voltage scaling flag
      VOSF           : Boolean;
      --  Read-only. Power voltage detector output
      PVDO           : Boolean;
      --  Read-only. Peripheral voltage monitoring output: VDDUSB vs. 1.2 V
      PVMO           : SR2_PVMO_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR2_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      REGLPS         at 0 range 8 .. 8;
      REGLPF         at 0 range 9 .. 9;
      VOSF           at 0 range 10 .. 10;
      PVDO           at 0 range 11 .. 11;
      PVMO           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  SCR_CWUF array
   type SCR_CWUF_Field_Array is array (1 .. 5) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for SCR_CWUF
   type SCR_CWUF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CWUF as a value
            Val : Interfaces.STM32.UInt5;
         when True =>
            --  CWUF as an array
            Arr : SCR_CWUF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for SCR_CWUF_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Power status clear register
   type SCR_Register is record
      --  Write-only. Clear wakeup flag 1
      CWUF          : SCR_CWUF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_5_7  : Interfaces.STM32.UInt3 := 16#0#;
      --  Write-only. Clear standby flag
      CSBF          : Boolean := False;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCR_Register use record
      CWUF          at 0 range 0 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      CSBF          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  PUCRA_PU array
   type PUCRA_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRA_PU
   type PUCRA_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRA_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRA_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port A pull-up control register
   type PUCRA_Register is record
      --  Port A pull-up bit y (y=0..15)
      PU             : PUCRA_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRA_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRA_PD array
   type PDCRA_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRA_PD
   type PDCRA_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRA_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRA_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port A pull-down control register
   type PDCRA_Register is record
      --  Port A pull-down bit y (y=0..15)
      PD             : PDCRA_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRA_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRB_PU array
   type PUCRB_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRB_PU
   type PUCRB_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRB_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRB_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port B pull-up control register
   type PUCRB_Register is record
      --  Port B pull-up bit y (y=0..15)
      PU             : PUCRB_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRB_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRB_PD array
   type PDCRB_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRB_PD
   type PDCRB_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRB_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRB_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port B pull-down control register
   type PDCRB_Register is record
      --  Port B pull-down bit y (y=0..15)
      PD             : PDCRB_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRB_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRC_PU array
   type PUCRC_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRC_PU
   type PUCRC_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRC_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRC_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port C pull-up control register
   type PUCRC_Register is record
      --  Port C pull-up bit y (y=0..15)
      PU             : PUCRC_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRC_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRC_PD array
   type PDCRC_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRC_PD
   type PDCRC_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRC_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRC_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port C pull-down control register
   type PDCRC_Register is record
      --  Port C pull-down bit y (y=0..15)
      PD             : PDCRC_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRC_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRD_PU array
   type PUCRD_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRD_PU
   type PUCRD_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRD_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRD_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port D pull-up control register
   type PUCRD_Register is record
      --  Port D pull-up bit y (y=0..15)
      PU             : PUCRD_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRD_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRD_PD array
   type PDCRD_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRD_PD
   type PDCRD_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRD_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRD_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port D pull-down control register
   type PDCRD_Register is record
      --  Port D pull-down bit y (y=0..15)
      PD             : PDCRD_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRD_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRE_PU array
   type PUCRE_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRE_PU
   type PUCRE_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRE_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRE_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port E pull-up control register
   type PUCRE_Register is record
      --  Port E pull-up bit y (y=0..15)
      PU             : PUCRE_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRE_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRE_PD array
   type PDCRE_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRE_PD
   type PDCRE_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRE_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRE_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port E pull-down control register
   type PDCRE_Register is record
      --  Port E pull-down bit y (y=0..15)
      PD             : PDCRE_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRE_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRF_PU array
   type PUCRF_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRF_PU
   type PUCRF_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRF_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRF_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port F pull-up control register
   type PUCRF_Register is record
      --  Port F pull-up bit y (y=0..15)
      PU             : PUCRF_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRF_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRF_PD array
   type PDCRF_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRF_PD
   type PDCRF_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRF_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRF_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port F pull-down control register
   type PDCRF_Register is record
      --  Port F pull-down bit y (y=0..15)
      PD             : PDCRF_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRF_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRG_PU array
   type PUCRG_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRG_PU
   type PUCRG_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRG_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRG_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port G pull-up control register
   type PUCRG_Register is record
      --  Port G pull-up bit y (y=0..15)
      PU             : PUCRG_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRG_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRG_PD array
   type PDCRG_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRG_PD
   type PDCRG_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRG_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRG_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port G pull-down control register
   type PDCRG_Register is record
      --  Port G pull-down bit y (y=0..15)
      PD             : PDCRG_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRG_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PUCRH_PU array
   type PUCRH_PU_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PUCRH_PU
   type PUCRH_PU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PU as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PU as an array
            Arr : PUCRH_PU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PUCRH_PU_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port H pull-up control register
   type PUCRH_Register is record
      --  Port G pull-up bit y (y=0..15)
      PU             : PUCRH_PU_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUCRH_Register use record
      PU             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  PDCRH_PD array
   type PDCRH_PD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PDCRH_PD
   type PDCRH_PD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PD as a value
            Val : Interfaces.STM32.UInt16;
         when True =>
            --  PD as an array
            Arr : PDCRH_PD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PDCRH_PD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Power Port H pull-down control register
   type PDCRH_Register is record
      --  Port G pull-down bit y (y=0..15)
      PD             : PDCRH_PD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PDCRH_Register use record
      PD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Power secure configuration register
   type SECCFGR_Register is record
      --  WKUP1 pin security
      WUP1SEC        : Boolean := False;
      --  WKUP2 pin security
      WUP2SEC        : Boolean := False;
      --  WKUP3 pin security
      WUP3SEC        : Boolean := False;
      --  WKUP4 pin security
      WUP4SEC        : Boolean := False;
      --  WKUP5 pin security
      WUP5SEC        : Boolean := False;
      --  unspecified
      Reserved_5_7   : Interfaces.STM32.UInt3 := 16#0#;
      --  LPMSEC
      LPMSEC         : Boolean := False;
      --  VDMSEC
      VDMSEC         : Boolean := False;
      --  VBSEC
      VBSEC          : Boolean := False;
      --  APCSEC
      APCSEC         : Boolean := False;
      --  unspecified
      Reserved_12_31 : Interfaces.STM32.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECCFGR_Register use record
      WUP1SEC        at 0 range 0 .. 0;
      WUP2SEC        at 0 range 1 .. 1;
      WUP3SEC        at 0 range 2 .. 2;
      WUP4SEC        at 0 range 3 .. 3;
      WUP5SEC        at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      LPMSEC         at 0 range 8 .. 8;
      VDMSEC         at 0 range 9 .. 9;
      VBSEC          at 0 range 10 .. 10;
      APCSEC         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
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

   --  Power control
   type PWR_Peripheral is record
      --  Power control register 1
      CR1      : aliased CR1_Register;
      --  Power control register 2
      CR2      : aliased CR2_Register;
      --  Power control register 3
      CR3      : aliased CR3_Register;
      --  Power control register 4
      CR4      : aliased CR4_Register;
      --  Power status register 1
      SR1      : aliased SR1_Register;
      --  Power status register 2
      SR2      : aliased SR2_Register;
      --  Power status clear register
      SCR      : aliased SCR_Register;
      --  Power Port A pull-up control register
      PUCRA    : aliased PUCRA_Register;
      --  Power Port A pull-down control register
      PDCRA    : aliased PDCRA_Register;
      --  Power Port B pull-up control register
      PUCRB    : aliased PUCRB_Register;
      --  Power Port B pull-down control register
      PDCRB    : aliased PDCRB_Register;
      --  Power Port C pull-up control register
      PUCRC    : aliased PUCRC_Register;
      --  Power Port C pull-down control register
      PDCRC    : aliased PDCRC_Register;
      --  Power Port D pull-up control register
      PUCRD    : aliased PUCRD_Register;
      --  Power Port D pull-down control register
      PDCRD    : aliased PDCRD_Register;
      --  Power Port E pull-up control register
      PUCRE    : aliased PUCRE_Register;
      --  Power Port E pull-down control register
      PDCRE    : aliased PDCRE_Register;
      --  Power Port F pull-up control register
      PUCRF    : aliased PUCRF_Register;
      --  Power Port F pull-down control register
      PDCRF    : aliased PDCRF_Register;
      --  Power Port G pull-up control register
      PUCRG    : aliased PUCRG_Register;
      --  Power Port G pull-down control register
      PDCRG    : aliased PDCRG_Register;
      --  Power Port H pull-up control register
      PUCRH    : aliased PUCRH_Register;
      --  Power Port H pull-down control register
      PDCRH    : aliased PDCRH_Register;
      --  Power secure configuration register
      SECCFGR  : aliased SECCFGR_Register;
      --  Power privilege configuration register
      PRIVCFGR : aliased PRIVCFGR_Register;
   end record
     with Volatile;

   for PWR_Peripheral use record
      CR1      at 16#0# range 0 .. 31;
      CR2      at 16#4# range 0 .. 31;
      CR3      at 16#8# range 0 .. 31;
      CR4      at 16#C# range 0 .. 31;
      SR1      at 16#10# range 0 .. 31;
      SR2      at 16#14# range 0 .. 31;
      SCR      at 16#18# range 0 .. 31;
      PUCRA    at 16#20# range 0 .. 31;
      PDCRA    at 16#24# range 0 .. 31;
      PUCRB    at 16#28# range 0 .. 31;
      PDCRB    at 16#2C# range 0 .. 31;
      PUCRC    at 16#30# range 0 .. 31;
      PDCRC    at 16#34# range 0 .. 31;
      PUCRD    at 16#38# range 0 .. 31;
      PDCRD    at 16#3C# range 0 .. 31;
      PUCRE    at 16#40# range 0 .. 31;
      PDCRE    at 16#44# range 0 .. 31;
      PUCRF    at 16#48# range 0 .. 31;
      PDCRF    at 16#4C# range 0 .. 31;
      PUCRG    at 16#50# range 0 .. 31;
      PDCRG    at 16#54# range 0 .. 31;
      PUCRH    at 16#58# range 0 .. 31;
      PDCRH    at 16#5C# range 0 .. 31;
      SECCFGR  at 16#78# range 0 .. 31;
      PRIVCFGR at 16#80# range 0 .. 31;
   end record;

   --  Power control
   PWR_Periph : aliased PWR_Peripheral
     with Import, Address => PWR_Base;

   --  Power control
   SEC_PWR_Periph : aliased PWR_Peripheral
     with Import, Address => SEC_PWR_Base;

end Interfaces.STM32.PWR;
