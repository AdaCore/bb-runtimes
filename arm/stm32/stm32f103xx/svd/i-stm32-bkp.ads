--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.BKP is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype DR1_D1_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR1_Register is record
      --  Backup data
      D1             : DR1_D1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR1_Register use record
      D1             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR2_D2_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR2_Register is record
      --  Backup data
      D2             : DR2_D2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR2_Register use record
      D2             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR3_D3_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR3_Register is record
      --  Backup data
      D3             : DR3_D3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR3_Register use record
      D3             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR4_D4_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR4_Register is record
      --  Backup data
      D4             : DR4_D4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR4_Register use record
      D4             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR5_D5_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR5_Register is record
      --  Backup data
      D5             : DR5_D5_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR5_Register use record
      D5             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR6_D6_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR6_Register is record
      --  Backup data
      D6             : DR6_D6_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR6_Register use record
      D6             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR7_D7_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR7_Register is record
      --  Backup data
      D7             : DR7_D7_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR7_Register use record
      D7             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR8_D8_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR8_Register is record
      --  Backup data
      D8             : DR8_D8_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR8_Register use record
      D8             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR9_D9_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR9_Register is record
      --  Backup data
      D9             : DR9_D9_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR9_Register use record
      D9             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR10_D10_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR10_Register is record
      --  Backup data
      D10            : DR10_D10_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR10_Register use record
      D10            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTCCR_CAL_Field is Interfaces.STM32.UInt7;
   subtype RTCCR_CCO_Field is Interfaces.STM32.Bit;
   subtype RTCCR_ASOE_Field is Interfaces.STM32.Bit;
   subtype RTCCR_ASOS_Field is Interfaces.STM32.Bit;

   --  RTC clock calibration register (BKP_RTCCR)
   type RTCCR_Register is record
      --  Calibration value
      CAL            : RTCCR_CAL_Field := 16#0#;
      --  Calibration Clock Output
      CCO            : RTCCR_CCO_Field := 16#0#;
      --  Alarm or second output enable
      ASOE           : RTCCR_ASOE_Field := 16#0#;
      --  Alarm or second output selection
      ASOS           : RTCCR_ASOS_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.STM32.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTCCR_Register use record
      CAL            at 0 range 0 .. 6;
      CCO            at 0 range 7 .. 7;
      ASOE           at 0 range 8 .. 8;
      ASOS           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype CR_TPE_Field is Interfaces.STM32.Bit;
   subtype CR_TPAL_Field is Interfaces.STM32.Bit;

   --  Backup control register (BKP_CR)
   type CR_Register is record
      --  Tamper pin enable
      TPE           : CR_TPE_Field := 16#0#;
      --  Tamper pin active level
      TPAL          : CR_TPAL_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : Interfaces.STM32.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      TPE           at 0 range 0 .. 0;
      TPAL          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CSR_CTE_Field is Interfaces.STM32.Bit;
   subtype CSR_CTI_Field is Interfaces.STM32.Bit;
   subtype CSR_TPIE_Field is Interfaces.STM32.Bit;
   subtype CSR_TEF_Field is Interfaces.STM32.Bit;
   subtype CSR_TIF_Field is Interfaces.STM32.Bit;

   --  BKP_CSR control/status register (BKP_CSR)
   type CSR_Register is record
      --  Write-only. Clear Tamper event
      CTE            : CSR_CTE_Field := 16#0#;
      --  Write-only. Clear Tamper Interrupt
      CTI            : CSR_CTI_Field := 16#0#;
      --  Tamper Pin interrupt enable
      TPIE           : CSR_TPIE_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : Interfaces.STM32.UInt5 := 16#0#;
      --  Read-only. Tamper Event Flag
      TEF            : CSR_TEF_Field := 16#0#;
      --  Read-only. Tamper Interrupt Flag
      TIF            : CSR_TIF_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.STM32.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      CTE            at 0 range 0 .. 0;
      CTI            at 0 range 1 .. 1;
      TPIE           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TEF            at 0 range 8 .. 8;
      TIF            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DR11_DR11_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR11_Register is record
      --  Backup data
      DR11           : DR11_DR11_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR11_Register use record
      DR11           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR12_DR12_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR12_Register is record
      --  Backup data
      DR12           : DR12_DR12_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR12_Register use record
      DR12           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR13_DR13_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR13_Register is record
      --  Backup data
      DR13           : DR13_DR13_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR13_Register use record
      DR13           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR14_D14_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR14_Register is record
      --  Backup data
      D14            : DR14_D14_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR14_Register use record
      D14            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR15_D15_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR15_Register is record
      --  Backup data
      D15            : DR15_D15_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR15_Register use record
      D15            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR16_D16_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR16_Register is record
      --  Backup data
      D16            : DR16_D16_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR16_Register use record
      D16            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR17_D17_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR17_Register is record
      --  Backup data
      D17            : DR17_D17_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR17_Register use record
      D17            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR18_D18_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR18_Register is record
      --  Backup data
      D18            : DR18_D18_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR18_Register use record
      D18            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR19_D19_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR19_Register is record
      --  Backup data
      D19            : DR19_D19_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR19_Register use record
      D19            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR20_D20_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR20_Register is record
      --  Backup data
      D20            : DR20_D20_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR20_Register use record
      D20            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR21_D21_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR21_Register is record
      --  Backup data
      D21            : DR21_D21_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR21_Register use record
      D21            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR22_D22_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR22_Register is record
      --  Backup data
      D22            : DR22_D22_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR22_Register use record
      D22            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR23_D23_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR23_Register is record
      --  Backup data
      D23            : DR23_D23_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR23_Register use record
      D23            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR24_D24_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR24_Register is record
      --  Backup data
      D24            : DR24_D24_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR24_Register use record
      D24            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR25_D25_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR25_Register is record
      --  Backup data
      D25            : DR25_D25_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR25_Register use record
      D25            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR26_D26_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR26_Register is record
      --  Backup data
      D26            : DR26_D26_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR26_Register use record
      D26            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR27_D27_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR27_Register is record
      --  Backup data
      D27            : DR27_D27_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR27_Register use record
      D27            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR28_D28_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR28_Register is record
      --  Backup data
      D28            : DR28_D28_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR28_Register use record
      D28            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR29_D29_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR29_Register is record
      --  Backup data
      D29            : DR29_D29_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR29_Register use record
      D29            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR30_D30_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR30_Register is record
      --  Backup data
      D30            : DR30_D30_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR30_Register use record
      D30            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR31_D31_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR31_Register is record
      --  Backup data
      D31            : DR31_D31_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR31_Register use record
      D31            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR32_D32_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR32_Register is record
      --  Backup data
      D32            : DR32_D32_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR32_Register use record
      D32            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR33_D33_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR33_Register is record
      --  Backup data
      D33            : DR33_D33_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR33_Register use record
      D33            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR34_D34_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR34_Register is record
      --  Backup data
      D34            : DR34_D34_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR34_Register use record
      D34            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR35_D35_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR35_Register is record
      --  Backup data
      D35            : DR35_D35_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR35_Register use record
      D35            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR36_D36_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR36_Register is record
      --  Backup data
      D36            : DR36_D36_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR36_Register use record
      D36            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR37_D37_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR37_Register is record
      --  Backup data
      D37            : DR37_D37_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR37_Register use record
      D37            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR38_D38_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR38_Register is record
      --  Backup data
      D38            : DR38_D38_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR38_Register use record
      D38            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR39_D39_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR39_Register is record
      --  Backup data
      D39            : DR39_D39_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR39_Register use record
      D39            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR40_D40_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR40_Register is record
      --  Backup data
      D40            : DR40_D40_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR40_Register use record
      D40            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR41_D41_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR41_Register is record
      --  Backup data
      D41            : DR41_D41_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR41_Register use record
      D41            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DR42_D42_Field is Interfaces.STM32.UInt16;

   --  Backup data register (BKP_DR)
   type DR42_Register is record
      --  Backup data
      D42            : DR42_D42_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR42_Register use record
      D42            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Backup registers
   type BKP_Peripheral is record
      --  Backup data register (BKP_DR)
      DR1   : aliased DR1_Register;
      --  Backup data register (BKP_DR)
      DR2   : aliased DR2_Register;
      --  Backup data register (BKP_DR)
      DR3   : aliased DR3_Register;
      --  Backup data register (BKP_DR)
      DR4   : aliased DR4_Register;
      --  Backup data register (BKP_DR)
      DR5   : aliased DR5_Register;
      --  Backup data register (BKP_DR)
      DR6   : aliased DR6_Register;
      --  Backup data register (BKP_DR)
      DR7   : aliased DR7_Register;
      --  Backup data register (BKP_DR)
      DR8   : aliased DR8_Register;
      --  Backup data register (BKP_DR)
      DR9   : aliased DR9_Register;
      --  Backup data register (BKP_DR)
      DR10  : aliased DR10_Register;
      --  RTC clock calibration register (BKP_RTCCR)
      RTCCR : aliased RTCCR_Register;
      --  Backup control register (BKP_CR)
      CR    : aliased CR_Register;
      --  BKP_CSR control/status register (BKP_CSR)
      CSR   : aliased CSR_Register;
      --  Backup data register (BKP_DR)
      DR11  : aliased DR11_Register;
      --  Backup data register (BKP_DR)
      DR12  : aliased DR12_Register;
      --  Backup data register (BKP_DR)
      DR13  : aliased DR13_Register;
      --  Backup data register (BKP_DR)
      DR14  : aliased DR14_Register;
      --  Backup data register (BKP_DR)
      DR15  : aliased DR15_Register;
      --  Backup data register (BKP_DR)
      DR16  : aliased DR16_Register;
      --  Backup data register (BKP_DR)
      DR17  : aliased DR17_Register;
      --  Backup data register (BKP_DR)
      DR18  : aliased DR18_Register;
      --  Backup data register (BKP_DR)
      DR19  : aliased DR19_Register;
      --  Backup data register (BKP_DR)
      DR20  : aliased DR20_Register;
      --  Backup data register (BKP_DR)
      DR21  : aliased DR21_Register;
      --  Backup data register (BKP_DR)
      DR22  : aliased DR22_Register;
      --  Backup data register (BKP_DR)
      DR23  : aliased DR23_Register;
      --  Backup data register (BKP_DR)
      DR24  : aliased DR24_Register;
      --  Backup data register (BKP_DR)
      DR25  : aliased DR25_Register;
      --  Backup data register (BKP_DR)
      DR26  : aliased DR26_Register;
      --  Backup data register (BKP_DR)
      DR27  : aliased DR27_Register;
      --  Backup data register (BKP_DR)
      DR28  : aliased DR28_Register;
      --  Backup data register (BKP_DR)
      DR29  : aliased DR29_Register;
      --  Backup data register (BKP_DR)
      DR30  : aliased DR30_Register;
      --  Backup data register (BKP_DR)
      DR31  : aliased DR31_Register;
      --  Backup data register (BKP_DR)
      DR32  : aliased DR32_Register;
      --  Backup data register (BKP_DR)
      DR33  : aliased DR33_Register;
      --  Backup data register (BKP_DR)
      DR34  : aliased DR34_Register;
      --  Backup data register (BKP_DR)
      DR35  : aliased DR35_Register;
      --  Backup data register (BKP_DR)
      DR36  : aliased DR36_Register;
      --  Backup data register (BKP_DR)
      DR37  : aliased DR37_Register;
      --  Backup data register (BKP_DR)
      DR38  : aliased DR38_Register;
      --  Backup data register (BKP_DR)
      DR39  : aliased DR39_Register;
      --  Backup data register (BKP_DR)
      DR40  : aliased DR40_Register;
      --  Backup data register (BKP_DR)
      DR41  : aliased DR41_Register;
      --  Backup data register (BKP_DR)
      DR42  : aliased DR42_Register;
   end record
     with Volatile;

   for BKP_Peripheral use record
      DR1   at 16#0# range 0 .. 31;
      DR2   at 16#4# range 0 .. 31;
      DR3   at 16#8# range 0 .. 31;
      DR4   at 16#C# range 0 .. 31;
      DR5   at 16#10# range 0 .. 31;
      DR6   at 16#14# range 0 .. 31;
      DR7   at 16#18# range 0 .. 31;
      DR8   at 16#1C# range 0 .. 31;
      DR9   at 16#20# range 0 .. 31;
      DR10  at 16#24# range 0 .. 31;
      RTCCR at 16#28# range 0 .. 31;
      CR    at 16#2C# range 0 .. 31;
      CSR   at 16#30# range 0 .. 31;
      DR11  at 16#3C# range 0 .. 31;
      DR12  at 16#40# range 0 .. 31;
      DR13  at 16#44# range 0 .. 31;
      DR14  at 16#48# range 0 .. 31;
      DR15  at 16#4C# range 0 .. 31;
      DR16  at 16#50# range 0 .. 31;
      DR17  at 16#54# range 0 .. 31;
      DR18  at 16#58# range 0 .. 31;
      DR19  at 16#5C# range 0 .. 31;
      DR20  at 16#60# range 0 .. 31;
      DR21  at 16#64# range 0 .. 31;
      DR22  at 16#68# range 0 .. 31;
      DR23  at 16#6C# range 0 .. 31;
      DR24  at 16#70# range 0 .. 31;
      DR25  at 16#74# range 0 .. 31;
      DR26  at 16#78# range 0 .. 31;
      DR27  at 16#7C# range 0 .. 31;
      DR28  at 16#80# range 0 .. 31;
      DR29  at 16#84# range 0 .. 31;
      DR30  at 16#88# range 0 .. 31;
      DR31  at 16#8C# range 0 .. 31;
      DR32  at 16#90# range 0 .. 31;
      DR33  at 16#94# range 0 .. 31;
      DR34  at 16#98# range 0 .. 31;
      DR35  at 16#9C# range 0 .. 31;
      DR36  at 16#A0# range 0 .. 31;
      DR37  at 16#A4# range 0 .. 31;
      DR38  at 16#A8# range 0 .. 31;
      DR39  at 16#AC# range 0 .. 31;
      DR40  at 16#B0# range 0 .. 31;
      DR41  at 16#B4# range 0 .. 31;
      DR42  at 16#B8# range 0 .. 31;
   end record;

   --  Backup registers
   BKP_Periph : aliased BKP_Peripheral
     with Import, Address => BKP_Base;

end Interfaces.STM32.BKP;
