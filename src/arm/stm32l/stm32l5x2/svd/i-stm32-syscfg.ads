--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32L562.svd


with System;

package Interfaces.STM32.SYSCFG is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  SYSCFG secure configuration register
   type SECCFGR_Register is record
      --  SYSCFG clock control security
      SYSCFGSEC     : Boolean := False;
      --  ClassB security
      CLASSBSEC     : Boolean := False;
      --  SRAM2 security
      SRAM2SEC      : Boolean := False;
      --  FPUSEC
      FPUSEC        : Boolean := False;
      --  unspecified
      Reserved_4_31 : Interfaces.STM32.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SECCFGR_Register use record
      SYSCFGSEC     at 0 range 0 .. 0;
      CLASSBSEC     at 0 range 1 .. 1;
      SRAM2SEC      at 0 range 2 .. 2;
      FPUSEC        at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  configuration register 1
   type CFGR1_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.STM32.Byte := 16#0#;
      --  I/O analog switch voltage booster enable
      BOOSTEN        : Boolean := False;
      --  GPIO analog switch control voltage selection
      ANASWVDD       : Boolean := False;
      --  unspecified
      Reserved_10_15 : Interfaces.STM32.UInt6 := 16#0#;
      --  Fast-mode Plus (Fm+) driving capability activation on PB6
      I2C_PB6_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB7
      I2C_PB7_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB8
      I2C_PB8_FMP    : Boolean := False;
      --  Fast-mode Plus (Fm+) driving capability activation on PB9
      I2C_PB9_FMP    : Boolean := False;
      --  I2C1 Fast-mode Plus driving capability activation
      I2C1_FMP       : Boolean := False;
      --  I2C2 Fast-mode Plus driving capability activation
      I2C2_FMP       : Boolean := False;
      --  I2C3 Fast-mode Plus driving capability activation
      I2C3_FMP       : Boolean := False;
      --  I2C4_FMP
      I2C4_FMP       : Boolean := False;
      --  unspecified
      Reserved_24_31 : Interfaces.STM32.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR1_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      BOOSTEN        at 0 range 8 .. 8;
      ANASWVDD       at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      I2C_PB6_FMP    at 0 range 16 .. 16;
      I2C_PB7_FMP    at 0 range 17 .. 17;
      I2C_PB8_FMP    at 0 range 18 .. 18;
      I2C_PB9_FMP    at 0 range 19 .. 19;
      I2C1_FMP       at 0 range 20 .. 20;
      I2C2_FMP       at 0 range 21 .. 21;
      I2C3_FMP       at 0 range 22 .. 22;
      I2C4_FMP       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  FPU interrupt mask register
   type FPUIMR_Register is record
      --  Floating point unit interrupts enable bits
      FPU_IE        : Interfaces.STM32.UInt6 := 16#1F#;
      --  unspecified
      Reserved_6_31 : Interfaces.STM32.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPUIMR_Register use record
      FPU_IE        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  SYSCFG CPU non-secure lock register
   type CNSLCKR_Register is record
      --  VTOR_NS register lock
      LOCKNSVTOR    : Boolean := False;
      --  Non-secure MPU registers lock
      LOCKNSMPU     : Boolean := False;
      --  unspecified
      Reserved_2_31 : Interfaces.STM32.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNSLCKR_Register use record
      LOCKNSVTOR    at 0 range 0 .. 0;
      LOCKNSMPU     at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SYSCFG CPU secure lock register
   type CSLOCKR_Register is record
      --  LOCKSVTAIRCR
      LOCKSVTAIRCR  : Boolean := False;
      --  LOCKSMPU
      LOCKSMPU      : Boolean := False;
      --  LOCKSAU
      LOCKSAU       : Boolean := False;
      --  unspecified
      Reserved_3_31 : Interfaces.STM32.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSLOCKR_Register use record
      LOCKSVTAIRCR  at 0 range 0 .. 0;
      LOCKSMPU      at 0 range 1 .. 1;
      LOCKSAU       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  CFGR2
   type CFGR2_Register is record
      --  Write-only. LOCKUP (hardfault) output enable bit
      CLL           : Boolean := False;
      --  Write-only. SRAM2 parity lock bit
      SPL           : Boolean := False;
      --  Write-only. PVD lock enable bit
      PVDL          : Boolean := False;
      --  Write-only. ECC Lock
      ECCL          : Boolean := False;
      --  unspecified
      Reserved_4_7  : Interfaces.STM32.UInt4 := 16#0#;
      --  SRAM2 parity error flag
      SPF           : Boolean := False;
      --  unspecified
      Reserved_9_31 : Interfaces.STM32.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      CLL           at 0 range 0 .. 0;
      SPL           at 0 range 1 .. 1;
      PVDL          at 0 range 2 .. 2;
      ECCL          at 0 range 3 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      SPF           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  SCSR
   type SCSR_Register is record
      --  SRAM2 Erase
      SRAM2ER       : Boolean := False;
      --  Read-only. SRAM2 busy by erase operation
      SRAM2BSY      : Boolean := False;
      --  unspecified
      Reserved_2_31 : Interfaces.STM32.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCSR_Register use record
      SRAM2ER       at 0 range 0 .. 0;
      SRAM2BSY      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SKR
   type SKR_Register is record
      --  Write-only. SRAM2 write protection key for software erase
      KEY           : Interfaces.STM32.Byte := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SKR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  SWPR
   type SWPR_Register is record
      --  Write-only. P0WP
      P0WP  : Boolean := False;
      --  Write-only. P1WP
      P1WP  : Boolean := False;
      --  Write-only. P2WP
      P2WP  : Boolean := False;
      --  Write-only. P3WP
      P3WP  : Boolean := False;
      --  Write-only. P4WP
      P4WP  : Boolean := False;
      --  Write-only. P5WP
      P5WP  : Boolean := False;
      --  Write-only. P6WP
      P6WP  : Boolean := False;
      --  Write-only. P7WP
      P7WP  : Boolean := False;
      --  Write-only. P8WP
      P8WP  : Boolean := False;
      --  Write-only. P9WP
      P9WP  : Boolean := False;
      --  Write-only. P10WP
      P10WP : Boolean := False;
      --  Write-only. P11WP
      P11WP : Boolean := False;
      --  Write-only. P12WP
      P12WP : Boolean := False;
      --  Write-only. P13WP
      P13WP : Boolean := False;
      --  Write-only. P14WP
      P14WP : Boolean := False;
      --  Write-only. P15WP
      P15WP : Boolean := False;
      --  Write-only. P16WP
      P16WP : Boolean := False;
      --  Write-only. P17WP
      P17WP : Boolean := False;
      --  Write-only. P18WP
      P18WP : Boolean := False;
      --  Write-only. P19WP
      P19WP : Boolean := False;
      --  Write-only. P20WP
      P20WP : Boolean := False;
      --  Write-only. P21WP
      P21WP : Boolean := False;
      --  Write-only. P22WP
      P22WP : Boolean := False;
      --  Write-only. P23WP
      P23WP : Boolean := False;
      --  Write-only. P24WP
      P24WP : Boolean := False;
      --  Write-only. P25WP
      P25WP : Boolean := False;
      --  Write-only. P26WP
      P26WP : Boolean := False;
      --  Write-only. P27WP
      P27WP : Boolean := False;
      --  Write-only. P28WP
      P28WP : Boolean := False;
      --  Write-only. P29WP
      P29WP : Boolean := False;
      --  Write-only. P30WP
      P30WP : Boolean := False;
      --  Write-only. SRAM2 page 31 write protection
      P31WP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWPR_Register use record
      P0WP  at 0 range 0 .. 0;
      P1WP  at 0 range 1 .. 1;
      P2WP  at 0 range 2 .. 2;
      P3WP  at 0 range 3 .. 3;
      P4WP  at 0 range 4 .. 4;
      P5WP  at 0 range 5 .. 5;
      P6WP  at 0 range 6 .. 6;
      P7WP  at 0 range 7 .. 7;
      P8WP  at 0 range 8 .. 8;
      P9WP  at 0 range 9 .. 9;
      P10WP at 0 range 10 .. 10;
      P11WP at 0 range 11 .. 11;
      P12WP at 0 range 12 .. 12;
      P13WP at 0 range 13 .. 13;
      P14WP at 0 range 14 .. 14;
      P15WP at 0 range 15 .. 15;
      P16WP at 0 range 16 .. 16;
      P17WP at 0 range 17 .. 17;
      P18WP at 0 range 18 .. 18;
      P19WP at 0 range 19 .. 19;
      P20WP at 0 range 20 .. 20;
      P21WP at 0 range 21 .. 21;
      P22WP at 0 range 22 .. 22;
      P23WP at 0 range 23 .. 23;
      P24WP at 0 range 24 .. 24;
      P25WP at 0 range 25 .. 25;
      P26WP at 0 range 26 .. 26;
      P27WP at 0 range 27 .. 27;
      P28WP at 0 range 28 .. 28;
      P29WP at 0 range 29 .. 29;
      P30WP at 0 range 30 .. 30;
      P31WP at 0 range 31 .. 31;
   end record;

   --  SWPR2
   type SWPR2_Register is record
      --  Write-only. P32WP
      P32WP : Boolean := False;
      --  Write-only. P33WP
      P33WP : Boolean := False;
      --  Write-only. P34WP
      P34WP : Boolean := False;
      --  Write-only. P35WP
      P35WP : Boolean := False;
      --  Write-only. P36WP
      P36WP : Boolean := False;
      --  Write-only. P37WP
      P37WP : Boolean := False;
      --  Write-only. P38WP
      P38WP : Boolean := False;
      --  Write-only. P39WP
      P39WP : Boolean := False;
      --  Write-only. P40WP
      P40WP : Boolean := False;
      --  Write-only. P41WP
      P41WP : Boolean := False;
      --  Write-only. P42WP
      P42WP : Boolean := False;
      --  Write-only. P43WP
      P43WP : Boolean := False;
      --  Write-only. P44WP
      P44WP : Boolean := False;
      --  Write-only. P45WP
      P45WP : Boolean := False;
      --  Write-only. P46WP
      P46WP : Boolean := False;
      --  Write-only. P47WP
      P47WP : Boolean := False;
      --  Write-only. P48WP
      P48WP : Boolean := False;
      --  Write-only. P49WP
      P49WP : Boolean := False;
      --  Write-only. P50WP
      P50WP : Boolean := False;
      --  Write-only. P51WP
      P51WP : Boolean := False;
      --  Write-only. P52WP
      P52WP : Boolean := False;
      --  Write-only. P53WP
      P53WP : Boolean := False;
      --  Write-only. P54WP
      P54WP : Boolean := False;
      --  Write-only. P55WP
      P55WP : Boolean := False;
      --  Write-only. P56WP
      P56WP : Boolean := False;
      --  Write-only. P57WP
      P57WP : Boolean := False;
      --  Write-only. P58WP
      P58WP : Boolean := False;
      --  Write-only. P59WP
      P59WP : Boolean := False;
      --  Write-only. P60WP
      P60WP : Boolean := False;
      --  Write-only. P61WP
      P61WP : Boolean := False;
      --  Write-only. P62WP
      P62WP : Boolean := False;
      --  Write-only. P63WP
      P63WP : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWPR2_Register use record
      P32WP at 0 range 0 .. 0;
      P33WP at 0 range 1 .. 1;
      P34WP at 0 range 2 .. 2;
      P35WP at 0 range 3 .. 3;
      P36WP at 0 range 4 .. 4;
      P37WP at 0 range 5 .. 5;
      P38WP at 0 range 6 .. 6;
      P39WP at 0 range 7 .. 7;
      P40WP at 0 range 8 .. 8;
      P41WP at 0 range 9 .. 9;
      P42WP at 0 range 10 .. 10;
      P43WP at 0 range 11 .. 11;
      P44WP at 0 range 12 .. 12;
      P45WP at 0 range 13 .. 13;
      P46WP at 0 range 14 .. 14;
      P47WP at 0 range 15 .. 15;
      P48WP at 0 range 16 .. 16;
      P49WP at 0 range 17 .. 17;
      P50WP at 0 range 18 .. 18;
      P51WP at 0 range 19 .. 19;
      P52WP at 0 range 20 .. 20;
      P53WP at 0 range 21 .. 21;
      P54WP at 0 range 22 .. 22;
      P55WP at 0 range 23 .. 23;
      P56WP at 0 range 24 .. 24;
      P57WP at 0 range 25 .. 25;
      P58WP at 0 range 26 .. 26;
      P59WP at 0 range 27 .. 27;
      P60WP at 0 range 28 .. 28;
      P61WP at 0 range 29 .. 29;
      P62WP at 0 range 30 .. 30;
      P63WP at 0 range 31 .. 31;
   end record;

   --  RSSCMDR
   type RSSCMDR_Register is record
      --  RSS commands
      RSSCMD        : Interfaces.STM32.Byte := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.STM32.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSSCMDR_Register use record
      RSSCMD        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System configuration controller
   type SYSCFG_Peripheral is record
      --  SYSCFG secure configuration register
      SECCFGR : aliased SECCFGR_Register;
      --  configuration register 1
      CFGR1   : aliased CFGR1_Register;
      --  FPU interrupt mask register
      FPUIMR  : aliased FPUIMR_Register;
      --  SYSCFG CPU non-secure lock register
      CNSLCKR : aliased CNSLCKR_Register;
      --  SYSCFG CPU secure lock register
      CSLOCKR : aliased CSLOCKR_Register;
      --  CFGR2
      CFGR2   : aliased CFGR2_Register;
      --  SCSR
      SCSR    : aliased SCSR_Register;
      --  SKR
      SKR     : aliased SKR_Register;
      --  SWPR
      SWPR    : aliased SWPR_Register;
      --  SWPR2
      SWPR2   : aliased SWPR2_Register;
      --  RSSCMDR
      RSSCMDR : aliased RSSCMDR_Register;
   end record
     with Volatile;

   for SYSCFG_Peripheral use record
      SECCFGR at 16#0# range 0 .. 31;
      CFGR1   at 16#4# range 0 .. 31;
      FPUIMR  at 16#8# range 0 .. 31;
      CNSLCKR at 16#C# range 0 .. 31;
      CSLOCKR at 16#10# range 0 .. 31;
      CFGR2   at 16#14# range 0 .. 31;
      SCSR    at 16#18# range 0 .. 31;
      SKR     at 16#1C# range 0 .. 31;
      SWPR    at 16#20# range 0 .. 31;
      SWPR2   at 16#24# range 0 .. 31;
      RSSCMDR at 16#2C# range 0 .. 31;
   end record;

   --  System configuration controller
   SEC_SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => SEC_SYSCFG_Base;

   --  System configuration controller
   SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => SYSCFG_Base;

end Interfaces.STM32.SYSCFG;
