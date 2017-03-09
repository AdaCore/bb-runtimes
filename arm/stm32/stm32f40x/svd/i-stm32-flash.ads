--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from STM32F40x.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.FLASH is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype ACR_LATENCY_Field is Interfaces.STM32.UInt3;
   subtype ACR_PRFTEN_Field is Interfaces.STM32.Bit;
   subtype ACR_ICEN_Field is Interfaces.STM32.Bit;
   subtype ACR_DCEN_Field is Interfaces.STM32.Bit;
   subtype ACR_ICRST_Field is Interfaces.STM32.Bit;
   subtype ACR_DCRST_Field is Interfaces.STM32.Bit;

   --  Flash access control register
   type ACR_Register is record
      --  Latency
      LATENCY        : ACR_LATENCY_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : Interfaces.STM32.UInt5 := 16#0#;
      --  Prefetch enable
      PRFTEN         : ACR_PRFTEN_Field := 16#0#;
      --  Instruction cache enable
      ICEN           : ACR_ICEN_Field := 16#0#;
      --  Data cache enable
      DCEN           : ACR_DCEN_Field := 16#0#;
      --  Write-only. Instruction cache reset
      ICRST          : ACR_ICRST_Field := 16#0#;
      --  Data cache reset
      DCRST          : ACR_DCRST_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.STM32.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR_Register use record
      LATENCY        at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PRFTEN         at 0 range 8 .. 8;
      ICEN           at 0 range 9 .. 9;
      DCEN           at 0 range 10 .. 10;
      ICRST          at 0 range 11 .. 11;
      DCRST          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype SR_EOP_Field is Interfaces.STM32.Bit;
   subtype SR_OPERR_Field is Interfaces.STM32.Bit;
   subtype SR_WRPERR_Field is Interfaces.STM32.Bit;
   subtype SR_PGAERR_Field is Interfaces.STM32.Bit;
   subtype SR_PGPERR_Field is Interfaces.STM32.Bit;
   subtype SR_PGSERR_Field is Interfaces.STM32.Bit;
   subtype SR_BSY_Field is Interfaces.STM32.Bit;

   --  Status register
   type SR_Register is record
      --  End of operation
      EOP            : SR_EOP_Field := 16#0#;
      --  Operation error
      OPERR          : SR_OPERR_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : Interfaces.STM32.UInt2 := 16#0#;
      --  Write protection error
      WRPERR         : SR_WRPERR_Field := 16#0#;
      --  Programming alignment error
      PGAERR         : SR_PGAERR_Field := 16#0#;
      --  Programming parallelism error
      PGPERR         : SR_PGPERR_Field := 16#0#;
      --  Programming sequence error
      PGSERR         : SR_PGSERR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : Interfaces.STM32.Byte := 16#0#;
      --  Read-only. Busy
      BSY            : SR_BSY_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : Interfaces.STM32.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      EOP            at 0 range 0 .. 0;
      OPERR          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      WRPERR         at 0 range 4 .. 4;
      PGAERR         at 0 range 5 .. 5;
      PGPERR         at 0 range 6 .. 6;
      PGSERR         at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      BSY            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CR_PG_Field is Interfaces.STM32.Bit;
   subtype CR_SER_Field is Interfaces.STM32.Bit;
   subtype CR_MER_Field is Interfaces.STM32.Bit;
   subtype CR_SNB_Field is Interfaces.STM32.UInt4;
   subtype CR_PSIZE_Field is Interfaces.STM32.UInt2;
   subtype CR_STRT_Field is Interfaces.STM32.Bit;
   subtype CR_EOPIE_Field is Interfaces.STM32.Bit;
   subtype CR_ERRIE_Field is Interfaces.STM32.Bit;
   subtype CR_LOCK_Field is Interfaces.STM32.Bit;

   --  Control register
   type CR_Register is record
      --  Programming
      PG             : CR_PG_Field := 16#0#;
      --  Sector Erase
      SER            : CR_SER_Field := 16#0#;
      --  Mass Erase
      MER            : CR_MER_Field := 16#0#;
      --  Sector number
      SNB            : CR_SNB_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.STM32.Bit := 16#0#;
      --  Program size
      PSIZE          : CR_PSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : Interfaces.STM32.UInt6 := 16#0#;
      --  Start
      STRT           : CR_STRT_Field := 16#0#;
      --  unspecified
      Reserved_17_23 : Interfaces.STM32.UInt7 := 16#0#;
      --  End of operation interrupt enable
      EOPIE          : CR_EOPIE_Field := 16#0#;
      --  Error interrupt enable
      ERRIE          : CR_ERRIE_Field := 16#0#;
      --  unspecified
      Reserved_26_30 : Interfaces.STM32.UInt5 := 16#0#;
      --  Lock
      LOCK           : CR_LOCK_Field := 16#1#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      PG             at 0 range 0 .. 0;
      SER            at 0 range 1 .. 1;
      MER            at 0 range 2 .. 2;
      SNB            at 0 range 3 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PSIZE          at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      STRT           at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      EOPIE          at 0 range 24 .. 24;
      ERRIE          at 0 range 25 .. 25;
      Reserved_26_30 at 0 range 26 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPTCR_OPTLOCK_Field is Interfaces.STM32.Bit;
   subtype OPTCR_OPTSTRT_Field is Interfaces.STM32.Bit;
   subtype OPTCR_BOR_LEV_Field is Interfaces.STM32.UInt2;
   subtype OPTCR_WDG_SW_Field is Interfaces.STM32.Bit;
   subtype OPTCR_nRST_STOP_Field is Interfaces.STM32.Bit;
   subtype OPTCR_nRST_STDBY_Field is Interfaces.STM32.Bit;
   subtype OPTCR_RDP_Field is Interfaces.STM32.Byte;
   subtype OPTCR_nWRP_Field is Interfaces.STM32.UInt12;

   --  Flash option control register
   type OPTCR_Register is record
      --  Option lock
      OPTLOCK        : OPTCR_OPTLOCK_Field := 16#0#;
      --  Option start
      OPTSTRT        : OPTCR_OPTSTRT_Field := 16#0#;
      --  BOR reset Level
      BOR_LEV        : OPTCR_BOR_LEV_Field := 16#1#;
      --  unspecified
      Reserved_4_4   : Interfaces.STM32.Bit := 16#1#;
      --  WDG_SW User option bytes
      WDG_SW         : OPTCR_WDG_SW_Field := 16#0#;
      --  nRST_STOP User option bytes
      nRST_STOP      : OPTCR_nRST_STOP_Field := 16#0#;
      --  nRST_STDBY User option bytes
      nRST_STDBY     : OPTCR_nRST_STDBY_Field := 16#0#;
      --  Read protect
      RDP            : OPTCR_RDP_Field := 16#0#;
      --  Not write protect
      nWRP           : OPTCR_nWRP_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : Interfaces.STM32.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTCR_Register use record
      OPTLOCK        at 0 range 0 .. 0;
      OPTSTRT        at 0 range 1 .. 1;
      BOR_LEV        at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      WDG_SW         at 0 range 5 .. 5;
      nRST_STOP      at 0 range 6 .. 6;
      nRST_STDBY     at 0 range 7 .. 7;
      RDP            at 0 range 8 .. 15;
      nWRP           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FLASH
   type FLASH_Peripheral is record
      --  Flash access control register
      ACR     : aliased ACR_Register;
      --  Flash key register
      KEYR    : aliased Interfaces.STM32.UInt32;
      --  Flash option key register
      OPTKEYR : aliased Interfaces.STM32.UInt32;
      --  Status register
      SR      : aliased SR_Register;
      --  Control register
      CR      : aliased CR_Register;
      --  Flash option control register
      OPTCR   : aliased OPTCR_Register;
   end record
     with Volatile;

   for FLASH_Peripheral use record
      ACR     at 16#0# range 0 .. 31;
      KEYR    at 16#4# range 0 .. 31;
      OPTKEYR at 16#8# range 0 .. 31;
      SR      at 16#C# range 0 .. 31;
      CR      at 16#10# range 0 .. 31;
      OPTCR   at 16#14# range 0 .. 31;
   end record;

   --  FLASH
   FLASH_Periph : aliased FLASH_Peripheral
     with Import, Address => System'To_Address (16#40023C00#);

end Interfaces.STM32.FLASH;
