--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

pragma Ada_2012;

with Interfaces.Bit_Types;
with System;

--  Embedded Flash Controller
package Interfaces.SAM.EFC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   ----------------------
   -- EFC_FMR_Register --
   ----------------------

   subtype FMR_FWS_Field is Interfaces.Bit_Types.UInt4;

   --  EEFC Flash Mode Register
   type EFC_FMR_Register is record
      --  Flash Ready Interrupt Enable
      FRDY           : Boolean := False;
      --  unspecified
      Reserved_1_7   : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Flash Wait State
      FWS            : FMR_FWS_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : Interfaces.Bit_Types.UInt4 := 16#0#;
      --  Sequential Code Optimization Disable
      SCOD           : Boolean := False;
      --  unspecified
      Reserved_17_23 : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Flash Access Mode
      FAM            : Boolean := False;
      --  unspecified
      Reserved_25_25 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Code Loop Optimization Enable
      CLOE           : Boolean := True;
      --  unspecified
      Reserved_27_31 : Interfaces.Bit_Types.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC_FMR_Register use record
      FRDY           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      FWS            at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SCOD           at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      FAM            at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      CLOE           at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   ----------------------
   -- EFC_FCR_Register --
   ----------------------

   --  Flash Command
   type FCMD_Field is
     (
      --  Get Flash descriptor
      Getd,
      --  Write page
      Wp,
      --  Write page and lock
      Wpl,
      --  Erase page and write page
      Ewp,
      --  Erase page and write page then lock
      Ewpl,
      --  Erase all
      Ea,
      --  Erase pages
      Epa,
      --  Set lock bit
      Slb,
      --  Clear lock bit
      Clb,
      --  Get lock bit
      Glb,
      --  Set GPNVM bit
      Sgpb,
      --  Clear GPNVM bit
      Cgpb,
      --  Get GPNVM bit
      Ggpb,
      --  Start read unique identifier
      Stui,
      --  Stop read unique identifier
      Spui,
      --  Get CALIB bit
      Gcalb,
      --  Erase sector
      Es,
      --  Write user signature
      Wus,
      --  Erase user signature
      Eus,
      --  Start read user signature
      Stus,
      --  Stop read user signature
      Spus,
      --  Reset value for the field
      Fcmd_Field_Reset)
     with Size => 8;
   for FCMD_Field use
     (Getd => 0,
      Wp => 1,
      Wpl => 2,
      Ewp => 3,
      Ewpl => 4,
      Ea => 5,
      Epa => 7,
      Slb => 8,
      Clb => 9,
      Glb => 10,
      Sgpb => 11,
      Cgpb => 12,
      Ggpb => 13,
      Stui => 14,
      Spui => 15,
      Gcalb => 16,
      Es => 17,
      Wus => 18,
      Eus => 19,
      Stus => 20,
      Spus => 21,
      Fcmd_Field_Reset => 224);

   subtype FCR_FARG_Field is Interfaces.Bit_Types.Short;

   --  Flash Writing Protection Key
   type FKEY_Field is
     (
      --  Reset value for the field
      Fkey_Field_Reset,
      --  The 0x5A value enables the command defined by the bits of the
      --  register. If the field is written with a different value, the write
      --  is not performed and no action is started.
      Passwd)
     with Size => 8;
   for FKEY_Field use
     (Fkey_Field_Reset => 5,
      Passwd => 90);

   --  EEFC Flash Command Register
   type EFC_FCR_Register is record
      --  Write-only. Flash Command
      FCMD : FCMD_Field := Fcmd_Field_Reset;
      --  Write-only. Flash Command Argument
      FARG : FCR_FARG_Field := 16#FFB3#;
      --  Write-only. Flash Writing Protection Key
      FKEY : FKEY_Field := Fkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC_FCR_Register use record
      FCMD at 0 range 0 .. 7;
      FARG at 0 range 8 .. 23;
      FKEY at 0 range 24 .. 31;
   end record;

   ----------------------
   -- EFC_FSR_Register --
   ----------------------

   --  EEFC Flash Status Register
   type EFC_FSR_Register is record
      --  Read-only. Flash Ready Status
      FRDY          : Boolean := True;
      --  Read-only. Flash Command Error Status
      FCMDE         : Boolean := False;
      --  Read-only. Flash Lock Error Status
      FLOCKE        : Boolean := False;
      --  Read-only. Flash Error Status
      FLERR         : Boolean := False;
      --  unspecified
      Reserved_4_31 : Interfaces.Bit_Types.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC_FSR_Register use record
      FRDY          at 0 range 0 .. 0;
      FCMDE         at 0 range 1 .. 1;
      FLOCKE        at 0 range 2 .. 2;
      FLERR         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------------
   -- EFC_WPMR_Register --
   -----------------------

   --  Write Protection Key
   type WPKEY_Field is
     (
      --  Reset value for the field
      Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 24;
   for WPKEY_Field use
     (Wpkey_Field_Reset => 0,
      Passwd => 4539971);

   --  Write Protection Mode Register
   type EFC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : WPKEY_Field := Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Embedded Flash Controller
   type EFC_Peripheral is record
      --  EEFC Flash Mode Register
      FMR  : EFC_FMR_Register;
      --  EEFC Flash Command Register
      FCR  : EFC_FCR_Register;
      --  EEFC Flash Status Register
      FSR  : EFC_FSR_Register;
      --  EEFC Flash Result Register
      FRR  : Interfaces.Bit_Types.Word;
      --  Write Protection Mode Register
      WPMR : EFC_WPMR_Register;
   end record
     with Volatile;

   for EFC_Peripheral use record
      FMR  at 0 range 0 .. 31;
      FCR  at 4 range 0 .. 31;
      FSR  at 8 range 0 .. 31;
      FRR  at 12 range 0 .. 31;
      WPMR at 228 range 0 .. 31;
   end record;

   --  Embedded Flash Controller
   EFC_Periph : aliased EFC_Peripheral
     with Import, Address => EFC_Base;

end Interfaces.SAM.EFC;
