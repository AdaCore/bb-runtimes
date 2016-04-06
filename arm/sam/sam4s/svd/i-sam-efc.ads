--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAM4SD32C.svd

pragma Ada_2012;

with Interfaces.Bit_Types;
with System;

package Interfaces.SAM.EFC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   -----------------------
   -- EFC0_FMR_Register --
   -----------------------

   subtype FMR_FWS_Field is Interfaces.Bit_Types.UInt4;

   --  EEFC Flash Mode Register
   type EFC0_FMR_Register is record
      --  Ready Interrupt Enable
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
      --  Code Loops Optimization Enable
      CLOE           : Boolean := True;
      --  unspecified
      Reserved_27_31 : Interfaces.Bit_Types.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC0_FMR_Register use record
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

   -----------------------
   -- EFC0_FCR_Register --
   -----------------------

   --  Flash Command
   type FCMD_Field is
     (
      --  Get Flash Descriptor
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
      --  Erase Pages
      Epa,
      --  Set Lock Bit
      Slb,
      --  Clear Lock Bit
      Clb,
      --  Get Lock Bit
      Glb,
      --  Set GPNVM Bit
      Sgpb,
      --  Clear GPNVM Bit
      Cgpb,
      --  Get GPNVM Bit
      Ggpb,
      --  Start Read Unique Identifier
      Stui,
      --  Stop Read Unique Identifier
      Spui,
      --  Get CALIB Bit
      Gcalb,
      --  Erase Sector
      Es,
      --  Write User Signature
      Wus,
      --  Erase User Signature
      Eus,
      --  Start Read User Signature
      Stus,
      --  Stop Read User Signature
      Spus)
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
      Spus => 21);

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
     (Fkey_Field_Reset => 0,
      Passwd => 90);

   --  EEFC Flash Command Register
   type EFC0_FCR_Register is record
      --  Write-only. Flash Command
      FCMD : FCMD_Field := Getd;
      --  Write-only. Flash Command Argument
      FARG : FCR_FARG_Field := 16#0#;
      --  Write-only. Flash Writing Protection Key
      FKEY : FKEY_Field := Fkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EFC0_FCR_Register use record
      FCMD at 0 range 0 .. 7;
      FARG at 0 range 8 .. 23;
      FKEY at 0 range 24 .. 31;
   end record;

   -----------------------
   -- EFC0_FSR_Register --
   -----------------------

   --  EEFC Flash Status Register
   type EFC0_FSR_Register is record
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

   for EFC0_FSR_Register use record
      FRDY          at 0 range 0 .. 0;
      FCMDE         at 0 range 1 .. 1;
      FLOCKE        at 0 range 2 .. 2;
      FLERR         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Embedded Flash Controller 0
   type EFC_Peripheral is record
      --  EEFC Flash Mode Register
      FMR : EFC0_FMR_Register;
      --  EEFC Flash Command Register
      FCR : EFC0_FCR_Register;
      --  EEFC Flash Status Register
      FSR : EFC0_FSR_Register;
      --  EEFC Flash Result Register
      FRR : Interfaces.Bit_Types.Word;
   end record
     with Volatile;

   for EFC_Peripheral use record
      FMR at 0 range 0 .. 31;
      FCR at 4 range 0 .. 31;
      FSR at 8 range 0 .. 31;
      FRR at 12 range 0 .. 31;
   end record;

   --  Embedded Flash Controller 0
   EFC0_Periph : aliased EFC_Peripheral
     with Import, Address => EFC0_Base;

   --  Embedded Flash Controller 1
   EFC1_Periph : aliased EFC_Peripheral
     with Import, Address => EFC1_Base;

end Interfaces.SAM.EFC;
