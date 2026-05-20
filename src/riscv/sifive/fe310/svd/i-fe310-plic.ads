--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from FE310.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.FE310.PLIC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype PRIORITY_PRIORITY_Field is Interfaces.FE310.UInt3;

   --  PLIC Interrupt Priority Register.
   type PRIORITY_Register is record
      PRIORITY      : PRIORITY_PRIORITY_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : Interfaces.FE310.UInt29 := 16#0#;
   end record
     with Volatile, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRIORITY_Register use record
      PRIORITY      at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  PLIC Interrupt Priority Register.
   type PRIORITY_Registers is array (0 .. 51) of PRIORITY_Register;

   --  PENDING_INT array
   type PENDING_INT_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  PLIC Interrupt Pending Register.
   type PENDING_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INT as a value
            Val : Interfaces.FE310.UInt32;
         when True =>
            --  INT as an array
            Arr : PENDING_INT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile,
          Bit_Order => System.Low_Order_First;

   for PENDING_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PLIC Interrupt Pending Register.
   type PENDING_Registers is array (0 .. 1) of PENDING_Register;

   ---------------------------------------
   -- TARGET_ENABLE cluster's Registers --
   ---------------------------------------

   --  ENABLE_TARGET_ENABLE_INT array
   type ENABLE_TARGET_ENABLE_INT_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  PLIC Interrupt Enable Register.
   type ENABLE_TARGET_ENABLE_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INT as a value
            Val : Interfaces.FE310.UInt32;
         when True =>
            --  INT as an array
            Arr : ENABLE_TARGET_ENABLE_INT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile,
          Bit_Order => System.Low_Order_First;

   for ENABLE_TARGET_ENABLE_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PLIC Interrupt Enable Register.
   type ENABLE_TARGET_ENABLE_Registers is array (0 .. 1)
     of ENABLE_TARGET_ENABLE_Register;

   type TARGET_ENABLE_Cluster is record
      --  PLIC Interrupt Enable Register.
      ENABLE : aliased ENABLE_TARGET_ENABLE_Registers;
   end record
     with Size => 64;

   for TARGET_ENABLE_Cluster use record
      ENABLE at 0 range 0 .. 63;
   end record;

   --------------------------------
   -- TARGET cluster's Registers --
   --------------------------------

   subtype THRESHOLD_TARGET_THRESHOLD_Field is Interfaces.FE310.UInt3;

   --  PLIC Interrupt Priority Threshold Register.
   type THRESHOLD_TARGET_Register is record
      THRESHOLD     : THRESHOLD_TARGET_THRESHOLD_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : Interfaces.FE310.UInt29 := 16#0#;
   end record
     with Volatile, Size => 32,
          Bit_Order => System.Low_Order_First;

   for THRESHOLD_TARGET_Register use record
      THRESHOLD     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   type TARGET_Disc is
     (Laim,
      Omplete);

   type TARGET_Cluster
     (Discriminent : TARGET_Disc := Laim)
   is record
      --  PLIC Interrupt Priority Threshold Register.
      THRESHOLD : aliased THRESHOLD_TARGET_Register;
      case Discriminent is
         when Laim =>
            --  PLIC Claim Register.
            CLAIM : aliased Interfaces.FE310.UInt32;
         when Omplete =>
            --  PLIC Complete Register.
            COMPLETE : aliased Interfaces.FE310.UInt32;
      end case;
   end record
     with Unchecked_Union, Size => 64;

   for TARGET_Cluster use record
      THRESHOLD at 16#0# range 0 .. 31;
      CLAIM     at 16#4# range 0 .. 31;
      COMPLETE  at 16#4# range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Platform-Level Interrupt Controller.
   type PLIC_Peripheral is record
      --  PLIC Interrupt Priority Register.
      PRIORITY      : aliased PRIORITY_Registers;
      --  PLIC Interrupt Pending Register.
      PENDING       : aliased PENDING_Registers;
      TARGET_ENABLE : aliased TARGET_ENABLE_Cluster;
      TARGET        : aliased TARGET_Cluster;
   end record
     with Volatile;

   for PLIC_Peripheral use record
      PRIORITY      at 16#0# range 0 .. 1663;
      PENDING       at 16#1000# range 0 .. 63;
      TARGET_ENABLE at 16#2080# range 0 .. 63;
      TARGET        at 16#201000# range 0 .. 63;
   end record;

   --  Platform-Level Interrupt Controller.
   PLIC_Periph : aliased PLIC_Peripheral
     with Import, Address => PLIC_Base;

end Interfaces.FE310.PLIC;
