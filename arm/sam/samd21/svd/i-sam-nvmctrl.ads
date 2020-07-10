--
--  Copyright (C) 2020, AdaCore
--

--  Copyright (c) 2018 Microchip Technology Inc.
--
--  SPDX-License-Identifier: Apache-2.0
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

--  This spec has been automatically generated from ATSAMD21G18AU.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.SAM.NVMCTRL is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Command
   type CTRLA_CMDSelect is
     (--  Reset value for the field
      Ctrla_Cmdselect_Reset,
      --  Erase Row - Erases the row addressed by the ADDR register.
      Er,
      --  Write Page - Writes the contents of the page buffer to the page addressed
--  by the ADDR register.
      Wp,
      --  Erase Auxiliary Row - Erases the auxiliary row addressed by the ADDR
--  register. This command can be given only when the security bit is not set
--  and only to the user configuration row.
      Ear,
      --  Write Auxiliary Page - Writes the contents of the page buffer to the page
--  addressed by the ADDR register. This command can be given only when the
--  security bit is not set and only to the user configuration row.
      Wap,
      --  Security Flow Command
      Sf,
      --  Write lockbits
      Wl,
      --  Lock Region - Locks the region containing the address location in the ADDR
--  register.
      Lr,
      --  Unlock Region - Unlocks the region containing the address location in the
--  ADDR register.
      Ur,
      --  Sets the power reduction mode.
      Sprm,
      --  Clears the power reduction mode.
      Cprm,
      --  Page Buffer Clear - Clears the page buffer.
      Pbc,
      --  Set Security Bit - Sets the security bit by writing 0x00 to the first byte
--  in the lockbit row.
      Ssb,
      --  Invalidates all cache lines.
      Invall)
     with Size => 7;
   for CTRLA_CMDSelect use
     (Ctrla_Cmdselect_Reset => 0,
      Er => 2,
      Wp => 4,
      Ear => 5,
      Wap => 6,
      Sf => 10,
      Wl => 15,
      Lr => 64,
      Ur => 65,
      Sprm => 66,
      Cprm => 67,
      Pbc => 68,
      Ssb => 69,
      Invall => 70);

   --  Command Execution
   type CTRLA_CMDEXSelect is
     (--  Reset value for the field
      Ctrla_Cmdexselect_Reset,
      --  Execution Key
      Key)
     with Size => 8;
   for CTRLA_CMDEXSelect use
     (Ctrla_Cmdexselect_Reset => 0,
      Key => 165);

   --  Control A
   type NVMCTRL_CTRLA_Register is record
      --  Command
      CMD          : CTRLA_CMDSelect := Ctrla_Cmdselect_Reset;
      --  unspecified
      Reserved_7_7 : Interfaces.SAM.Bit := 16#0#;
      --  Command Execution
      CMDEX        : CTRLA_CMDEXSelect := Ctrla_Cmdexselect_Reset;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_CTRLA_Register use record
      CMD          at 0 range 0 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      CMDEX        at 0 range 8 .. 15;
   end record;

   --  NVM Read Wait States
   type CTRLB_RWSSelect is
     (--  Single Auto Wait State
      Single,
      --  Half Auto Wait State
      Half,
      --  Dual Auto Wait State
      Dual)
     with Size => 4;
   for CTRLB_RWSSelect use
     (Single => 0,
      Half => 1,
      Dual => 2);

   subtype NVMCTRL_CTRLB_MANW_Field is Interfaces.SAM.Bit;

   --  Power Reduction Mode during Sleep
   type CTRLB_SLEEPPRMSelect is
     (--  NVM block enters low-power mode when entering sleep.NVM block exits
--  low-power mode upon first access.
      Wakeonaccess,
      --  NVM block enters low-power mode when entering sleep.NVM block exits
--  low-power mode when exiting sleep.
      Wakeupinstant,
      --  Auto power reduction disabled.
      Disabled)
     with Size => 2;
   for CTRLB_SLEEPPRMSelect use
     (Wakeonaccess => 0,
      Wakeupinstant => 1,
      Disabled => 3);

   --  NVMCTRL Read Mode
   type CTRLB_READMODESelect is
     (--  The NVM Controller (cache system) does not insert wait states on a cache
--  miss. Gives the best system performance.
      No_Miss_Penalty,
      --  Reduces power consumption of the cache system, but inserts a wait state
--  each time there is a cache miss. This mode may not be relevant if CPU
--  performance is required, as the application will be stalled and may lead to
--  increase run time.
      Low_Power,
      --  The cache system ensures that a cache hit or miss takes the same amount of
--  time, determined by the number of programmed flash wait states. This mode
--  can be used for real-time applications that require deterministic execution
--  timings.
      Deterministic)
     with Size => 2;
   for CTRLB_READMODESelect use
     (No_Miss_Penalty => 0,
      Low_Power => 1,
      Deterministic => 2);

   subtype NVMCTRL_CTRLB_CACHEDIS_Field is Interfaces.SAM.Bit;

   --  Control B
   type NVMCTRL_CTRLB_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.SAM.Bit := 16#0#;
      --  NVM Read Wait States
      RWS            : CTRLB_RWSSelect := Interfaces.SAM.NVMCTRL.Single;
      --  unspecified
      Reserved_5_6   : Interfaces.SAM.UInt2 := 16#0#;
      --  Manual Write
      MANW           : NVMCTRL_CTRLB_MANW_Field := 16#0#;
      --  Power Reduction Mode during Sleep
      SLEEPPRM       : CTRLB_SLEEPPRMSelect :=
                        Interfaces.SAM.NVMCTRL.Wakeonaccess;
      --  unspecified
      Reserved_10_15 : Interfaces.SAM.UInt6 := 16#0#;
      --  NVMCTRL Read Mode
      READMODE       : CTRLB_READMODESelect :=
                        Interfaces.SAM.NVMCTRL.No_Miss_Penalty;
      --  Cache Disable
      CACHEDIS       : NVMCTRL_CTRLB_CACHEDIS_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_CTRLB_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      RWS            at 0 range 1 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      MANW           at 0 range 7 .. 7;
      SLEEPPRM       at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      READMODE       at 0 range 16 .. 17;
      CACHEDIS       at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype NVMCTRL_PARAM_NVMP_Field is Interfaces.SAM.UInt16;

   --  Page Size
   type PARAM_PSZSelect is
     (--  8 bytes
      Val_8,
      --  16 bytes
      Val_16,
      --  32 bytes
      Val_32,
      --  64 bytes
      Val_64,
      --  128 bytes
      Val_128,
      --  256 bytes
      Val_256,
      --  512 bytes
      Val_512,
      --  1024 bytes
      Val_1024)
     with Size => 3;
   for PARAM_PSZSelect use
     (Val_8 => 0,
      Val_16 => 1,
      Val_32 => 2,
      Val_64 => 3,
      Val_128 => 4,
      Val_256 => 5,
      Val_512 => 6,
      Val_1024 => 7);

   --  NVM Parameter
   type NVMCTRL_PARAM_Register is record
      --  Read-only. NVM Pages
      NVMP           : NVMCTRL_PARAM_NVMP_Field;
      --  Read-only. Page Size
      PSZ            : PARAM_PSZSelect;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM.UInt13;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_PARAM_Register use record
      NVMP           at 0 range 0 .. 15;
      PSZ            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype NVMCTRL_INTENCLR_READY_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_INTENCLR_ERROR_Field is Interfaces.SAM.Bit;

   --  Interrupt Enable Clear
   type NVMCTRL_INTENCLR_Register is record
      --  NVM Ready Interrupt Enable
      READY        : NVMCTRL_INTENCLR_READY_Field := 16#0#;
      --  Error Interrupt Enable
      ERROR        : NVMCTRL_INTENCLR_ERROR_Field := 16#0#;
      --  unspecified
      Reserved_2_7 : Interfaces.SAM.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_INTENCLR_Register use record
      READY        at 0 range 0 .. 0;
      ERROR        at 0 range 1 .. 1;
      Reserved_2_7 at 0 range 2 .. 7;
   end record;

   subtype NVMCTRL_INTENSET_READY_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_INTENSET_ERROR_Field is Interfaces.SAM.Bit;

   --  Interrupt Enable Set
   type NVMCTRL_INTENSET_Register is record
      --  NVM Ready Interrupt Enable
      READY        : NVMCTRL_INTENSET_READY_Field := 16#0#;
      --  Error Interrupt Enable
      ERROR        : NVMCTRL_INTENSET_ERROR_Field := 16#0#;
      --  unspecified
      Reserved_2_7 : Interfaces.SAM.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_INTENSET_Register use record
      READY        at 0 range 0 .. 0;
      ERROR        at 0 range 1 .. 1;
      Reserved_2_7 at 0 range 2 .. 7;
   end record;

   subtype NVMCTRL_INTFLAG_READY_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_INTFLAG_ERROR_Field is Interfaces.SAM.Bit;

   --  Interrupt Flag Status and Clear
   type NVMCTRL_INTFLAG_Register is record
      --  Read-only. NVM Ready
      READY        : NVMCTRL_INTFLAG_READY_Field := 16#0#;
      --  Error
      ERROR        : NVMCTRL_INTFLAG_ERROR_Field := 16#0#;
      --  unspecified
      Reserved_2_7 : Interfaces.SAM.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_INTFLAG_Register use record
      READY        at 0 range 0 .. 0;
      ERROR        at 0 range 1 .. 1;
      Reserved_2_7 at 0 range 2 .. 7;
   end record;

   subtype NVMCTRL_STATUS_PRM_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_STATUS_LOAD_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_STATUS_PROGE_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_STATUS_LOCKE_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_STATUS_NVME_Field is Interfaces.SAM.Bit;
   subtype NVMCTRL_STATUS_SB_Field is Interfaces.SAM.Bit;

   --  Status
   type NVMCTRL_STATUS_Register is record
      --  Read-only. Power Reduction Mode
      PRM           : NVMCTRL_STATUS_PRM_Field := 16#0#;
      --  NVM Page Buffer Active Loading
      LOAD          : NVMCTRL_STATUS_LOAD_Field := 16#0#;
      --  Programming Error Status
      PROGE         : NVMCTRL_STATUS_PROGE_Field := 16#0#;
      --  Lock Error Status
      LOCKE         : NVMCTRL_STATUS_LOCKE_Field := 16#0#;
      --  NVM Error
      NVME          : NVMCTRL_STATUS_NVME_Field := 16#0#;
      --  unspecified
      Reserved_5_7  : Interfaces.SAM.UInt3 := 16#0#;
      --  Read-only. Security Bit Status
      SB            : NVMCTRL_STATUS_SB_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : Interfaces.SAM.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_STATUS_Register use record
      PRM           at 0 range 0 .. 0;
      LOAD          at 0 range 1 .. 1;
      PROGE         at 0 range 2 .. 2;
      LOCKE         at 0 range 3 .. 3;
      NVME          at 0 range 4 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      SB            at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   subtype NVMCTRL_ADDR_ADDR_Field is Interfaces.SAM.UInt22;

   --  Address
   type NVMCTRL_ADDR_Register is record
      --  NVM Address
      ADDR           : NVMCTRL_ADDR_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : Interfaces.SAM.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NVMCTRL_ADDR_Register use record
      ADDR           at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Non-Volatile Memory Controller
   type NVMCTRL_Peripheral is record
      --  Control A
      CTRLA    : aliased NVMCTRL_CTRLA_Register;
      --  Control B
      CTRLB    : aliased NVMCTRL_CTRLB_Register;
      --  NVM Parameter
      PARAM    : aliased NVMCTRL_PARAM_Register;
      --  Interrupt Enable Clear
      INTENCLR : aliased NVMCTRL_INTENCLR_Register;
      --  Interrupt Enable Set
      INTENSET : aliased NVMCTRL_INTENSET_Register;
      --  Interrupt Flag Status and Clear
      INTFLAG  : aliased NVMCTRL_INTFLAG_Register;
      --  Status
      STATUS   : aliased NVMCTRL_STATUS_Register;
      --  Address
      ADDR     : aliased NVMCTRL_ADDR_Register;
      --  Lock Section
      LOCK     : aliased Interfaces.SAM.UInt16;
   end record
     with Volatile;

   for NVMCTRL_Peripheral use record
      CTRLA    at 16#0# range 0 .. 15;
      CTRLB    at 16#4# range 0 .. 31;
      PARAM    at 16#8# range 0 .. 31;
      INTENCLR at 16#C# range 0 .. 7;
      INTENSET at 16#10# range 0 .. 7;
      INTFLAG  at 16#14# range 0 .. 7;
      STATUS   at 16#18# range 0 .. 15;
      ADDR     at 16#1C# range 0 .. 31;
      LOCK     at 16#20# range 0 .. 15;
   end record;

   --  Non-Volatile Memory Controller
   NVMCTRL_Periph : aliased NVMCTRL_Peripheral
     with Import, Address => NVMCTRL_Base;

end Interfaces.SAM.NVMCTRL;
