pragma Style_Checks (Off);

--  Copyright (c) 2010 - 2021, Nordic Semiconductor ASA All rights reserved.
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY, AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR ASA OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52833.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.NRF52.APPROTECT is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Write 0x0 to force enable APPROTECT mechanism
   type FORCEPROTECT_FORCEPROTECT_Field is
     (--  Software force enable APPROTECT mechanism
      Force,
      --  Reset value for the field
      FORCEPROTECT_FORCEPROTECT_Field_Reset)
     with Size => 8;
   for FORCEPROTECT_FORCEPROTECT_Field use
     (Force => 0,
      FORCEPROTECT_FORCEPROTECT_Field_Reset => 255);

   --  Software force enable APPROTECT mechanism until next reset.
   type FORCEPROTECT_Register is record
      --  Read-Write-once. Write 0x0 to force enable APPROTECT mechanism
      FORCEPROTECT  : FORCEPROTECT_FORCEPROTECT_Field :=
                       FORCEPROTECT_FORCEPROTECT_Field_Reset;
      --  unspecified
      Reserved_8_31 : Interfaces.NRF52.UInt24 := 16#FFFFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FORCEPROTECT_Register use record
      FORCEPROTECT  at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Software disable APPROTECT mechanism
   type DISABLE_DISABLE_Field is
     (--  Reset value for the field
      DISABLE_DISABLE_Field_Reset,
      --  Software disable APPROTECT mechanism
      SwDisable)
     with Size => 8;
   for DISABLE_DISABLE_Field use
     (DISABLE_DISABLE_Field_Reset => 0,
      SwDisable => 90);

   --  Software disable APPROTECT mechanism
   type DISABLE_Register is record
      --  Software disable APPROTECT mechanism
      DISABLE       : DISABLE_DISABLE_Field := DISABLE_DISABLE_Field_Reset;
      --  unspecified
      Reserved_8_31 : Interfaces.NRF52.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DISABLE_Register use record
      DISABLE       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Access Port Protection
   type APPROTECT_Peripheral is record
      --  Software force enable APPROTECT mechanism until next reset.
      FORCEPROTECT : aliased FORCEPROTECT_Register;
      --  Software disable APPROTECT mechanism
      DISABLE      : aliased DISABLE_Register;
   end record
     with Volatile;

   for APPROTECT_Peripheral use record
      FORCEPROTECT at 16#550# range 0 .. 31;
      DISABLE      at 16#558# range 0 .. 31;
   end record;

   --  Access Port Protection
   APPROTECT_Periph : aliased APPROTECT_Peripheral
     with Import, Address => APPROTECT_Base;

end Interfaces.NRF52.APPROTECT;
