--
--  Copyright (C) 2019, AdaCore
--

--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form, except as embedded into a Nordic
--  Semiconductor ASA integrated circuit in a product or a software update
--  for such product, must reproduce the above copyright notice, this list
--  of conditions and the following disclaimer in the documentation and/or
--  other materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  4. This software, with or without modification, must only be used with a
--  Nordic Semiconductor ASA integrated circuit.
--
--  5. Any software provided in binary form under this license must not be
--  reverse engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR
--  ASA OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52840.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.NRF52.FICR is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Description collection[n]: Device identifier

   --  Description collection[n]: Device identifier
   type DEVICEID_Registers is array (0 .. 1) of Interfaces.NRF52.UInt32;

   --  Description collection[n]: Encryption root, word n

   --  Description collection[n]: Encryption root, word n
   type ER_Registers is array (0 .. 3) of Interfaces.NRF52.UInt32;

   --  Description collection[n]: Identity Root, word n

   --  Description collection[n]: Identity Root, word n
   type IR_Registers is array (0 .. 3) of Interfaces.NRF52.UInt32;

   --  Device address type
   type DEVICEADDRTYPE_DEVICEADDRTYPE_Field is
     (--  Public address
      Public,
      --  Random address
      Random)
     with Size => 1;
   for DEVICEADDRTYPE_DEVICEADDRTYPE_Field use
     (Public => 0,
      Random => 1);

   --  Device address type
   type DEVICEADDRTYPE_Register is record
      --  Read-only. Device address type
      DEVICEADDRTYPE : DEVICEADDRTYPE_DEVICEADDRTYPE_Field;
      --  unspecified
      Reserved_1_31  : Interfaces.NRF52.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICEADDRTYPE_Register use record
      DEVICEADDRTYPE at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Description collection[n]: Device address n

   --  Description collection[n]: Device address n
   type DEVICEADDR_Registers is array (0 .. 1) of Interfaces.NRF52.UInt32;

   -----------------------------------
   -- FICR_INFO cluster's Registers --
   -----------------------------------

   --  Unspecified

   --  Unspecified
   type UNUSED8_INFO_Registers is array (0 .. 2) of Interfaces.NRF52.UInt32;

   --  Device info
   type FICR_INFO_Cluster is record
      --  Part code
      PART      : aliased Interfaces.NRF52.UInt32;
      --  Build code (hardware version and production configuration)
      VARIANT   : aliased Interfaces.NRF52.UInt32;
      --  Package option
      PACKAGE_k : aliased Interfaces.NRF52.UInt32;
      --  RAM variant
      RAM       : aliased Interfaces.NRF52.UInt32;
      --  Flash variant
      FLASH     : aliased Interfaces.NRF52.UInt32;
      --  Unspecified
      UNUSED8   : aliased UNUSED8_INFO_Registers;
   end record
     with Size => 256;

   for FICR_INFO_Cluster use record
      PART      at 16#0# range 0 .. 31;
      VARIANT   at 16#4# range 0 .. 31;
      PACKAGE_k at 16#8# range 0 .. 31;
      RAM       at 16#C# range 0 .. 31;
      FLASH     at 16#10# range 0 .. 31;
      UNUSED8   at 16#14# range 0 .. 95;
   end record;

   --  Description collection[n]: Production test signature n

   --  Description collection[n]: Production test signature n
   type PRODTEST_Registers is array (0 .. 2) of Interfaces.NRF52.UInt32;

   -----------------------------------
   -- FICR_TEMP cluster's Registers --
   -----------------------------------

   subtype A_A_Field is Interfaces.NRF52.UInt12;

   --  Slope definition A0
   type A_Register is record
      --  Read-only. A (slope definition) register.
      A              : A_A_Field;
      --  unspecified
      Reserved_12_31 : Interfaces.NRF52.UInt20;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for A_Register use record
      A              at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype B_B_Field is Interfaces.NRF52.UInt14;

   --  Y-intercept B0
   type B_Register is record
      --  Read-only. B (y-intercept)
      B              : B_B_Field;
      --  unspecified
      Reserved_14_31 : Interfaces.NRF52.UInt18;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for B_Register use record
      B              at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype T_T_Field is Interfaces.NRF52.Byte;

   --  Segment end T0
   type T_Register is record
      --  Read-only. T (segment end) register
      T             : T_T_Field;
      --  unspecified
      Reserved_8_31 : Interfaces.NRF52.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for T_Register use record
      T             at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Registers storing factory TEMP module linearization coefficients
   type FICR_TEMP_Cluster is record
      --  Slope definition A0
      A0 : aliased A_Register;
      --  Slope definition A1
      A1 : aliased A_Register;
      --  Slope definition A2
      A2 : aliased A_Register;
      --  Slope definition A3
      A3 : aliased A_Register;
      --  Slope definition A4
      A4 : aliased A_Register;
      --  Slope definition A5
      A5 : aliased A_Register;
      --  Y-intercept B0
      B0 : aliased B_Register;
      --  Y-intercept B1
      B1 : aliased B_Register;
      --  Y-intercept B2
      B2 : aliased B_Register;
      --  Y-intercept B3
      B3 : aliased B_Register;
      --  Y-intercept B4
      B4 : aliased B_Register;
      --  Y-intercept B5
      B5 : aliased B_Register;
      --  Segment end T0
      T0 : aliased T_Register;
      --  Segment end T1
      T1 : aliased T_Register;
      --  Segment end T2
      T2 : aliased T_Register;
      --  Segment end T3
      T3 : aliased T_Register;
      --  Segment end T4
      T4 : aliased T_Register;
   end record
     with Size => 544;

   for FICR_TEMP_Cluster use record
      A0 at 16#0# range 0 .. 31;
      A1 at 16#4# range 0 .. 31;
      A2 at 16#8# range 0 .. 31;
      A3 at 16#C# range 0 .. 31;
      A4 at 16#10# range 0 .. 31;
      A5 at 16#14# range 0 .. 31;
      B0 at 16#18# range 0 .. 31;
      B1 at 16#1C# range 0 .. 31;
      B2 at 16#20# range 0 .. 31;
      B3 at 16#24# range 0 .. 31;
      B4 at 16#28# range 0 .. 31;
      B5 at 16#2C# range 0 .. 31;
      T0 at 16#30# range 0 .. 31;
      T1 at 16#34# range 0 .. 31;
      T2 at 16#38# range 0 .. 31;
      T3 at 16#3C# range 0 .. 31;
      T4 at 16#40# range 0 .. 31;
   end record;

   ----------------------------------
   -- FICR_NFC cluster's Registers --
   ----------------------------------

   subtype TAGHEADER0_NFC_MFGID_Field is Interfaces.NRF52.Byte;
   --  TAGHEADER0_NFC_UD array element
   subtype TAGHEADER0_NFC_UD_Element is Interfaces.NRF52.Byte;

   --  TAGHEADER0_NFC_UD array
   type TAGHEADER0_NFC_UD_Field_Array is array (1 .. 3)
     of TAGHEADER0_NFC_UD_Element
     with Component_Size => 8, Size => 24;

   --  Type definition for TAGHEADER0_NFC_UD
   type TAGHEADER0_NFC_UD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  UD as a value
            Val : Interfaces.NRF52.UInt24;
         when True =>
            --  UD as an array
            Arr : TAGHEADER0_NFC_UD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 24;

   for TAGHEADER0_NFC_UD_Field use record
      Val at 0 range 0 .. 23;
      Arr at 0 range 0 .. 23;
   end record;

   --  Default header for NFC tag. Software can read these values to populate
   --  NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
   type TAGHEADER0_NFC_Register is record
      --  Read-only. Default Manufacturer ID: Nordic Semiconductor ASA has ICM
      --  0x5F
      MFGID : TAGHEADER0_NFC_MFGID_Field;
      --  Read-only. Unique identifier byte 1
      UD    : TAGHEADER0_NFC_UD_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAGHEADER0_NFC_Register use record
      MFGID at 0 range 0 .. 7;
      UD    at 0 range 8 .. 31;
   end record;

   --  TAGHEADER1_NFC_UD array element
   subtype TAGHEADER1_NFC_UD_Element is Interfaces.NRF52.Byte;

   --  TAGHEADER1_NFC_UD array
   type TAGHEADER1_NFC_UD_Field_Array is array (4 .. 7)
     of TAGHEADER1_NFC_UD_Element
     with Component_Size => 8, Size => 32;

   --  Default header for NFC tag. Software can read these values to populate
   --  NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
   type TAGHEADER1_NFC_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  UD as a value
            Val : Interfaces.NRF52.UInt32;
         when True =>
            --  UD as an array
            Arr : TAGHEADER1_NFC_UD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TAGHEADER1_NFC_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  TAGHEADER2_NFC_UD array element
   subtype TAGHEADER2_NFC_UD_Element is Interfaces.NRF52.Byte;

   --  TAGHEADER2_NFC_UD array
   type TAGHEADER2_NFC_UD_Field_Array is array (8 .. 11)
     of TAGHEADER2_NFC_UD_Element
     with Component_Size => 8, Size => 32;

   --  Default header for NFC tag. Software can read these values to populate
   --  NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
   type TAGHEADER2_NFC_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  UD as a value
            Val : Interfaces.NRF52.UInt32;
         when True =>
            --  UD as an array
            Arr : TAGHEADER2_NFC_UD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TAGHEADER2_NFC_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  TAGHEADER3_NFC_UD array element
   subtype TAGHEADER3_NFC_UD_Element is Interfaces.NRF52.Byte;

   --  TAGHEADER3_NFC_UD array
   type TAGHEADER3_NFC_UD_Field_Array is array (12 .. 15)
     of TAGHEADER3_NFC_UD_Element
     with Component_Size => 8, Size => 32;

   --  Default header for NFC tag. Software can read these values to populate
   --  NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
   type TAGHEADER3_NFC_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  UD as a value
            Val : Interfaces.NRF52.UInt32;
         when True =>
            --  UD as an array
            Arr : TAGHEADER3_NFC_UD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TAGHEADER3_NFC_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Unspecified
   type FICR_NFC_Cluster is record
      --  Default header for NFC tag. Software can read these values to
      --  populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
      TAGHEADER0 : aliased TAGHEADER0_NFC_Register;
      --  Default header for NFC tag. Software can read these values to
      --  populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
      TAGHEADER1 : aliased TAGHEADER1_NFC_Register;
      --  Default header for NFC tag. Software can read these values to
      --  populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
      TAGHEADER2 : aliased TAGHEADER2_NFC_Register;
      --  Default header for NFC tag. Software can read these values to
      --  populate NFCID1_3RD_LAST, NFCID1_2ND_LAST and NFCID1_LAST.
      TAGHEADER3 : aliased TAGHEADER3_NFC_Register;
   end record
     with Size => 128;

   for FICR_NFC_Cluster use record
      TAGHEADER0 at 16#0# range 0 .. 31;
      TAGHEADER1 at 16#4# range 0 .. 31;
      TAGHEADER2 at 16#8# range 0 .. 31;
      TAGHEADER3 at 16#C# range 0 .. 31;
   end record;

   --------------------------------------
   -- FICR_TRNG90B cluster's Registers --
   --------------------------------------

   --  NIST800-90B RNG calibration data
   type FICR_TRNG90B_Cluster is record
      --  Amount of bytes for the required entropy bits
      BYTES    : aliased Interfaces.NRF52.UInt32;
      --  Repetition counter cutoff
      RCCUTOFF : aliased Interfaces.NRF52.UInt32;
      --  Adaptive proportion cutoff
      APCUTOFF : aliased Interfaces.NRF52.UInt32;
      --  Amount of bytes for the startup tests
      STARTUP  : aliased Interfaces.NRF52.UInt32;
      --  Sample count for ring oscillator 1
      ROSC1    : aliased Interfaces.NRF52.UInt32;
      --  Sample count for ring oscillator 2
      ROSC2    : aliased Interfaces.NRF52.UInt32;
      --  Sample count for ring oscillator 3
      ROSC3    : aliased Interfaces.NRF52.UInt32;
      --  Sample count for ring oscillator 4
      ROSC4    : aliased Interfaces.NRF52.UInt32;
   end record
     with Size => 256;

   for FICR_TRNG90B_Cluster use record
      BYTES    at 16#0# range 0 .. 31;
      RCCUTOFF at 16#4# range 0 .. 31;
      APCUTOFF at 16#8# range 0 .. 31;
      STARTUP  at 16#C# range 0 .. 31;
      ROSC1    at 16#10# range 0 .. 31;
      ROSC2    at 16#14# range 0 .. 31;
      ROSC3    at 16#18# range 0 .. 31;
      ROSC4    at 16#1C# range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Factory information configuration registers
   type FICR_Peripheral is record
      --  Code memory page size
      CODEPAGESIZE   : aliased Interfaces.NRF52.UInt32;
      --  Code memory size
      CODESIZE       : aliased Interfaces.NRF52.UInt32;
      --  Description collection[n]: Device identifier
      DEVICEID       : aliased DEVICEID_Registers;
      --  Description collection[n]: Encryption root, word n
      ER             : aliased ER_Registers;
      --  Description collection[n]: Identity Root, word n
      IR             : aliased IR_Registers;
      --  Device address type
      DEVICEADDRTYPE : aliased DEVICEADDRTYPE_Register;
      --  Description collection[n]: Device address n
      DEVICEADDR     : aliased DEVICEADDR_Registers;
      --  Device info
      INFO           : aliased FICR_INFO_Cluster;
      --  Description collection[n]: Production test signature n
      PRODTEST       : aliased PRODTEST_Registers;
      --  Registers storing factory TEMP module linearization coefficients
      TEMP           : aliased FICR_TEMP_Cluster;
      --  Unspecified
      NFC            : aliased FICR_NFC_Cluster;
      --  NIST800-90B RNG calibration data
      TRNG90B        : aliased FICR_TRNG90B_Cluster;
   end record
     with Volatile;

   for FICR_Peripheral use record
      CODEPAGESIZE   at 16#10# range 0 .. 31;
      CODESIZE       at 16#14# range 0 .. 31;
      DEVICEID       at 16#60# range 0 .. 63;
      ER             at 16#80# range 0 .. 127;
      IR             at 16#90# range 0 .. 127;
      DEVICEADDRTYPE at 16#A0# range 0 .. 31;
      DEVICEADDR     at 16#A4# range 0 .. 63;
      INFO           at 16#100# range 0 .. 255;
      PRODTEST       at 16#350# range 0 .. 95;
      TEMP           at 16#404# range 0 .. 543;
      NFC            at 16#450# range 0 .. 127;
      TRNG90B        at 16#C00# range 0 .. 255;
   end record;

   --  Factory information configuration registers
   FICR_Periph : aliased FICR_Peripheral
     with Import, Address => FICR_Base;

end Interfaces.NRF52.FICR;
