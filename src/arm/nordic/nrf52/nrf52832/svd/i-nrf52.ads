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
--  Semiconductor ASA integrated circuit in a product or a software update for
--  such product, must reproduce the above copyright notice, this list of
--  conditions and the following disclaimer in the documentation and/or other
--  materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
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

--  This spec has been automatically generated from nrf52.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  nRF52832 reference description for radio MCU with ARM 32-bit Cortex-M4
--  Microcontroller
package Interfaces.NRF52 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Base type --
   ---------------

   type UInt32 is new Interfaces.Unsigned_32;
   type UInt16 is new Interfaces.Unsigned_16;
   type Byte is new Interfaces.Unsigned_8;
   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   --------------------
   -- Base addresses --
   --------------------

   FICR_Base : constant System.Address :=
     System'To_Address (16#10000000#);
   UICR_Base : constant System.Address :=
     System'To_Address (16#10001000#);
   BPROT_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   POWER_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   CLOCK_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   RADIO_Base : constant System.Address :=
     System'To_Address (16#40001000#);
   UARTE0_Base : constant System.Address :=
     System'To_Address (16#40002000#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#40002000#);
   SPIM0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SPIS0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   TWIM0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   TWIS0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SPI0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   TWI0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SPIM1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SPIS1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   TWIM1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   TWIS1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SPI1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   TWI1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   NFCT_Base : constant System.Address :=
     System'To_Address (16#40005000#);
   GPIOTE_Base : constant System.Address :=
     System'To_Address (16#40006000#);
   SAADC_Base : constant System.Address :=
     System'To_Address (16#40007000#);
   TIMER0_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   TIMER1_Base : constant System.Address :=
     System'To_Address (16#40009000#);
   TIMER2_Base : constant System.Address :=
     System'To_Address (16#4000A000#);
   RTC0_Base : constant System.Address :=
     System'To_Address (16#4000B000#);
   TEMP_Base : constant System.Address :=
     System'To_Address (16#4000C000#);
   RNG_Base : constant System.Address :=
     System'To_Address (16#4000D000#);
   ECB_Base : constant System.Address :=
     System'To_Address (16#4000E000#);
   CCM_Base : constant System.Address :=
     System'To_Address (16#4000F000#);
   AAR_Base : constant System.Address :=
     System'To_Address (16#4000F000#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   RTC1_Base : constant System.Address :=
     System'To_Address (16#40011000#);
   QDEC_Base : constant System.Address :=
     System'To_Address (16#40012000#);
   COMP_Base : constant System.Address :=
     System'To_Address (16#40013000#);
   LPCOMP_Base : constant System.Address :=
     System'To_Address (16#40013000#);
   SWI0_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   EGU0_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   SWI1_Base : constant System.Address :=
     System'To_Address (16#40015000#);
   EGU1_Base : constant System.Address :=
     System'To_Address (16#40015000#);
   SWI2_Base : constant System.Address :=
     System'To_Address (16#40016000#);
   EGU2_Base : constant System.Address :=
     System'To_Address (16#40016000#);
   SWI3_Base : constant System.Address :=
     System'To_Address (16#40017000#);
   EGU3_Base : constant System.Address :=
     System'To_Address (16#40017000#);
   SWI4_Base : constant System.Address :=
     System'To_Address (16#40018000#);
   EGU4_Base : constant System.Address :=
     System'To_Address (16#40018000#);
   SWI5_Base : constant System.Address :=
     System'To_Address (16#40019000#);
   EGU5_Base : constant System.Address :=
     System'To_Address (16#40019000#);
   TIMER3_Base : constant System.Address :=
     System'To_Address (16#4001A000#);
   TIMER4_Base : constant System.Address :=
     System'To_Address (16#4001B000#);
   PWM0_Base : constant System.Address :=
     System'To_Address (16#4001C000#);
   PDM_Base : constant System.Address :=
     System'To_Address (16#4001D000#);
   NVMC_Base : constant System.Address :=
     System'To_Address (16#4001E000#);
   PPI_Base : constant System.Address :=
     System'To_Address (16#4001F000#);
   MWU_Base : constant System.Address :=
     System'To_Address (16#40020000#);
   PWM1_Base : constant System.Address :=
     System'To_Address (16#40021000#);
   PWM2_Base : constant System.Address :=
     System'To_Address (16#40022000#);
   SPIM2_Base : constant System.Address :=
     System'To_Address (16#40023000#);
   SPIS2_Base : constant System.Address :=
     System'To_Address (16#40023000#);
   SPI2_Base : constant System.Address :=
     System'To_Address (16#40023000#);
   RTC2_Base : constant System.Address :=
     System'To_Address (16#40024000#);
   I2S_Base : constant System.Address :=
     System'To_Address (16#40025000#);
   FPU_Base : constant System.Address :=
     System'To_Address (16#40026000#);
   P0_Base : constant System.Address :=
     System'To_Address (16#50000000#);

end Interfaces.NRF52;
