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

--  This is a version for the nRF52840 reference description for radio MCU with
--  ARM 32-bit Cortex-M4 Microcontroller  MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt                           : constant Interrupt_ID := -1;
   POWER_CLOCK_Interrupt                        : constant Interrupt_ID := 0;
   RADIO_Interrupt                              : constant Interrupt_ID := 1;
   UARTE0_UART0_Interrupt                       : constant Interrupt_ID := 2;
   SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_Interrupt  : constant Interrupt_ID := 3;
   SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_Interrupt  : constant Interrupt_ID := 4;
   NFCT_Interrupt                               : constant Interrupt_ID := 5;
   GPIOTE_Interrupt                             : constant Interrupt_ID := 6;
   SAADC_Interrupt                              : constant Interrupt_ID := 7;
   TIMER0_Interrupt                             : constant Interrupt_ID := 8;
   TIMER1_Interrupt                             : constant Interrupt_ID := 9;
   TIMER2_Interrupt                             : constant Interrupt_ID := 10;
   RTC0_Interrupt                               : constant Interrupt_ID := 11;
   TEMP_Interrupt                               : constant Interrupt_ID := 12;
   RNG_Interrupt                                : constant Interrupt_ID := 13;
   ECB_Interrupt                                : constant Interrupt_ID := 14;
   CCM_AAR_Interrupt                            : constant Interrupt_ID := 15;
   WDT_Interrupt                                : constant Interrupt_ID := 16;
   RTC1_Interrupt                               : constant Interrupt_ID := 17;
   QDEC_Interrupt                               : constant Interrupt_ID := 18;
   COMP_LPCOMP_Interrupt                        : constant Interrupt_ID := 19;
   SWI0_EGU0_Interrupt                          : constant Interrupt_ID := 20;
   SWI1_EGU1_Interrupt                          : constant Interrupt_ID := 21;
   SWI2_EGU2_Interrupt                          : constant Interrupt_ID := 22;
   SWI3_EGU3_Interrupt                          : constant Interrupt_ID := 23;
   SWI4_EGU4_Interrupt                          : constant Interrupt_ID := 24;
   SWI5_EGU5_Interrupt                          : constant Interrupt_ID := 25;
   TIMER3_Interrupt                             : constant Interrupt_ID := 26;
   TIMER4_Interrupt                             : constant Interrupt_ID := 27;
   PWM0_Interrupt                               : constant Interrupt_ID := 28;
   PDM_Interrupt                                : constant Interrupt_ID := 29;
   MWU_Interrupt                                : constant Interrupt_ID := 32;
   PWM1_Interrupt                               : constant Interrupt_ID := 33;
   PWM2_Interrupt                               : constant Interrupt_ID := 34;
   SPIM2_SPIS2_SPI2_Interrupt                   : constant Interrupt_ID := 35;
   RTC2_Interrupt                               : constant Interrupt_ID := 36;
   I2S_Interrupt                                : constant Interrupt_ID := 37;
   FPU_Interrupt                                : constant Interrupt_ID := 38;
   USBD_Interrupt                               : constant Interrupt_ID := 39;
   UARTE1_Interrupt                             : constant Interrupt_ID := 40;
   QSPI_Interrupt                               : constant Interrupt_ID := 41;
   CRYPTOCELL_Interrupt                         : constant Interrupt_ID := 42;
   PWM3_Interrupt                               : constant Interrupt_ID := 45;
   SPIM3_Interrupt                              : constant Interrupt_ID := 47;

end Ada.Interrupts.Names;
