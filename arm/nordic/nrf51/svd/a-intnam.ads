--
--  Copyright (C) 2018, AdaCore
--

--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

--  This is a version for the nRF51 reference description for radio MCU with
--  ARM 32-bit Cortex-M0 Microcontroller at 16MHz CPU clock MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick               : constant Interrupt_ID := -1;
   POWER_CLOCK            : constant Interrupt_ID := 0;
   RADIO                  : constant Interrupt_ID := 1;
   UART0                  : constant Interrupt_ID := 2;
   SPI0_TWI0              : constant Interrupt_ID := 3;
   SPI1_TWI1              : constant Interrupt_ID := 4;
   GPIOTE                 : constant Interrupt_ID := 6;
   ADC                    : constant Interrupt_ID := 7;
   TIMER0                 : constant Interrupt_ID := 8;
   TIMER1                 : constant Interrupt_ID := 9;
   TIMER2                 : constant Interrupt_ID := 10;
   RTC0                   : constant Interrupt_ID := 11;
   TEMP                   : constant Interrupt_ID := 12;
   RNG                    : constant Interrupt_ID := 13;
   ECB                    : constant Interrupt_ID := 14;
   CCM_AAR                : constant Interrupt_ID := 15;
   WDT                    : constant Interrupt_ID := 16;
   RTC1                   : constant Interrupt_ID := 17;
   QDEC                   : constant Interrupt_ID := 18;
   LPCOMP                 : constant Interrupt_ID := 19;
   SWI0                   : constant Interrupt_ID := 20;
   SWI1                   : constant Interrupt_ID := 21;
   SWI2                   : constant Interrupt_ID := 22;
   SWI3                   : constant Interrupt_ID := 23;
   SWI4                   : constant Interrupt_ID := 24;
   SWI5                   : constant Interrupt_ID := 25;

end Ada.Interrupts.Names;
