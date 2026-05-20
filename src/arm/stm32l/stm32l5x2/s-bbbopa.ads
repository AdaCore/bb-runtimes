------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                   Copyright (C) 2016-2021, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines board parameters for the STM32L562 boards

package System.BB.Board_Parameters is

   pragma No_Elaboration_Code_All;
   pragma Pure;

   Main_Clock_Frequency : constant := 110_000_000;
   --  Maximum frequency of the system clock, per RM0438 section 9.3.3, when
   --  the PLL is used.

   HSE_Clock_Frequency : constant := 0;
   --  The high speed external oscillator is optional and not mounted by
   --  default on the STM32L562QE Discovery Board, per UM2617 Rev 4, section
   --  6.5.2, page 22/61 (User Manual for STM32L562QE MCU). Also, 8MHz is
   --  typical but not necessarily the value configured if the HSE is actually
   --  present. Set the value above to whatever is physically present.

   HSI_Clock_Frequency : constant := 16_000_000;
   LSI_Clock_Frequency : constant := 32_000;
   LSE_Clock_Frequency : constant := 32_768;

   MSI_Clock_Frequency : constant := 48_000_000;
   --  The Multispeed Internal clock, a configurable oscillator used in our
   --  config to drive the PLL. NB: requires setting bits in RCC_CR.MSIRANGE
   --  field! Otherwise is 4MHz on powerup.

   FLASH_Latency : constant := 5;
   --  After reset, the CPU clock frequency is 4 MHz and 0 wait state (WS) is
   --  configured in the FLASH_ACR register. We will run at 110MHz so we use
   --  5 wait states, per RM0438 Rev 6 pg 179/2194, section 6.3.3 Read access
   --  latency, table 31 "Number of wait states according to CPU clock (HCLK)
   --  frequency"

end System.BB.Board_Parameters;
