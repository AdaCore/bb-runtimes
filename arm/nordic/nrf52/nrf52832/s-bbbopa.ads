------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                   Copyright (C) 2016-2019, AdaCore                       --
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

--  This package defines board parameters for the nRF52840-DK board

package System.BB.Board_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   RTC_Tick_Scaling_Factor : constant := 32; --  32.768 kHz * 32 = 1.048576 MHz
   --  Use a fairly high scaling factor so that Ada.Real_Time.Time_Unit is
   --  at least 1 microsecond. This improves the long-running accuracy of
   --  periodic tasks where the period is not integer divisible by 32.768 kHz.
   --
   --  The maximum permitted scaling factor is 255, otherwise the 24-bit RTC
   --  period (@ 32 kHz) cannot be scaled to the 32-bit range of
   --  Timer_Interval.

   Main_Clock_Frequency : constant := 32_768 * RTC_Tick_Scaling_Factor;
   --  On the nRF52 we use the RTC peripheral as the system tick, instead of
   --  the Cortex-M4 SysTick because the SysTick is powered down when the CPU
   --  enters sleep mode (via the "wfi" instruction). Since we still want to
   --  be able to put the CPU to sleep (to save power) we instead use the
   --  low-power RTC peripheral, which runs at 32.768 kHz.
   --
   --  In this runtime, the minimum allowed frequency is 50_000 Hz so that
   --  Ada.Real_Time.Time_Unit does not exceed 20 µs as required by
   --  Ada RM D.8 (30). Since the RTC's actual period is 30.518 µs we multiply
   --  the RTC frequency by RTC_Tick_Scaling_Factor so that Time_Unit meets the
   --  requirement in Ada RM D.8 (30).

end System.BB.Board_Parameters;
