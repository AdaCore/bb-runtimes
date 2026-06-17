------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                   Copyright (C) 2016-2018, AdaCore                       --
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

package System.BB.Board_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant := 100_000;
   --  The clock frequency value here is virtual since the timer implementation
   --  relies on ticks and is never reading a real hardware timer value.

   RTC_Prescaler : constant := 31;
   --  We use the RTC0 timer as a periodic timer with 1024Hz rate. This is a
   --  trade-off between accurate delays, limited overhead and maximum time
   --  that interrupts may be disabled.
   --
   --  The formula for tick frequency is as follow:
   --  Tick frequency = 32_768 / (RTC_Prescaler + 1)
   --
   --  This value can be adjusted, increasing RTC_Prescaler will reduce the
   --  tick frequency which means less overhead but also a lower precision for
   --  time features such as delays and Timming_Events. Lowering RTC_Prescaler
   --  will have the inverse effect, higher precision for time features but
   --  more overhead caused by the higher number of interrupt that the run-time
   --  will have to handle.

end System.BB.Board_Parameters;
