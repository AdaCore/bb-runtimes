------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                   Copyright (C) 2016-2017, AdaCore                       --
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

--  This package defines board parameters for the leon4 boards

package System.BB.Board_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant Positive := 100_000_000;
   --  Frequency of the system clock

   Prescaler_Min : constant := 4;
   --  In order to obtain the highest granularity of the clock we set the
   --  minimum allowed prescaler division factor, which is 5, corresponding
   --  to a prescaler reload register value of 4.

   ----------------
   -- Interrupts --
   ----------------

   --  Support of extended interrupts. Must be 0 if extended interrupts are not
   --  available. Otherwise, extended interrupts are supported (so there are 31
   --  31 interrupts) and they are assigned to that priority level.

   Extended_Interrupts_Level : constant := 1;

   ---------------------------
   -- Peripheral addressess --
   ---------------------------

   APB_Base   : constant := 16#c000_0000#;

   Irqmp_Base : constant := APB_Base + 16#080_0100#;
   --  Address of the interrupt controler

   Timer_Base : constant := APB_Base + 16#300#;
   --  Address of the general purpose timer unit

   UART_Base  : constant := APB_Base + 16#100#;
   --  Address of the UART for the console

   Timer_1_Interrupt : constant := 8;
   --  IRQ line of the timer

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 2;
   --  Maximum number of CPUs

end System.BB.Board_Parameters;
