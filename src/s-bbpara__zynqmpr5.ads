------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P A R A M E T E R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines basic parameters used by the low level tasking system

--  This is the Xilinx Ultrascale+ MPSoC Cortex-R5 version of this package

pragma Restrictions (No_Elaboration_Code);

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   LPD_APB_CLK_Frequency : constant := 100_000_000;
   --  100MHz input frequency

   TTC_Prescaler    : constant := 2;
   Ticks_Per_Second : constant :=
                        LPD_APB_CLK_Frequency / (2 ** (TTC_Prescaler + 1));
   --  Frequency of the TTC counter: 12.50 MHz

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 0 .. 191;
   --  Number of interrupts supported by the zynqmp.

   Trap_Vectors : constant := 7;
   --  ARM in general has these traps:
   --    0   (at 16#0000#) Reset
   --    1   (at 16#0004#) Undefined Instruction (synchronous)
   --    2   (at 16#0008#) Supervisor Call (synchronous)
   --    3   (at 16#000C#) Abort - Prefetch (synchronous)
   --    4   (at 16#0010#) Abort - Data (asynchronous)
   --    5   (at 16#0014#) IRQ Trap (asynchronous)
   --    6   (at 16#0018#) FIQ Trap (asynchronous)

   Interrupt_Unmask_Priority : constant System.Interrupt_Priority :=
                                 System.Interrupt_Priority'Last;
   --  The priority under which we unmask interrupts.
   --  Useful when we use FIQ to simulate priorities on ARM.

   -----------------------------
   -- GIC peripheral location --
   -----------------------------

   GICD_Base_Address : constant := 16#F900_0000#;
   GICC_Base_Address : constant := 16#F900_1000#;

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 8192;
   --  Size of each of the interrupt stacks. Each processor has its own
   --  interrupt stack. In case interrupts are nested, the same stack is used.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 4;
   --  Maximum number of CPUs avaialble on the target.
   --  Note: the actual number of CPUs can be lower than this number, in
   --  which case the runtime will adjust the CPUs according to the actual
   --  CPU count.
   --  The actual number of CPUs can be also bigger, in which case the runtime
   --  will not use the additional CPUs.

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
