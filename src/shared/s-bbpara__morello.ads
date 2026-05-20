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
--                     Copyright (C) 2003-2022, AdaCore                     --
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

--  This is the Arm Morello version of this package

package System.BB.Parameters with
   Pure,
   No_Elaboration_Code_All
is

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant := 2_500_000_000;
   --  CPU frequency

   Ticks_Per_Second : constant := 62_500_000;
   --  Frequency of the Global timestamp timer in Hz.

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 0 .. 1023;
   --  Number of interrupts supported by Morello.

   Trap_Vectors : constant := 7;
   --  ARM in general has these traps:
   --    0   (at 16#0000#) Reset
   --    1   (at 16#0004#) Undefined Instruction (synchronous)
   --    2   (at 16#0008#) Supervisor Call (synchronous)
   --    3   (at 16#000C#) Abort - Prefetch (synchronous)
   --    4   (at 16#0010#) Abort - Data (asynchronous)
   --    5   (at 16#0014#) IRQ Trap (asynchronous)
   --    6   (at 16#0018#) FIQ Trap (asynchronous)

   -----------------------------
   -- GIC peripheral location --
   -----------------------------

   GIC_Base_Address : constant := 16#3000_0000#;

   GICR_Base_Address : constant := 16#300C_0000#;

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 32 * 1024;
   --  Size of each of the interrupt stacks. Each processor has its own
   --  interrupt stack. In case interrupts are nested, the same stack is used.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   ------------------------
   -- CPU Number Mapping --
   ------------------------

   --  These constants are used to map the processor affinity to a CPU number.
   --
   --  The affinity is defined by four fields used in various system registers:
   --   * Aff0: Processing Element (PE) number within the CPU core.
   --   * Aff1: CPU number within the cluster.
   --   * Aff2: Cluster number within a chip.
   --   * Aff3: Chip number.
   --
   --  Morello has 2 clusters, each containing two single-threaded cores
   --  resulting in a total of 4 PEs.

   Num_PEs_Per_CPU : constant := 1;
   --  Number of Processing Elements (PE) per CPU core.
   --
   --  A single-threaded core has one PE, and a multi-threaded core has
   --  multiple PEs.

   Num_CPUs_Per_Cluster : constant := 2;
   --  Number of CPUs (cores) per processing cluster

   Num_Clusters : constant := 2;
   --  Number of clusters per chip

   Num_PEs_Per_Cluster : constant := Num_PEs_Per_CPU * Num_CPUs_Per_Cluster;
   --  Total number of PEs within each cluster

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs available on the target.
   --
   --  The actual number of CPUs can be bigger than this number, in which case
   --  the runtime will not use the additional CPUs.

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

   ----------
   -- UART --
   ----------

   PL011_Base : constant := 16#2A40_0000#;
   --  APUART0

   --  F_UARTCLK = 50_000_000, baud = 115_200
   --  (F_UARTCLK / 16) / baud ~= 27 + 8/64

   PL011_IBRD : constant := 27;
   PL011_FBRD : constant := 8;

end System.BB.Parameters;
