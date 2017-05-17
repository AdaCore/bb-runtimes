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
--                     Copyright (C) 2003-2016, AdaCore                     --
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

--  This is the TMS570 (ARMv7) version of this package

pragma Restrictions (No_Elaboration_Code);

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant Natural := 180_000_000;
   --  Frequency of the CPU clock in Hz. We hard-code this hear to allow static
   --  computation of the required prescaler.

   RTI_kHz : constant := Clock_Frequency / 2000;
   --  Period of the unscaled clock in kHz. Assuming the clock frequency is
   --  an integral number of kHz allows for sufficient precision with typical
   --  clock frequencies. Account for the implicit 2x clock divider.

   pragma Assert (RTI_kHz in 100 .. 1_000_000, "Invalid RTI_FREQ");
   --  The system clock frequency has to be between 100 kHz and 1 GHz

   --  Try clock periods of 100 ns, 125 ns, 200 ns, 250 ns, 500 ns, 1 us,
   --  correspondsing to 10 MHz, 8 MHz, 5 MHz, 4 MHz, 2MHz, 1 MHz frequencies.
   --  This will get a fast clock, where each period is an integral number
   --  of nanoseconds, which is important for accurate conversions.

   --  For other input frequencies, try scaling the clock down to about 10 Mhz,
   --  while leaving Ticks_Per_Second exact.

   --  This will a few minutes before the 32-bit timer will wrap around

   Prescaler : constant :=
     (if    RTI_kHz mod 10_000 = 0 then RTI_kHz / 10_000  -- 10 MHz,   100 ns
      elsif RTI_kHz mod  8_000 = 0 then RTI_kHz /  8_000  --  8 MHz,   125 ns
      elsif RTI_kHz mod  5_000 = 0 then RTI_kHz /  5_000  --  5 MHz,   250 ns
      elsif RTI_kHz mod  4_000 = 0 then RTI_kHz /  4_000  --  4 MHz,   200 ns
      elsif RTI_kHz mod  2_000 = 0 then RTI_kHz /  2_000  --  2 MHz,   500 ns
      elsif RTI_kHz mod  1_000 = 0 then RTI_kHz /  1_000  --  1 MHz, 1_000 ns
      elsif RTI_kHz >= 100_000 then 10 --   10 MHz .. 50 MHz, 20 ..    100 ns
      elsif RTI_kHz >=  40_000 then  5 --    8 MHz .. 20 MHz, 50 ..    125 ns
      else 2);                         --   50 kHz .. 20 Mhz, 50 .. 20_000 ns

   Ticks_Per_Second : constant := (RTI_kHz * 1000) / Prescaler;
   --  Number of ticks per second

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 0 .. 95;
   --  Number of interrupts supported by the VIC. For the TMS570 interrupts,
   --  we really only consider the 95 usable interrupt channels. The run time
   --  assumes the interrupt source to interrupt channel map is direct (1:1),
   --  as is the default, but the user can change this as long as the IRQ used
   --  by the system for alarms stays unchanged.

   Trap_Vectors : constant := 7;
   --  ARM in general has these traps:
   --    0   (at 16#0000#) Reset
   --    1   (at 16#0004#) Undefined Instruction (synchronous)
   --    2   (at 16#0008#) Supervisor Call (synchronous)
   --    3   (at 16#000C#) Abort - Prefetch (synchronous)
   --    4   (at 16#0010#) Abort - Data (asynchronous)
   --    5   (at 16#0014#) IRQ Trap (asynchronous)
   --    6   (at 16#0018#) FIQ Trap (asynchronous)

   ------------------------
   -- Context Management --
   ------------------------

   --  The run time stores a minimal amount of state in the thread context.
   --  Most state will be saved on the task's stack when calling a potentially
   --  blocking operation, or on the interrupt stack when the task is pre-
   --  empted. Most of the space is currently required for floating point
   --  state, which is saved lazily.

   --  The TMS570 processor needs to save:

   --   * 6 integer registers of 32 bits (r0, r1, PC, CPSR, R12, SP)
   --     for normal processing

   --   * 33 floating point registers of 32 bits (s0 .. s31, FPCSR)

   --  This amounts to 39 registers, rounded up to 40 for alignment.

   Context_Buffer_Capacity : constant := 40;

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 4096;  --  bytes
   --  Size of each of the interrupt stacks. Each processor has its own
   --  set of interrupt stacks, one per interrupt priority.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
