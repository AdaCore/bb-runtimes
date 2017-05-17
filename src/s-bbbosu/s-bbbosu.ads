------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
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

--  This package defines an interface used for handling the peripherals
--  available in the target board that are needed by the target-independent
--  part of the run time.

pragma Restrictions (No_Elaboration_Code);

with System.Multiprocessors;
with System.BB.Interrupts;
with System.BB.CPU_Primitives;
with System.BB.Time;

package System.BB.Board_Support is
   pragma Preelaborate;

   -----------------------------
   -- Hardware Initialization --
   -----------------------------

   procedure Initialize_Board;
   --  Procedure that performs the hardware initialization of the board. Should
   --  be called before any other operations in this package.

   ------------------------------------------------
   -- Clock and Timer Definitions and Primitives --
   ------------------------------------------------

   package Time is
      type Timer_Interval is mod 2 ** 32;
      for Timer_Interval'Size use 32;
      --  This type represents any interval that we can measure within a
      --  Clock_Interrupt_Period. Even though this type is always 32 bits, its
      --  actual allowed range is 0 .. Max_Timer_Interval, which may be less,
      --  depending on the target.

      function Read_Clock return BB.Time.Time;
      --  Read the value contained in the clock hardware counter, and return
      --  the number of ticks elapsed since the last clock interrupt, that is,
      --  since the clock counter was last reloaded.

      function Max_Timer_Interval return Timer_Interval;
      pragma Inline (Max_Timer_Interval);
      --  The maximum value of the hardware clock. The is the maximum value
      --  that Read_Clock may return, and the longest interval that Set_Alarm
      --  may use. The hardware clock period is Max_Timer_Interval + 1 clock
      --  ticks. An interrupt occurs after this number of ticks.

      procedure Set_Alarm (Ticks : Timer_Interval);
      --  Set an alarm that will expire after the specified number of clock
      --  ticks. This cancels any previous alarm set.

      procedure Install_Alarm_Handler (Handler : Interrupts.Interrupt_Handler);
      --  Install Handler as alarm interrupt handler

      procedure Clear_Alarm_Interrupt;
      pragma Inline (Clear_Alarm_Interrupt);
      --  Acknowledge the alarm interrupt
   end Time;

   ----------------
   -- Interrupts --
   ----------------

   package Interrupts is
      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority;
      pragma Inline (Priority_Of_Interrupt);
      --  Function to obtain the priority associated with an interrupt.

      procedure Install_Interrupt_Handler
        (Interrupt : System.BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority);
      --  Determine the trap vector that will be called for handling the given
      --  external interrupt on the current CPU, and install the given handler
      --  there. It is an error to try to install two different handlers for
      --  the vector, though this procedure may be called for multiple
      --  interrupts that share the same vector, as long as they use the same
      --  handler. The handler expects a single argument indicating the vector
      --  called. This routine may need to set up the interrupt controller to
      --  enable the given interrupt source, so it will actually cause a trap
      --  on the CPU. Note, this should not actually enable interrupts, as this
      --  is only done through CPU_Primitives.Enable_Interrupts, which
      --  typically uses a processor status register. Prio is the priority for
      --  the interrupt, and the hardware can be programmed to use that
      --  priority.

      procedure Set_Current_Priority (Priority : Integer);
      pragma Inline (Set_Current_Priority);
      --  Only allow interrupts higher than the specified priority. This
      --  routine differes from the Enable_Interrupts/Disable_Interrupts
      --  procedures in CPU_Primitives in that it disables interrupts at the
      --  board level, rather than the CPU. Typically if board-specific support
      --  of an interrupt controller is needed to block interrupts of
      --  insufficient priority, this routine will be needed. On other systems,
      --  where the processor has this control, or where only a single
      --  interrupt priority is supported, this may be a null procedure.

      procedure Power_Down;
      pragma Inline (Power_Down);
      --  Power-down the current CPU. This procedure is called only by the idle
      --  task, with interrupt enabled.
   end Interrupts;

   package Multiprocessors is
      --  The following functions define an interface for handling the "poke"
      --  interrupts that are used to signal other processors in an
      --  multiprocessor system.

      function Number_Of_CPUs return System.Multiprocessors.CPU;
      pragma Inline (Number_Of_CPUs);
      --  Return the number of available CPUs on the target

      function Current_CPU return System.Multiprocessors.CPU;
      pragma Inline (Current_CPU);
      --  Return the id of the current CPU

      procedure Poke_CPU (CPU_Id : System.Multiprocessors.CPU);
      --  Poke the given CPU to signal that a rescheduling may be required

      procedure Start_All_CPUs;
      --  Start all slave CPUs
   end Multiprocessors;
end System.BB.Board_Support;
