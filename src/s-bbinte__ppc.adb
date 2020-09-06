------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . I N T E R R U P T S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;
with System.BB.CPU_Primitives;
with System.BB.CPU_Specific;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.Board_Support;
with System.BB.Time;
with System.Multiprocessors;

package body System.BB.Interrupts is

   use System.Multiprocessors;
   use System.BB.Threads;
   use System.BB.Time;

   ----------------
   -- Local data --
   ----------------

   type Stack_Space is new Storage_Elements.Storage_Array
     (1 .. Storage_Elements.Storage_Offset (Parameters.Interrupt_Stack_Size));
   for Stack_Space'Alignment use CPU_Specific.Stack_Alignment;
   pragma Suppress_Initialization (Stack_Space);
   --  Type used to represent the stack area for each interrupt. The stack must
   --  be aligned to 8 bytes to allow double word data movements.

   Interrupt_Stacks : array (CPU) of Stack_Space;
   pragma Linker_Section (Interrupt_Stacks, ".interrupt_stacks");
   --  Array that contains the stack used for each interrupt priority on each
   --  CPU.
   --
   --  The interrupt stacks are assigned a special section so the linker script
   --  can put them at a specific place and avoid useless initialization.
   --
   --  Having a separate interrupt stask (from user tasks stack) helps to
   --  reduce the memory footprint as there is no need to reserve space for
   --  interrupts in user stacks.
   --
   --  Because several interrupts can share the same priority and because there
   --  can be many priorities we prefered not to have one stack per priority.
   --  Instead we have one interrupt stack per CPU. Such interrupts cannot be
   --  executing at the same time.
   --
   --  An interrupt handler doesn't need to save non-volatile registers,
   --  because the interrupt is always completed before the interrupted task is
   --  resumed. This is obvious for non interrupt priority tasks and for
   --  active task at interrupt priority. The idle task (the one actived in
   --  System.BB.Protection.Leave_Kernel) cannot be at interrupt priority as
   --  there is always one task not in the interrupt priority range (the
   --  environmental task), and this one must be active or idle when an higher
   --  priority task is resumed.

   Interrupt_Stack_Table : array (CPU) of System.Address;
   pragma Export (Asm, Interrupt_Stack_Table, "interrupt_stack_table");
   --  Table that contains a pointer to the top of the stack for each processor

   type Handlers_Table is array (Interrupt_ID) of Interrupt_Handler;
   --  Type used to represent the procedures used as interrupt handlers

   Interrupt_Handlers_Table : Handlers_Table := (others => null);
   --  Table containing handlers attached to the different external interrupts

   Interrupt_Being_Handled : Any_Interrupt_ID := No_Interrupt;
   pragma Volatile (Interrupt_Being_Handled);
   --  Interrupt_Being_Handled contains the interrupt currently being
   --  handled if any. It is equal to No_Interrupt when no interrupt
   --  is handled. Its value is updated by the trap handler.

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (Handler : not null Interrupt_Handler;
      Id      : Interrupt_ID;
      Prio    : Interrupt_Priority)
   is
   begin
      --  Check that we are attaching to a real interrupt

      pragma Assert (Id /= No_Interrupt);

      --  Check that no previous interrupt handler has been registered

      if Interrupt_Handlers_Table (Id) /= null then
         raise Program_Error;
      end if;

      --  Copy the user's handler to the appropriate place within the table

      Interrupt_Handlers_Table (Id) := Handler;

      --  The BSP determines the vector that will be called when the given
      --  interrupt occurs, and then installs the handler there. This may
      --  include programming the interrupt controller.

      Board_Support.Interrupts.Install_Interrupt_Handler (Id, Prio);
   end Attach_Handler;

   -----------------------
   -- Current_Interrupt --
   -----------------------

   function Current_Interrupt return Any_Interrupt_ID is
      Result : constant Any_Interrupt_ID := Interrupt_Being_Handled;

   begin
      if Threads.Thread_Self.In_Interrupt then

         pragma Assert (Result /= No_Interrupt);
         return Result;

      else
         return No_Interrupt;
      end if;
   end Current_Interrupt;

   -----------------------
   -- Interrupt_Wrapper --
   -----------------------

   procedure Interrupt_Wrapper (Id : Interrupt_ID)
   is
      Self_Id         : constant Threads.Thread_Id := Threads.Thread_Self;
      Caller_Priority : constant Integer :=
                          Threads.Get_Priority (Self_Id);
      Int_Priority    : constant Interrupt_Priority :=
                          Board_Support.Interrupts.Priority_Of_Interrupt (Id);
      Previous_Int    : constant Any_Interrupt_ID := Interrupt_Being_Handled;
      Prev_In_Interr  : constant Boolean := Self_Id.In_Interrupt;

   begin
      --  Update execution time for the interrupted task

      if Scheduling_Event_Hook /= null then
         Scheduling_Event_Hook.all;
      end if;

      --  Store the interrupt being handled

      Interrupt_Being_Handled := Id;

      --  Then, we must set the appropriate software priority corresponding
      --  to the interrupt being handled. It also deals with the appropriate
      --  interrupt masking.

      --  When this wrapper is called all interrupts are masked, and the active
      --  priority of the interrupted task must be lower than the priority of
      --  the interrupt (otherwise the interrupt would have been masked). The
      --  only exception to this is when a task is temporarily inserted in the
      --  ready queue because there is not a single task ready to execute; this
      --  temporarily inserted task may have a priority in the range of the
      --  interrupt priorities (it may be waiting in an entry for a protected
      --  handler), but interrupts would not be masked.

      pragma Assert
        (Caller_Priority <= Int_Priority or else Self_Id.State /= Runnable);

      Self_Id.In_Interrupt := True;
      Threads.Queues.Change_Priority (Self_Id, Int_Priority);

      CPU_Primitives.Enable_Interrupts (Int_Priority);

      --  Call the user handler

      Interrupt_Handlers_Table (Id).all (Id);

      CPU_Primitives.Disable_Interrupts;

      --  Update execution time for the interrupt. This must be done before
      --  changing priority (Scheduling_Event use priority to determine which
      --  task/interrupt will get the elapsed time).

      if Scheduling_Event_Hook /= null then
         Scheduling_Event_Hook.all;
      end if;

      --  Restore the software priority to the state before the interrupt
      --  happened. Interrupt unmasking is not done here (it will be done
      --  later by the interrupt epilogue).

      Threads.Queues.Change_Priority (Self_Id, Caller_Priority);

      --  Restore the interrupt that was being handled previously (if any)

      Interrupt_Being_Handled := Previous_Int;

      --  Restore previous interrupt number (which is False unless interrupt
      --  is nested).

      Self_Id.In_Interrupt := Prev_In_Interr;

      --  Switch back to previous priority
      --
      --  The priority used (Caller_Priority) may not be correct if a task has
      --  been unblocked. But in that case, the task was blocked inside the
      --  kernel (so with interrupt disabled), and the correct priority will
      --  be set by Leave_Kernel.

      Board_Support.Interrupts.Set_Current_Priority (Caller_Priority);
   end Interrupt_Wrapper;

   ----------------------------
   -- Within_Interrupt_Stack --
   ----------------------------

   function Within_Interrupt_Stack
     (Stack_Address : System.Address) return Boolean
   is
      (Current_Interrupt /= No_Interrupt and then Stack_Address in
          Interrupt_Stacks (CPU'First)(Stack_Space'First)'Address
             ..
          Interrupt_Stacks (CPU'Last)(Stack_Space'Last)'Address);

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   procedure Initialize_Interrupts is
   begin
      for Proc in CPU loop

         CPU_Primitives.Initialize_Stack
           (Interrupt_Stacks (Proc)'Address,
            Stack_Space'Length,
            Interrupt_Stack_Table (Proc));
      end loop;
   end Initialize_Interrupts;
end System.BB.Interrupts;
