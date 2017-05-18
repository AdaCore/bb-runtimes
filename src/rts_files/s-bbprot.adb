------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P R O T E C T I O N                --
--                                                                          --
--                                  B o d y                                 --
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

pragma Restrictions (No_Elaboration_Code);

with System.BB.CPU_Primitives;
with System.BB.Parameters;
with System.BB.Board_Support;
with System.BB.Threads;
with System.BB.Time;

with System.BB.Threads.Queues;

--  The following pragma Elaborate is anomalous. We generally do not like
--  to use pragma Elaborate, since it disconnects the static elaboration
--  model checking (and generates a warning when using this model). So
--  either replace with Elaborate_All, or document why we need this and
--  why it is safe ???

pragma Warnings (Off);
pragma Elaborate (System.BB.Threads.Queues);
pragma Warnings (On);

package body System.BB.Protection is

   ------------------
   -- Enter_Kernel --
   ------------------

   procedure Enter_Kernel is
   begin
      --  Interrupts are disabled to avoid concurrency problems when modifying
      --  kernel data. This way, external interrupts cannot be raised.

      CPU_Primitives.Disable_Interrupts;
   end Enter_Kernel;

   ------------------
   -- Leave_Kernel --
   ------------------

   procedure Leave_Kernel is
      use System.BB.Time;
      use type System.BB.Threads.Thread_Id;
      use type System.BB.Threads.Thread_States;

   begin
      --  Interrupts are always disabled when entering here

      --  Wake up served entry calls

      if Parameters.Multiprocessor
        and then Wakeup_Served_Entry_Callback /= null
      then
         Wakeup_Served_Entry_Callback.all;
      end if;

      --  The idle task is always runnable, so there is always a task to be
      --  run.

      --  We need to check whether a context switch is needed

      if Threads.Queues.Context_Switch_Needed then

         --  Perform a context switch because the currently executing thread
         --  is blocked or it is no longer the one with the highest priority.

         --  Update execution time before context switch

         if Scheduling_Event_Hook /= null then
            Scheduling_Event_Hook.all;
         end if;

         CPU_Primitives.Context_Switch;
      end if;

      --  There is always a running thread (at worst the idle thread)

      pragma Assert (Threads.Queues.Running_Thread.State = Threads.Runnable);

      --  Now we need to set the hardware interrupt masking level equal to the
      --  software priority of the task that is executing.

      CPU_Primitives.Enable_Interrupts
        (Threads.Queues.Running_Thread.Active_Priority);
   end Leave_Kernel;

end System.BB.Protection;
