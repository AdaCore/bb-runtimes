------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 SYSTEM.BB.CPU_PRIMITIVES.MULTIPROCESSORS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Board_Support;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.Protection;
with System.BB.Timing_Events;
with System.BB.Time;

package body System.BB.CPU_Primitives.Multiprocessors is
   use System.Multiprocessors;

   --------------------
   -- Start_All_CPUs --
   --------------------

   procedure Start_All_CPUs is
   begin
      --  Nothing to do when there's only one CPU

      if System.Multiprocessors.Number_Of_CPUs = 1 then
         return;
      end if;

      System.BB.Board_Support.Multiprocessors.Start_All_CPUs;
   end Start_All_CPUs;

   ------------------
   -- Poke_Handler --
   ------------------

   procedure Poke_Handler is
      use type Threads.Thread_States;

      Now : Time.Time;

   begin
      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      --  Handle alarms in the case the alarm is system-wide

      Now := Time.Clock;

      --  Execute expired events of the current CPU

      Timing_Events.Execute_Expired_Timing_Events (Now);

      --  Wake up alarms

      Threads.Queues.Wakeup_Expired_Alarms (Now);

      Protection.Leave_Kernel;
   end Poke_Handler;

end System.BB.CPU_Primitives.Multiprocessors;
