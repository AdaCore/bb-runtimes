------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . T H R E A D S . Q U E U E S            --
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

package body System.BB.Threads.Queues is

   use System.Multiprocessors;
   use System.BB.Board_Support.Multiprocessors;

   ----------------
   -- Local data --
   ----------------

   Alarms_Table : array (CPU) of Thread_Id := (others => Null_Thread_Id);
   pragma Volatile_Components (Alarms_Table);
   --  Identifier of the thread that is in the first place of the alarm queue

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority (Thread : Thread_Id; Priority : Integer)
   is
      CPU_Id       : constant CPU := BOSUMU.Current_CPU;
      Head         : Thread_Id;
      Prev_Pointer : Thread_Id;

   begin
      --  A CPU can only change the priority of its own tasks

      pragma Assert (CPU_Id = Get_CPU (Thread));

      --  Return now if there is no change. This is a rather common case, as
      --  it happens if user is not using priorities, or if the priority of
      --  an interrupt handler is the same as the priority of the interrupt.
      --  In any case, the check is quick enough.

      if Thread.Active_Priority = Priority then
         return;
      end if;

      --  Change the active priority. The base priority does not change

      Thread.Active_Priority := Priority;

      --  Outside of the executive kernel, the running thread is also the first
      --  thread in the First_Thread_Table list. This is also true in general
      --  within the kernel, except during transcient period when a task is
      --  extracted from the list (blocked by a delay until or on an entry),
      --  when a task is inserted (after a wakeup), after a yield or after
      --  this procedure. But then a context_switch put things in order.

      --  However, on ARM Cortex-M, context switches can be delayed by
      --  interrupts. They are performed via a special interrupt (Pend_SV),
      --  which is at the lowest priority. This has three consequences:
      --   A) it is not possible to have tasks in the Interrupt_Priority range
      --   B) the head of First_Thread_Table list may be different from the
      --      running thread within user interrupt handler
      --   C) the running thread may not be in the First_Thread_Table list.
      --  The following scenario shows case B: while a thread is running, an
      --  interrupt awakes a task at a higher priority; it is put in front of
      --  the First_Thread_Table queue, and a context switch is requested. But
      --  before the end of the interrupt, another interrupt triggers. It
      --  increases the priority of  the current thread, which is not the
      --  first in queue.
      --  The following scenario shows case C: a task is executing a delay
      --  until and therefore it is removed from the First_Thread_Table. But
      --  before the context switch, an interrupt triggers and change the
      --  priority of the running thread.

      --  First, find THREAD in the queue and remove it temporarly.

      Head := First_Thread_Table (CPU_Id);

      if Head = Thread then

         --  This is the very common case: THREAD is the first in the queue

         if Thread.Next = Null_Thread_Id
           or else Priority >= Thread.Next.Active_Priority
         then
            --  Already at the right place.
            return;
         end if;

         --  Remove THREAD from the queue

         Head := Thread.Next;
      else

         --  Uncommon case: less than 0.1% on a Cortex-M test.

         --  Search the thread before THREAD.

         Prev_Pointer := Head;
         loop
            if Prev_Pointer = null then
               --  THREAD is not in the queue. This corresponds to case B.
               return;
            end if;

            exit when Prev_Pointer.Next = Thread;

            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         --  Remove THREAD from the queue.

         Prev_Pointer.Next := Thread.Next;
      end if;

      --  Now insert THREAD.

      --  FIFO_Within_Priorities dispatching policy. In ALRM D.2.2 it is
      --  said that when the active priority is lowered due to the loss of
      --  inherited priority (the only possible case within the Ravenscar
      --  profile) the task is added at the head of the ready queue for
      --  its new active priority.

      if Priority >= Head.Active_Priority then

         --  THREAD is the highest priority thread, so put it in the front of
         --  the queue.

         Thread.Next := Head;
         Head := Thread;
      else

         --  Search the right place in the queue.

         Prev_Pointer := Head;
         while Prev_Pointer.Next /= Null_Thread_Id
           and then Priority < Prev_Pointer.Next.Active_Priority
         loop
            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         Thread.Next := Prev_Pointer.Next;
         Prev_Pointer.Next := Thread;
      end if;

      First_Thread_Table (CPU_Id) := Head;
   end Change_Priority;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed return Boolean is
   begin
      --  A context switch is needed when there is a higher priority task ready
      --  to execute. It means that First_Thread is not null and it is not
      --  equal to the task currently executing (Running_Thread).

      return First_Thread /= Running_Thread;
   end Context_Switch_Needed;

   ----------------------
   -- Current_Priority --
   ----------------------

   function Current_Priority
     (CPU_Id : System.Multiprocessors.CPU) return Integer
   is
      Thread : constant Thread_Id := Running_Thread_Table (CPU_Id);
   begin
      if Thread = null or else Thread.State /= Threads.Runnable then
         return System.Any_Priority'First;
      else
         return Thread.Active_Priority;
      end if;
   end Current_Priority;

   -------------
   -- Extract --
   -------------

   procedure Extract (Thread : Thread_Id) is
      CPU_Id : constant CPU := Get_CPU (Thread);

   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      First_Thread_Table (CPU_Id) := Thread.Next;
      Thread.Next := Null_Thread_Id;
   end Extract;

   -------------------------
   -- Extract_First_Alarm --
   -------------------------

   function Extract_First_Alarm return Thread_Id is
      CPU_Id : constant CPU       := Current_CPU;
      Result : constant Thread_Id := Alarms_Table (CPU_Id);

   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      Alarms_Table (CPU_Id) := Result.Next_Alarm;
      Result.Alarm_Time := System.BB.Time.Time'Last;
      Result.Next_Alarm := Null_Thread_Id;
      return Result;
   end Extract_First_Alarm;

   ------------------
   -- First_Thread --
   ------------------

   function First_Thread return Thread_Id is
   begin
      return First_Thread_Table (Current_CPU);
   end First_Thread;

   -------------------------
   -- Get_Next_Alarm_Time --
   -------------------------

   function Get_Next_Alarm_Time (CPU_Id : CPU) return System.BB.Time.Time is
      Thread : Thread_Id;

   begin
      Thread := Alarms_Table (CPU_Id);

      if Thread = Null_Thread_Id then

         --  If alarm queue is empty then next alarm to raise will be Time'Last

         return System.BB.Time.Time'Last;

      else
         return Thread.Alarm_Time;
      end if;
   end Get_Next_Alarm_Time;

   ------------
   -- Insert --
   ------------

   procedure Insert (Thread : Thread_Id) is
      Aux_Pointer : Thread_Id;
      CPU_Id      : constant CPU := Get_CPU (Thread);

   begin
      --  ??? This pragma is disabled because the Tasks_Activated only
      --  represents the end of activation for one package not all the
      --  packages. We have to find a better milestone for the end of
      --  tasks activation.

      --  --  A CPU can only insert alarm in its own queue, except during
      --  --  initialization.

      --  pragma Assert (CPU_Id = Current_CPU or else not Tasks_Activated);

      --  It may be the case that we try to insert a task that is already in
      --  the queue. This can only happen if the task was not runnable and its
      --  context was being used for handling an interrupt. Hence, if the task
      --  is already in the queue and we try to insert it, we need to check
      --  whether it is in the correct place.

      --  No insertion if the task is already at the head of the queue

      if First_Thread_Table (CPU_Id) = Thread then
         null;

      --  Insert at the head of queue if there is no other thread with a higher
      --  priority.

      elsif First_Thread_Table (CPU_Id) = Null_Thread_Id
        or else
          Thread.Active_Priority > First_Thread_Table (CPU_Id).Active_Priority
      then
         Thread.Next := First_Thread_Table (CPU_Id);
         First_Thread_Table (CPU_Id) := Thread;

      --  Middle or tail insertion

      else
         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer := First_Thread_Table (CPU_Id);
         while Aux_Pointer.Next /= Null_Thread_Id
           and then Aux_Pointer.Next /= Thread
           and then Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  If we found the thread already in the queue, then we need to move
         --  it to its right place.

         if Aux_Pointer.Next = Thread then

            --  Extract it from its current location

            Aux_Pointer.Next := Thread.Next;

            --  Look for the Aux_Pointer to insert the thread just after it

            while Aux_Pointer.Next /= Null_Thread_Id
              and then
                Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
            loop
               Aux_Pointer := Aux_Pointer.Next;
            end loop;
         end if;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Insert;

   ------------------
   -- Insert_Alarm --
   ------------------

   procedure Insert_Alarm
     (T        : System.BB.Time.Time;
      Thread   : Thread_Id;
      Is_First : out Boolean)
   is
      CPU_Id       : constant CPU := Get_CPU (Thread);
      Alarm_Id_Aux : Thread_Id;

   begin
      --  A CPU can only insert alarm in its own queue

      pragma Assert (CPU_Id = Current_CPU);

      --  Set the Alarm_Time within the thread descriptor

      Thread.Alarm_Time := T;

      --  Case of empty queue, or new alarm expires earlier, insert the thread
      --  as the first thread.

      if Alarms_Table (CPU_Id) = Null_Thread_Id
        or else T < Alarms_Table (CPU_Id).Alarm_Time
      then
         Thread.Next_Alarm := Alarms_Table (CPU_Id);
         Alarms_Table (CPU_Id) := Thread;
         Is_First := True;

      --  Otherwise, place in the middle

      else
         --  Find the minimum greater than T alarm within the alarm queue

         Alarm_Id_Aux := Alarms_Table (CPU_Id);
         while Alarm_Id_Aux.Next_Alarm /= Null_Thread_Id and then
           Alarm_Id_Aux.Next_Alarm.Alarm_Time < T
         loop
            Alarm_Id_Aux := Alarm_Id_Aux.Next_Alarm;
         end loop;

         Thread.Next_Alarm := Alarm_Id_Aux.Next_Alarm;
         Alarm_Id_Aux.Next_Alarm := Thread;

         Is_First := False;
      end if;
   end Insert_Alarm;

   --------------------
   -- Running_Thread --
   --------------------

   function Running_Thread return Thread_Id is
   begin
      return Running_Thread_Table (Current_CPU);
   end Running_Thread;

   ---------------------------
   -- Wakeup_Expired_Alarms --
   ---------------------------

   procedure Wakeup_Expired_Alarms (Now : Time.Time) is
      use Time;

      CPU_Id        : constant CPU := Current_CPU;
      Wakeup_Thread : Thread_Id;

   begin
      --  Extract all the threads whose delay has expired

      while Get_Next_Alarm_Time (CPU_Id) <= Now loop

         --  Extract the task(s) that was waiting in the alarm queue and insert
         --  it in the ready queue.

         Wakeup_Thread := Extract_First_Alarm;

         --  We can only awake tasks that are delay statement

         pragma Assert (Wakeup_Thread.State = Delayed);

         Wakeup_Thread.State := Runnable;

         Insert (Wakeup_Thread);
      end loop;

      --  Note: the caller (BB.Time.Alarm_Handler) must set the next alarm
   end Wakeup_Expired_Alarms;

   -----------
   -- Yield --
   -----------

   procedure Yield (Thread : Thread_Id) is
      CPU_Id      : constant CPU     := Get_CPU (Thread);
      Prio        : constant Integer := Thread.Active_Priority;
      Aux_Pointer : Thread_Id;

   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      if Thread.Next /= Null_Thread_Id
        and then Thread.Next.Active_Priority = Prio
      then
         First_Thread_Table (CPU_Id) := Thread.Next;

         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer  := First_Thread_Table (CPU_Id);
         while Aux_Pointer.Next /= Null_Thread_Id
           and then Prio = Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Yield;

   ------------------
   -- Queue_Length --
   ------------------

   function Queue_Length return Natural is
      Res : Natural   := 0;
      T   : Thread_Id := First_Thread_Table (Current_CPU);

   begin
      while T /= null loop
         Res := Res + 1;
         T := T.Next;
      end loop;

      return Res;
   end Queue_Length;

   -------------------
   -- Queue_Ordered --
   -------------------

   function Queue_Ordered return Boolean is
      T : Thread_Id := First_Thread_Table (Current_CPU);
      N : Thread_Id;

   begin
      if T = Null_Thread_Id then
         --  True if the queue is empty
         return True;
      end if;

      loop
         N := T.Next;
         if N = Null_Thread_Id then
            --  True if at end of the queue
            return True;
         end if;

         if T.Active_Priority < N.Active_Priority then
            return False;
         end if;

         T := N;
      end loop;
   end Queue_Ordered;

end System.BB.Threads.Queues;
