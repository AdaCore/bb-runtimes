------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASKING.PROTECTED_OBJECTS.OPERATIONS                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1998-2016, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains all extended primitives related to Protected_Objects
--  with entries.

--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the simple routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Entries.

--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  This package contains all primitives related to Protected_Objects.
--  Note: the compiler generates direct calls to this interface, via Rtsfind.

with System.Task_Primitives.Operations;
with System.Tasking.Queuing;
with System.Tasking.Debug;
with System.Parameters;
with System.Restrictions;
with System.Multiprocessors;
with System.Tasking.Protected_Objects.Multiprocessors;

package body System.Tasking.Protected_Objects.Operations is

   package STPO renames System.Task_Primitives.Operations;
   package STPOM renames System.Tasking.Protected_Objects.Multiprocessors;

   use Parameters;
   use Task_Primitives;
   use Ada.Exceptions;
   use Entries;
   use System.Multiprocessors;

   use System.Restrictions;
   use System.Restrictions.Rident;

   Multiprocessor : constant Boolean := CPU'Range_Length /= 1;

   procedure PO_Do_Or_Queue
     (Self_ID    : Task_Id;
      Object     : Entries.Protection_Entries_Access;
      Entry_Call : Entry_Call_Link);
   --  This procedure either executes or queues an entry call, depending
   --  on the status of the corresponding barrier. It assumes that abort
   --  is deferred and that the specified object is locked.

   procedure PO_Service_Entries
     (Self_ID       : Task_Id;
      Object        : Entries.Protection_Entries_Access);
   --  Service all entry queues of the specified object, executing the
   --  corresponding bodies of any queued entry calls that are waiting
   --  on True barriers. This is used when the state of a protected
   --  object may have changed, in particular after the execution of
   --  the statement sequence of a protected procedure.
   --
   --  Note that servicing an entry may change the value of one or more
   --  barriers, so this routine keeps checking barriers until all of
   --  them are closed.
   --
   --  This must be called with abort deferred and with the corresponding
   --  object locked.

   -------------------------
   -- Complete_Entry_Body --
   -------------------------

   procedure Complete_Entry_Body (Object : Protection_Entries_Access) is
   begin
      Exceptional_Complete_Entry_Body (Object, Ada.Exceptions.Null_Id);
   end Complete_Entry_Body;

   -------------------------------------
   -- Exceptional_Complete_Entry_Body --
   -------------------------------------

   procedure Exceptional_Complete_Entry_Body
     (Object : Protection_Entries_Access;
      Ex     : Ada.Exceptions.Exception_Id)
   is
      procedure Transfer_Occurrence
        (Target : Ada.Exceptions.Exception_Occurrence_Access;
         Source : Ada.Exceptions.Exception_Occurrence);
      pragma Import (C, Transfer_Occurrence, "__gnat_transfer_occurrence");
      --  Import a private declaration from Ada.Exceptions

      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;
      Self_Id    : Task_Id;

   begin
      pragma Assert (Entry_Call /= null);

      Entry_Call.Exception_To_Raise := Ex;

      if Ex /= Ada.Exceptions.Null_Id then

         --  An exception was raised and abort was deferred, so adjust
         --  before propagating, otherwise the task will stay with deferral
         --  enabled for its remaining life.

         Self_Id := STPO.Self;

         Transfer_Occurrence
           (Entry_Call.Self.Common.Compiler_Data.Current_Excep'Access,
            Self_Id.Common.Compiler_Data.Current_Excep);
      end if;
   end Exceptional_Complete_Entry_Body;

   --------------------
   -- PO_Do_Or_Queue --
   --------------------

   procedure PO_Do_Or_Queue
     (Self_ID    : Task_Id;
      Object     : Protection_Entries_Access;
      Entry_Call : Entry_Call_Link)
   is
      E     : constant Protected_Entry_Index :=
                Protected_Entry_Index (Entry_Call.E);
      Index : constant Protected_Entry_Index :=
                Object.Find_Body_Index (Object.Compiler_Info, E);

      Barrier_Value : Boolean;
      Queue_Length  : Natural;

   begin
      --  Evaluate barrier. Due to the Pure_Barrier restriction, this cannot
      --  raise exception.

      Barrier_Value :=
        Object.Entry_Bodies (Index).Barrier (Object.Compiler_Info, E);

      if Barrier_Value then
         Object.Call_In_Progress := Entry_Call;

         --  Execute the entry. Exceptions cannot propagate from the entry, as
         --  they must be handled by Exceptional_Complete_Entry_Body.

         Object.Entry_Bodies (Index).Action
           (Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);

         --  Body of current entry served call to completion

         Object.Call_In_Progress := null;

      else
         if Run_Time_Restrictions.Set (Max_Entry_Queue_Length)
           or else Object.Entry_Queue_Maxes /= null
         then
            --  Need to check the queue length. Computing the length is an
            --  unusual case and is slow (need to walk the queue).

            Queue_Length := Queuing.Count_Waiting (Object.Entry_Queues (E));

            if (Run_Time_Restrictions.Set (Max_Entry_Queue_Length)
                 and then Queue_Length >=
                   Run_Time_Restrictions.Value (Max_Entry_Queue_Length))
              or else
                (Object.Entry_Queue_Maxes /= null
                  and then Object.Entry_Queue_Maxes (Index) /= 0
                  and then Queue_Length >= Object.Entry_Queue_Maxes (Index))
            then
               --  This violates the Max_Entry_Queue_Length restriction or the
               --  Max_Queue_Length bound, raise Program_Error.

               Entry_Call.Exception_To_Raise := Program_Error'Identity;

               return;
            end if;
         end if;

         Queuing.Enqueue (Object.Entry_Queues (E), Entry_Call);

         --  Suspend until entry call has been completed. On exit, the call
         --  will not be queued.

         Unlock_Entries (Object);

         Self_ID.Common.State := Entry_Caller_Sleep;
         STPO.Sleep (Self_ID, Entry_Caller_Sleep);

         Self_ID.Common.State := Runnable;

         --  We need to get the lock again

         Lock_Entries (Object);
      end if;
   end PO_Do_Or_Queue;

   ------------------------
   -- PO_Service_Entries --
   ------------------------

   procedure PO_Service_Entries
     (Self_ID       : Task_Id;
      Object        : Entries.Protection_Entries_Access)
   is
      E          : Protected_Entry_Index;
      Caller     : Task_Id;
      Entry_Call : Entry_Call_Link;

   begin
      loop
         Queuing.Select_Protected_Entry_Call (Self_ID, Object, Entry_Call);

         exit when Entry_Call = null;

         E := Protected_Entry_Index (Entry_Call.E);

         Object.Call_In_Progress := Entry_Call;

         --  Execute the entry

         Object.Entry_Bodies
            (Object.Find_Body_Index (Object.Compiler_Info, E)).Action
               (Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);

         --  Signal the entry caller that the entry is completed (it it needs
         --  to wake up and continue execution).

         Caller := Entry_Call.Self;

         if not Multiprocessor
           or else Caller.Common.Base_CPU = STPO.Self.Common.Base_CPU
         then
            --  Entry caller and servicing tasks are on the same CPU.
            --  We are allowed to directly wake up the task.

            STPO.Wakeup (Caller, Entry_Caller_Sleep);
         else
            --  The entry caller is on a different CPU.

            STPOM.Served (Entry_Call);
         end if;
      end loop;
   end PO_Service_Entries;

   ---------------------
   -- Protected_Count --
   ---------------------

   function Protected_Count
     (Object : Protection_Entries;
      E      : Protected_Entry_Index) return Natural
   is
   begin
      return Queuing.Count_Waiting (Object.Entry_Queues (E));
   end Protected_Count;

   --------------------------
   -- Protected_Entry_Call --
   --------------------------

   --  Compiler interface only (do not call from within the RTS)

   --  select r.e;
   --     ...A...
   --  else
   --     ...B...
   --  end select;

   --  declare
   --     X : protected_entry_index := 1;
   --     B85b : communication_block;
   --     communication_blockIP (B85b);

   --  begin
   --     protected_entry_call (rTV!(r)._object'unchecked_access, X,
   --       null_address, conditional_call, B85b, objectF => 0);

   --     if cancelled (B85b) then
   --        ...B...
   --     else
   --        ...A...
   --     end if;
   --  end;

   --  See also Cancel_Protected_Entry_Call for code expansion of asynchronous
   --  entry call.

   --  The initial part of this procedure does not need to lock the calling
   --  task's ATCB, up to the point where the call record first may be queued
   --  (PO_Do_Or_Queue), since before that no other task will have access to
   --  the record.

   --  If this is a call made inside of an abort deferred region, the call
   --  should be never abortable.

   --  If the call was not queued abortably, we need to wait until it is before
   --  proceeding with the abortable part.

   --  There are some heuristics here, just to save time for frequently
   --  occurring cases. For example, we check Initially_Abortable to try to
   --  avoid calling the procedure Wait_Until_Abortable, since the normal case
   --  for async. entry calls is to be queued abortably.

   procedure Protected_Entry_Call
     (Object              : Protection_Entries_Access;
      E                   : Protected_Entry_Index;
      Uninterpreted_Data  : System.Address;
      Mode                : Call_Modes;
      Block               : out Communication_Block)
   is
      pragma Unreferenced (Mode);

      procedure Internal_Raise (X : Ada.Exceptions.Exception_Id);
      pragma Import (C, Internal_Raise, "__gnat_raise_with_msg");

      Self_ID    : constant Task_Id := STPO.Self;
      Entry_Call : Entry_Call_Link;

   begin
      --  For this run time, pragma Detect_Blocking is always active, so we
      --  must raise Program_Error if this potentially blocking operation is
      --  called from a protected action.

      if Self_ID.Common.Protected_Action_Nesting > 0 then
         raise Program_Error with "potentially blocking operation";
      end if;

      --  Exclusive access to the protected object

      Lock_Entries (Object);

      Block.Self := Self_ID;

      --  Initialize Entry_Call. No need to clear Exception_To_Raise, as it is
      --  cleared at the end of the entry by Complete_Entry_Body.

      Entry_Call := Self_ID.Entry_Call'Access;
      Entry_Call.Next := null;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;

      --  Execute the entry if the barrier is open, or enqueue the call until
      --  the barrier opens.

      PO_Do_Or_Queue (Self_ID, Object, Entry_Call);

      --  Check whether there are entries pending (barriers may have changed)

      PO_Service_Entries (Self_ID, Object);

      --  End of exclusive access

      Unlock_Entries (Object);

      --  Check if an exception has to be propagated from the entry to the
      --  caller.

      if Entry_Call.Exception_To_Raise /= Ada.Exceptions.Null_Id then

         --  If this is the case, propagate it. A raise statement cannot be
         --  used, as the call stack must not be modified.

         Internal_Raise (Entry_Call.Exception_To_Raise);
      end if;
   end Protected_Entry_Call;

   ----------------------------
   -- Protected_Entry_Caller --
   ----------------------------

   function Protected_Entry_Caller
     (Object : Protection_Entries) return Task_Id is
   begin
      return Object.Call_In_Progress.Self;
   end Protected_Entry_Caller;

   ---------------------
   -- Service_Entries --
   ---------------------

   procedure Service_Entries (Object : Protection_Entries_Access) is
      Self_ID : constant Task_Id := STPO.Self;
   begin
      PO_Service_Entries (Self_ID, Object);

      Unlock_Entries (Object);
   end Service_Entries;
end System.Tasking.Protected_Objects.Operations;
