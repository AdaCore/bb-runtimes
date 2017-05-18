------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This is a Ravenscar bare board version of this body. Tasking version of
--  these functions are always used.

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get an
--  infinite loop from the code within the Poll routine itself.

with System.Tasking;
with System.Task_Primitives.Operations;

package body System.Soft_Links is

   use System.Task_Primitives.Operations;
   use type System.Tasking.Termination_Handler;

   ----------------
   -- Local data --
   ----------------

   Caller_Priority : Any_Priority;
   --  Task's active priority when the global lock is seized. This priority is
   --  restored when the task releases the global lock.

   ----------------------------
   -- Get_Current_Excep_Soft --
   ----------------------------

   function Get_Current_Excep_Soft return EOA is
   begin
      return Self.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep_Soft;

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id is
   begin
      return Ada.Exceptions.Exception_Identity (Get_Current_Excep.all.all);
   end Get_GNAT_Exception;

   -------------------
   -- Adafinal_Soft --
   -------------------

   procedure Adafinal_Soft is
   begin
      --  Handle normal task termination by the environment task, but only for
      --  the normal task termination. Abnormal termination is not supported by
      --  this run time, and in the case of Unhandled_Exception the last chance
      --  handler is invoked (which does not return).

      Task_Termination_Handler.all (Ada.Exceptions.Null_Occurrence);

      --  Here we should typically finalize all library-level controlled
      --  objects. However, in Ravenscar tasks (including the environment task)
      --  are non-terminating, so we avoid finalization.

      --  We used to raise a Program_Error here to signal the task termination
      --  event in order to avoid silent task death. It has been removed
      --  because the Ada.Task_Termination functionality serves the same
      --  purpose in a more flexible (and standard) way. In addition, this
      --  exception triggered a second execution of the termination handler
      --  (if any was installed).

   end Adafinal_Soft;

   --------------------
   -- Task_Lock_Soft --
   --------------------

   procedure Task_Lock_Soft is
      Self_Id : constant System.Tasking.Task_Id := Self;

   begin
      Self_Id.Common.Global_Task_Lock_Nesting :=
        Self_Id.Common.Global_Task_Lock_Nesting + 1;

      if Self_Id.Common.Global_Task_Lock_Nesting = 1 then
         declare
            Prio : constant System.Any_Priority := Get_Priority (Self_Id);

         begin
            --  Increase priority

            Set_Priority (Self_Id, System.Any_Priority'Last);

            --  Store caller's active priority so that it can be later restored
            --  when releasing the global lock.

            Caller_Priority := Prio;
         end;
      end if;
   end Task_Lock_Soft;

   ---------------------------
   -- Task_Termination_Soft --
   ---------------------------

   procedure Task_Termination_Soft (Except : EO) is
      pragma Unreferenced (Except);

      Self_Id : constant System.Tasking.Task_Id := Self;
      TH      : System.Tasking.Termination_Handler := null;

   begin
      --  Raise the priority to prevent race conditions when using
      --  System.Tasking.Fall_Back_Handler.

      Set_Priority (Self_Id, Any_Priority'Last);

      TH := System.Tasking.Fall_Back_Handler;

      --  Restore original priority after retrieving shared data

      Set_Priority (Self_Id, Self_Id.Common.Base_Priority);

      --  Execute the task termination handler if we found it

      if TH /= null then
         TH.all (Self_Id);
      end if;
   end Task_Termination_Soft;

   ----------------------
   -- Task_Unlock_Soft --
   ----------------------

   procedure Task_Unlock_Soft is
      Self_Id : constant System.Tasking.Task_Id := Self;

   begin
      pragma Assert (Self_Id.Common.Global_Task_Lock_Nesting > 0);

      Self_Id.Common.Global_Task_Lock_Nesting :=
        Self_Id.Common.Global_Task_Lock_Nesting - 1;

      if Self_Id.Common.Global_Task_Lock_Nesting = 0 then

         --  Restore the task's active priority

         Set_Priority (Self_Id, Caller_Priority);
      end if;
   end Task_Unlock_Soft;

end System.Soft_Links;
