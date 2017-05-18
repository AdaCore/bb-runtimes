------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains a set of subprogram access variables that access
--  some low-level primitives that are different depending whether tasking is
--  involved or not (e.g. the Get/Set_Jmpbuf_Address that needs to provide a
--  different value for each task). To avoid dragging in the tasking runtimes
--  all the time, we use a system of soft links where the links are
--  initialized to non-tasking versions, and then if the tasking support is
--  initialized, they are set to the real tasking versions.

--  This is a Ravenscar bare board version of this package. Tasking versions
--  of the primitives are always used.

with Ada.Exceptions;

package System.Soft_Links is
   pragma Preelaborate;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO is Ada.Exceptions.Exception_Occurrence;

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values, which by default point to dummy no tasking versions of routines.

   type No_Param_Proc is access procedure;
   pragma Suppress_Initialization (No_Param_Proc);
   type EO_Param_Proc is access procedure (Excep : EO);
   pragma Favor_Top_Level (EO_Param_Proc);

   type Get_EOA_Call is access function return EOA;

   --  Suppress checks on all these types, since we know the corrresponding
   --  values can never be null (the soft links are always initialized).

   pragma Suppress (Access_Check, No_Param_Proc);
   pragma Suppress (Access_Check, EO_Param_Proc);
   pragma Suppress (Access_Check, Get_EOA_Call);

   --  The following one is not related to tasking/no-tasking but to the
   --  traceback decorators for exceptions.

   type Traceback_Decorator_Wrapper_Call is access
     function (Traceback : System.Address; Len : Natural) return String;
   pragma Favor_Top_Level (Traceback_Decorator_Wrapper_Call);

   procedure Abort_Defer_Raven is null;
   --  Defer task abort (Ravenscar case, does nothing)

   procedure Abort_Undefer_Raven is null;
   --  Undefer task abort (Ravenscar case, does nothing)

   procedure Task_Lock_Soft;
   --  Lock out other tasks

   procedure Task_Unlock_Soft;
   --  Release lock set by Task_Lock

   Lock_Task : No_Param_Proc := Task_Lock_Soft'Access;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   Unlock_Task : No_Param_Proc := Task_Unlock_Soft'Access;
   --  Releases lock previously set by call to Lock_Task. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.
   --
   --  Note: the recommended protocol for using Lock_Task and Unlock_Task
   --  is as follows:
   --
   --    Locked_Processing : begin
   --       System.Soft_Links.Lock_Task.all;
   --       ...
   --       System.Soft_Links.Unlock_Task.all;
   --
   --    exception
   --       when others =>
   --          System.Soft_Links.Unlock_Task.all;
   --          raise;
   --    end Locked_Processing;
   --
   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.

   procedure Adafinal_Soft;
   --  Programs do not terminate in Ravenscar

   Adafinal : No_Param_Proc := Adafinal_Soft'Access;
   --  Performs the finalization of the Ada Runtime

   Abort_Defer : constant No_Param_Proc := Abort_Defer_Raven'Access;
   pragma Suppress (Access_Check, Abort_Defer);
   --  Defer task abort (task/non-task case as appropriate)

   Abort_Undefer : constant No_Param_Proc := Abort_Undefer_Raven'Access;
   pragma Suppress (Access_Check, Abort_Undefer);
   --  Undefer task abort (task/non-task case as appropriate)

   --  Declarations for the no tasking versions of the required routines

   function Get_Current_Excep_Soft return EOA;
   pragma Inline (Get_Current_Excep_Soft);

   Get_Current_Excep : constant Get_EOA_Call := Get_Current_Excep_Soft'Access;

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data, i.e.
   --  the call is equivalent to:
   --    Exception_Identity (Get_Current_Exception.all)

   procedure Task_Termination_Soft (Except : EO);
   --  Handle task termination routines for the environment task (non-tasking
   --  case, does nothing).

   Task_Termination_Handler : EO_Param_Proc := Task_Termination_Soft'Access;
   --  Handle task termination routines (task/non-task case as appropriate)

   -------------------------------------
   -- Exception Tracebacks Soft-Links --
   -------------------------------------

   Library_Exception : EO;
   --  Library-level finalization routines use this common reference to store
   --  the first library-level exception which occurs during finalization.

   Library_Exception_Set : Boolean := False;
   --  Used in conjunction with Library_Exception, set when an exception has
   --  been stored.

   Traceback_Decorator_Wrapper : Traceback_Decorator_Wrapper_Call;
   --  Wrapper to the possible user specified traceback decorator to be
   --  called during automatic output of exception data.
   --
   --  The null value of this wrapper corresponds to the null value of the
   --  current actual decorator. This is ensured first by the null initial
   --  value of the corresponding variables, and then by Set_Trace_Decorator
   --  in g-exctra.adb.

   pragma Atomic (Traceback_Decorator_Wrapper);
   --  Since concurrent read/write operations may occur on this variable. See
   --  the body of Tailored_Exception_Traceback in Ada.Exceptions for a more
   --  detailed description of the potential problems.

end System.Soft_Links;
