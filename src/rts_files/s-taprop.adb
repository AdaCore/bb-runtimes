------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

--  This is the generic bare board version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying kernel.

pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion;

with System.Storage_Elements;
with System.Tasking.Debug;
with System.Task_Info;

package body System.Task_Primitives.Operations is

   use System.OS_Interface;
   use System.Parameters;
   use System.Storage_Elements;
   use System.Multiprocessors;

   use type System.Tasking.Task_Id;

   ---------------------
   -- Local Functions --
   ---------------------

   function To_Address is new
     Ada.Unchecked_Conversion (ST.Task_Id, System.Address);

   function To_Task_Id is new
     Ada.Unchecked_Conversion (System.Address, ST.Task_Id);

   procedure Initialize_Idle (CPU_Id : CPU);
   --  Initialize an Idle task for CPU_ID

   procedure Initialize_Slave (CPU_Id : System.Multiprocessors.CPU);
   pragma Export (Asm, Initialize_Slave, "__gnat_initialize_slave");
   --  Initialize a fake environment task for the current CPU. This fake task
   --  is used to give a context during interrupt handling if the CPU doesn't
   --  have a regular task.

   procedure Idle (Param : Address);
   --  Procedure executed by an idle task

   Idle_Stack_Size : constant System.Storage_Elements.Storage_Count :=
     (2048 / Standard'Maximum_Alignment) * Standard'Maximum_Alignment;
   --  2 KB stacks for each of the idle tasks

   type Idle_Stack_Space is
     new Storage_Elements.Storage_Array (1 .. Idle_Stack_Size);
   for Idle_Stack_Space'Alignment use Standard'Maximum_Alignment;
   --  Stack for idle tasks

   Idle_Stacks : array (CPU) of Idle_Stack_Space;
   --  Array that contains the stack space for idle tasks

   Idle_Stacks_Table : array (CPU) of System.Address;
   pragma Export (Asm, Idle_Stacks_Table, "__gnat_idle_stack_table");
   --  Array that contains the stack pointers for idle tasks

   Idle_Tasks : array (Multiprocessors.CPU) of
                   aliased Tasking.Ada_Task_Control_Block (Entry_Num => 0);
   --  ATCB for the idle tasks. They are used to put the cpu in idle mode,
   --  and for slave cpus, they are also present to correctly handle interrupts
   --  (changing the current priority).

   ----------
   -- Self --
   ----------

   function Self return ST.Task_Id is
   begin
      return To_Task_Id (System.OS_Interface.Get_ATCB);
   end Self;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : ST.Task_Id;
      Reason  : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);
   begin
      --  A task can only suspend itself

      pragma Assert (Self_ID = Self);

      System.OS_Interface.Sleep;
   end Sleep;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Abs_Time : Time) is
      Self_ID : constant ST.Task_Id := Self;
   begin
      Self_ID.Common.State := ST.Delay_Sleep;
      System.OS_Interface.Delay_Until (System.OS_Interface.Time (Abs_Time));
      Self_ID.Common.State := ST.Runnable;
   end Delay_Until;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Time is
   begin
      return Time (System.OS_Interface.Clock);
   end Monotonic_Clock;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : ST.Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
   begin
      System.OS_Interface.Wakeup (T.Common.LL.Thread);
   end Wakeup;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (T : ST.Task_Id; Prio : ST.Extended_Priority) is
   begin
      --  A task can only change its own priority

      pragma Assert (T = Self);

      --  Change the priority in the underlying executive

      System.OS_Interface.Set_Priority (Prio);
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : ST.Task_Id) return ST.Extended_Priority is
   begin
      --  Get current active priority

      return System.OS_Interface.Get_Priority (T.Common.LL.Thread);
   end Get_Priority;

   ------------------
   -- Get_Affinity --
   ------------------

   function Get_Affinity
     (T : ST.Task_Id) return System.Multiprocessors.CPU_Range
   is
   begin
      return System.OS_Interface.Get_Affinity (T.Common.LL.Thread);
   end Get_Affinity;

   -------------
   -- Get_CPU --
   -------------

   function Get_CPU (T : ST.Task_Id) return System.Multiprocessors.CPU is
   begin

      return System.OS_Interface.Get_CPU (T.Common.LL.Thread);
   end Get_CPU;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : ST.Task_Id) is
   begin
      --  Set lwp (for gdb)

      Self_ID.Common.LL.Lwp := Lwp_Self;

      --  Register the task to System.Tasking.Debug

      System.Tasking.Debug.Add_Task_Id (Self_ID);

      --  Ensure that the task has the right priority priority at the end
      --  of its initialization (before calling the task's code). This will
      --  reschedule if needed.

      System.OS_Interface.Set_Priority (Self_ID.Common.Base_Priority);
   end Enter_Task;

   ----------
   -- Idle --
   ----------

   procedure Idle (Param : Address)
   is
      pragma Unreferenced (Param);
      T : constant Tasking.Task_Id := Self;
   begin
      Enter_Task (T);

      loop
         OS_Interface.Power_Down;
      end loop;
   end Idle;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : ST.Task_Id; Succeeded : out Boolean) is
      pragma Unreferenced (Self_ID);
   begin
      --  Nothing to be done as part of the initialization of TCBs

      Succeeded := True;
   end Initialize_TCB;

   ----------------------
   -- Initialize_Slave --
   ----------------------

   procedure Initialize_Slave (CPU_Id : CPU) is
      Idle_Task : Tasking.Ada_Task_Control_Block renames Idle_Tasks (CPU_Id);

      Success  : Boolean;
      pragma Warnings (Off, Success);

   begin
      --  Initialize ATCB for the idle task

      Initialize_Idle (CPU_Id);

      --  Initialize the environment thread

      System.OS_Interface.Initialize_Slave
        (Idle_Task.Common.LL.Thread, Idle_Task.Common.Base_Priority,
         Idle_Task.Common.Compiler_Data.Pri_Stack_Info.Start_Address,
         Idle_Task.Common.Compiler_Data.Pri_Stack_Info.Size);

      --  Link the underlying executive thread to the Ada task

      System.OS_Interface.Set_ATCB
        (Idle_Task.Common.LL.Thread, To_Address (Idle_Task'Access));

      --  Run the idle procedure

      Idle (Null_Address);
   end Initialize_Slave;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : ST.Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : ST.Extended_Priority;
      Base_CPU   : System.Multiprocessors.CPU_Range;
      Succeeded  : out Boolean)
   is
   begin
      --  The stack has been preallocated for these targets

      pragma Assert
        (T.Common.Compiler_Data.Pri_Stack_Info.Start_Address /= Null_Address
         and then Storage_Offset (Stack_Size) =
           T.Common.Compiler_Data.Pri_Stack_Info.Size);

      T.Common.LL.Thread := T.Common.LL.Thread_Desc'Access;

      --  Create the underlying task

      System.OS_Interface.Thread_Create
        (T.Common.LL.Thread,
         Wrapper,
         To_Address (T),
         Priority,
         Base_CPU,
         T.Common.Compiler_Data.Pri_Stack_Info.Start_Address,
         T.Common.Compiler_Data.Pri_Stack_Info.Size);

      --  Link the underlying executive thread to the Ada task

      System.OS_Interface.Set_ATCB (T.Common.LL.Thread, To_Address (T));

      Succeeded := True;
   end Create_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : ST.Task_Id) is
      T : Thread_Id renames Environment_Task.Common.LL.Thread;
   begin
      --  Set the thread

      T := Environment_Task.Common.LL.Thread_Desc'Access;

      --  Clear Activation_Link, as required by Add_Task_Id

      Environment_Task.Common.Activation_Link := null;

      --  First the underlying multitasking executive must be initialized.
      --  The ATCB is already initialized and task priority is set.

      System.OS_Interface.Initialize
        (T, Environment_Task.Common.Base_Priority);

      --  Link the underlying executive thread to the Ada task

      System.OS_Interface.Set_ATCB (T, To_Address (Environment_Task));

      --  The environment task must also execute its initialization

      Enter_Task (Environment_Task);

      --  Store the identifier for the environment task

      Operations.Environment_Task := Environment_Task;

      --  Compute the stack pointers of idle tasks

      for CPU_Id in CPU loop
         Idle_Stacks_Table (CPU_Id) :=
           (if System.Parameters.Stack_Grows_Down
            then (Idle_Stacks (CPU_Id)'Address + Idle_Stack_Size)
            else Idle_Stacks (CPU_Id)'Address);
      end loop;

      --  Create the idle task for the main cpu

      declare
         Idle_Task : Tasking.Ada_Task_Control_Block renames
                        Idle_Tasks (CPU'First);
         Success : Boolean;
         pragma Unreferenced (Success);

      begin
         Initialize_Idle (CPU'First);

         Create_Task
           (Idle_Task'Access, Idle'Address,
            Parameters.Size_Type
              (Idle_Task.Common.Compiler_Data.Pri_Stack_Info.Size),
            Tasking.Idle_Priority, CPU'First, Success);
      end;
   end Initialize;

   ---------------------
   -- Initialize_Idle --
   ---------------------

   procedure Initialize_Idle (CPU_Id : CPU) is
      Success  : Boolean;
      pragma Warnings (Off, Success);

      Idle_Task : Tasking.Ada_Task_Control_Block renames Idle_Tasks (CPU_Id);
   begin
      --  Initialize a fake environment task for this slave CPU

      Tasking.Initialize_ATCB
        (Idle'Access, Null_Address, Tasking.Idle_Priority, CPU_Id,
         Task_Info.Unspecified_Task_Info,
         Idle_Stacks (CPU_Id)'Address,
         Parameters.Size_Type (Idle_Stack_Size),
         Parameters.Unspecified_Size,
         Idle_Task'Access, Success);

      Idle_Task.Common.LL.Thread := Idle_Task.Common.LL.Thread_Desc'Access;
      Idle_Task.Entry_Call.Self := Idle_Task'Access;
      Idle_Task.Common.State := Tasking.Runnable;
   end Initialize_Idle;

   ---------------------
   -- Is_Task_Context --
   ---------------------

   function Is_Task_Context return Boolean is
   begin
      return System.OS_Interface.Current_Interrupt = No_Interrupt;
   end Is_Task_Context;

end System.Task_Primitives.Operations;
