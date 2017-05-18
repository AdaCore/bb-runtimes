------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                      Copyright (C) 2009-2017, AdaCore                    --
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
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for PikeOS

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

pragma Restrictions (No_Elaboration_Code);

with System.Parameters;
with System.Multiprocessors;

package System.OS_Interface is
   pragma Preelaborate;

   --  PikeOS binding

   --  The definitions are in p4.h

   type P4_uint8_t  is mod 2**8  with Convention => C;
   type P4_uint32_t is mod 2**32 with Convention => C;
   type P4_uint64_t is mod 2**64 with Convention => C;

   subtype P4_uid_t is P4_uint32_t;
   subtype P4_thr_t is P4_uint32_t;
   subtype P4_time_t is P4_uint64_t;
   subtype P4_prio_t is P4_uint32_t;
   subtype P4_timeout_t is P4_uint64_t;
   subtype P4_cpureg_t is P4_uint32_t;
   subtype P4_hm_type_t is P4_uint32_t;  --  An enum
   subtype P4_intid_t is P4_uint32_t;

   type P4_rulock_list_elem_t is record
      Next : Address;
      Prev : Address;
   end record with Convention => C;

   type P4_tls_area_t is record
      Tls_Self        : Address;
      Uid             : P4_uid_t;
      Uprio           : P4_uint8_t;
      Kprio           : P4_uint8_t;
      Cpuid           : P4_uint8_t;
      Padding         : P4_uint8_t;
      Rulock_Trying   : Address;
      Rulock_List     : P4_rulock_list_elem_t;
      Rulock_Count    : P4_uint32_t;
      Error           : P4_uint32_t;
      Preempt_Handler : Address;
      Preempt_Stack   : Address;
      Preempt_Regs    : Address;
      Preempt_Flags   : P4_cpureg_t;
      Sysemu_Handler  : Address;
      Sysemu_Stack    : Address;
      Sysemu_Regs     : Address;
      Sysemu_Flags    : P4_cpureg_t;
      Except_Handler  : Address;
      Except_Stack    : Address;
      Except_Regs     : Address;
      Except_Flags    : P4_cpureg_t;
      Error_Domain    : P4_uint32_t;
      Error_Type      : P4_hm_type_t;
      Error_Id        : P4_uint32_t;
      Error_Code      : P4_uint32_t;
      Error_Msg       : Address;
      Error_Msg_Size  : P4_uint32_t;
   end record with Convention => C;

   --  Safety check (the value 104 is sizeof (P4_tls_area_t)
   pragma Assert (P4_tls_area_t'Size = 104 * 8);

   P4_TIMEOUT_ABSOLUTE : constant P4_timeout_t := 8 * 2 ** 60;
   P4_TIMEOUT_INFINITE : constant P4_timeout_t := 16#0fffffff_ffffffff#;

   subtype P4_e_t is Integer;

   P4_E_OK          : constant P4_e_t      := 0;
   P4_E_STATE       : constant P4_e_t      := 28;
   P4_E_BADTIMEOUT  : constant P4_e_t      := 16;
   P4_TIMEPART_KEEP : constant P4_uint32_t := 16#FFFF_FFFE#;
   P4_PRIO_KEEP     : constant P4_prio_t   := 16#FFFF_FFFE#;

   subtype P4_int_mode_t is Integer;
   P4_INT_MODE_ANY : constant P4_int_mode_t := 0;

   function p4_int_attach_syscall (intid : P4_intid_t; mode : P4_int_mode_t)
                                  return P4_e_t;
   pragma Import (C, p4_int_attach_syscall);

   function p4_int_wait (timeout : P4_timeout_t; flags : P4_uint32_t)
                        return P4_e_t;
   pragma Import (C, p4_int_wait);

   ----------------
   -- Interrupts --
   ----------------

   subtype Interrupt_ID is P4_intid_t;
   --  Interrupt identifiers

   subtype Interrupt_Range is P4_intid_t range 0 .. 15;
   --  For s-interr. The number of interruptions is limited, as the runtime
   --  reserves resources (stack, thread) to handle an interrupt.

   No_Interrupt : constant := 16#Ffff_Ffff#;
   --  Special value indicating no interrupt (corresponds to P4_INT_DETACH)

   type Interrupt_Handler is access procedure (Id : Interrupt_ID);

   --------------------------
   -- Interrupt processing --
   --------------------------

   function Current_Interrupt return Interrupt_ID;
   --  Function that returns the hardware interrupt currently being handled (if
   --  any). In case no hardware interrupt is being handled the returned value
   --  is No_Interrupt.

   procedure Set_Interrupt (Id : Interrupt_ID);
   --  Indicate that the current thread handles interrupt Id. Set once only on
   --  interrupt threads (see s-interr).

   ----------
   -- Time --
   ----------

   subtype Time is P4_time_t;
   --  Representation of the time in the underlying tasking system

   type Time_Span is range -2 ** 63 .. 2 ** 63 - 1;
   for Time_Span'Size use 64;
   --  Represents length of time intervals in the underlying tasking system

   Ticks_Per_Second : constant := 1_000_000_000;
   --  Number of clock ticks per second (PikeOS resolution is 1ns)

   function Clock return Time;
   --  Get the number of ticks elapsed since startup

   procedure Delay_Until (T : Time);
   --  Suspend the calling task until the absolute time specified by T

   -------------
   -- Threads --
   -------------

   type Thread_Descriptor is private;
   --  Type that contains information about a thread (registers, priority, etc)

   type Thread_Id is access all Thread_Descriptor;
   --  Identifiers for the underlying threads

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier for a non valid thread

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority);
   --  Procedure for initializing the underlying tasking system

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type);
   --  Create a new thread

   function Thread_Self return Thread_Id;
   --  Return the thread identifier for the calling task

   function Lwp_Self return System.Address;
   --  Return the LWP for the calling task

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address);
   --  Associate the specified ATCB to the currently running thread

   function Get_ATCB return System.Address;
   --  Get the ATCB associated to the currently running thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority  : System.Any_Priority);
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority;
   --  Get the current base priority of a thread

   procedure Sleep;
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id);
   --  The referred thread becomes ready (the thread must be suspended)

   ---------------------
   -- Multiprocessors --
   ---------------------

   function Current_CPU return Multiprocessors.CPU;
   --  Return the id of the current CPU

   function Get_Affinity (Id : Thread_Id) return Multiprocessors.CPU_Range;
   --  Return CPU affinity of the given thread (maybe Not_A_Specific_CPU)

   function Get_CPU (Id : Thread_Id) return Multiprocessors.CPU;
   --  Return the CPU in charge of the given thread (always a valid CPU)

private
   type Thread_Descriptor is record
      Tls : P4_tls_area_t;
      --  TLS area defined by pikeos-4

      ATCB : System.Address;
      --  Address of the Ada Task Control Block corresponding to the Ada task
      --  that executes on this thread.

      Num : P4_thr_t;
      --  PikeOS thread identifier

      Interrupt : Interrupt_ID;
      --  Interrupt handled by the thread, or No_Interrupt if none.
   end record;
end System.OS_Interface;
