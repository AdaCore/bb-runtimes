------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
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
------------------------------------------------------------------------------

--  This version is for ARM bareboard targets using the ARMv7-M targets,
--  which only use Thumb2 instructions.

with Ada.Unchecked_Conversion; use Ada;

with System.Multiprocessors;
with System.BB.Board_Support;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.Machine_Code; use System.Machine_Code;
with System.BB.CPU_Primitives.Context_Switch_Trigger;

package body System.BB.CPU_Primitives is
   use Board_Support;
   use Board_Support.Time;
   use System.BB.CPU_Primitives.Context_Switch_Trigger;
   use Parameters;
   use Threads.Queues;

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator in Asm templates

   Has_VTOR : constant Boolean := System.BB.Parameters.Has_VTOR;
   --  Set True iff the Vector Table Offset Register (VTOR) can be used
   --  (armv7-m architecture or Cortex-M0+).

   Has_OS_Extensions : constant Boolean :=
     System.BB.Parameters.Has_OS_Extensions;
   --  Set True iff the core implements the armv6-m OS extensions

   Is_ARMv6m : constant Boolean := System.BB.Parameters.Is_ARMv6m;
   --  Set True iff the core implements the armv6-m architecture

   -----------
   -- Traps --
   -----------

   Reset_Vector             : constant Vector_Id :=  1;
   NMI_Vector               : constant Vector_Id :=  2;
   Hard_Fault_Vector        : constant Vector_Id :=  3;
   --  Mem_Manage_Vector    : constant Vector_Id :=  4; --  Never referenced
   Bus_Fault_Vector         : constant Vector_Id :=  5;
   Usage_Fault_Vector       : constant Vector_Id :=  6;
   SV_Call_Vector           : constant Vector_Id := 11;
   --  Debug_Mon_Vector     : constant Vector_Id := 12; --  Never referenced
   Pend_SV_Vector           : constant Vector_Id := 14;
   Sys_Tick_Vector          : constant Vector_Id := 15;
   Interrupt_Request_Vector : constant Vector_Id := 16;

   pragma Assert (Interrupt_Request_Vector = Vector_Id'Last);

   type Trap_Handler_Ptr is access procedure (Id : Vector_Id);
   function To_Pointer is new Unchecked_Conversion (Address, Trap_Handler_Ptr);

   type Trap_Handler_Table is array (Vector_Id) of Trap_Handler_Ptr;
   pragma Suppress_Initialization (Trap_Handler_Table);

   Trap_Handlers : Trap_Handler_Table;
   pragma Export (C, Trap_Handlers, "__gnat_bb_exception_handlers");

   System_Vectors : constant System.Address;
   pragma Import (Asm, System_Vectors, "__vectors");

   --  As ARMv7M does not directly provide a single-shot alarm timer, and
   --  we have to use Sys_Tick for that, we need to have this clock generate
   --  interrupts at a relatively high rate. To avoid unnecessary overhead
   --  when no alarms are requested, we'll only call the alarm handler if
   --  the current time exceeds the Alarm_Time by at most half the modulus
   --  of Timer_Interval.

   Alarm_Time : Board_Support.Time.Timer_Interval;
   pragma Volatile (Alarm_Time);
   pragma Import (C, Alarm_Time, "__gnat_alarm_time");

   procedure SV_Call_Handler;
   pragma Export (Asm, SV_Call_Handler, "__gnat_sv_call_trap");

   procedure Sys_Tick_Handler;
   pragma Export (Asm, Sys_Tick_Handler, "__gnat_sys_tick_trap");

   procedure Interrupt_Request_Handler;
   pragma Export (Asm, Interrupt_Request_Handler, "__gnat_irq_trap");

   procedure GNAT_Error_Handler (Trap : Vector_Id);
   pragma No_Return (GNAT_Error_Handler);

   -----------------------
   -- Context Switching --
   -----------------------

   --  This port uses the ARMv6/7-M hardware for saving volatile context for
   --  interrupts, see the Hardware_Context type below for details. Any
   --  non-volatile registers will be preserved by the interrupt handler in
   --  the same way as it happens for ordinary procedure calls.

   --  The non-volatile registers, as well as the value of the stack pointer
   --  (SP_process) are saved in the Context buffer of the Thread_Descriptor.
   --  Any non-volatile floating-point registers are saved on the stack.

   --  R4 .. R11 are at offset 0 .. 7

   SP_process : constant Context_Id := 8;

   type Hardware_Context is record
      R0, R1, R2, R3   : Word;
      R12, LR, PC, PSR : Word;
   end record;

   VTOR : Address with Volatile, Address => 16#E000_ED08#; -- Vec. Table Offset

   AIRCR : Word with Volatile, Address => 16#E000_ED0C#; -- App Int/Reset Ctrl
   CCR   : Word with Volatile, Address => 16#E000_ED14#; -- Config. Control
   SHPR1 : Word with Volatile, Address => 16#E000_ED18#; -- Sys Hand  4- 7 Prio
   SHPR2 : Word with Volatile, Address => 16#E000_ED1C#; -- Sys Hand  8-11 Prio
   SHPR3 : Word with Volatile, Address => 16#E000_ED20#; -- Sys Hand 12-15 Prio
   SHCSR : Word with Volatile, Address => 16#E000_ED24#; -- Sys Hand Ctrl/State

   function PRIMASK return Word with Inline, Export, Convention => C;
   --  Function returning the contents of the PRIMASK register

   -------------
   -- PRIMASK --
   -------------

   function PRIMASK return Word is
      R : Word;
   begin
      Asm ("mrs %0, PRIMASK", Outputs => Word'Asm_Output ("=r", R),
           Volatile => True);
      return R;
   end PRIMASK;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      Interrupt_Stack_Table : array (System.Multiprocessors.CPU)
        of System.Address;
      pragma Import (Asm, Interrupt_Stack_Table, "interrupt_stack_table");
      --  Table containing a pointer to the top of the stack for each processor

   begin

      if Has_OS_Extensions then
         --  Switch the stack pointer to SP_process (PSP)

         Asm ("mrs r0, MSP" & NL &
                "msr PSP, r0" & NL &
                "mrs r0, CONTROL" & NL &
                "movs r1, #2" & NL &
                "orr r0,r0,r1" & NL &
                "msr CONTROL,r0" & NL &
                "mrs r0, CONTROL",
              Clobber => "r0,r1",
              Volatile => True);

         --  Initialize SP_main (MSP)

         Asm ("msr MSP, %0",
              Inputs => Address'Asm_Input ("r", Interrupt_Stack_Table (1)),
              Volatile => True);
      end if;

      if Has_VTOR then
         --  Initialize vector table
         VTOR := System_Vectors'Address;
      end if;

      --  Set configuration: stack is 8 byte aligned, trap on divide by 0,
      --  no trap on unaligned access, can enter thread mode from any level.

      CCR := CCR or 16#211#;

      --  Set priorities of system handlers. The Pend_SV handler runs at the
      --  lowest priority, so context switching does not block higher priority
      --  interrupt handlers. All other system handlers run at the highest
      --  priority (0), so they will not be interrupted. This is also true for
      --  the SysTick interrupt, as this interrupt must be serviced promptly in
      --  order to avoid losing track of time.

      SHPR1 := 0;
      SHPR2 := 0;
      SHPR3 := 16#00_FF_00_00#;

      if not Is_ARMv6m then

         --  Write the required key (16#05FA#) and desired PRIGROUP value. We
         --  configure this to 3, to have 16 group priorities

         AIRCR := 16#05FA_0300#;
         pragma Assert (AIRCR = 16#FA05_0300#); --  Key value is swapped
      end if;

      --  Enable usage, bus and memory management fault

      SHCSR := SHCSR or 16#7_0000#;

      --  Call context switch hardware initialization
      Initialize_Context_Switch;

      --  Unmask Fault

      Asm ("cpsie f", Volatile => True);

   end Initialize_CPU;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
   begin
      --  Interrupts must be disabled at this point

      pragma Assert (PRIMASK = 1);

      Trigger_Context_Switch;

      --  Memory must be clobbered, as task switching causes a task to signal,
      --  which means its memory changes must be visible to all other tasks.
      Asm ("", Volatile => True, Clobber => "memory");
   end Context_Switch;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Context : Context_Buffer;
      Index   : Context_Id) return Word
   is
      (Word (Context (Index)));

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Vector_Id) is
   begin
      case Trap is
         when Reset_Vector =>
            raise Program_Error with "unexpected reset";
         when NMI_Vector =>
            raise Program_Error with "non-maskable interrupt";
         when Hard_Fault_Vector =>
            raise Program_Error with "hard fault";
         when Bus_Fault_Vector  =>
            raise Program_Error with "bus fault";
         when Usage_Fault_Vector =>
            raise Constraint_Error with "usage fault";
         when others =>
            raise Program_Error with "unhandled trap";
      end case;
   end GNAT_Error_Handler;

   ----------------------------------
   -- Interrupt_Request_Handler -- --
   ----------------------------------

   procedure Interrupt_Request_Handler is
   begin
      --  Call the handler (System.BB.Interrupts.Interrupt_Wrapper)

      Trap_Handlers (Interrupt_Request_Vector)(Interrupt_Request_Vector);

      --  The handler has changed the current priority (BASEPRI), although
      --  being useless on ARMv7m. We need to revert it.

      --  The interrupt handler may have scheduled a new task, so we need to
      --  check whether a context switch is needed.

      if Has_OS_Extensions then
         if Context_Switch_Needed then

            --  Perform a context switch because the currently executing thread
            --  is no longer the one with the highest priority.

            --  No need to update execution time. Already done in the wrapper.

            --  Note that the following context switch is not immediate, but
            --  will only take effect after interrupts are enabled.

            Context_Switch;
         end if;
      else
         --  When OS extensions are not available, the context switch will be
         --  handled in the lower level trap handler:
         --  __gnat_irq_trap_without_os_extensions
         null;
      end if;

      --  Restore interrupt masking of interrupted thread

      Enable_Interrupts (Running_Thread.Active_Priority);
   end Interrupt_Request_Handler;

   ---------------------
   -- SV_Call_Handler --
   ---------------------

   procedure SV_Call_Handler is
   begin
      GNAT_Error_Handler (SV_Call_Vector);
   end SV_Call_Handler;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context : in out Context_Buffer;
      Index   : Context_Id;
      Value   : Word) is
   begin
      Context (Index) := Address (Value);
   end Set_Context;

   ----------------------
   -- Sys_Tick_Handler --
   ----------------------

   procedure Sys_Tick_Handler is
      Max_Alarm_Interval : constant Timer_Interval := Timer_Interval'Last / 2;
      Now : constant Timer_Interval := Timer_Interval (Read_Clock);

   begin
      --  The following allows max. efficiency for "useless" tick interrupts

      if Alarm_Time - Now <= Max_Alarm_Interval then

         --  Alarm is still in the future, nothing to do, so return quickly

         return;
      end if;

      Alarm_Time := Now + Max_Alarm_Interval;

      --  Call the alarm handler

      Trap_Handlers (Sys_Tick_Vector)(Sys_Tick_Vector);

      --  The interrupt handler may have scheduled a new task

      if Context_Switch_Needed then
         Context_Switch;
      end if;

      Enable_Interrupts (Running_Thread.Active_Priority);
   end Sys_Tick_Handler;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      HW_Ctx_Bytes : constant System.Address := Hardware_Context'Size / 8;
      New_SP       : constant System.Address :=
                       (Stack_Pointer - HW_Ctx_Bytes) and not 4;

      HW_Ctx : Hardware_Context with Address => New_SP;

   begin
      --  No need to initialize the context of the environment task

      if Program_Counter = Null_Address then
         return;
      end if;

      HW_Ctx := (R0     => Word (Argument),
                 PC     => Word (Program_Counter),
                 PSR    => 2**24, -- Set thumb bit
                 others => 0);

      Buffer.all := (SP_process => New_SP, others => 0);
   end Initialize_Context;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
      EH : constant Address := GNAT_Error_Handler'Address;
   begin
      Install_Trap_Handler (EH, Reset_Vector);
      Install_Trap_Handler (EH, NMI_Vector);
      Install_Trap_Handler (EH, Hard_Fault_Vector);
      Install_Trap_Handler (EH, Bus_Fault_Vector);
      Install_Trap_Handler (EH, Usage_Fault_Vector);

      if Has_OS_Extensions then
         Install_Trap_Handler (EH, Pend_SV_Vector);
         Install_Trap_Handler (EH, SV_Call_Vector);
      end if;
   end Install_Error_Handlers;

   --------------------------
   -- Install_Trap_Handler --
   --------------------------

   procedure Install_Trap_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id;
      Synchronous     : Boolean := False)
   is
      pragma Unreferenced (Synchronous);
   begin
      Trap_Handlers (Vector) := To_Pointer (Service_Routine);
   end Install_Trap_Handler;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("cpsid i", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : Integer) is

      procedure Clear_PRIMASK_Register;
      --  Wrapper around the Clear PRIMASK register instruction

      ----------------------------
      -- Clear_PRIMASK_Register --
      ----------------------------

      procedure Clear_PRIMASK_Register is
      begin
         --  Enabling interrupts will cause any pending interrupts to take
         --  effect. The instruction barrier is required by the architecture
         --  to ensure subsequent instructions are executed with interrupts
         --  enabled and at the right hardware priority level.

         Asm ("cpsie i" & NL &
              "isb",
              Clobber => "memory", Volatile => True);
      end Clear_PRIMASK_Register;

   begin
      if Is_ARMv6m then
         --  The absence of the BASEPRI register on the ARMv6-M architecture
         --  means only one interrupt priority can be supported on this
         --  architecture. Consequently, interrupts have to remain
         --  disabled while we are at a priority level of Interrupt_Priority,
         --  otherwise it would allow interrupt handlers to run when a task or
         --  another interrupt handler is running at this level; creating
         --  a scenario where a protected object's mutual exclusion may be
         --  violated.

         if Level /= Interrupt_Priority'Last then
            Clear_PRIMASK_Register;
         end if;
      else
         --  Set BASEPRI to mask interrupts below Level and enable interrupts

         Board_Support.Interrupts.Set_Current_Priority (Level);
         Clear_PRIMASK_Register;
      end if;
   end Enable_Interrupts;

end System.BB.CPU_Primitives;
