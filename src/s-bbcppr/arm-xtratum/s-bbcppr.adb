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
--                     Copyright (C) 2003-2015, AdaCore                     --
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

--  This version is for xtratum on tms570A. As Xtratum doesn't provide
--  access to fpexc, it is not possible to do lazy save/restore of the fpu.

with Ada.Unchecked_Conversion; use Ada;

with System.Storage_Elements;
with System.Multiprocessors;
with System.BB.Threads;
with System.BB.CPU_Primitives.Multiprocessors;
with System.BB.Threads.Queues;
with System.BB.Board_Support;
with System.BB.Protection;
with System.BB.Interrupts;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.CPU_Primitives is
   use BB.Parameters;
   use System.BB.Threads;
   use System.BB.CPU_Primitives.Multiprocessors;
   use System.Multiprocessors;

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator in Asm templates

   -----------
   -- Traps --
   -----------

   Reset_Vector                  : constant Vector_Id := 0; -- RESET
   Undefined_Instruction_Vector  : constant Vector_Id := 1; -- UNDEF
   Supervisor_Call_Vector        : constant Vector_Id := 2; -- SVC
   Prefetch_Abort_Vector         : constant Vector_Id := 3; -- PABT
   Data_Abort_Vector             : constant Vector_Id := 4; -- DABT
--   Interrupt_Request_Vector      : constant Vector_Id := 5; -- IRQ
   Fast_Interrupt_Request_Vector : constant Vector_Id := 6; -- FIQ

   type Trap_Handler_Ptr is access procedure (Id : Vector_Id);
   function To_Pointer is new Unchecked_Conversion (Address, Trap_Handler_Ptr);

   type Trap_Handler_Table is array (Vector_Id) of Trap_Handler_Ptr;
   pragma Suppress_Initialization (Trap_Handler_Table);

   Trap_Handlers  : Trap_Handler_Table;
   pragma Export (C, Trap_Handlers, "__gnat_trap_handlers");

   CPSR_I : constant := 2 ** 7;
   --  Interrupt disable bit

   procedure GNAT_Error_Handler (Trap : Vector_Id);
   pragma No_Return (GNAT_Error_Handler);

   procedure Undef_Handler;
   pragma Machine_Attribute (Undef_Handler, "interrupt");
   pragma Export (Asm, Undef_Handler, "__gnat_undef_trap");

   procedure Dabt_Handler;
   pragma Machine_Attribute (Dabt_Handler, "interrupt");
   pragma Export (Asm, Dabt_Handler, "__gnat_dabt_trap");

   procedure Pabt_Handler;
   pragma Machine_Attribute (Dabt_Handler, "interrupt");
   pragma Export (Asm, Pabt_Handler, "__gnat_pabt_trap");

   procedure SVC_Handler;
   pragma Machine_Attribute (SVC_Handler, "interrupt");
   pragma Export (Asm, SVC_Handler, "__gnat_svc_trap");

   procedure FIQ_Handler;
   pragma Machine_Attribute (FIQ_Handler, "interrupt");
   pragma Export (Asm, FIQ_Handler, "__gnat_fiq_trap");

   ---------------------------
   -- Context Buffer Layout --
   ---------------------------

   --  These are the registers that are initialized: program counter, two
   --  argument registers, program counter, processor state register,
   --  stack pointer and link register.

   R4 : constant Context_Id := 0;
   R5 : constant Context_Id := 1;
   SP : constant Context_Id := 8; -- stack pointer, aka R13
   LR : constant Context_Id := 9; -- link register, R14

   procedure FPU_Context_Switch (To : Thread_Id) with Inline, Unreferenced;
   function Get_CPSR return Word with Inline;
   procedure Set_CPSR (CPSR : Word) with Inline;

   --------------
   -- Get_CPSR --
   --------------

   function Get_CPSR return Word is
      procedure Xm_Arm_Get_Cpsr (Addr : Address);
      pragma Import (C, Xm_Arm_Get_Cpsr, "XM_arm_get_cpsr");

      CPSR : Word;
   begin
      Xm_Arm_Get_Cpsr (CPSR'Address);
      return CPSR;
   end Get_CPSR;

   --------------
   -- Set_CPSR --
   --------------

   procedure Set_CPSR (CPSR : Word) is
      procedure Xm_Arm_Set_Cpsr (CPSR : Word);
      pragma Import (C, Xm_Arm_Set_Cpsr, "XM_arm_set_cpsr");
   begin
      Xm_Arm_Set_Cpsr (CPSR);
   end Set_CPSR;

   ------------------
   -- Dabt_Handler --
   ------------------

   procedure Dabt_Handler is
   begin
      Trap_Handlers (Data_Abort_Vector) (Data_Abort_Vector);
   end Dabt_Handler;

   ------------------
   -- Pabt_Handler --
   ------------------

   procedure Pabt_Handler is
   begin
      Trap_Handlers (Prefetch_Abort_Vector) (Prefetch_Abort_Vector);
   end Pabt_Handler;

   -------------------
   -- Undef_Handler --
   -------------------

   procedure Undef_Handler is
   begin
      Trap_Handlers (Undefined_Instruction_Vector)
        (Undefined_Instruction_Vector);
   end Undef_Handler;

   -----------------
   -- SVC_Handler --
   -----------------

   procedure SVC_Handler is
   begin
      Trap_Handlers (Supervisor_Call_Vector)(Supervisor_Call_Vector);
   end SVC_Handler;

   -----------------
   -- FIQ_Handler --
   -----------------

   procedure FIQ_Handler is
   begin
      Trap_Handlers (Fast_Interrupt_Request_Vector)
        (Fast_Interrupt_Request_Vector);
   end FIQ_Handler;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
      procedure Asm_Context_Switch;
      pragma Import (Asm, Asm_Context_Switch, "__gnat_context_switch");
   begin
      --  When calling this routine from modes other than user or system,
      --  the caller is responsible for saving the (banked) SPSR register.
      --  This register is only visible in banked modes, so can't be saved
      --  here.

      Asm_Context_Switch;
   end Context_Switch;

   ------------------------
   -- FPU_Context_Switch --
   ------------------------

   procedure FPU_Context_Switch (To : Thread_Id) is
      C : constant Thread_Id := To;

      Fpscr : Word;

      type Fpu_Context is array (0 .. 31) of Word;
      Fpu : Fpu_Context;
   begin
      if C /= null then
         Asm (Template => "vstm %1, {d0-d15}" & NL & "fmrx %0, fpscr",
              Outputs  => Word'Asm_Output ("=r", Fpscr),
              Inputs   => Address'Asm_Input ("r", Fpu'Address),
              Clobber  => "memory",
              Volatile => True);
      end if;

      if To /= null then
         Asm (Template => "vldm %1, {d0-d15}" & NL & "fmxr fpscr, %0",
              Inputs   =>
                (Word'Asm_Input ("r", Fpscr),
                 Address'Asm_Input ("r", Fpu'Address)),
              Clobber  => "memory",
              Volatile => True);
      end if;
   end FPU_Context_Switch;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Context : Context_Buffer;
      Index   : Context_Id) return Word
   is
   begin
      return Word (Context (Index));
   end Get_Context;

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Vector_Id) is
   begin
      case Trap is
         when Reset_Vector =>
            raise Program_Error with "unexpected reset";
         when Undefined_Instruction_Vector =>
            raise Program_Error with "illegal instruction";
         when Supervisor_Call_Vector =>
            raise Program_Error with "unhandled SVC";
         when Prefetch_Abort_Vector =>
            raise Program_Error with "prefetch abort";
         when Data_Abort_Vector =>
            raise Constraint_Error with "data abort";
         when others =>
            raise Program_Error with "unhandled trap";
      end case;
   end GNAT_Error_Handler;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context : in out Context_Buffer;
      Index   : Context_Id;
      Value   : Word)
   is
   begin
      Context (Index) := Address (Value);
   end Set_Context;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      procedure Start_Thread;
      pragma Import (Asm, Start_Thread, "__gnat_start_thread");
   begin
      Buffer.all :=
        (R4     => Argument,
         R5     => Program_Counter,
         SP     => Stack_Pointer,
         LR     => Start_Thread'Address,
         others => 0);
   end Initialize_Context;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
      EH : constant Address := GNAT_Error_Handler'Address;

   begin
      Install_Trap_Handler (EH, Reset_Vector);
      Install_Trap_Handler (EH, Undefined_Instruction_Vector, True);
      Install_Trap_Handler (EH, Supervisor_Call_Vector, True);
      Install_Trap_Handler (EH, Prefetch_Abort_Vector, True);
      Install_Trap_Handler (EH, Data_Abort_Vector);

      --  Do not install a handler for the Interrupt_Request_Vector, as
      --  the Ravenscar run time will handle that one, and may already
      --  have installed its handler before calling Install_Error_Handlers.

      Install_Trap_Handler (EH, Fast_Interrupt_Request_Vector);
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
      Set_CPSR (Get_CPSR or CPSR_I);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : System.Any_Priority) is
   begin
      Board_Support.Set_Current_Priority (Level);

      if Level in System.Priority'Range then
         Set_CPSR (Get_CPSR and not CPSR_I);
      end if;
   end Enable_Interrupts;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
   begin
      null;
   end Initialize_CPU;
end System.BB.CPU_Primitives;
