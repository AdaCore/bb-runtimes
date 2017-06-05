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
--                     Copyright (C) 2003-2017, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements aarch64 architecture specific support for the GNAT
--  Ravenscar run time.

with System.Machine_Code;    use System.Machine_Code;
with System.Multiprocessors;
with System.BB.CPU_Specific; use System.BB.CPU_Specific;
with System.BB.Interrupts;
with System.BB.Threads.Queues;
with System.BB.Protection;
with System.BB.Board_Support;
with System.BB.CPU_Primitives.Multiprocessors;
with Interfaces;
with Interfaces.AArch64;     use Interfaces.AArch64;

package body System.BB.CPU_Primitives is
   use System.BB.Threads;
   use Interfaces;

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   type FPU_Context_Table is
     array (System.Multiprocessors.CPU) of FPU_Context_Access;
   pragma Volatile_Components (FPU_Context_Table);

   Default_FPCR : Unsigned_32 := 0;

   Current_FPU_Context :  FPU_Context_Table := (others => null);
   --  This variable contains the last thread that used the floating point unit
   --  for each CPU. Hence, it points to the place where the floating point
   --  state must be stored. Null means no task using it.

   Running_IRQ_Context : FPU_Context_Table := (others => null);

   type Context_Switch_Params is record
      Running_Thread_Address : Address;
      --  Address of the running thread entry for the current cpu

      First_Thread_Address : Address;
      --  Address of the first read thread for the current cpu
   end record;
   pragma Convention (C, Context_Switch_Params);
   pragma Suppress_Initialization (Context_Switch_Params);
   --  This record describe data that are passed from Pre_Context_Switch
   --  to Context_Switch. In the assembly code we take advantage of the ABI
   --  so that the data returned are in the registers of the incoming call.
   --  So there is no need to copy or to move the data between both calls.

   function Pre_Context_Switch return Context_Switch_Params;
   pragma Export (Asm, Pre_Context_Switch, "__gnat_pre_context_switch");
   --  The full context switch is split in 2 stages:
   --  - Pre_Context_Switch: adjust the current priority (but don't modify
   --    the DAIF bits), and return the running and first thread queue
   --    addresses.
   --  - The assembly routine (context_switch) which does the real context
   --    switch.
   --  When called from interrupt handler, the stack pointer is saved before
   --  and restore after the context switch. Therefore the context switch
   --  cannot allocate a frame but only assembly code can guarantee that. We
   --  also take advantage of this two stage call to extract queue pointers
   --  in the Ada code.

   procedure Disable_FPU;
   procedure Enable_FPU;
   --  Disable/enable FPU by changing the FPEN bit of CPACR or the TFP bit of
   --  CPTR.

   function Running_FPU_Context
     (CPU_Id : System.Multiprocessors.CPU) return FPU_Context_Access
     with Inline_Always;
   --  The FPU context of the running thread/interruption

   procedure Fpen_Trap;
   pragma Export (C, Fpen_Trap, "__gnat_fpen_trap");
   --  FPU enable trap handler

   procedure Pre_IRQ_Handling (Ctxt : FPU_Context_Access);
   pragma Export (Asm, Pre_IRQ_Handling, "__gnat_irq_pre_handler");

   procedure Post_IRQ_Handling;
   pragma Export (Asm, Post_IRQ_Handling, "__gnat_irq_post_handler");

   ------------------------
   -- Pre_Context_Switch --
   ------------------------

   function Pre_Context_Switch return Context_Switch_Params is
      use System.BB.Threads.Queues;

      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;
      New_Priority : constant Integer :=
                       First_Thread_Table (CPU_Id).Active_Priority;

   begin
      --  Called with interrupts disabled

      --  Set interrupt priority. Unlike the SPARC implementation, the
      --  interrupt priority is not part of the context (not in a register).
      --  However full interrupt disabling is part of the context switch.

      if New_Priority < Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (New_Priority);
      end if;

      --  Lazy FPU context switch. The FPU context will be saved and restored
      --  only when required, so disable the FPU for the next task.
      --  In case the proper context is already there, do nothing: this
      --  prevents case where only one task is performing FPU operations, and
      --  has to constantly re-enable the FPU

      if Current_FPU_Context (CPU_Id) =
        First_Thread_Table (CPU_Id).Context.FPU'Access
      then
         Enable_FPU;
      else
         Disable_FPU;
      end if;

      --  Parameters for the assembly routine

      return (Running_Thread_Table (CPU_Id)'Address,
              First_Thread_Table (CPU_Id)'Address);
   end Pre_Context_Switch;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is

      procedure Context_Switch_Asm
        (Running_Thread_Table_Element_Address : System.Address;
         Ready_Thread_Table_Element_Address : System.Address);
      pragma Import (Asm, Context_Switch_Asm, "__gnat_context_switch");
      --  Real context switch in assembly code

      Params : Context_Switch_Params;
   begin
      --  First set priority and get pointers

      Params := Pre_Context_Switch;

      --  Then the real context switch

      Context_Switch_Asm (Params.Running_Thread_Address,
                          Params.First_Thread_Address);
   end Context_Switch;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("msr DAIFset, #3", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : Integer) is
   begin
      if Level /= System.Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (Level);

         --  Really enable interrupts
         --  ??? Handle differently I and F

         Asm ("msr DAIFclr, #3", Volatile => True);
      end if;
   end Enable_Interrupts;

   ----------------------
   -- Initialize_Stack --
   ----------------------

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address)
   is
      use System.Storage_Elements;

      Minimum_Stack_Size_In_Bytes : constant Integer_Address :=
                                      CPU_Specific.Stack_Alignment;

      Initial_SP : constant System.Address :=
                     To_Address
                       (To_Integer (Base + Size) -
                          Minimum_Stack_Size_In_Bytes);
   begin
      --  ??? Force alignment
      Stack_Pointer := Initial_SP;
   end Initialize_Stack;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      procedure Start_Thread_Asm;
      pragma Import (Asm, Start_Thread_Asm, "__gnat_start_thread");

      Initial_SP : Address;

   begin
      --  No need to initialize the context of the environment task

      if Program_Counter = Null_Address then
         return;
      end if;

      --  We cheat as we don't know the stack size nor the stack base

      Initialize_Stack (Stack_Pointer, 0, Initial_SP);

      --  Overwrite Stack Pointer and Program Counter with values that have
      --  been passed as arguments.

      Buffer.SP := Unsigned_64 (Initial_SP);
      Buffer.X30 := Unsigned_64 (Start_Thread_Asm'Address);

      Buffer.X19 := Unsigned_64 (Program_Counter);
      Buffer.X20 := Unsigned_64 (Argument);

      --  Mark the FPU context as uninitialized

      Buffer.FPU.V_Init := False;
   end Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU
   is
      CPU_Id : constant System.Multiprocessors.CPU_Range :=
                 Board_Support.Multiprocessors.Current_CPU;
      use type System.Multiprocessors.CPU_Range;
   begin
      if CPU_Id = 1 then
         --  Save the value of the FPCR register at startup
         --  This will be used as initial value for new FPU contexts
         Asm ("mrs %0, fpsr",
              Outputs => Unsigned_32'Asm_Output ("=r", Default_FPCR),
              Volatile => True);
      end if;
      --  Start with FPU disabled
      Disable_FPU;

      Running_IRQ_Context (CPU_Id) := null;
   end Initialize_CPU;

   -----------------
   -- Disable_FPU --
   -----------------

   procedure Disable_FPU is
      V : Unsigned_64;
   begin
      case Parameters.Runtime_EL is
         when 1 =>
            V := Get_CPACR_EL1;
            V := V and not CPACR_FPEN;
            Set_CPACR_EL1 (V);
         when 2 =>
            V := Get_CPTR_EL2;
            V := V or CPTR_TFP;
            Set_CPTR_EL2 (V);
      end case;
   end Disable_FPU;

   ----------------
   -- Enable_FPU --
   ----------------

   procedure Enable_FPU is
      V : Unsigned_64;
   begin
      case Parameters.Runtime_EL is
         when 1 =>
            V := Get_CPACR_EL1;
            V := V or CPACR_FPEN;
            Set_CPACR_EL1 (V);
         when 2 =>
            V := Get_CPTR_EL2;
            V := V and not CPTR_TFP;
            Set_CPTR_EL2 (V);
      end case;
   end Enable_FPU;

   ----------------------
   -- Pre_IRQ_Handling --
   ----------------------

   procedure Pre_IRQ_Handling (Ctxt : FPU_Context_Access)
   is
      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;
   begin
      Ctxt.Interrupted_Context :=
        Queues.Running_Thread_Table (CPU_Id).Context.FPU'Access;
      Running_IRQ_Context (CPU_Id) := Ctxt;
   end Pre_IRQ_Handling;

   -----------------------
   -- Post_IRQ_Handling --
   -----------------------

   procedure Post_IRQ_Handling
   is
      CPU_Id  : constant System.Multiprocessors.CPU :=
                  Board_Support.Multiprocessors.Current_CPU;
      Old_Ctx : constant FPU_Context_Access :=
                  Running_IRQ_Context (CPU_Id).Interrupted_Context;
   begin
      if Current_FPU_Context (CPU_Id) = Running_IRQ_Context (CPU_Id) then
         --  FPU was used during IRQ handling:
         --  Invalidate the context as we're leaving the IRQ handler, so we
         --  won't have use for it, and it will be poped from the stack
         Current_FPU_Context (CPU_Id) := null;
         --  Disable the FPU to provoque a FPU fault when accessed, to save
         --  the context of the last thread that accessed FPU.
         Disable_FPU;
      end if;

      if Old_Ctx.Interrupted_Context = null then
         --  Leaving IRQ handling: no running IRQ context
         Running_IRQ_Context (CPU_Id) := null;
      end if;
   end Post_IRQ_Handling;

   -------------------------
   -- Running_FPU_Context --
   -------------------------

   function Running_FPU_Context
     (CPU_Id : System.Multiprocessors.CPU) return FPU_Context_Access
   is
   begin
      if Running_IRQ_Context (CPU_Id) /= null then
         return Running_IRQ_Context (CPU_Id);
      else
         return Queues.Running_Thread_Table (CPU_Id).Context.FPU'Access;
      end if;
   end Running_FPU_Context;

   ---------------
   -- Fpen_Trap --
   ---------------

   procedure Fpen_Trap
   is
      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;
      From   : constant FPU_Context_Access := Current_FPU_Context (CPU_Id);
      To     : constant FPU_Context_Access := Running_FPU_Context (CPU_Id);

   begin
      --  This procedure will handle FPU registers
      Enable_FPU;

      --  Return now if there is nothing to do
      if From = To then
         return;
      end if;

      --  Save FP context

      if From /= null then
         Asm ("mrs %0, fpsr",
              Outputs => Unsigned_32'Asm_Output ("=r", From.FPSR),
              Volatile => True);
         Asm ("mrs %0, fpcr",
              Outputs => Unsigned_32'Asm_Output ("=r", From.FPCR),
              Volatile => True);
         Asm ("stnp q0, q1, [%0, #0x000]" & ASCII.LF & ASCII.HT &
              "stnp q2, q3, [%0, #0x020]" & ASCII.LF & ASCII.HT &
              "stnp q4, q5, [%0, #0x040]" & ASCII.LF & ASCII.HT &
              "stnp q6, q7, [%0, #0x060]" & ASCII.LF & ASCII.HT &
              "stnp q8, q9, [%0, #0x080]" & ASCII.LF & ASCII.HT &
              "stnp q10, q11, [%0, #0x0a0]" & ASCII.LF & ASCII.HT &
              "stnp q12, q13, [%0, #0x0c0]" & ASCII.LF & ASCII.HT &
              "stnp q14, q15, [%0, #0x0e0]" & ASCII.LF & ASCII.HT &
              "stnp q16, q17, [%0, #0x100]" & ASCII.LF & ASCII.HT &
              "stnp q18, q19, [%0, #0x120]" & ASCII.LF & ASCII.HT &
              "stnp q20, q21, [%0, #0x140]" & ASCII.LF & ASCII.HT &
              "stnp q22, q23, [%0, #0x160]" & ASCII.LF & ASCII.HT &
              "stnp q24, q25, [%0, #0x180]" & ASCII.LF & ASCII.HT &
              "stnp q26, q27, [%0, #0x1a0]" & ASCII.LF & ASCII.HT &
              "stnp q28, q29, [%0, #0x1c0]" & ASCII.LF & ASCII.HT &
              "stnp q30, q31, [%0, #0x1e0]" & ASCII.LF & ASCII.HT,
              Inputs => Address'Asm_Input ("r", From.V'Address),
              Volatile => True);
         --  Mark the FPU context as valid
         From.V_Init := True;
      end if;

      --  Load FP context
      if To.V_Init then
         Asm ("msr fpsr, %0",
              Inputs => Unsigned_32'Asm_Input ("r", To.FPSR),
              Volatile => True);
         Asm ("msr fpcr, %0",
              Inputs => Unsigned_32'Asm_Input ("r", To.FPCR),
              Volatile => True);
         Asm ("ldnp q0, q1, [%0, #0x000]" & ASCII.LF & ASCII.HT &
                "ldnp q2, q3, [%0, #0x020]" & ASCII.LF & ASCII.HT &
                "ldnp q4, q5, [%0, #0x040]" & ASCII.LF & ASCII.HT &
                "ldnp q6, q7, [%0, #0x060]" & ASCII.LF & ASCII.HT &
                "ldnp q8, q9, [%0, #0x080]" & ASCII.LF & ASCII.HT &
                "ldnp q10, q11, [%0, #0x0a0]" & ASCII.LF & ASCII.HT &
                "ldnp q12, q13, [%0, #0x0c0]" & ASCII.LF & ASCII.HT &
                "ldnp q14, q15, [%0, #0x0e0]" & ASCII.LF & ASCII.HT &
                "ldnp q16, q17, [%0, #0x100]" & ASCII.LF & ASCII.HT &
                "ldnp q18, q19, [%0, #0x120]" & ASCII.LF & ASCII.HT &
                "ldnp q20, q21, [%0, #0x140]" & ASCII.LF & ASCII.HT &
                "ldnp q22, q23, [%0, #0x160]" & ASCII.LF & ASCII.HT &
                "ldnp q24, q25, [%0, #0x180]" & ASCII.LF & ASCII.HT &
                "ldnp q26, q27, [%0, #0x1a0]" & ASCII.LF & ASCII.HT &
                "ldnp q28, q29, [%0, #0x1c0]" & ASCII.LF & ASCII.HT &
                "ldnp q30, q31, [%0, #0x1e0]" & ASCII.LF & ASCII.HT,
              Inputs => Address'Asm_Input ("r", To.V'Address),
              Volatile => True);
      else
         --  Initialize the FPU registers with default startup values
         Asm ("msr fpsr, %0",
              Inputs => Unsigned_32'Asm_Input ("r", 0),
              Volatile => True);
         Asm ("msr fpcr, %0",
              Inputs => Unsigned_32'Asm_Input ("r", Default_FPCR),
              Volatile => True);
         Asm ("fmov s0, xzr" & ASCII.LF & ASCII.HT &
              "fmov s1, xzr" & ASCII.LF & ASCII.HT &
              "fmov s2, xzr" & ASCII.LF & ASCII.HT &
              "fmov s3, xzr" & ASCII.LF & ASCII.HT &
              "fmov s4, xzr" & ASCII.LF & ASCII.HT &
              "fmov s5, xzr" & ASCII.LF & ASCII.HT &
              "fmov s6, xzr" & ASCII.LF & ASCII.HT &
              "fmov s7, xzr" & ASCII.LF & ASCII.HT &
              "fmov s8, xzr" & ASCII.LF & ASCII.HT &
              "fmov s9, xzr" & ASCII.LF & ASCII.HT &
              "fmov s10, xzr" & ASCII.LF & ASCII.HT &
              "fmov s11, xzr" & ASCII.LF & ASCII.HT &
              "fmov s12, xzr" & ASCII.LF & ASCII.HT &
              "fmov s13, xzr" & ASCII.LF & ASCII.HT &
              "fmov s14, xzr" & ASCII.LF & ASCII.HT &
              "fmov s15, xzr" & ASCII.LF & ASCII.HT &
              "fmov s16, xzr" & ASCII.LF & ASCII.HT &
              "fmov s17, xzr" & ASCII.LF & ASCII.HT &
              "fmov s18, xzr" & ASCII.LF & ASCII.HT &
              "fmov s19, xzr" & ASCII.LF & ASCII.HT &
              "fmov s20, xzr" & ASCII.LF & ASCII.HT &
              "fmov s21, xzr" & ASCII.LF & ASCII.HT &
              "fmov s22, xzr" & ASCII.LF & ASCII.HT &
              "fmov s23, xzr" & ASCII.LF & ASCII.HT &
              "fmov s24, xzr" & ASCII.LF & ASCII.HT &
              "fmov s25, xzr" & ASCII.LF & ASCII.HT &
              "fmov s26, xzr" & ASCII.LF & ASCII.HT &
              "fmov s27, xzr" & ASCII.LF & ASCII.HT &
              "fmov s28, xzr" & ASCII.LF & ASCII.HT &
              "fmov s29, xzr" & ASCII.LF & ASCII.HT &
              "fmov s30, xzr" & ASCII.LF & ASCII.HT &
              "fmov s31, xzr" & ASCII.LF & ASCII.HT,
              Volatile => True);
      end if;

      Current_FPU_Context (CPU_Id) := To;
   end Fpen_Trap;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      null;
   end Install_Error_Handlers;

end System.BB.CPU_Primitives;
