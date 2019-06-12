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
with System.BB.Threads.Queues;
with System.BB.Board_Support;
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
   --  FPCR used at start of a thread, extracted from the environmental thread

   Current_FPU_Context :  FPU_Context_Table := (others => null);
   --  This variable contains the last thread that used the floating point unit
   --  for each CPU. Hence, it points to the place where the floating point
   --  state must be stored. Null means no task using it.

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

   procedure Fpen_Trap;
   pragma Export (C, Fpen_Trap, "__gnat_fpen_trap");
   --  FPU enable trap handler

   function IRQ_Pre_Handler
     (Ctxt : FPU_Context_Access) return FPU_Context_Access;
   pragma Export (Asm, IRQ_Pre_Handler, "__gnat_irq_pre_handler");
   --  Sets the IRQ context as running context and returns the previously
   --  running context

   procedure IRQ_Post_Handler
     (Ctxt      : FPU_Context_Access;
      Prev_Ctxt : FPU_Context_Access);
   pragma Export (Asm, IRQ_Post_Handler, "__gnat_irq_post_handler");

   ------------------------
   -- Pre_Context_Switch --
   ------------------------

   function Pre_Context_Switch return Context_Switch_Params
   is
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
      --  only when required, so we:
      --  * disable the FPU for the next task if some other task ever accessed
      --    the FPU (or no task ever used the FPU)
      --  * unless this task is the task we're switching to

      if Current_FPU_Context (CPU_Id) /=
        First_Thread_Table (CPU_Id).Context.Running
      then
         Disable_FPU;
      else
         Enable_FPU;
      end if;

      return (Running_Thread_Table (CPU_Id)'Address,
              First_Thread_Table (CPU_Id)'Address);
   end Pre_Context_Switch;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch
   is
      procedure Context_Switch_Asm
        (Running_Thread_Table_Element_Address : System.Address;
         Ready_Thread_Table_Element_Address : System.Address);
      pragma Import (Asm, Context_Switch_Asm, "__gnat_context_switch");
      --  Real context switch in assembly code

      Params : constant Context_Switch_Params := Pre_Context_Switch;
   begin

      --  Then the real context switch

      Context_Switch_Asm
        (Params.Running_Thread_Address,
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
      --  Set the task's running FPU context to self
      Buffer.Running := Buffer.FPU'Access;
      --  Mark the FPU context as uninitialized
      Buffer.FPU.V_Init := False;

      if Program_Counter = Null_Address then
         --  no stack init for the environment task, nor SP/LR/PC/ARG init
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
   end Initialize_CPU;

   -----------------
   -- Disable_FPU --
   -----------------

   procedure Disable_FPU is
      V : Unsigned_64;
   begin
      V := Get_CPACR_EL1;
      V := V and not CPACR_FPEN;
      Set_CPACR_EL1 (V);
   end Disable_FPU;

   ----------------
   -- Enable_FPU --
   ----------------

   procedure Enable_FPU is
      V : Unsigned_64;
   begin
      V := Get_CPACR_EL1;
      V := V or CPACR_FPEN;
      Set_CPACR_EL1 (V);
   end Enable_FPU;

   ---------------------
   -- IRQ_Pre_Handler --
   ---------------------

   function IRQ_Pre_Handler
     (Ctxt : FPU_Context_Access) return FPU_Context_Access
   is
      use System.BB.Threads.Queues;
      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;
      Old    : constant FPU_Context_Access :=
                 Running_Thread_Table (CPU_Id).Context.Running;

   begin
      --  FPU context is not anymore valid
      Disable_FPU;

      --  And the new one (for the interrupt) is not initialized
      Ctxt.V_Init := False;

      --  The FPU is for the interrupt handler
      Running_Thread_Table (CPU_Id).Context.Running := Ctxt;

      return Old;
   end IRQ_Pre_Handler;

   ----------------------
   -- IRQ_Post_Handler --
   ----------------------

   procedure IRQ_Post_Handler
     (Ctxt      : FPU_Context_Access;
      Prev_Ctxt : FPU_Context_Access)
   is
      use System.BB.Threads.Queues;
      CPU_Id  : constant System.Multiprocessors.CPU :=
                  Board_Support.Multiprocessors.Current_CPU;
   begin
      if Current_FPU_Context (CPU_Id) = Ctxt then
         --  FPU was used during IRQ handling:
         --  Invalidate the context as we're leaving the IRQ handler, so we
         --  won't have use for it, and it will be poped from the stack
         Current_FPU_Context (CPU_Id) := null;

         --  Disable the FPU to provoque a FPU fault when accessed, to save
         --  the context of the last thread that accessed FPU.
         Disable_FPU;

      elsif Current_FPU_Context (CPU_Id) = Prev_Ctxt then
         --  The FPU contains the context for the running thread, so FPU
         --  context is valid.
         Enable_FPU;
      end if;

      --  Leaving IRQ handling: restore the running FPU context
      Running_Thread_Table (CPU_Id).Context.Running := Prev_Ctxt;
   end IRQ_Post_Handler;

   ---------------
   -- Fpen_Trap --
   ---------------

   procedure Fpen_Trap
   is
      use System.BB.Threads.Queues;
      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;
      From   : constant FPU_Context_Access := Current_FPU_Context (CPU_Id);
      To     : constant FPU_Context_Access :=
                 Running_Thread_Table (CPU_Id).Context.Running;

   begin
      --  This procedure will handle FPU registers
      Enable_FPU;

      --  Return now if there is nothing to do
      if From = To then
         return;
      end if;

      --  Save FP context

      if From /= null then
         --  The current FPU context may belong to no thread (for example if
         --  an interrupt handler has used the FPU). In that case, there is no
         --  need to save it.
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
         Asm ("eor v0.16B, v0.16B, v0.16B" & ASCII.LF & ASCII.HT &
              "eor v1.16B, v1.16B, v1.16B" & ASCII.LF & ASCII.HT &
              "eor v2.16B, v2.16B, v2.16B" & ASCII.LF & ASCII.HT &
              "eor v3.16B, v3.16B, v3.16B" & ASCII.LF & ASCII.HT &
              "eor v4.16B, v4.16B, v4.16B" & ASCII.LF & ASCII.HT &
              "eor v5.16B, v5.16B, v5.16B" & ASCII.LF & ASCII.HT &
              "eor v6.16B, v6.16B, v6.16B" & ASCII.LF & ASCII.HT &
              "eor v7.16B, v7.16B, v7.16B" & ASCII.LF & ASCII.HT &
              "eor v8.16B, v8.16B, v8.16B" & ASCII.LF & ASCII.HT &
              "eor v9.16B, v9.16B, v9.16B" & ASCII.LF & ASCII.HT &
              "eor v10.16B, v10.16B, v10.16B" & ASCII.LF & ASCII.HT &
              "eor v11.16B, v11.16B, v11.16B" & ASCII.LF & ASCII.HT &
              "eor v12.16B, v12.16B, v12.16B" & ASCII.LF & ASCII.HT &
              "eor v13.16B, v13.16B, v13.16B" & ASCII.LF & ASCII.HT &
              "eor v14.16B, v14.16B, v14.16B" & ASCII.LF & ASCII.HT &
              "eor v15.16B, v15.16B, v15.16B" & ASCII.LF & ASCII.HT &
              "eor v16.16B, v16.16B, v16.16B" & ASCII.LF & ASCII.HT &
              "eor v17.16B, v17.16B, v17.16B" & ASCII.LF & ASCII.HT &
              "eor v18.16B, v18.16B, v18.16B" & ASCII.LF & ASCII.HT &
              "eor v19.16B, v19.16B, v19.16B" & ASCII.LF & ASCII.HT &
              "eor v20.16B, v20.16B, v20.16B" & ASCII.LF & ASCII.HT &
              "eor v21.16B, v21.16B, v21.16B" & ASCII.LF & ASCII.HT &
              "eor v22.16B, v22.16B, v22.16B" & ASCII.LF & ASCII.HT &
              "eor v23.16B, v23.16B, v23.16B" & ASCII.LF & ASCII.HT &
              "eor v24.16B, v24.16B, v24.16B" & ASCII.LF & ASCII.HT &
              "eor v25.16B, v25.16B, v25.16B" & ASCII.LF & ASCII.HT &
              "eor v26.16B, v26.16B, v26.16B" & ASCII.LF & ASCII.HT &
              "eor v27.16B, v27.16B, v27.16B" & ASCII.LF & ASCII.HT &
              "eor v28.16B, v28.16B, v28.16B" & ASCII.LF & ASCII.HT &
              "eor v29.16B, v29.16B, v29.16B" & ASCII.LF & ASCII.HT &
              "eor v30.16B, v30.16B, v30.16B" & ASCII.LF & ASCII.HT &
              "eor v31.16B, v31.16B, v31.16B",
              Volatile => True);
      end if;

      --  Update pointer to current FPU context
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
