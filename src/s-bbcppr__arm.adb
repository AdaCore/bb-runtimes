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

--  This version is for ARM bareboard targets using the ARMv7-R or ARMv7-A
--  instruction set. It is not suitable for ARMv7-M targets, which use
--  Thumb2.

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with System.Multiprocessors;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.Board_Support;
with System.BB.Parameters;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.CPU_Primitives is
   use System.BB.Threads;
   use System.BB.Board_Support.Multiprocessors;
   use System.Multiprocessors;
   use System.BB.CPU_Specific;

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator in Asm templates

   -----------
   -- Traps --
   -----------

   procedure Undef_Handler;
   pragma Machine_Attribute (Undef_Handler, "interrupt");
   pragma Export (Asm, Undef_Handler, "__gnat_undef_trap");

   procedure Dabt_Handler;
   pragma Machine_Attribute (Dabt_Handler, "interrupt");
   pragma Export (Asm, Dabt_Handler, "__gnat_dabt_trap");

   procedure FIQ_Handler;
   pragma Machine_Attribute (FIQ_Handler, "interrupt");
   pragma Export (Asm, FIQ_Handler, "__gnat_fiq_trap");

   procedure IRQ_Handler;
   pragma Machine_Attribute (IRQ_Handler, "interrupt");
   pragma Export (Asm, IRQ_Handler, "__gnat_irq_trap");

   procedure Irq_User_Handler;
   pragma Import (Ada, Irq_User_Handler, "__gnat_irq_handler");

   procedure Fiq_User_Handler;
   pragma Import (Ada, Fiq_User_Handler, "__gnat_fiq_handler");

   ----------------------------
   -- Floating Point Context --
   ----------------------------

   --  This port uses lazy context switching for the FPU context. Rather than
   --  saving and restoring floating point registers on a context switch or
   --  interrupt, the FPU is disabled unless the switch is to a thread that is
   --  equal the Current_FPU_Context. This is on the expectation that the new
   --  context will not use floating point during its execution window. If it
   --  does, then an undefined instruction trap will be executed that performs
   --  the context switch and retries. We also don't restore the FPU enabled
   --  state when leaving an interrupt handler that didn't use the FPU as we
   --  rather incur the trap at the user level than leaving interrupt masked
   --  longer than absolutely necessary.

   type Thread_Table is array (System.Multiprocessors.CPU) of Thread_Id;
   pragma Volatile_Components (Thread_Table);

   function  Is_FPU_Enabled return Boolean with Inline;
   procedure Set_FPU_Enabled (Enabled : Boolean) with Inline;
   procedure FPU_Context_Switch (To : Thread_Id) with Inline;
   function Get_SPSR return Unsigned_32 with Inline;

   Current_FPU_Context :  Thread_Table := (others => Null_Thread_Id);
   --  This variable contains the last thread that used the floating point unit
   --  for each CPU. Hence, it points to the place where the floating point
   --  state must be stored. Null means no task using it.

   --------------
   -- Get_SPSR --
   --------------

   function Get_SPSR return Unsigned_32 is
      SPSR : Unsigned_32;
   begin
      Asm ("mrs %0, SPSR",
           Outputs  => Unsigned_32'Asm_Output ("=r", SPSR),
           Volatile => True);
      return SPSR;
   end Get_SPSR;

   ------------------
   -- Dabt_Handler --
   ------------------

   procedure Dabt_Handler is
   begin
      raise Constraint_Error with "data abort";
   end Dabt_Handler;

   -----------------
   -- FIQ_Handler --
   -----------------

   procedure FIQ_Handler is
   begin
      --  Force trap if handler uses floating point

      Set_FPU_Enabled (False);

      Fiq_User_Handler;
   end FIQ_Handler;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler is
      SPSR : Unsigned_32;

   begin
      --  Force trap if handler uses floating point

      Set_FPU_Enabled (False);

      --  If we are going to do context switches or otherwise allow IRQ's
      --  from within the interrupt handler, the SPSR register needs to
      --  be saved too.

      SPSR := Get_SPSR;

      --  Call the handler

      Irq_User_Handler;

      --  As the System.BB.Interrupts.Interrupt_Wrapper returns to the low
      --  level interrupt handler without checking for required context
      --  switches, we need to do that here.

      if Threads.Queues.Context_Switch_Needed then

         --  The interrupt handler caused pre-emption of the thread that
         --  was executing. This means we need to switch context. We do not
         --  explicitly enable IRQ's at this point, as that will done by the
         --  CPSR update as part of the context switch.

         --  Note that the part of the thread state is still on the interrupt
         --  stack, and will be restored when the pre-empted thread continues.

         Context_Switch;

         --  The pre-empted thread can now resume
      end if;

      Asm ("msr   SPSR_cxsf, %0",
         Inputs   => (Unsigned_32'Asm_Input ("r", SPSR)),
         Volatile => True);
   end IRQ_Handler;

   -------------------
   -- Undef_Handler --
   -------------------

   procedure Undef_Handler is
   begin
      if not Is_FPU_Enabled then
         --  If FPU is not enabled, do an FPU context switch first and resume.
         --  If the fault is not due to the FPU, it will trigger again.

         declare
            SPSR          : constant Unsigned_32 := Get_SPSR;
            In_IRQ_Or_FIQ : constant Boolean := (SPSR mod 32) in 17 | 18;
         begin
            Set_FPU_Enabled (True);
            FPU_Context_Switch
              (if In_IRQ_Or_FIQ then null
              else Queues.Running_Thread_Table (Current_CPU));
         end;

      else
         raise Program_Error with "illegal instruction";
      end if;
   end Undef_Handler;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
      use System.BB.Threads.Queues;

      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;

      New_Priority : constant Integer :=
                       First_Thread_Table (CPU_Id).Active_Priority;
   begin
      --  Whenever switching to a new context, disable the FPU, so we don't
      --  have to worry about its state. It is much more efficient to lazily
      --  switch the FPU when it is actually used.

      --  When calling this routine from modes other than user or system,
      --  the caller is responsible for saving the (banked) SPSR register.
      --  This register is only visible in banked modes, so can't be saved
      --  here.

      Set_FPU_Enabled (False);

      --  Called with interrupts disabled

      --  Set interrupt priority. Unlike the SPARC implementation, the
      --  interrupt priority is not part of the context (not in a register).
      --  However full interrupt disabling is part of the context switch.

      if New_Priority < Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (New_Priority);
      end if;

      --  Some notes about the Asm insert:

      --    * While we only need to save callee-save registers in principle,
      --      GCC may use caller-save variables, so if we don't save them
      --      they must be marked clobbered.

      --    * Changing SPSR is far cheaper than changing CPSR, so switching
      --      to supervisor mode is beneficial.

      --    * Mark LR as clobbered, so the compiler won't use this register
      --      for any input arguments, as it is banked in supervisor mode

      --    * The user-mode LR register must also be preserved in the context,
      --      as the shadowing of LR will not help in case of pre-emption.

      --    * Memory must be clobbered, as task switching causes a task to
      --      signal, which means its memory changes must be visible to all
      --      other tasks.

      --    * We need three registers with fixed (known) offsets for the
      --      Program_Counter, Program_Status and Stack_Pointer, and we need
      --      to leave at least some registers for GCC to pass us arguments
      --      and for its own use, so we save 6 registers and mark the rest
      --      clobbered.

      --    * While we could mark R0 and R1 as clobbered, and not save them
      --      across the context switch, this does not help. The registers are
      --      used and must be saved somehow. Also, this would mean we need an
      --      extra routine for starting a thread, so we can pass in the
      --      argument.

      --    * Note that the first register to save should be even for most
      --      efficient save/restore.

      --    * Don't forget that stm/ldm works on *user* registers when
      --      executed in PL1 other than system (like supervisor). So the
      --      user/system stack pointer is saved and restored with these
      --      instructions.

      --    * This routine may be inlined, therefore it is very important
      --      that the Asm constraints are correct.

      Asm
        (Template =>
           "mrs   r3, CPSR"      & NL  -- Save CPSR
         & "ldr   r4, [%0]"      & NL  -- Load Running_Thread
         & "cps   #19"           & NL  -- Switch to supervisor mode
         & "adr   r2, 0f"        & NL  -- Adjust R0 to point past ctx switch
         & "stm   r4, {r0-r3,sp,lr}^"  & NL  -- Save user registers
         & "str   %1, [%0]"      & NL  -- Set Running_Thread := First_Thread
         & "ldm   %1, {r0-r3,sp,lr}^"  & NL  -- Restore user registers
         & "msr   SPSR_cxsf, r3" & NL  -- Move user CPSR to our SPSR
         & "movs  pc, r2"        & NL  -- Switch back to current thread mode
         & "0:",                       -- Label indicating where to continue
         Inputs   =>
           (Address'Asm_Input ("r", Running_Thread_Table (CPU_Id)'Address),
            Thread_Id'Asm_Input ("r", First_Thread_Table (CPU_Id))),
         Volatile => True,
         Clobber  => "memory,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,lr");
   end Context_Switch;

   ------------------------
   -- FPU_Context_Switch --
   ------------------------

   procedure FPU_Context_Switch (To : Thread_Id) is
      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;
      C : constant Thread_Id := Current_FPU_Context (CPU_Id);

   begin
      if C /= To then
         if C /= null then
            Asm (Template => "vstm %1, {d0-d15}" & NL & "fmrx %0, fpscr",
                 Outputs  =>
                   (Unsigned_32'Asm_Output ("=r", C.Context.VFPU.FPSCR)),
                 Inputs   =>
                   (Address'Asm_Input ("r", C.Context.VFPU.V'Address)),
                 Clobber  => "memory",
                 Volatile => True);
         end if;

         if To /= null then
            Asm (Template => "vldm %1, {d0-d15}" & NL & "fmxr fpscr, %0",
                 Inputs   =>
                   (Unsigned_32'Asm_Input ("r", To.Context.VFPU.FPSCR),
                    Address'Asm_Input ("r", To.Context.VFPU.V'Address)),
                 Clobber  => "memory",
                 Volatile => True);
         end if;

         Current_FPU_Context (CPU_Id) := To;
      end if;
   end FPU_Context_Switch;

   ----------------------
   -- Initialize_Stack --
   ----------------------

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address)
   is
      use System.Storage_Elements;
   begin
      --  Force alignment
      Stack_Pointer := Base + (Size - (Size mod Stack_Alignment));
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
      User_CPSR   : Unsigned_32;
      Mask_CPSR   : constant Unsigned_32 := 16#07f0_ffe0#;
      System_Mode : constant Unsigned_32 := 2#11111#; -- #31

   begin
      --  Use a read-modify-write strategy for computing the CPSR for the new
      --  task: we clear any freely user-accessible bits, as well as the mode
      --  bits, then add in the new mode.

      Asm ("mrs %0, CPSR",
           Outputs  => Unsigned_32'Asm_Output ("=r", User_CPSR),
           Volatile => True);
      User_CPSR := (User_CPSR and Mask_CPSR) + System_Mode;

      Buffer.all :=
        (R0     => Unsigned_32 (Argument),
         PC     => Unsigned_32 (Program_Counter),
         CPSR   => User_CPSR,
         SP     => Unsigned_32 (Stack_Pointer),
         others => <>);
   end Initialize_Context;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      null;
   end Install_Error_Handlers;

   --------------------
   -- Is_FPU_Enabled --
   --------------------

   function Is_FPU_Enabled return Boolean is
      R : Unsigned_32;
   begin
      Asm ("fmrx   %0, fpexc",
           Outputs  => Unsigned_32'Asm_Output ("=r", R),
           Volatile => True);
      return (R and 16#4000_0000#) /= 0;
   end Is_FPU_Enabled;

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
   begin
      Board_Support.Interrupts.Set_Current_Priority (Level);

      if Level < System.BB.Parameters.Interrupt_Unmask_Priority then
         Asm ("cpsie i", Volatile => True);
      end if;
   end Enable_Interrupts;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
   begin
      --  We start with not allowing floating point. This way there never will
      --  be overhead saving unused floating point registers, We'll also be
      --  able to tell if floating point instructions were ever used.

      Set_FPU_Enabled (False);
   end Initialize_CPU;

   ---------------------
   -- Set_FPU_Enabled --
   ---------------------

   procedure Set_FPU_Enabled (Enabled : Boolean) is
   begin
      Asm ("fmxr   fpexc, %0",
           Inputs    => Unsigned_32'Asm_Input
                          ("r", (if Enabled then 16#4000_0000# else 0)),
           Volatile  => True);
      pragma Assert (Is_FPU_Enabled = Enabled);
   end Set_FPU_Enabled;
end System.BB.CPU_Primitives;
