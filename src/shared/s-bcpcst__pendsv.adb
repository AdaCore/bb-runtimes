------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              SYSTEM.BB.CPU_PRIMITIVES.CONTEXT_SWITCH_TRIGGER             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--            Copyright (C) 2005-2019, Free Software Foundation, Inc.       --
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

with System.BB.Parameters;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.CPU_Primitives.Context_Switch_Trigger is

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator in Asm templates

   No_Floating_Point : constant Boolean := not System.BB.Parameters.Has_FPU;
   --  Set True iff the FPU should not be used

   Is_ARMv6m : constant Boolean := System.BB.Parameters.Is_ARMv6m;
   --  Set True iff the core implements the armv6-m architecture

   ICSR : Word with Volatile, Address => 16#E000_ED04#; -- Int. Control/State

   ICSR_Pend_SV_Set : constant Word := 2**28;

   procedure Pend_SV_Handler;
   pragma Machine_Attribute (Pend_SV_Handler, "naked");
   pragma Export (Asm, Pend_SV_Handler, "__gnat_pend_sv_trap");
   --  This assembly routine needs to save and restore registers without
   --  interference. The "naked" machine attribute communicates this to GCC.

   -------------------------------
   -- Initialize_Context_Switch --
   -------------------------------

   procedure Initialize_Context_Switch is null;
   --  There's no hardware to initialize here

   ----------------------------
   -- Trigger_Context_Switch --
   ----------------------------

   procedure Trigger_Context_Switch is
   begin
      --  Make pending supervisor call. The context switch will occur when
      --  interrupts are enabled. The call is followed by a data barrier to
      --  ensure the call is set before interrupts are re-enabled.

      ICSR := ICSR_Pend_SV_Set;
      Asm ("dsb", Volatile => True);

      --  The context switch better be pending, as otherwise it means
      --  interrupts were not disabled.

      pragma Assert ((ICSR and ICSR_Pend_SV_Set) /= 0);
   end Trigger_Context_Switch;

   ---------------------
   -- Pend_SV_Handler --
   ---------------------

   procedure Pend_SV_Handler is
   begin
      --  At most one instance of this handler can run at a time, and
      --  interrupts will preserve all state, so interrupts can be left
      --  enabled. Note the invariant that at all times the active context is
      --  in the ("__gnat_running_thread_table"). Only this handler may update
      --  that variable.

      Asm
        (Template =>
         "ldr r2,=__gnat_running_thread_table" & NL &
         "mrs  r12, PSP "     & NL & -- Retrieve current PSP
         "ldr  r3, [r2]"      & NL & -- Load address of running context buffer

         --  If floating point is enabled, we may have to save the non-volatile
         --  floating point registers, and save bit 4 of the LR register, as
         --  this will indicate whether the floating point context was saved
         --  or not.

         (if No_Floating_Point then "" -- No FP context to save
          else
            "tst  lr, #16"            & NL &  -- if FPCA flag was set,
            "itte  eq"                & NL &  -- then
            "vstmdbeq r12!,{s16-s31}" & NL &  --   save FP context below PSP
            "addeq  r12, #1"          & NL &  --   save flag in bit 0 of PSP
            "subne  lr, #16"          & NL) & -- else set FPCA flag in LR

         --  Store R4-R11 and PSP (stored in R12) in the context buffer. The
         --  context buffer is not on the stack.
         (if Is_ARMv6m then
              --  Save context using armv6-m instructions
              "stm  r3!, {r4-r7}"     & NL &
              "mov  r4, r8"           & NL &
              "mov  r5, r9"           & NL &
              "mov  r6, r10"          & NL &
              "mov  r7, r11"          & NL &
              "stm  r3!, {r4-r7}"     & NL &
              "mov  r4, r12"          & NL &
              "stm  r3!, {r4}"        & NL
         else
              "stm  r3, {r4-r12}"     & NL) & -- Save context

         "ldr  r3,=first_thread_table" & NL &
         "ldr  r3, [r3]"  & NL & -- Load address of new context
         "str  r3, [r2]"  & NL & -- Update value of __gnat_running_thread_table

         --  Load R4-R11 and PSP (stored in R12) from the new context buffer
         (if Is_ARMv6m then
                --  Load context using armv6-m instructions
                "movs r2, #0x20"  & NL &
                "add  r2, r3, r2" & NL &  -- Move R2 where PSP is stored in
                                    NL &  -- the context buffer.
                "ldr  r4, [r2]"   & NL &  -- Load PSP from context buffer
                "mov  r12, r4"    & NL &  -- Set new stack

                "movs r2, #0x10"    & NL &
                "add  r2, r3, r2"   & NL & -- Move R2 where R8 is stored
                "ldm  r2!, {r4-r7}" & NL & -- Load R8-R11 from context buffer
                "mov  r8,  r4"      & NL &
                "mov  r9,  r5"      & NL &
                "mov  r10, r6"      & NL &
                "mov  r11, r7"      & NL &

                "mov  r2, r3" & NL & -- Move R2 where R4 is stored in
                                NL & -- the context buffer.
                "ldm  r2!, {r4-r7}" & NL   -- Load R4-R7 from context buffer
         else
                "ldm  r3, {r4-r12}" & NL) & -- Load context and new PSP

         --  If floating point is enabled, check bit 0 of PSP to see if we
         --  need to restore the floating point context.

         (if No_Floating_Point then ""     -- No FP context to restore
          else
            "tst  r12, #1"            & NL &  -- if FPCA was set,
            "itte  ne"                & NL &  -- then
            "subne r12, #1"           & NL &  --   remove flag from PSP
            "vldmiane r12!,{s16-s31}" & NL &  --   Restore FP context
            "addeq lr, #16"           & NL) & -- else clear FPCA flag in LR

         --  Finally, update PSP and perform the exception return

         "msr  PSP, r12" & NL &        -- Update PSP
         "bx   lr",                    -- return to caller
         Volatile => True);
   end Pend_SV_Handler;

end System.BB.CPU_Primitives.Context_Switch_Trigger;
