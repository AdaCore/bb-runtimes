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
--                       Copyright (C) 2018, AdaCore                        --
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

with Interfaces.M1AGL.CoreInterrupt; use Interfaces.M1AGL.CoreInterrupt;
with System.BB.Interrupts;

package body System.BB.CPU_Primitives.Context_Switch_Trigger is

   Context_Switch_Int_ID : constant System.BB.Interrupts.Interrupt_ID := 8;
   --  Iff the core doesn't have OS extensions, the context switch needs to
   --  be done with an IRQ instead of the PendSV trap.

   procedure Context_Switch_Handler
     (Id : System.BB.Interrupts.Interrupt_ID);

   -------------------------------
   -- Initialize_Context_Switch --
   -------------------------------

   procedure Initialize_Context_Switch is
   begin
      --  When OS extensions are not available there's no Pend_SV trap to do
      --  the context switch, so we will use an IRQ and trigger it from the
      --  software (see Trigger_Context_Switch). To do that we first need to
      --  attach a handler.
      BB.Interrupts.Attach_Handler
        (Context_Switch_Handler'Access,
         Context_Switch_Int_ID,
         Interrupt_Priority'Last);
   end Initialize_Context_Switch;

   ----------------------------
   -- Trigger_Context_Switch --
   ----------------------------

   procedure Trigger_Context_Switch is
   begin
      CoreInterrupt_Periph.IRQ_Soft_Interrupt.IRQ.Arr
        (Context_Switch_Int_ID) := True;
   end Trigger_Context_Switch;

   ----------------------------
   -- Context_Switch_Handler --
   ----------------------------

   procedure Context_Switch_Handler
     (Id : System.BB.Interrupts.Interrupt_ID)
   is
   begin

      --  We just clear the software IRQ and return

      CoreInterrupt_Periph.IRQ_Soft_Interrupt_Clear.IRQ.Arr (Id) := True;

   end Context_Switch_Handler;

end System.BB.CPU_Primitives.Context_Switch_Trigger;
