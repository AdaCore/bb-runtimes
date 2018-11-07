------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
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

with System.BB.CPU_Specific;
with System.BB.RISCV_PLIC;

package body System.BB.Board_Support is

   package PLIC renames RISCV_PLIC;

   procedure External_Interrupt_Trap_Handler
     (Unused : BB.Interrupts.Interrupt_ID);
   --  Procedure called in case of external interrupt trap

   -----------------------------
   -- External_Interrupt_Trap --
   -----------------------------

   procedure External_Interrupt_Trap_Handler
     (Unused : BB.Interrupts.Interrupt_ID)
   is
      Int_Id : BB.Interrupts.Interrupt_ID;
   begin

      --  While there is at least one pending interrupt above threshold for the
      --  current hart...
      while PLIC.Pending loop

         --  Claim the interrupt
         Int_Id := PLIC.Claim;

         if Int_Id /= BB.Interrupts.No_Interrupt then

            --  Note: There is a situation where we might service an interrupt
            --  below the threshold. If an interrupt above threshold is
            --  triggered, causes the external interrupt trap, but is then
            --  clear before we can claim it. In that situation, we will claim
            --  whatever interrupt is pending at the time, potentially an
            --  interrupt below the threshold. This risk is limited by the fact
            --  that we check if an interrupt if pending for the current hart
            --  before doing a claim. So the faulty situation can only occur if
            --  the interrupt is cleared from the source between the call to
            --  Pending and the call to Claim.
            pragma Assert
              (PLIC.Priority_Of_Interrupt (Int_Id) > PLIC.Threshold);

            --  Call the wrapper that will execute the interrupt handler
            BB.Interrupts.Interrupt_Wrapper (Int_Id);

            --  It is not possible to claim a new interrupt until the current
            --  one is signaled as completed. So we signal completion right
            --  away to allow nested interrupt handling.
            PLIC.Complete (Int_Id);

         end if;
      end loop;

   end External_Interrupt_Trap_Handler;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      PLIC.Initialize;

      CPU_Specific.Install_Trap_Handler
        (External_Interrupt_Trap_Handler'Access,
         CPU_Specific.External_Interrupt_Trap);
   end Initialize_Board;

   package body Interrupts is

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : System.BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
      begin

         --  For the PLIC there is nothing more to do than setting the priority
         --  and enable the interrupt. All interrupts go to the same trap
         --  handler before being dispatched.
         PLIC.Set_Priority (Interrupt, Prio);
         PLIC.Enable (Interrupt);

      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
      is (PLIC.Priority_Of_Interrupt (Interrupt));

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         null;
      end Power_Down;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         PLIC.Set_Priority_Threshold (Priority);
      end Set_Current_Priority;
   end Interrupts;

   package body Time is separate;

   package body Multiprocessors is separate;
end System.BB.Board_Support;
