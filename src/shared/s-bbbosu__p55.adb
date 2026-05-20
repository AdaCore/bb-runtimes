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

--  Board support for mpc5566

with Interfaces; use Interfaces;

with System.BB.CPU_Specific;
with System.Machine_Code;

package body System.BB.Board_Support is
   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Implement Time.Clear_Alarm_Interrupt

   procedure Interrupt_Handler;
   --  Procedure called in case of interrupt

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      INTC_MCR : Unsigned_32;
      for INTC_MCR'Address use 16#FFF4_8000#;
      pragma Volatile (INTC_MCR);
      pragma Import (Ada, INTC_MCR);

   begin
      --  Initialize the INTC

      INTC_MCR := 0; -- VTES=0, HVEN=0

      --  Mask interrupts

      Interrupts.Set_Current_Priority (Interrupt_Priority'Last - 1);

      --  Install handler for external interrupts

      CPU_Specific.Install_Exception_Handler
        (Interrupt_Handler'Address, CPU_Specific.External_Interrupt_Excp);
   end Initialize_Board;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
      use System.Machine_Code;

   begin
      --  Clear TSR[DIS]

      Asm ("mtspr 336,%0",
           Inputs => Unsigned_32'Asm_Input ("r", 2 ** (63 - 36)),
           Volatile => True);
   end Clear_Alarm_Interrupt;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : System.BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is null;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
      is
         type Intc_Psr_Type is
           array (System.BB.Interrupts.Interrupt_ID) of Unsigned_8;
         INTC_PSR : Intc_Psr_Type;
         for INTC_PSR'Address use 16#FFF4_8040#;
         pragma Volatile (INTC_PSR);
         pragma Import (Ada, INTC_PSR);
      begin
         return Interrupt_Priority'First +
           Natural (INTC_PSR (Interrupt) and 16#0F#);
      end Priority_Of_Interrupt;

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
         INTC_CPR : Unsigned_32;
         for INTC_CPR'Address use 16#FFF4_8008#;
         pragma Volatile (INTC_CPR);
         pragma Import (Ada, INTC_CPR);
         --  INTC current priority register

      begin
         --  Note that Priority cannot be the last one, as this procedure is
         --  unable to disable the decrementer interrupt.

         pragma Assert (Priority /= Interrupt_Priority'Last);

         if Priority in Interrupt_Priority then
            INTC_CPR := Unsigned_32 (Priority - Interrupt_Priority'First);
         else
            INTC_CPR := 0;
         end if;
      end Set_Current_Priority;
   end Interrupts;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      INTC_IACKR : Unsigned_32
        with Import, Volatile, Address => 16#FFF4_8010#;
      --  Interrupt acknowledge register

      INTC_EOIR : Unsigned_32
        with Import, Volatile, Address => 16#FFF4_8018#;
      --  INTC end of interrupt register

      Id : BB.Interrupts.Interrupt_ID;
   begin
      Id := BB.Interrupts.Interrupt_ID ((INTC_IACKR and 16#7FC#) / 4);

      --  No spurious interrupts on p5566, so always call the wrapper

      System.BB.Interrupts.Interrupt_Wrapper (Id);

      INTC_EOIR := 0;
   end Interrupt_Handler;

   package body Time is separate;

   package body Multiprocessors is separate;
end System.BB.Board_Support;
