------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Board_Support.LEON; use System.BB.Board_Support.LEON;
with System.BB.Board_Parameters;
with System.BB.CPU_Primitives;

package body System.BB.Board_Support is
   use BB.CPU_Primitives;
   use BB.Interrupts;

   -----------------------
   -- Local Definitions --
   -----------------------

   Periodic_Count : constant := 2**24 - 2;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timers_Counter'Last (2**24 -1) because the
   --  timeout period will count an extra cycle for reloading the counter.

   --  Constants defining the external interrupts

   General_Purpose_Timer : constant := 8;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      Prescaler : constant Prescaler_Register :=
                    (Value => Board_Parameters.Prescaler_Min,
                     Reserved => (others => False));
      --  Minimum prescaler to be used to achieve best granularity

      Real_Time_Clock_Reload : constant Timer_Register :=
                                 (Timer_Value => Periodic_Count,
                                  Reserved    => (others => False));
      --  Periodic count to be used for the clock

      Real_Time_Clock_Control : constant Timer_Control_Register :=
                                  (Enable         => True,
                                   Reload_Counter => True,
                                   Load_Counter   => True,
                                   Reserved       => (others => False));
      --  Program the timer in periodic mode to serve as a clock

   begin
      --  Set the prescaler value to achieve the required granularity

      Prescaler_Reload := Prescaler;

      --  Load the counter for the real-time clock

      Timer_2_Reload := Real_Time_Clock_Reload;

      --  Enable Timer 2

      Timer_2_Control := Real_Time_Clock_Control;
   end Initialize_Board;

   package body Time is
      Alarm_Interrupt_ID : constant Interrupt_ID := General_Purpose_Timer;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler
        (Handler : BB.Interrupts.Interrupt_Handler) is
      begin
         BB.Interrupts.Attach_Handler
           (Handler,
            Alarm_Interrupt_ID,
            Interrupts.Priority_Of_Interrupt (Alarm_Interrupt_ID));
      end Install_Alarm_Handler;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         --  Interrupts are cleared automatically when they are acknowledged

         null;
      end Clear_Alarm_Interrupt;

      ------------------------
      -- Max_Timer_Interval --
      ------------------------

      function Max_Timer_Interval return Timer_Interval is
      begin
         return Periodic_Count;
      end Max_Timer_Interval;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
         Timer_Counter : constant Timer_Register := Timer_2_Counter;
         --  Make copy of atomic variable to avoid warning on partial access

      begin
         return BB.Time.Time (Periodic_Count - Timer_Counter.Timer_Value);
      end Read_Clock;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         Timer_Reload_Aux : constant Timer_Register :=
                              (Timer_Value => Timers_Counter (Ticks),
                               Reserved    => (others => False));
         --  Load the required ticks

         Timer_Control_Aux : constant Timer_Control_Register :=
                               (Enable         => True,
                                Reload_Counter => False,
                                Load_Counter   => True,
                                Reserved       => (others => False));
         --  Program the timer in one-shot mode

         Interrupt_Mask_Aux : Interrupt_Mask_and_Priority_Register;

      begin
         --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
         --  time intervals is equal to Clock Period.

         --  Set the prescaler: already done in Initialize_Clock

         --  Load the counter

         Timer_1_Reload := Timer_Reload_Aux;

         --  Write Timer Control Register

         Timer_1_Control := Timer_Control_Aux;

         --   Enable Timer 1 Interrupts

         Interrupt_Mask_Aux := Interrupt_Mask_and_Priority;
         Interrupt_Mask_Aux.Timer_1 := True;
         Interrupt_Mask_and_Priority := Interrupt_Mask_Aux;
      end Set_Alarm;
   end Time;

   package body Interrupts is
      procedure Interrupt_Handler (Vector : CPU_Primitives.Vector_Id);
      --  Low level interrupt handler

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         null;
      end Power_Down;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority is
      begin
         --  Assert that it is a real interrupt

         pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

         return (Any_Priority (Interrupt) + Interrupt_Priority'First - 1);
      end Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         null; --  No board-specific actions necessary
      end Set_Current_Priority;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler (Vector : CPU_Primitives.Vector_Id) is
         Id : Interrupt_ID;
      begin
         --  The range corresponding to asynchronous traps is 16#11# .. 16#1F#

         pragma Assert (Vector in 16#11# .. 16#1F#);

         Id := System.BB.Interrupts.Interrupt_ID (Vector - 16#10#);

         Interrupt_Wrapper (Id);
      end Interrupt_Handler;

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
      begin
         CPU_Primitives.Install_Trap_Handler
           (Interrupt_Handler'Address,
            CPU_Primitives.Vector_Id (Interrupt + 16#10#));
      end Install_Interrupt_Handler;
   end Interrupts;

   package body Multiprocessors is separate;

end System.BB.Board_Support;
