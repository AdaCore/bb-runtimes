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

--  This is the LEON3 version of this package

pragma Restrictions (No_Elaboration_Code);

with Interfaces.Leon3.Timers;
with Interfaces.Leon3.Irqmp;
with System.BB.Board_Parameters;
with System.BB.CPU_Primitives.Multiprocessors;
with System.Machine_Code;
with Interfaces;

package body System.BB.Board_Support is

   use CPU_Primitives;
   use Interfaces.Leon3.Timers;
   use Interfaces.Leon3.Irqmp;
   use System.Multiprocessors;
   use BB.Interrupts;
   use Time;
   use Interfaces;

   -----------------------
   -- Local Definitions --
   -----------------------

   Periodic_Count : constant := Time.Timer_Interval'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timer_Interval'Last because the timeout period
   --  will count an extra cycle for reloading the counter.

   subtype Normal_Interrupt_Range is Natural range 1 .. 15;
   subtype Extended_Interrupt_Range is Natural range 16 .. 31;
   --  Interrupts range

   Poke_Interrupt : constant System.BB.Interrupts.Interrupt_ID := 14;
   --  Use interrupt 14 because 15 is unmaskable with PIL field of PSR register
   --  (see SPARCv8 manual 7.3. Trap Control).
   --
   --  The value is copied in trap_handler-bb-sparc.S

   Alarm_CPU : constant System.Multiprocessors.CPU :=
                 System.Multiprocessors.CPU'First;
   --  CPU which will handle the alarm interrupt

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   procedure Extended_Interrupt_Handler (Vector : CPU_Primitives.Vector_Id);
   --  Low-level interrupt handler for extended interrupts

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      Is_SMP : constant Boolean := Board_Parameters.Max_Number_Of_CPUs > 1;
      Has_Extended_Interrupts : constant Boolean :=
        Board_Parameters.Extended_Interrupts_Level /= 0;
   begin
      Initialize_Clock;

      --  Initialize interrupts.

      for CPU_Id in CPU loop

         --  Mask all interrupts

         Interrupt_Mask (CPU_Id) := 0;
      end loop;

      if Has_Extended_Interrupts then

         --  Check interrupt number used for extended interrupts

         declare
            Reg : constant Multiprocessor_Status_Register :=
              Multiprocessor_Status;
         begin
            pragma Assert
              (Reg.EIRQ = Board_Parameters.Extended_Interrupts_Level);
         end;

         --  Install handler

         CPU_Primitives.Install_Trap_Handler
           (Extended_Interrupt_Handler'Address,
            CPU_Primitives.Vector_Id
              (Board_Parameters.Extended_Interrupts_Level + 16#10#));

         --  There is no need to unmask the extended interrupt, as the mask
         --  is only used for the normal interrupt and the extended interrupt
         --  is always enabled.
      end if;

      if Is_SMP then

         --  Enable Poke interrupts for all CPUs

         for CPU_Id in CPU loop
            Interrupt_Mask (CPU_Id) :=
              Interrupt_Mask (CPU_Id) or 2**Poke_Interrupt;
         end loop;
      end if;

   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Prescaler               : constant Prescaler_Register :=
                                  (Value    => Board_Parameters.Prescaler_Min,
                                   Reserved => (others => False));
      --  Minimum prescaler to be used to achieve best granularity

      Periodic_Mode : constant Timer_Control_Register :=
        (Enable            => True,
         Reload_Counter    => True,
         Load_Counter      => True,
         Interrupt_Enable  => False,
         Interrupt_Pending => False,
         Chain             => False,
         Debug_Halt        => False,
         Reserved          => (others => False));

   begin
      --  Set the prescaler value to achieve the required granularity

      Prescaler_Reload := Prescaler;

      --  Load the counter for the real-time clock

      Timer_2_Reload := Periodic_Count;

      --  Program the timer in periodic mode to serve as a clock

      Timer_2_Control := Periodic_Mode;
   end Initialize_Clock;

   package body Time is
      Alarm_Interrupt_ID : constant Interrupt_ID :=
        Board_Parameters.Timer_1_Interrupt;

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
         return Timer_Interval'Last;
      end Max_Timer_Interval;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
      begin
         return BB.Time.Time (Periodic_Count - Timer_2_Counter);
      end Read_Clock;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         One_Shot_Mode : constant Timer_Control_Register :=
           (Enable            => True,
            Reload_Counter    => False,
            Load_Counter      => True,
            Interrupt_Enable  => True,
            Interrupt_Pending => False,
            Chain             => False,
            Debug_Halt        => False,
            Reserved => (others => False));

      begin
         --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
         --  time intervals is equal to Clock Period.

         --  Set the prescaler: already done in Initialize_Clock

         --  Load the counter

         Timer_1_Reload := Ticks;

         --  Program the timer in one-shot mode

         Timer_1_Control := One_Shot_Mode;

         --   Enable Timer 1 Interrupts

         Interrupt_Mask (Alarm_CPU) :=
           Interrupt_Mask (Alarm_CPU) or 2**Board_Parameters.Timer_1_Interrupt;
      end Set_Alarm;
   end Time;

   procedure Extended_Interrupt_Handler (Vector : CPU_Primitives.Vector_Id) is
      CPU_Id : constant CPU := Multiprocessors.Current_CPU;

      Id     : Interrupt_ID;
   begin
      --  Vector must be the interrupt for extended interrupts

      pragma Assert
        (Vector = 16#10# + Board_Parameters.Extended_Interrupts_Level);

      Id := System.BB.Interrupts.Interrupt_ID
        (Extended_Interrupt_Acknowledge_Register (CPU_Id));

      Interrupt_Wrapper (Id);
   end Extended_Interrupt_Handler;

   package body Interrupts is
      procedure Interrupt_Handler (Vector : CPU_Primitives.Vector_Id);
      --  Low-level interrupt handler

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         System.Machine_Code.Asm ("wr %%g0, %%asr19", Volatile => True);
      end Power_Down;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : Interrupt_ID) return System.Any_Priority
      is
      begin
         --  Assert that it is a real interrupt

         pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

         --  Some leon3 board may only have normal interrupts

         pragma Warnings (Off, "explicit membership test may be optimized*");

         if Interrupt in Normal_Interrupt_Range then
            return Any_Priority (Interrupt) + Interrupt_Priority'First - 1;
         else
            --  All extended interrupts have the same priority (the one set in
            --  EIRQ).
            pragma Assert (Interrupt in Extended_Interrupt_Range);

            return Board_Parameters.Extended_Interrupts_Level
              + Interrupt_Priority'First - 1;
         end if;

         pragma Warnings (On, "explicit membership test may be optimized*");
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
         pragma Warnings (Off, "explicit membership test may be optimized*");

         if Interrupt in Normal_Interrupt_Range then
            CPU_Primitives.Install_Trap_Handler
              (Interrupt_Handler'Address,
               CPU_Primitives.Vector_Id (Interrupt + 16#10#));
         end if;

         pragma Warnings (On, "explicit membership test may be optimized*");
      end Install_Interrupt_Handler;
   end Interrupts;

   package body Multiprocessors is

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID);
      --  Handler for the Poke interrupt

      function ASR_17 return Unsigned_32;
      --  Return current value of the ASR 17 register

      procedure Start_CPU (CPU_Id : CPU);
      --  Start one cpu

      --------------------
      -- Number_Of_CPUs --
      --------------------

      function Number_Of_CPUs return CPU is
      begin
         return CPU'Val (Multiprocessor_Status.NCPUS + 1);
      end Number_Of_CPUs;

      ------------
      -- ASR_17 --
      ------------

      function ASR_17 return Unsigned_32 is
         R : Unsigned_32;
      begin
         System.Machine_Code.Asm ("mov %%asr17, %0" & ASCII.LF & ASCII.HT,
                                  Outputs => Unsigned_32'Asm_Output ("=r", R),
                                  Volatile => True);
         return R;
      end ASR_17;

      -----------------
      -- Current_CPU --
      -----------------

      function Current_CPU return CPU is

         --  Get CPU Id from bits 31-28 of the Asr17 register

         (if CPU'Last = 1 then 1 else CPU (Shift_Right (ASR_17, 28) + 1));

      --------------
      -- Poke_CPU --
      --------------

      procedure Poke_CPU (CPU_Id : CPU) is
      begin
         --  There is no need to protect access to the register since the only
         --  operation applied to it is this assignment and it's always with
         --  the same value (2**Poke_Interrupt_ID).

         --  No race condition possible here.

         Interrupt_Force (CPU_Id) :=
           Interrupt_Force (CPU_Id) or 2 ** Poke_Interrupt;
      end Poke_CPU;

      ---------------
      -- Start_CPU --
      ---------------

      procedure Start_CPU (CPU_Id : CPU) is
         Reg : Multiprocessor_Status_Register;
      begin
         --  Set bit n in Status Register to start CPU n

         Reg := Multiprocessor_Status;
         Reg.Status := 2**(CPU'Pos (CPU_Id) - 1);
         Multiprocessor_Status := Reg;
      end Start_CPU;

      --------------------
      -- Start_All_CPUs --
      --------------------

      procedure Start_All_CPUs is
      begin
         BB.Interrupts.Attach_Handler
           (Poke_Handler'Access, Poke_Interrupt, Interrupt_Priority'Last);

         --  Disable warnings in case of one cpu

         pragma Warnings (Off, "loop range is null*");

         for CPU_Id in CPU'First + 1 .. CPU'Last loop
            Start_CPU (CPU_Id);
         end loop;

         pragma Warnings (On, "loop range is null*");
      end Start_All_CPUs;

      ------------------
      -- Poke_Handler --
      ------------------

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID) is
      begin
         --  Make sure we are handling the right interrupt

         pragma Assert (Interrupt = Poke_Interrupt);

         System.BB.CPU_Primitives.Multiprocessors.Poke_Handler;
      end Poke_Handler;
   end Multiprocessors;
end System.BB.Board_Support;
