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
--                     Copyright (C) 2003-2021, AdaCore                     --
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

--  This is the Xilinx Ultrascale+ Zynqmp Cortex-R5 version of this package
--  This uses the TTC (Triple Timer Clock) peripheral for timing events and
--  the PL390 GIC controller for Interrupts support.

with System.ARM_GIC;
with System.BB.Parameters;
with Interfaces;                 use Interfaces;

package body System.BB.Board_Support is
   use BB.Interrupts, BB.Time, Parameters;
   use System.Multiprocessors;
   use Time;

   -----------
   -- GICv2 --
   -----------

   package GIC renames System.ARM_GIC;

   -----------
   -- Timer --
   -----------

   --  The zynqmp has 4 Triple Timer counters. We use Timer 0 & 1 from TTC0,
   --  the first timer being used to retrieve the time, and the second one
   --  to generate timed events.

   type TTC_Registers is record
      Clock_Control       : Unsigned_32;
      Counter_Control     : Unsigned_32;
      Counter_Value       : Unsigned_32;
      Interval_Counter    : Unsigned_32;
      Match_1_Counter     : Unsigned_32;
      Match_2_Counter     : Unsigned_32;
      Match_3_Counter     : Unsigned_32;
      Interrupt_Register  : Unsigned_32;
      Interrupt_Enable    : Unsigned_32;
      Event_Control_Timer : Unsigned_32;
      Event_Register      : Unsigned_32;
   end record with Volatile;

   for TTC_Registers use record
      Clock_Control       at 16#00# range 0 .. 31;
      Counter_Control     at 16#0C# range 0 .. 31;
      Counter_Value       at 16#18# range 0 .. 31;
      Interval_Counter    at 16#24# range 0 .. 31;
      Match_1_Counter     at 16#30# range 0 .. 31;
      Match_2_Counter     at 16#3C# range 0 .. 31;
      Match_3_Counter     at 16#48# range 0 .. 31;
      Interrupt_Register  at 16#54# range 0 .. 31;
      Interrupt_Enable    at 16#60# range 0 .. 31;
      Event_Control_Timer at 16#6C# range 0 .. 31;
      Event_Register      at 16#78# range 0 .. 31;
   end record;

   TTC_Clock : aliased TTC_Registers with Address => 16#FF11_0000#;
   --  TTC0[0]: Used to get current time
   Clock_Interrupt_ID : constant BB.Interrupts.Interrupt_ID := 68;
   --  TTC0_0_Interrupt
   TTC_Alarm : aliased TTC_Registers with Address => 16#FF11_0004#;
   --  TTC0[1]: Used to handle timed events
   Alarm_Interrupt_ID : constant BB.Interrupts.Interrupt_ID := 69;
   --  TTC0_1_Interrupt

   TTC_Intv_Int   : constant Unsigned_32 := 2 ** 0;
   TTC_Match1_Int : constant Unsigned_32 := 2 ** 1;
   TTC_Ovf_Int    : constant Unsigned_32 := 2 ** 4;

   Alarm_Ticks_Periods : Unsigned_32 with Volatile;
   Alarm_Ticks_Match   : Unsigned_32 with Volatile;
   Last_Clock_Time     : BB.Time.Time with Volatile;
   Timer_Handler       : BB.Interrupts.Interrupt_Handler := null;

   procedure Reset_Timer (TTC : access TTC_Registers);
   procedure LL_Alarm_Handler (Id : Interrupt_ID);
   procedure LL_Clock_Handler (Id : Interrupt_ID);

   -----------------
   -- Reset_Timer --
   -----------------

   procedure Reset_Timer (TTC : access TTC_Registers)
   is
   begin
      --  Disable the counter Dis to 1
      TTC.Counter_Control    := 1;

      --  Reset the other registers to their default values
      TTC.Clock_Control      := 0;
      TTC.Interval_Counter   := 0;
      TTC.Match_1_Counter    := 0;
      TTC.Match_2_Counter    := 0;
      TTC.Match_3_Counter    := 0;
      TTC.Interrupt_Enable   := 0;

      --  Clear any pending interrupt
      TTC.Interrupt_Register := 16#1F#;

      --  Setup the prescaler
      pragma Assert (TTC_Prescaler < 16);
      TTC.Clock_Control      := Shift_Left (TTC_Prescaler, 1) or 1;
   end Reset_Timer;

   ----------------------
   -- LL_Clock_Handler --
   ----------------------

   procedure LL_Clock_Handler (Id : BB.Interrupts.Interrupt_ID)
   is
      pragma Unreferenced (Id);
   begin
      if (TTC_Clock.Interrupt_Register and TTC_Ovf_Int) /= 0 then
         Last_Clock_Time := Last_Clock_Time + 16#1_0000_0000#;
      end if;
   end LL_Clock_Handler;

   ----------------------
   -- LL_Alarm_Handler --
   ----------------------

   procedure LL_Alarm_Handler (Id : BB.Interrupts.Interrupt_ID)
   is
      Flags   : Unsigned_32;
   begin
      --  The alarm is configured to either:
      --  * send Intv IRQs each time the counter reaches 8FFF_FFFF (half of its
      --    total period
      --  * send a Match IRQ when the counter reaches the actual alarm time

      Flags   := TTC_Alarm.Interrupt_Register;

      if (Flags and TTC_Match1_Int) /= 0 then
         Alarm_Ticks_Periods := 0;
         Alarm_Ticks_Match   := 0;
         TTC_Alarm.Interrupt_Enable := 0;
         --  Disable the alarm counter
         TTC_Alarm.Counter_Control := 1;
         --  Now actually call the handler
         Timer_Handler (Id);

      else
         if Alarm_Ticks_Periods > 0 then
            Alarm_Ticks_Periods := Alarm_Ticks_Periods - 1;
         end if;

         if Alarm_Ticks_Periods = 0 then
            TTC_Alarm.Match_1_Counter := Alarm_Ticks_Match;
            TTC_Alarm.Interrupt_Enable := TTC_Match1_Int;
            TTC_Alarm.Counter_Control := 2 ** 3;
         end if;

      end if;
   end LL_Alarm_Handler;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board
   is
   begin
      GIC.Initialize_GICD;
      GIC.Initialize_GICC;
      BB.Interrupts.Attach_Handler
        (LL_Clock_Handler'Access,
         Clock_Interrupt_ID,
         Interrupt_Priority'Last);
      BB.Interrupts.Attach_Handler
        (LL_Alarm_Handler'Access,
         Alarm_Interrupt_ID,
         Interrupt_Priority'Last);

      Alarm_Ticks_Match   := 0;
      Alarm_Ticks_Periods := 0;
      Last_Clock_Time     := 0;

      Reset_Timer (TTC_Clock'Access);
      Reset_Timer (TTC_Alarm'Access);

      --  Start the clock
      TTC_Clock.Interrupt_Enable := TTC_Ovf_Int;
      TTC_Clock.Counter_Control  := 2 ** 4; --  RST bit

   end Initialize_Board;

   package body Time is

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time)
      is
         Now         : BB.Time.Time;
         Alarm_Ticks : BB.Time.Time;

      begin
         --  Disable the alarm counter
         TTC_Alarm.Counter_Control := 1;
         TTC_Alarm.Interrupt_Enable := 0;

         Alarm_Ticks_Periods := 0;
         Alarm_Ticks_Match   := 0;

         if Ticks < BB.Time.Time'Last then
            Now := Read_Clock;

            if Now >= Ticks then
               --  Enable the Match1 irq
               TTC_Alarm.Interrupt_Enable := TTC_Match1_Int;
               --  Raise IRQ on first count
               TTC_Alarm.Match_1_Counter := 1;
               --  Reset the Alarm counter and enable match mode
               TTC_Alarm.Counter_Control := 2 ** 3 + 2 ** 4;
            else
               Alarm_Ticks := Ticks - Now;
               --  Calculate the value for the Match register
               Alarm_Ticks_Match :=
                 Unsigned_32 (Alarm_Ticks and 16#FFFF_FFFF#);
               --  Calculate the number of counter periods before setting the
               --  match register
               Alarm_Ticks_Periods :=
                 Unsigned_32
                   (Shift_Right
                      (Unsigned_64 (Alarm_Ticks) and
                           16#8FFF_FFFF_0000_0000#,
                       32));

               --  Calculate the actual period used: 3 cases here
               --  1- The alarm is soon enough so that the counter won't
               --     overflow. We just set the Match register and set the
               --     alarm in match mode.
               --  2- The alarm would be set just after an interval: there's
               --     then the risk that we don't set the match register soon
               --     enough after the interval is notified, so miss the actual
               --     alarm.
               --     We then use half periods, and make sure the alarm is
               --     always set with half period + something
               --  3- else, we apply the maximum timer period
               if Alarm_Ticks_Periods = 0 then
                  TTC_Alarm.Interval_Counter := 0;
                  TTC_Alarm.Match_1_Counter  := Alarm_Ticks_Match;
                  TTC_Alarm.Interrupt_Enable := TTC_Match1_Int;
                  --  Start the alarm counter in MATCH mode
                  TTC_Alarm.Counter_Control  := 2 ** 3 + 2 ** 4;

               elsif Alarm_Ticks_Match < 16#8000_0000#
                 and then Alarm_Ticks_Periods > 0
               then
                  --  Setup the interval used for the alarm clock: it's half
                  --  the overflow interval, so that the actual alarm is always
                  --  set some time before the last overflow, to prevent too
                  --  short notice to the counter.
                  Alarm_Ticks_Periods := Alarm_Ticks_Periods * 2 - 1;
                  Alarm_Ticks_Match   := Alarm_Ticks_Match + 16#8000_0000#;
                  TTC_Alarm.Interval_Counter := 16#8000_0000#;
                  TTC_Alarm.Interrupt_Enable := TTC_Intv_Int;
                  --  Start the alarm counter in INT mode
                  TTC_Alarm.Counter_Control  := 2 ** 1 + 2 ** 4;

               else
                  TTC_Alarm.Interval_Counter := 0;
                  TTC_Alarm.Match_1_Counter  := 0;
                  TTC_Alarm.Interrupt_Enable := TTC_Ovf_Int;
                  --  Start the alarm counter as free running mode
                  TTC_Alarm.Counter_Control  := 2 ** 4;
               end if;
            end if;
         end if;
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time
      is
         Count         : Unsigned_32;
         Flag          : Boolean;
         Res           : BB.Time.Time;
         Old_Itf_Value : constant Unsigned_32 := TTC_Clock.Interrupt_Enable;

      begin
         --  First ensure that we're not interrupted
         TTC_Clock.Interrupt_Enable := 0;

         --  We must read the counter before the flag: if we read the flag
         --  first, a reload can occur just after the read and the count
         --  register would wrap around: we'd end up with a Count value close
         --  to 0 but a flag at 0 and therefore miss the reload and return a
         --  wrong clock value.

         Count := TTC_Clock.Counter_Value;

         Flag  := (TTC_Clock.Interrupt_Register and TTC_Ovf_Int) /= 0;
         if Flag then
            --  Counter has just reached the ovf value. So just ignore
            --  Count that may still be close to Interval_Count is the wrap
            --  occurred between the two read operations
            Last_Clock_Time := Last_Clock_Time + 16#1_0000_0000#;
            Res := Last_Clock_Time;
         else
            Res := Last_Clock_Time + BB.Time.Time (Count);
         end if;

         --  Re-enable the TTC interrupt
         TTC_Clock.Interrupt_Enable := Old_Itf_Value;

         return Res;
      end Read_Clock;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler (Handler : Interrupt_Handler)
      is

      begin
         Timer_Handler := Handler;
      end Install_Alarm_Handler;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
         Dummy : Unsigned_32 with Volatile;
      begin
         --  The Interrupt_Register is "Clear on read"
         Dummy := TTC_Clock.Interrupt_Register;
      end Clear_Alarm_Interrupt;
   end Time;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler is new GIC.IRQ_Handler
     (Interrupt_Wrapper => Interrupt_Wrapper);
   pragma Export (C, IRQ_Handler, "__gnat_irq_handler");
   --  Low-level interrupt handler

   procedure FIQ_Handler is null;
   pragma Export (C, FIQ_Handler, "__gnat_fiq_handler");

   package body Interrupts is

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority) renames GIC.Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority renames GIC.Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer)
        renames GIC.Set_Current_Priority;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down renames GIC.Power_Down;
   end Interrupts;

   package body Multiprocessors is separate;

end System.BB.Board_Support;
