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

with System.Machine_Code;

with System.BB.CPU_Primitives;
with System.BB.Board_Parameters;

with Interfaces.NRF52;       use Interfaces.NRF52;
with Interfaces.NRF52.RTC;   use Interfaces.NRF52.RTC;
with Interfaces.NRF52.CLOCK; use Interfaces.NRF52.CLOCK;

package body System.BB.Board_Support is
   use CPU_Primitives, BB.Interrupts, Machine_Code, Time;

   package BBOPA renames System.BB.Board_Parameters;

   Interrupt_Request_Vector : constant Vector_Id := 16;
   --  See vector definitions in ARMv7-M version of System.BB.CPU_Primitives.
   --  Defined by ARMv7-M specifications.

   Alarm_Time : Time.Timer_Interval;
   pragma Volatile (Alarm_Time);
   pragma Export (C, Alarm_Time, "__gnat_alarm_time");

   Alarm_Interrupt_ID : constant Interrupt_ID := 11; --  RTC0 IRQ

   -------------------
   -- RTC0 Handling --
   -------------------

   --  RTC0 is used as the clock source, which we use to implement
   --  "tick-less" alarm handling.
   --
   --  The RTC is a 24-bit timer running at 32.768 kHz, resulting in a period
   --  of 512 seconds (2**24 / 32_768).
   --
   --  We use the COMPARE feature of the RTC to provide accurate alarms.
   --  We achieve this by updating CC[0] each time Set_Alarm is called so
   --  that the alarm is triggered exactly at the alarm time. This results in
   --  an alarm accuracy of 30.518 µs.
   --
   --  Note that the underlying 24-bit RTC runs at a frequency of 32.768 kHz,
   --  but Timer_Interval is scaled up that, at 65.536 kHz ticks (or higher,
   --  depending on RTC_Tick_Scaling_Factor) to ensure that
   --  Ada.Real_Time.Time_Unit meets the requirements in Ada RM D.8/30

   ----------------------------------------------
   -- New Vectored Interrupt Controller (NVIC) --
   ----------------------------------------------

   NVIC_Base : constant := 16#E000_E000#;
   --  Nested Vectored Interrupt Controller (NVIC) base.

   NVIC_ISER0 : constant Address := NVIC_Base + 16#100#;
   --  Writing a bit mask to this register enables the corresponding interrupts

   NVIC_STIR : Word with Volatile, Address => NVIC_Base + 16#F00#;
   --  Software Trigger Interrupt Register

   type PRI is mod 2**8;
   --  Type for ARMv7-M interrupt priorities. Note that 0 is the highest
   --  priority, which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest. We assume the PRIGROUP
   --  setting is such that the 3 most significant bits determine the priority
   --  group used for preemption.

   function To_PRI (P : Integer) return PRI is
     (if P not in Interrupt_Priority then 0
      else PRI (Interrupt_Priority'Last - P + 1) * 32);
   --  Return the BASEPRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.

   function To_Priority (P : PRI) return Interrupt_Priority is
     (if P = 0 then Interrupt_Priority'Last
      else (Interrupt_Priority'Last - Any_Priority'Base (P / 32) + 1));
   --  Given an ARM interrupt priority (PRI value), determine the Ada priority
   --  While the value 0 is reserved for the kernel and has no Ada priority
   --  that represents it, Interrupt_Priority'Last is closest.

   IP : array (0 .. Interrupt_ID'Last) of PRI
       with Volatile, Address => 16#E000_E400#;

   --  Local utility functions

   procedure Enable_Interrupt_Request
     (Interrupt : Interrupt_ID;
      Prio      : Interrupt_Priority);
   --  Enable interrupt requests for the given interrupt

   procedure Trigger_Interrupt (Interrupt : Interrupt_ID);
   --  Trigger an interrupt in the NVIC.

   procedure Interrupt_Handler;
   --  Low-level interrupt handlers

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Mask interrupts
      Disable_Interrupts;

      -- Timer --

      --  The 32.768 kHz RTC0 peripheral is used as the clock source on this
      --  board. This is used instead of the SysTick timer because the "wfi"
      --  instruction (used for entering the CPU sleep mode to save power)
      --  powers down the entire CPU, *including* the SysTick.
      --  Since we still want to use "wfi" to save power whilst keeping task
      --  delays alive, we instead use the RTC0 peripheral.

      --  Start LFCLK
      --  We assume that the LFCLK source (Xtal, Rc, or Synth) has already been
      --  configured in setup_clocks.adb
      CLOCK_Periph.TASKS_LFCLKSTART := (TASKS_LFCLKSTART => 1, others => <>);

      --  Wait for LFCLK to start-up.
      loop
         exit when CLOCK_Periph.EVENTS_LFCLKSTARTED.EVENTS_LFCLKSTARTED /= 0;
      end loop;

      --  Clear event
      CLOCK_Periph.EVENTS_LFCLKSTARTED := (EVENTS_LFCLKSTARTED => 0,
                                           others              => <>);

      --  Ensure RTC is stopped.
      RTC0_Periph.TASKS_STOP := (TASKS_STOP => 1, others => <>);

      --  Set to 0 before setting TASKS_CLEAR to prevent triggering a COMPARE
      --  event.
      RTC0_Periph.CC (0).COMPARE      := 0;

      --  Clear RTC
      RTC0_Periph.TASKS_CLEAR := (TASKS_CLEAR => 1, others => <>);

      --  Run at 32.768 kHz
      RTC0_Periph.PRESCALER.PRESCALER := 0;

      --  Enable CC[0] interrupt only; TICK and OVRFLW aren't needed.
      RTC0_Periph.INTENSET.TICK       := Intenset_Tick_Field_Reset;
      RTC0_Periph.INTENSET.OVRFLW     := Intenset_Ovrflw_Field_Reset;
      RTC0_Periph.INTENSET.COMPARE    := (As_Array => False, --  Use COMPARE0
                                          Val      => 2#0001#);

      Time.Set_Alarm (Max_Timer_Interval);
      Time.Clear_Alarm_Interrupt;

      --  We do not start the timer until the handler is ready to receive the
      --  interrupt, i.e. in Install_Alarm_Handler.

      -- Interrupts --

      Install_Trap_Handler
        (Interrupt_Handler'Address, Interrupt_Request_Vector);
   end Initialize_Board;

   package body Time is

      ------------------------
      -- Max_Timer_Interval --
      ------------------------

      function Max_Timer_Interval return Timer_Interval
      is ((2**24 - 1) * BBOPA.RTC_Tick_Scaling_Factor);
      --  nRF52840 RTC is a 24-bit timer @ 32.768 kHz.
      --  Since Timer_Interval is in a scaled up unit (e.g. 65.536 kHz ticks)
      --  we need to also scaled up the 24-bit resolution.

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
         PRIMASK : Word;
         Res     : Timer_Interval;

      begin
         --  As several registers and variables need to be read or modified, do
         --  it atomically.

         Asm ("mrs %0, PRIMASK",
              Outputs => Word'Asm_Output ("=&r", PRIMASK),
              Volatile => True);
         Asm ("msr PRIMASK, %0",
              Inputs  => Word'Asm_Input  ("r", 1),
              Volatile => True);

         --  Double the value of the COUNTER register since the RTC runs at
         --  32.768 kHz, but our Timer_Interval values are in scaled up units
         --  (e.g. 65.536 kHz if RTC_Tick_Scaling_Factor is 2)
         Res := Timer_Interval (RTC0_Periph.COUNTER.COUNTER);
         Res := Res * BBOPA.RTC_Tick_Scaling_Factor;

         --  Restore interrupt mask

         Asm ("msr PRIMASK, %0",
              Inputs => Word'Asm_Input ("r", PRIMASK),
              Volatile => True);

         return BB.Time.Time (Res);
      end Read_Clock;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         --  Only clear the COMPARE event; don't clear OVRFLW here since we
         --  read (and clear) that event in Read_Clock to return the correct
         --  time when an overflow occurs.
         RTC0_Periph.EVENTS_COMPARE (0) := (EVENTS_COMPARE => 0, others => <>);
      end Clear_Alarm_Interrupt;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         RTC_Counter : UInt24;

         --  Remember that 'Ticks' is in scaled up ticks, but the RTC
         --  peripheral actually runs at 32.768 kHz.
         RTC_Ticks             : UInt24 :=
            UInt24 (Ticks / BBOPA.RTC_Tick_Scaling_Factor);

         CC0_Value             : UInt24;
         RTC_Ticks_Until_Alarm : UInt24;

      begin

         --  If the COMPARE time is within 2 RTC cycles then it might be
         --  missed due to mirroring of the RTC registers between the
         --  PCLK16M and LFCLK domains.
         --  Setting the CC[0] to COUNTER+2 is guaranteed to trigger an
         --  event. See Section 23.7 of the nRF52832 Objective Product Spec.
         RTC_Ticks := UInt24'Max (RTC_Ticks, 2);

         --  Set an interrupt to trigger after the requested number of ticks.
         RTC_Counter                := RTC0_Periph.COUNTER.COUNTER;
         CC0_Value                  := RTC_Counter + RTC_Ticks;
         RTC0_Periph.CC (0).COMPARE := CC0_Value;

         --  Note that the RTC might have ticked between reading COUNTER and
         --  setting CC[0], which may break the guarantee that CC[0] is always
         --  written as at least COUNTER+2.
         --
         --  We check for this below, and re-write CC[0] so that it is
         --  guaranteed to trigger an interrupt. We also check for the unlikely
         --  scenario that the COUNTER has gone past CC[0], in which case we
         --  trigger the interrupt immediately.
         --  This might result in an extra unecessary interrupt just before
         --  the alarm time, but ensures the alarm time is not missed.

         RTC_Counter           := RTC0_Periph.COUNTER.COUNTER;
         RTC_Ticks_Until_Alarm := CC0_Value - RTC_Counter;

         if RTC_Ticks_Until_Alarm < 2
            or Ticks = 0
            or RTC_Ticks_Until_Alarm > RTC_Ticks
         then
            CC0_Value                  := RTC_Counter + RTC_Ticks;
            RTC0_Periph.CC (0).COMPARE := CC0_Value;

            Trigger_Interrupt (Alarm_Interrupt_ID);
         end if;
      end Set_Alarm;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler
        (Handler : BB.Interrupts.Interrupt_Handler) is
      begin
         BB.Interrupts.Attach_Handler
           (Handler,
            Alarm_Interrupt_ID,
            Interrupt_Priority'Last);

         --  Clear pending timer interrupt if any
         Time.Clear_Alarm_Interrupt;

         --  Now that the interrupt handler is attached, we can start the timer
         RTC0_Periph.TASKS_START := (TASKS_START => 1, others => <>);
      end Install_Alarm_Handler;
   end Time;

   package body Multiprocessors is separate;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      Id : Interrupt_ID;
      Res : Word;
      PRIMASK : Word;
   begin
      Asm ("mrs %0, PRIMASK",
           Outputs => Word'Asm_Output ("=&r", PRIMASK),
           Volatile => True);
      Asm ("msr PRIMASK, %0",
           Inputs  => Word'Asm_Input  ("r", 1),
           Volatile => True);

      --  The exception number is read from the IPSR

      Asm ("mrs %0, ipsr",
           Word'Asm_Output ("=r", Res),
           Volatile => True);

      Res := Res and 16#FF#;

      --  Convert it to IRQ number by substracting 16 (number of cpu
      --  exceptions).

      Id := Interrupt_ID'Base (Res) - 16;

      Interrupt_Wrapper (Id);

      --  Restore interrupt mask

      Asm ("msr PRIMASK, %0",
           Inputs => Word'Asm_Input ("r", PRIMASK),
           Volatile => True);
   end Interrupt_Handler;

   ------------------------------
   -- Enable_Interrupt_Request --
   ------------------------------

   procedure Enable_Interrupt_Request
     (Interrupt : Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
   begin
      if Interrupt = Alarm_Interrupt_ID then

         --  Consistency check with Priority_Of_Interrupt

         pragma Assert (Prio = Interrupt_Priority'Last);

         Time.Clear_Alarm_Interrupt;
      end if;

      declare
         pragma Assert (Interrupt >= 0);
         IRQ    : constant Natural := Interrupt;
         Regofs : constant Natural := IRQ / 32;
         Regbit : constant Word := 2** (IRQ mod 32);
         NVIC_ISER : array (0 .. 15) of Word
           with Volatile, Address => NVIC_ISER0;

         --  Many NVIC registers use 16 words of 32 bits each to serve as a
         --  bitmap for all interrupt channels. Regofs indicates register
         --  offset (0 .. 15), and Regbit indicates the mask required for
         --  addressing the bit.

      begin
         NVIC_ISER (Regofs) := Regbit;
      end;
   end Enable_Interrupt_Request;

   -----------------------
   -- Trigger_Interrupt --
   -----------------------
   procedure Trigger_Interrupt (Interrupt : Interrupt_ID) is
   begin
      NVIC_STIR := Word (Interrupt) and 16#0000_01FF#;
   end Trigger_Interrupt;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
      begin
         if Interrupt /= Alarm_Interrupt_ID then
            IP (Interrupt) := To_PRI (Prio);
         end if;

         Enable_Interrupt_Request (Interrupt, Prio);
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : Interrupt_ID) return Any_Priority
      is
         (if Interrupt = Alarm_Interrupt_ID then Interrupt_Priority'Last
         else To_Priority (IP (Interrupt)));

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         Asm ("wfi", Volatile => True);
      end Power_Down;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         --  Writing a 0 to BASEPRI disables interrupt masking, while values
         --  15 .. 1 correspond to interrupt priorities 255 .. 241 in that
         --  order.

         Asm ("msr BASEPRI, %0",
              Inputs => PRI'Asm_Input ("r", To_PRI (Priority)),
              Volatile => True);
      end Set_Current_Priority;
   end Interrupts;
end System.BB.Board_Support;
