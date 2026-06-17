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
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Interfaces.NRF51;       use Interfaces.NRF51;
with Interfaces.NRF51.RTC;   use Interfaces.NRF51.RTC;
with Interfaces.NRF51.CLOCK; use Interfaces.NRF51.CLOCK;

package body System.BB.Board_Support is
   use CPU_Primitives, BB.Interrupts, Machine_Code, Time;

   package BBOPA renames System.BB.Board_Parameters;

   Interrupt_Request_Vector : constant Vector_Id := 16;
   --  See vector definitions in ARMv7-M version of System.BB.CPU_Primitives.
   --  Defined by ARMv7-M specifications.

   Alarm_Time : Time.Timer_Interval;
   pragma Volatile (Alarm_Time);
   pragma Export (C, Alarm_Time, "__gnat_alarm_time");

   Alarm_Interrupt_ID : constant Interrupt_ID := 11;

   -------------------
   -- RTC0 Handling --
   -------------------

   Tick_Period : constant Time.Timer_Interval :=
     BBOPA.Clock_Frequency / (32_768 / (BBOPA.RTC_Prescaler + 1));

   Next_Tick_Time : Timer_Interval with Volatile;
   --  Time when systick will expire. This gives the high digits of the time

   ----------------------------------------------
   -- New Vectored Interrupt Controller (NVIC) --
   ----------------------------------------------

   NVIC_Base : constant := 16#E000_E000#;
   --  Nested Vectored Interrupt Controller (NVIC) base.

   NVIC_ISER0 : constant Address := NVIC_Base + 16#100#;
   --  Writing a bit mask to this register enables the corresponding interrupts

   type PRI is mod 2**8;
   --  Type for ARMv7-M interrupt priorities. Note that 0 is the highest
   --  priority, which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest. We assume the PRIGROUP
   --  setting is such that the 4 most significant bits determine the priority
   --  group used for preemption. However, if less bits are implemented, this
   --  should still work.

   function To_PRI (P : Integer) return PRI is
     (if P not in Interrupt_Priority then 0
      else PRI (Interrupt_Priority'Last - P + 1) * 16);
   --  Return the BASEPRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.

   function To_Priority (P : PRI) return Interrupt_Priority is
     (if P = 0 then Interrupt_Priority'Last
      else (Interrupt_Priority'Last - Any_Priority'Base (P / 16) + 1));
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

      --  Configure the low frequency clock required for RTC0
      CLOCK_Periph.LFCLKSRC.SRC := Rc; -- Use internal RC oscillator

      --  Start the low frequency clock
      CLOCK_Periph.TASKS_LFCLKSTART := 1;

      --  Wait until the low frequency clock is started
      while CLOCK_Periph.EVENTS_LFCLKSTARTED = 0 loop
         null;
      end loop;

      --  We run the counter at 32.768KHz
      RTC0_Periph.PRESCALER.PRESCALER := BBOPA.RTC_Prescaler;

      RTC0_Periph.INTENSET.TICK := Set;
      RTC0_Periph.EVTENSET.TICK := Set;

      Next_Tick_Time := Tick_Period;
      Time.Set_Alarm (Timer_Interval'Last);
      Time.Clear_Alarm_Interrupt;

      --  We do not start the timer until the handler is ready to receive the
      --  interrupt, i.e. in Install_Alarm_Handler.

      -- Interrupts --

      Install_Trap_Handler
        (Interrupt_Handler'Address, Interrupt_Request_Vector);
   end Initialize_Board;

   package body Time is

      Upper_Alarm_Handler : BB.Interrupts.Interrupt_Handler := null;

      procedure Pre_Alarm_Handler (Id : Interrupt_ID);

      -----------------------
      -- Pre_Alarm_Handler --
      -----------------------

      procedure Pre_Alarm_Handler (Id : Interrupt_ID) is
      begin
         Next_Tick_Time := Next_Tick_Time + Tick_Period;

         pragma Assert (Upper_Alarm_Handler /= null);

         Upper_Alarm_Handler (Id);
      end Pre_Alarm_Handler;

      ------------------------
      -- Max_Timer_Interval --
      ------------------------

      function Max_Timer_Interval return Timer_Interval is (2**32 - 1);

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

         Res := Next_Tick_Time;

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
         RTC0_Periph.EVENTS_TICK := 0;
      end Clear_Alarm_Interrupt;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         Now : constant Timer_Interval := Timer_Interval (Read_Clock);

      begin
         --  As we will have periodic interrupts for alarms regardless, the
         --  only thing to do is force an interrupt if the alarm has already
         --  expired.

         Alarm_Time :=
           Now + Timer_Interval'Min (Timer_Interval'Last / 2, Ticks);

         --  FIXME: We can't do that with RTC0
         --  if Ticks = 0 then
         --     ICSR := ICSR_Pend_ST_Set;
         --  end if;
      end Set_Alarm;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler
        (Handler : BB.Interrupts.Interrupt_Handler) is
      begin
         pragma Assert (Upper_Alarm_Handler = null);

         Upper_Alarm_Handler := Handler;

         BB.Interrupts.Attach_Handler
           (Pre_Alarm_Handler'Access,
            Alarm_Interrupt_ID,
            Interrupt_Priority'Last);

         --  Clear pending timer interrupt if any
         Time.Clear_Alarm_Interrupt;

         --  Now that the interrupt handler is attached, we can start the timer
         RTC0_Periph.TASKS_START := 1;
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
