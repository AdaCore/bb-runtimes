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

with System.Machine_Code;

with System.BB.CPU_Primitives;
with System.Multiprocessors;   use System.Multiprocessors;

with Interfaces.RP2040;       use Interfaces.RP2040;
with Interfaces.RP2040.TIMER; use Interfaces.RP2040.TIMER;

package body System.BB.Board_Support is
   use CPU_Primitives, BB.Interrupts, Machine_Code, Time;

   Num_IRQs : constant := 32;
   --  Number of IRQ lines per core.

   type IRQ_ID is range 0 .. Num_IRQs - 1;
   --  Interrupt_ID ranges from 0 .. 63 to model two sets of
   --  32 IRQs (32 for core0 and 32 for core1).
   --
   --  This type represents the IRQ number on a single core.

   subtype Core0_Interrupt_ID is Interrupt_ID range 0  .. 31;
   subtype Core1_Interrupt_ID is Interrupt_ID range 32 .. 63;

   Interrupt_Request_Vector : constant Vector_Id := 16;
   --  See vector definitions in ARMv6-M version of System.BB.CPU_Primitives.
   --  Defined by ARMv6-M specifications.

   Alarm_Time : Time.Timer_Interval;
   pragma Volatile (Alarm_Time);
   pragma Export (C, Alarm_Time, "__gnat_alarm_time");

   Alarm_Interrupt_ID : constant Interrupt_ID := 3; --  TIMER_IRQ_3 (core0)

   ----------------------------------------------
   -- New Vectored Interrupt Controller (NVIC) --
   ----------------------------------------------

   NVIC_Base : constant := 16#E000_E000#;
   --  Nested Vectored Interrupt Controller (NVIC) base.

   NVIC_ISER0 : constant Address := NVIC_Base + 16#100#;
   --  Writing a bit mask to this register enables the corresponding interrupts

   NVIC_ISPR0 : constant Address := NVIC_Base + 16#200#;

   NVIC_ISER : Word
     with Volatile, Address => NVIC_ISER0;
   --  NVIC Interrupt Set-Enable Register (ISER)

   NVIC_ISPR : Word
     with Volatile, Address => NVIC_ISPR0;
   --  NVIC Interrupt Set Pending Register (ISPR)

   type PRI is mod 2**8;
   --  Type for ARMv6-M interrupt priorities. Note that 0 is the highest
   --  priority, which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest.
   --
   --  On the RP2040 he PRIGROUP setting is such that the 2 most significant
   --  bits determine the priority group used for preemption.

   function To_PRI (P : Integer) return PRI is
     (if P not in Interrupt_Priority then 0
      else PRI (Interrupt_Priority'Last - P + 1) * 64);
   --  Return the BASEPRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.

   function To_Priority (P : PRI) return Interrupt_Priority is
     (if P = 0 then Interrupt_Priority'Last
      else (Interrupt_Priority'Last - Any_Priority'Base (P / 64) + 1));
   --  Given an ARM interrupt priority (PRI value), determine the Ada priority
   --  While the value 0 is reserved for the kernel and has no Ada priority
   --  that represents it, Interrupt_Priority'Last is closest.

   function Priority_Of_IRQ (IRQ : IRQ_ID) return Any_Priority;
   --  Get the priority of the specified IRQ on the current core.

   function To_Core1_IRQ (Interrupt : Core1_Interrupt_ID) return IRQ_ID
   is (IRQ_ID (Interrupt - Core1_Interrupt_ID'First));
   --  Map an Interrupt_ID to the corresponding IRQ line for core1.

   function To_Core0_IRQ (Interrupt : Core0_Interrupt_ID) return IRQ_ID
   is (IRQ_ID (Interrupt - Core0_Interrupt_ID'First));
   --  Map an Interrupt_ID to the corresponding IRQ line for core0.

   function To_Interrupt_ID_Core0 (IRQ : IRQ_ID) return Interrupt_ID
   is (Core0_Interrupt_ID'First + Interrupt_ID (IRQ));
   --  Map an IRQ line on core0 to the corresponding Interrupt_ID

   function To_Interrupt_ID_Core1 (IRQ : IRQ_ID) return Interrupt_ID
   is (Core1_Interrupt_ID'First + Interrupt_ID (IRQ));
   --  Map an IRQ line on core1 to the corresponding Interrupt_ID

   type PRI_Array is array (IRQ_ID) of PRI;

   IP : PRI_Array with Volatile, Address => 16#E000_E400#;

   IP_Core1 : PRI_Array := (others => 0);
   --  Interrupt priorities for core1 are temporarily stored here during
   --  elaboration (from the environment thread on core0). When core1 is
   --  started it loads these priorities into its IP registers at startup.

   ISER_Core1 : Word := 0;
   --  Same as IP_Core1, but for the ISER register.

   --  Local utility functions

   procedure Enable_Interrupt_Request
     (IRQ  : IRQ_ID;
      Prio : Interrupt_Priority);
   --  Enable interrupt requests for the given interrupt on the current core.

   procedure Set_Pending_Interrupt (IRQ : IRQ_ID);
   --  Change interrupt state to pending on the current core.

   procedure Interrupt_Handler;
   --  Low-level interrupt handlers

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Mask interrupts
      Disable_Interrupts;

      Time.Set_Alarm (Max_Timer_Interval);
      Time.Clear_Alarm_Interrupt;

      Install_Trap_Handler
        (Interrupt_Handler'Address, Interrupt_Request_Vector);

      Enable_Interrupts (Priority'Last);
   end Initialize_Board;

   package body Time is
      ------------------------
      -- Max_Timer_Interval --
      ------------------------

      function Max_Timer_Interval return Timer_Interval is (2**32 - 1);

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
      begin
         --  Raw read from bits 31:0 of time (no side effects)
         return BB.Time.Time (TIMER_Periph.TIMERAWL);
      end Read_Clock;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         TIMER_Periph.INTF.ALARM_3 := 0; --  Remove forced interrupt.
         TIMER_Periph.INTR.ALARM_3 := 1; --  Acknowledge interrupt
      end Clear_Alarm_Interrupt;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         Now : UInt32 := TIMER_Periph.TIMERAWL;

      begin
         --  Set the alarm time.

         Alarm_Time := Timer_Interval (Now) + Ticks;

         TIMER_Periph.ALARM3 := UInt32 (Alarm_Time);

         --  Check if the alarm time passed while we were configuring it.
         --  This may happen for very small delays (e.g. Ticks = 1).

         Now := TIMER_Periph.TIMERAWL;
         if Alarm_Time - Timer_Interval (Now) > Ticks then
            TIMER_Periph.INTF.ALARM_3 := 1; --  Force alarm interrupt.
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

         --  Enable interrupt
         NVIC_ISER                 := NVIC_ISER or 2**Alarm_Interrupt_ID;
         TIMER_Periph.INTE.ALARM_3 := 1;
      end Install_Alarm_Handler;
   end Time;

   package body Multiprocessors is separate;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      use System.BB.Board_Support.Multiprocessors;

      IRQ       : IRQ_ID;
      Interrupt : Interrupt_ID;
      Res       : Word;

   begin
      --  The exception number is read from the IPSR

      Asm ("mrs %0, ipsr",
           Word'Asm_Output ("=r", Res),
           Volatile => True);

      Res := Res and 16#FF#;

      --  Convert it to IRQ number by substracting 16 (number of cpu
      --  exceptions).

      IRQ := IRQ_ID (Res - 16);

      --  Map to the corresponding interrupt number depending on
      --  the CPU on which the interrupt is executing.

      case Current_CPU is
         when 1 =>
            Interrupt := To_Interrupt_ID_Core0 (IRQ);
         when 2 =>
            Interrupt := To_Interrupt_ID_Core1 (IRQ);
      end case;

      Interrupt_Wrapper (Interrupt);

   end Interrupt_Handler;

   ------------------------------
   -- Enable_Interrupt_Request --
   ------------------------------

   procedure Enable_Interrupt_Request
     (IRQ  : IRQ_ID;
      Prio : Interrupt_Priority)
   is
   begin
      if IRQ = To_Core0_IRQ (Alarm_Interrupt_ID) then

         --  Consistency check with Priority_Of_Interrupt

         pragma Assert (Prio = Interrupt_Priority'Last);

         Time.Clear_Alarm_Interrupt;
      else
         declare
            Regbit : constant Word := 2**Natural (IRQ);

         begin
            NVIC_ISER := NVIC_ISER or Regbit;
         end;
      end if;
   end Enable_Interrupt_Request;

   -----------------------------
   -- Set_Pending_Interrupt --
   -----------------------------

   procedure Set_Pending_Interrupt (IRQ : IRQ_ID) is
   begin
      NVIC_ISPR := 2**Natural (IRQ);
   end Set_Pending_Interrupt;

   ---------------------
   -- Priority_Of_IRQ --
   ---------------------

   function Priority_Of_IRQ (IRQ : IRQ_ID) return Any_Priority is
     (if System.BB.Board_Support.Multiprocessors.Current_CPU = 1 and
         IRQ = To_Core0_IRQ (Alarm_Interrupt_ID)
      then Interrupt_Priority'Last
      else To_Priority (IP (IRQ)));

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         use System.BB.Board_Support.Multiprocessors;

         IRQ : IRQ_ID;

      begin
         --  Ravenscar runtimes do not permit dynamic attachment
         --  of interrupt handlers, so all handlers should be installed
         --  by the environment task during elaboration (on core0).

         pragma Assert (Current_CPU = 1);

         if Interrupt in Core1_Interrupt_ID then
            --  core0 cannot directly access core1's NVIC, so the register
            --  configuration for core1 is cached in shared RAM. core1 will
            --  read these values to configure its own NVIC when it is started
            --  later, after elaboration (see Core1_Entry in s-bbsumu.adb).

            IRQ := To_Core1_IRQ (Interrupt);

            IP_Core1 (IRQ) := To_PRI (Prio);
            ISER_Core1     := 2**Natural (IRQ);

         elsif Interrupt in Core0_Interrupt_ID then
            --  core0 can configure its own NVIC directly.

            IRQ := To_Core0_IRQ (Interrupt);

            if Interrupt /= Alarm_Interrupt_ID then
               IP (IRQ) := To_PRI (Prio);
            end if;

            Enable_Interrupt_Request (IRQ, Prio);
         end if;
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : Interrupt_ID) return Any_Priority
      is
         (if Interrupt in Core0_Interrupt_ID
          then Priority_Of_IRQ (To_Core0_IRQ (Interrupt))
          else Priority_Of_IRQ (To_Core1_IRQ (Interrupt)));

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         --  Enable interrupts before sleeping.
         --
         --  This is important on core1 where interrupts are kept disabled
         --  during kernel initialization (__gnat_initialize_slave)
         --  until the Idle task start. This prevents poke interrupts from
         --  core0 being processed before the kernel is fully initialized.

         Enable_Interrupts (Priority'Last);
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
