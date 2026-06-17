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
with System.BB.Board_Parameters;
with System.BB.CPU_Primitives;
with Interfaces;
with Interfaces.M1AGL.CoreTimer;     use Interfaces.M1AGL.CoreTimer;
with Interfaces.M1AGL.CoreInterrupt; use Interfaces.M1AGL.CoreInterrupt;
with Interfaces.M1AGL;

package body System.BB.Board_Support is
   use CPU_Primitives, BB.Interrupts, Machine_Code, Time, Interfaces;

   Interrupt_Request_Vector : constant Vector_Id := 16;
   --  See vector definitions in ARMv6-M version of System.BB.CPU_Primitives.
   --  Defined by ARMv6-M specifications.

   ----------------------------------------------
   -- New Vectored Interrupt Controller (NVIC) --
   ----------------------------------------------

   NVIC_Base : constant := 16#E000_E000#;
   --  Nested Vectored Interrupt Controller (NVIC) base.

   NVIC_ISER : Word with Volatile, Address => NVIC_Base + 16#100#;
   --  Writing a bit mask to this register enables the corresponding interrupts

   Alarm_Time : Time.Timer_Interval;
   pragma Volatile (Alarm_Time);
   pragma Export (C, Alarm_Time, "__gnat_alarm_time");

   Alarm_Interrupt_ID : constant Interrupt_ID := 2;
   --  Return the interrupt level to use for the alarm clock handler.

   ----------------------------
   -- Test and set intrinsic --
   ----------------------------

   --  The run-time package System.Multiprocessors.Spin_Locks is based on the
   --  implementation of test and set intrinsic. The Cortex-M1 architecture
   --  does not implement the machine instructions LDREX/STREX so these
   --  intrinsic functions are not available. This is only a problem at link
   --  time since the Spin_Locks are only used in multi-processor systems,
   --  which is not the case here.
   --
   --  To workaround the linker error, we export here the required intrinsic
   --  functions. As explained above, they should never be executed so the body
   --  of the functions only raises an Program_Error.

   function Lock_Test_And_Set
     (Ptr   : access Unsigned_8;
      Value : Unsigned_8)
     return Unsigned_8;
   pragma Export (C, Lock_Test_And_Set, "__sync_lock_test_and_set_1");

   procedure Lock_Release (Ptr : access Unsigned_8);
   pragma Export (C, Lock_Release, "__sync_lock_release");

   -----------------------
   -- Lock_Test_And_Set --
   -----------------------

   function Lock_Test_And_Set
     (Ptr   : access Unsigned_8;
      Value : Unsigned_8)
     return Unsigned_8
   is
   begin
      raise Program_Error;
      return 0;
   end Lock_Test_And_Set;

   ------------------
   -- Lock_Release --
   ------------------

   procedure Lock_Release (Ptr : access Unsigned_8) is
   begin
      raise Program_Error;
   end Lock_Release;

   --------------------
   -- Timer Handling --
   --------------------

   --  We use the Microsemi CoreTime as a periodic timer with 1 kHz rate. This
   --  is a trade-off between accurate delays, limited overhead and maximum
   --  time that interrupts may be disabled.

   Tick_Period : constant Time.Timer_Interval :=
     System.BB.Board_Parameters.Timer_Frequency / 1000;

   RVR_Last       : constant := 2**24 - 1;
   pragma Assert (Tick_Period <= RVR_Last + 1);

   Next_Tick_Time : Timer_Interval with Volatile;
   --  Time when systick will expire. This gives the high digits of the time

   procedure Interrupt_Handler;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Mask interrupts
      Disable_Interrupts;

      -- Time --

      --  Configure CoreTimer
      CoreTimer_Periph.Control.Enable := False;
      CoreTimer_Periph.Control.Interrupt_Enable := True;
      CoreTimer_Periph.Control.Timer_Mode := Continuous;

      CoreTimer_Periph.Prescale.Value := Divide_By_2;

      CoreTimer_Periph.Load_Value :=
        Interfaces.M1AGL.UInt32 (Tick_Period - 1);

      --  We do not enable the timer until the handler is ready to receive the
      --  interrupt, i.e. in Install_Alarm_Handler.

      Next_Tick_Time := Tick_Period;
      Time.Set_Alarm (Timer_Interval'Last);
      Time.Clear_Alarm_Interrupt;

      -- Interrupts --

      Install_Trap_Handler
        (Interrupt_Handler'Address, Interrupt_Request_Vector);

      --  On the Microsemi Cortex-M1 only one IRQ line of the NVIC is used, the
      --  real interrupt handling is done by another device: CoreInterrupt. So
      --  on the NVIC, we always enable the IRQ corresponding to
      --  Core_Interrupt.
      NVIC_ISER := 1;
   end Initialize_Board;

   package body Time is

      Upper_Alarm_Handler : BB.Interrupts.Interrupt_Handler := null;

      procedure Pre_Alarm_Handler (Id : Interrupt_ID);

      -----------------------
      -- Pre_Alarm_Handler --
      -----------------------

      procedure Pre_Alarm_Handler (Id : Interrupt_ID) is
      begin
         --  Next_Tick_Time is usually updated in Read_Clock, but we can't do
         --  that for CoreTimer because there's no way to know if we are

         --  We need to update the Next_Tick_Time before calling the s.bb.time
         --  alarm handler executes because it expects

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
         Flag    : Boolean;
         Count   : Timer_Interval;
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

         --  We must read the counter register before the flag

         Count := Timer_Interval (CoreTimer_Periph.Current_Value);

         --  If we read the flag first, a reload can occur just after the read
         --  and the count register would wrap around. We'd end up with a Count
         --  value close to the Tick_Period value but a flag at zero and
         --  therefore miss the reload and return a wrong clock value.

         --  This flag is set when the counter has reached zero. Next_Tick_Time
         --  has to be incremented. This will trigger an interrupt very soon
         --  (or has just triggered the interrupt), so count is either zero or
         --  not far from Tick_Period.

         Flag := CoreTimer_Periph.Raw_Interrupt_Status.Pending;

         if Flag then

            --  CoreTimer counter has just reached zero, pretend it is still
            --  zero.

            Res := Next_Tick_Time;

            --  The following is not applicable to CoreTimer, so we increase
            --  Next_Tick_Time in the alarm handler (see below):
            --  Note that reading the Control and Status
            --  register (SYST_CSR) clears the COUNTFLAG bit, so even if we
            --  have sequential calls to this function, the increment of
            --  Next_Tick_Time will happen only once.
            --  Next_Tick_Time := Next_Tick_Time + Tick_Period;

         else
            --  The counter is decremented, so compute the actual time

            Res := Next_Tick_Time - Count;
         end if;

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
         --  Any write to this register will clear the interrupt
         CoreTimer_Periph.Interrupt_Clear := 42;
      end Clear_Alarm_Interrupt;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         Now : constant Timer_Interval := Timer_Interval (Read_Clock);

      begin
         Alarm_Time :=
           Now + Timer_Interval'Min (Timer_Interval'Last / 2, Ticks);

         --  As we will have periodic interrupts for alarms regardless, the
         --  only thing to do is force an interrupt if the alarm has already
         --  expired.
         --  NOTE: We can't do this with the CoreTimer

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

         --  Now we are ready to handle the CoreTimer interrupt

         CoreTimer_Periph.Control.Enable := True;

      end Install_Alarm_Handler;
   end Time;

   package body Multiprocessors is separate;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      Status : constant IRQ_Status_IRQ_Field_Array :=
        CoreInterrupt_Periph.IRQ_Status.IRQ.Arr;
   begin

      --  For each interrupt
      for Index in Status'Range loop

         --  Check if the interrupt is flagged
         if Status (Index) then

            --  Call the wrapper
            Interrupt_Wrapper (Index);
         end if;
      end loop;
   end Interrupt_Handler;

   package body Interrupts is

      procedure Enable_Interrupt_Request
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority);
      --  Enable interrupt requests for the given interrupt

      ------------------------------
      -- Enable_Interrupt_Request --
      ------------------------------

      procedure Enable_Interrupt_Request
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
      begin
         CoreInterrupt_Periph.IRQ_Enable.IRQ.Arr (Interrupt) := True;
      end Enable_Interrupt_Request;

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
      begin
         Enable_Interrupt_Request (Interrupt, Prio);
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : Interrupt_ID) return Any_Priority
      is (Interrupt_Priority'Last);

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
         --  There's no interrupt priority support on this platform
         null;
      end Set_Current_Priority;
   end Interrupts;
end System.BB.Board_Support;
