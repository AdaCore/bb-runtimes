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

--  This is the TI AM64x/AM263x Arm Cortex-R5 version of this package.
--
--  The package uses the following hardware resources:
--
--    * The Global Timebase Counter (GTC) as the source for Clock;
--    * Timers 0 and 1 as the alarm source for R5FSS0 CPU 0 and 1 respectively;
--    * Timers 2 and 3 as the alarm source for R5FSS1 CPU 0 and 1 respectively;
--    * The local VIM controller for interrupt support.
--
--  The timers are configured to use HFOSC0_CLKOUT as their clock source while
--  the GTC uses the default MAIN_PLL2_HSDIV5_CLKOUT source.

with Interfaces;
with System.Machine_Code;
with System.BB.Parameters;
with System.TI.Vectored_Interrupt_Manager;

package body System.BB.Board_Support is
   use BB.Interrupts, BB.Time, Parameters;
   use Interfaces;
   use System.Machine_Code, System.Multiprocessors;
   use Time;

   --------------------------
   -- Hardware Definitions --
   --------------------------

   Number_CPU_Clusters : constant := 2;
   --  Number of CPU clusters for our chip

   Number_Of_Timers : constant := 12;
   --  Number of timers on the SoC

   type Timer_ID is mod Number_Of_Timers;
   --  ID for each timer on the SoC

   ------------------------
   -- Hardware Registers --
   ------------------------

   type MPIDR is record
      Aff2 : Unsigned_8;
      Aff1 : Unsigned_8;
      Aff  : Unsigned_8;
   end record with Size => 32;
   --  Multiprocessor Affinity Register

   for MPIDR use record
      Aff2 at 0 range 16 .. 23;
      Aff1 at 0 range 8  .. 15;
      Aff  at 0 range 0  .. 7;
   end record;

   function MPIDR_Register return MPIDR;
   --  Return the contents of the Multiprocessor Affinity Register

   --------------------
   -- Control Module --
   --------------------

   --  See TI AM64x/AM243x Technical Reference Manual, 5.1 Control Module for
   --  details

   Control_Module_Base_Address : constant := 16#4300_0000#;
   --  Base address of the Control Module

   --  Control Module types

   type Timer_Clock_Source is (HFOSC0_CLKOUT);
   for Timer_Clock_Source use (HFOSC0_CLKOUT => 0);

   type CTRLMMR_TIMERx_CLKSEL is record
      Clock : Timer_Clock_Source;
   end record;

   for CTRLMMR_TIMERx_CLKSEL use record
      Clock at 0 range 0 .. 3;
   end record;

   --  Control Module Registers

   Timer_Clock_Selection : array (Timer_ID'Range) of CTRLMMR_TIMERx_CLKSEL
      with Volatile_Components,
           Address =>
             System'To_Address (Control_Module_Base_Address + 16#81B0#);
   --  CTRLMMR_TIMERx_CLKSEL registers

   -----------------------------
   -- Global Timebase Counter --
   -----------------------------

   --  See TI AM64x/AM243x Technical Reference Manual, 12.5.1 Global Timebase
   --  Counter for details.

   GTC_Base_Address : constant := 16#00A9_0000#;
   --  Base address of the GTC module

   --  GTC types

   type Requested_Frequency is mod 2 ** 24;

   type Enable_Bit is (Disable, Enable);
   for Enable_Bit use (Disable => 0, Enable => 1);

   type GTC_Counter_Control is record
      Frequency_Change_Request : Requested_Frequency;
      Halt_On_Debug            : Boolean;
      Enable_System_Counter    : Boolean;
   end record with Size => 32;

   for GTC_Counter_Control use record
      Frequency_Change_Request at 0 range 8 .. 31;
      Halt_On_Debug            at 0 range 1 .. 1;
      Enable_System_Counter    at 0 range 0 .. 0;
   end record;

   --  GTC registers

   GTC_Counter_Control_Register : GTC_Counter_Control
     with Address => System'To_Address (GTC_Base_Address + 16#0#),
          Volatile_Full_Access;
   --  GTC0_CNTCR Register

   GTC_Counter_Low_Register : Unsigned_32
     with Address => System'To_Address (GTC_Base_Address + 16#8#);
   --  GTC0_LO Register

   GTC_Counter_High_Register : Unsigned_32
     with Address => System'To_Address (GTC_Base_Address + 16#C#);
   --  GTC0_HI Register

   ------------
   -- Timers --
   ------------

   --  See TI AM64x/AM243x Technical Reference Manual, 12.5.3 Timers for
   --  details.

   Timers_Base_Address : constant := 16#240_0000#;
   --  Base address of the Timer modules

   TIMER0_INTR_PEND_0 : constant Interrupt_ID := 152;
   --  Interrupt ID of Timer 0. The other timers are number sequentially. We
   --  cannot use Ada.Interrupts.Names here since Ada.Interrupts is not
   --  Preelaborate.

   --  Timer types

   type Timer_Idle_Mode is
     (Force_Idle, No_Idle, Smart_Idle, Smart_Idle_Wake_Up);
   for Timer_Idle_Mode use
     (Force_Idle => 0, No_Idle => 1, Smart_Idle => 2, Smart_Idle_Wake_Up => 3);

   type Timer_Emulation_Mode is (Frozen, Free);
   for Timer_Emulation_Mode use (Frozen => 0, Free => 1);

   type TIMER_TIOCP_CFG is record
      Idle_Mode      : Timer_Idle_Mode;
      Emulation_Mode : Timer_Emulation_Mode;
      Software_Reset : Boolean;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_TIOCP_CFG use record
      Idle_Mode      at 0 range 2 .. 3;
      Emulation_Mode at 0 range 1 .. 1;
      Software_Reset at 0 range 0 .. 0;
   end record;

   type Interrupt_Line is range 0 .. 0;

   type TIMER_IRQ_EOI is record
      Line_Number : Interrupt_Line;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_IRQ_EOI use record
      Line_Number at 0 range 0 .. 0;
   end record;

   type Timer_GPIO_Config is (Output, Input);
   for Timer_GPIO_Config use (Output => 0, Input => 1);

   type Timer_Capture_Mode is (Single, Double);
   for Timer_Capture_Mode use (Single => 0, Double => 1);

   type Timer_Pulse_Toggle_Mode is (Pulse_Modulation, Toggle_Modulation);
   for Timer_Pulse_Toggle_Mode use
     (Pulse_Modulation => 0, Toggle_Modulation => 1);

   type Timer_Trigger_Mode is (No_Trigger, On_Overflow, On_Overflow_And_Match);
   for Timer_Trigger_Mode use
     (No_Trigger => 0, On_Overflow => 1, On_Overflow_And_Match => 2);

   type Timer_Transition_Capture_Mode is
     (No_Capture, Rising_Edge, Falling_Edge, Both_Edges);
   for Timer_Transition_Capture_Mode use
     (No_Capture => 0, Rising_Edge => 1, Falling_Edge => 2, Both_Edges => 3);

   type Timer_PWM_Output_Pusle is (Positive, Negative);
   for Timer_PWM_Output_Pusle use (Positive => 0, Negative => 1);

   type Timer_Prescale is mod 2 ** 3;

   type Timer_Control is (Stop, Start);
   for Timer_Control use (Stop => 0, Start => 1);

   type TIMER_TCLR is record
      GPO_Config               : Timer_GPIO_Config;
      Capture_Mode             : Timer_Capture_Mode;
      Output_Pulse_Toggle_Mode : Timer_Pulse_Toggle_Mode;
      Output_Trigger_Mode      : Timer_Trigger_Mode;
      Transition_Capture_Mode  : Timer_Transition_Capture_Mode;
      PWM_Output_Pusle         : Timer_PWM_Output_Pusle;
      Compare_Enable           : Boolean;
      Prescaler_Enable         : Boolean;
      Prescale_Timer_Value     : Timer_Prescale;
      Autoreload               : Boolean;
      Start_Stop               : Timer_Control;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_TCLR use record
      GPO_Config               at 0 range 14 .. 14;
      Capture_Mode             at 0 range 13 .. 13;
      Output_Pulse_Toggle_Mode at 0 range 12 .. 12;
      Output_Trigger_Mode      at 0 range 10 .. 11;
      Transition_Capture_Mode  at 0 range 8 .. 9;
      PWM_Output_Pusle         at 0 range 7 .. 7;
      Compare_Enable           at 0 range 6 .. 6;
      Prescaler_Enable         at 0 range 5 .. 5;
      Prescale_Timer_Value     at 0 range 2 .. 4;
      Autoreload               at 0 range 1 .. 1;
      Start_Stop               at 0 range 0 .. 0;
   end record;

   type Pending_Clear_Bit is (No_Event, Pending_Clear);
   for Pending_Clear_Bit use (No_Event => 0, Pending_Clear => 1);

   type TIMER_IRQSTATUS is record
      Compare  : Pending_Clear_Bit;
      Overflow : Pending_Clear_Bit;
      Match    : Pending_Clear_Bit;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_IRQSTATUS use record
      Compare  at 0 range 2 .. 2;
      Overflow at 0 range 1 .. 1;
      Match    at 0 range 0 .. 0;
   end record;

   type TIMER_IRQSTATUS_SET is record
      Compare  : Enable_Bit;
      Overflow : Enable_Bit;
      Match    : Enable_Bit;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_IRQSTATUS_SET use record
      Compare  at 0 range 2 .. 2;
      Overflow at 0 range 1 .. 1;
      Match    at 0 range 0 .. 0;
   end record;

   type Timer_Read_Mode is (Posted, Non_Posted);
   for Timer_Read_Mode use (Posted => 0, Non_Posted => 1);

   type TIMER_TSICR is record
      Read_After_Ide   : Boolean;
      Read_Mode        : Timer_Read_Mode;
      Posted           : Enable_Bit;
      Reset_Posted_Bit : Boolean;
   end record with Size => 32, Volatile_Full_Access;

   for TIMER_TSICR use record
      Read_After_Ide   at 0 range 4 .. 4;
      Read_Mode        at 0 range 3 .. 3;
      Posted           at 0 range 2 .. 2;
      Reset_Posted_Bit at 0 range 1 .. 1;
   end record;

   pragma Warnings (Off, "*bits of*");
   --  Suppress warning of unused bits in Timer. The unused bits is
   --  expected for the Timers module as each module is placed at 16#1_0000#
   --  offsets.

   type Timer is record
      CBASS0_Configuration_Register          : TIMER_TIOCP_CFG;
      End_Of_Interrupt_Register              : TIMER_IRQ_EOI;
      Interrupt_Status_Register              : TIMER_IRQSTATUS;
      Interrupt_Enable_Register              : TIMER_IRQSTATUS_SET;
      Control_Register                       : TIMER_TCLR;
      Counter_Register                       : Unsigned_32;
      Compare_Register                       : Unsigned_32;
      Synchronous_Interface_Control_Register : TIMER_TSICR;
   end record with Size => 16#1_0000# * Storage_Unit;

   for Timer use record
      CBASS0_Configuration_Register          at 16#10# range 0 .. 31;
      End_Of_Interrupt_Register              at 16#20# range 0 .. 31;
      Interrupt_Status_Register              at 16#28# range 0 .. 31;
      Interrupt_Enable_Register              at 16#2C# range 0 .. 31;
      Control_Register                       at 16#38# range 0 .. 31;
      Counter_Register                       at 16#3C# range 0 .. 31;
      Compare_Register                       at 16#4C# range 0 .. 31;
      Synchronous_Interface_Control_Register at 16#54# range 0 .. 31;
   end record;

   --  Timer Registers

   Timer_Register : array (Timer_ID) of Timer
      with Address => Timers_Base_Address, Volatile_Components;
   --  TIMERx_CFG Registers

   --  Timer helper functions

   function CPU_Timer return Timer_ID;
   --  Get the timer used for runtime alarm

   ---------------
   -- CPU_Timer --
   ---------------

   function CPU_Timer return Timer_ID is
      ID_Reg : constant MPIDR := MPIDR_Register;
   begin
      --  CPUs are allocated timers based on their group and processors ID
      --  starting from Cluster 0 CPU0 through to Cluster 1 CPU 1.

      return Timer_ID (ID_Reg.Aff1 * Number_CPU_Clusters + ID_Reg.Aff);
   end CPU_Timer;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      T : constant Timer_ID := CPU_Timer;
      Alarm_Timer renames Timer_Register (T);
   begin

      TI.Vectored_Interrupt_Manager.Initialize;

      --  Start the Global Timebase Counter

      GTC_Counter_Control_Register.Enable_System_Counter := True;

      --  Initialize alarm timer using HFOSC0_CLKOUT as the clock source. The
      --  timer configured as a one-shot match timer.

      Timer_Clock_Selection (T) := (Clock => HFOSC0_CLKOUT);

      Alarm_Timer.CBASS0_Configuration_Register.Software_Reset := True;

      loop
         exit when
           not Alarm_Timer.CBASS0_Configuration_Register.Software_Reset;
      end loop;

      Alarm_Timer.CBASS0_Configuration_Register :=
        (Idle_Mode      => Smart_Idle,
         Emulation_Mode => Frozen,
         Software_Reset => False);

      Alarm_Timer.Synchronous_Interface_Control_Register :=
        (Read_After_Ide   => False,
         Read_Mode        => Posted,
         Posted           => Enable,
         Reset_Posted_Bit => False);

      Alarm_Timer.Control_Register :=
        (GPO_Config               => Output,
         Capture_Mode             => Single,
         Output_Pulse_Toggle_Mode => Pulse_Modulation,
         Output_Trigger_Mode      => No_Trigger,
         Transition_Capture_Mode  => No_Capture,
         PWM_Output_Pusle         => Positive,
         Compare_Enable           => False,
         Prescaler_Enable         => False,
         Prescale_Timer_Value     => 0,
         Autoreload               => False,
         Start_Stop               => Stop);

      Alarm_Timer.Interrupt_Enable_Register :=
        (Compare => Disable, Overflow => Disable, Match => Enable);
   end Initialize_Board;

   package body Interrupts is

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
        renames TI.Vectored_Interrupt_Manager.Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
        renames TI.Vectored_Interrupt_Manager.Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer)
        renames TI.Vectored_Interrupt_Manager.Set_Current_Priority;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down renames TI.Vectored_Interrupt_Manager.Power_Down;
   end Interrupts;

   --------------------
   -- MPIDR_Register --
   --------------------

   function MPIDR_Register return MPIDR is
      MPIDR_State : MPIDR;
   begin
      Asm ("mrc p15,0,%0,c0,c0,5",
           Outputs  => MPIDR'Asm_Output ("=r", MPIDR_State),
           Volatile => True);
      return MPIDR_State;
   end MPIDR_Register;

   package body Multiprocessors is separate;

   package body Time is

      ---------------
      -- Set_Alarm --
      ---------------

      --  Set_Alarm sets the timer to fire (Ticks - Now) time in the future,
      --  bounded by 1 (the shortest delay we can have) and Max_Timer_Value. As
      --  part of the procedure, the delay needs to be converted from the GTC
      --  timebase to the Timer timebase. To ensure that we can perform the
      --  timebase conversion in 64 bits without overflow, the bounds are
      --  applied before conversion in the GTC timebase.

      procedure Set_Alarm (Ticks : BB.Time.Time) is
         T : constant Timer_ID := CPU_Timer;
         Alarm_Timer renames Timer_Register (T);
         --  Timer used for the alarm

         Now  : constant BB.Time.Time := Read_Clock;
         --  The current time

         Max_Timer_Value : constant := Unsigned_32'Last;
         --  The highest value the timer can be set to

         Max_Timer_Value_In_GTC_Ticks : constant :=
           (Max_Timer_Value * GTC_Frequency) / Timer_Frequency;
         --  The highest value the timer can set to in terms of GTC Ticks

         GTC_Ticks_To_Alarm : constant Unsigned_64 :=
           Unsigned_64
             (if Ticks <= Now then 1
              else
                BB.Time.Time'Min (Ticks - Now, Max_Timer_Value_In_GTC_Ticks));
         --  Number of GTC ticks until the timer needs to fire, bounded by the
         --  range 1 .. Max_Timer_Value_In_GTC_Ticks. The lower bound is needed
         --  as we cannot set the timer to the past and the upper bound is the
         --  the maximum value we can set in our timer; if we have an alarm
         --  value greater than this then the runtime will set a new alarm once
         --  this alarm fires. The upper bound is imposed here as it allows
         --  conversion from GTC ticks to Timer ticks in 64-bit space without
         --  overflow.

         Timer_Value : Unsigned_64;
         --  The requested value for the timer

      begin
         --  Ensure the timer is stopped before reseting the timer and setting
         --  the compare value.

         Alarm_Timer.Control_Register.Start_Stop := Stop;

         --  Convert GTC_Ticks_To_Alarm from GTC timebase to the Timer
         --  timebase to get the timer match value. We add an offset of
         --  (GTC_Frequency/Timer_Frequency - 1) to GTC_Ticks_To_Alarm to have
         --  the division round up, ensuring the timer does not fire earlier
         --  than the desired time.

         Timer_Value :=
           ((GTC_Ticks_To_Alarm + GTC_Frequency / Timer_Frequency - 1) *
             Timer_Frequency) / GTC_Frequency;

         Alarm_Timer.Counter_Register := 0;
         Alarm_Timer.Compare_Register := Unsigned_32 (Timer_Value);
         Alarm_Timer.Control_Register.Compare_Enable := True;
         Alarm_Timer.Control_Register.Start_Stop := Start;
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
         Low         : Unsigned_32;
         High        : Unsigned_32;
         Reread_High : Unsigned_32;
         --  Copies of the GTC Clock registers
      begin
         --  As we need to read the 64-bit clock in two seperate 32-bit reads,
         --  we need to ensure the upper 32 bits do not change while the lower
         --  32 bits are read (which occurs when the lower part wraps around)
         --  Consequently, we reread the upper half after the lower half is
         --  read to ensure no change occurred. If it happens, we reread the
         --  the clock registers again.

         High := GTC_Counter_High_Register;
         loop
            Low := GTC_Counter_Low_Register;
            Reread_High := GTC_Counter_High_Register;
            exit when High = Reread_High;
            High := Reread_High;
         end loop;

         return (BB.Time.Time (High) * 2 ** 32) + BB.Time.Time (Low);
      end Read_Clock;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler (Handler : Interrupt_Handler) is
      begin
         --  The timer we attach our handler to is TIMER0_INTR_PEND_0 + the
         --  CPU timer number (which starts from 0).

         BB.Interrupts.Attach_Handler
           (Handler,
            (TIMER0_INTR_PEND_0 + Interrupt_ID (CPU_Timer)),
            Interrupt_Priority'Last);
      end Install_Alarm_Handler;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
         T : constant Timer_ID := CPU_Timer;
         --  Timer used for the alarm

         Alarm_Timer renames Timer_Register (T);
      begin
         --  Note interrupt is a level line interrupt so we do not (and should)
         --  not write to the End_Of_Interrupt register.

         Alarm_Timer.Control_Register.Start_Stop := Stop;
         Alarm_Timer.Control_Register.Compare_Enable := False;
         Alarm_Timer.Interrupt_Status_Register :=
           (Compare => No_Event, Overflow => No_Event, Match => Pending_Clear);
      end Clear_Alarm_Interrupt;
   end Time;

end System.BB.Board_Support;
