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
--                     Copyright (C) 2003-2016, AdaCore                     --
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

--  There are page numbers in the comments below, please provide the reference
--  to the document (here in this header) to which these references apply ???

pragma Restrictions (No_Elaboration_Code);

with System.BB.Board_Support.ERC32;
with System.BB.Parameters;

package body System.BB.Board_Support is
   use type ERC32.Scaler_8;
   use type ERC32.Timers_Counter;
   use CPU_Primitives;

   package Registers renames ERC32;

   -----------------------
   -- Local Definitions --
   -----------------------

   Periodic_Scaler : constant := 0;
   --  In order to obtain the highest granularity of the clock we set the
   --  scaler to 0.

   Alarm_Scaler : constant := 0;
   --  In order to obtain the highest resolution of the alarm timer we set
   --  the scaler to 0.

   Periodic_Count : constant := Registers.Timers_Counter'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timers_Counter'Last because when the Scaler is
   --  set to 0, the timeout period will be the counter reload value  plus 1.

   --  Constants defining the external interrupts

   General_Purpose_Timer : constant System.BB.Interrupts.Interrupt_ID := 12;

   Timer_Control_Mirror : Registers.Timer_Control_Register;
   pragma Volatile (Timer_Control_Mirror);
   --  Timer_Control register cannot be read. So the following object holds a
   --  copy of the Timer_Control register value.

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Stop_Watch_Dog;
   pragma Inline (Stop_Watch_Dog);
   --  Stop the watch dog timer

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   ------------------------
   -- Alarm_Interrupt_ID --
   ------------------------

   function Alarm_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      return General_Purpose_Timer;
   end Alarm_Interrupt_ID;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 35

      --  The MEC includes a specific register called Interrupt Pending
      --  Register, which reflects the pending interrupts.

      --  The interrupts in the IPR are cleared automatically when the
      --  interrupt is acknowledged. The MEC will sample the trap address in
      --  order to know which bit to clear. Therefore, this procedure has a
      --  null body for this target.

      null;
   end Clear_Alarm_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
   begin
      --  Nothing to do for the IPIC

      null;
   end Clear_Interrupt_Request;

   --------------------------
   -- Clear_Poke_Interrupt --
   --------------------------

   procedure Clear_Poke_Interrupt is
   begin
      --  No Poke interrupt available for ERC32

      raise Program_Error;
   end Clear_Poke_Interrupt;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

      return (Any_Priority (Interrupt) + Interrupt_Priority'First - 1);
   end Priority_Of_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the ERC32 board consists on stopping the watch
      --  dog timer, initializing the memory, and initializing the clock in
      --  order to have the desired granularity and range.

      Stop_Watch_Dog;
      Initialize_Memory;
      Initialize_Clock;
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Real_Time_Clock_Scaler_Aux : Registers.Real_Time_Clock_Scaler_Register;

   begin
      --  Set the scaler for the clock

      Real_Time_Clock_Scaler_Aux       := Registers.Real_Time_Clock_Scaler;
      Real_Time_Clock_Scaler_Aux.RTCS  := Periodic_Scaler;
      Registers.Real_Time_Clock_Scaler := Real_Time_Clock_Scaler_Aux;

      --  Load the counter for the clock

      Registers.Real_Time_Clock_Counter := Periodic_Count;

      --  Set the proper bits in mirrored Timer Control Register. The timer
      --  used for the clock is programmed in periodic mode.

      --  From MEC Specification Document (MCD/SPC/0009/SE) page 50

      --  NOTE: All reserved bits have to be written with zeros in order to
      --  avoid parity error resulting in a MEC internal error.

      Timer_Control_Mirror.Reserved4  := (others => False);
      Timer_Control_Mirror.Reserved20 := (others => False);

      Timer_Control_Mirror.RTCCR := True;
      Timer_Control_Mirror.RTCCL := True;
      Timer_Control_Mirror.RTCSL := True;
      Timer_Control_Mirror.RTCSE := True;

      --  Do not modify General Purpose Timer downcounter

      Timer_Control_Mirror.GCL := False;
      Timer_Control_Mirror.GSL := False;

      --  Write MEC Timer Control Register

      Registers.Timer_Control := Timer_Control_Mirror;
   end Initialize_Clock;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for the ERC32

      null;
   end Initialize_Memory;

   ------------------------
   -- Max_Timer_Interval --
   ------------------------

   function Max_Timer_Interval return Timer_Interval is
   begin
      return Timer_Interval'Last;
   end Max_Timer_Interval;

   -----------------------
   -- Poke_Interrupt_ID --
   -----------------------

   function Poke_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      --  No Poke interrupt available for ERC32

      raise Program_Error;

      --  Unreachable code

      return Interrupts.Interrupt_ID'First;
   end Poke_Interrupt_ID;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Primitives.Vector_Id)
      return System.BB.Interrupts.Interrupt_ID
   is
   begin
      --  The range corresponding to asynchronous traps is in 16#11# .. 16#1F#

      pragma Assert (Vector in 16#11# .. 16#1F#);

      return System.BB.Interrupts.Interrupt_ID (Vector - 16#10#);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      pragma Unreferenced (Prio);
   begin
      CPU_Primitives.Install_Trap_Handler
        (Handler, CPU_Primitives.Vector_Id (Interrupt + 16#10#));
   end Install_Interrupt_Handler;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
   begin
      return
        Timer_Interval (Periodic_Count - Registers.Real_Time_Clock_Counter);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      General_Purpose_Timer_Scaler_Aux :
        Registers.General_Purpose_Timer_Scaler_Register;

      Interrupt_Mask_Aux : Registers.Interrupt_Mask_Register;

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the scaler

      General_Purpose_Timer_Scaler_Aux :=
        Registers.General_Purpose_Timer_Scaler;
      General_Purpose_Timer_Scaler_Aux.GPTS := Alarm_Scaler;
      Registers.General_Purpose_Timer_Scaler :=
        General_Purpose_Timer_Scaler_Aux;

      --  Load the counter

      Registers.General_Purpose_Timer_Counter :=
        Registers.Timers_Counter (Ticks);

      --  Set the proper bits in mirrored Timer Control Register.
      --  General Purpose Timer is used in one-shot mode.

      Timer_Control_Mirror.GCR := False;

      Timer_Control_Mirror.GCL := True;
      Timer_Control_Mirror.GSE := True;
      Timer_Control_Mirror.GSL := True;

      --  Do not modify Timer downcount

      Timer_Control_Mirror.RTCCL := False;
      Timer_Control_Mirror.RTCSL := False;

      --  From MEC Specification Document (MCD/SPC/0009/SE) page 50

      --  NOTE: All reserved bits have to be written with zeros in order to
      --  avoid parity error resulting in a MEC internal error.

      Timer_Control_Mirror.Reserved4 := (others => False);
      Timer_Control_Mirror.Reserved20 := (others => False);

      --  Write MEC Timer Control Register

      Registers.Timer_Control := Timer_Control_Mirror;

      --  Enable GPT Interrupts

      Interrupt_Mask_Aux := Registers.Interrupt_Mask;
      Interrupt_Mask_Aux.General_Purpose_Timer := False;
      Registers.Interrupt_Mask := Interrupt_Mask_Aux;
   end Set_Alarm;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      null; --  No board-specific actions necessary
   end Set_Current_Priority;

   --------------------
   -- Stop_Watch_Dog --
   --------------------

   procedure Stop_Watch_Dog is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 39

      --  After system reset or processor reset, the watch dog timer is enabled
      --  and starts running. By writing to the Trap Door Set after system
      --  reset, the timer can be disabled.

      Registers.Watchdog_Trap_Door_Set := 0;
   end Stop_Watch_Dog;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      --  The prescaler is clocked by the system clock. When it underflows, it
      --  is reloaded from the prescaler reload register and a timer tick is
      --  generated. The effective division rate is therefore equal to the
      --  prescaler reload register value plus 1.

      return Parameters.Clock_Frequency / (Periodic_Scaler + 1);
   end Ticks_Per_Second;

end System.BB.Board_Support;
