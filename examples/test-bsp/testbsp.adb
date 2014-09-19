------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

--  This test program directly interfaces to the BSP to test its functionality.
--  Its main goals are to ensure basic functionality of the various routines,
--  asses performance of key BSP routines, and help with debugging elementary
--  problems during initial porting to a new platform.
--  The program is not meant as substitute for other test suites.

--  While usually one should not directly use these packages, the purpose
--  of this program is specifically to test the individual routines, rather
--  than complete Ada tasks or interrupt handlers, so suppress warnings.

pragma Warnings (Off);
with System.BB.Board_Support;  use System.BB.Board_Support;
with System.BB.CPU_Primitives; use System.BB.CPU_Primitives;
with System.BB.Interrupts;
pragma Warnings (On);

with Ada.Text_IO; use Ada.Text_IO;
with Testdata; use Testdata;
with Testthread;
with Testhandler;
with Testfpu;

procedure TestBSP is

   pragma Warnings (Off);
   --  This variable is not changed, but must not be declared constant, as
   --  we want it to go into the initialized data section.
   Marker : Integer := 16#5555_AAAA#;
   pragma Warnings (On);

   function Ident (X : Integer) return Integer is (X);

   procedure Put_Dec (X : Integer; Exp : Natural := 0);
   procedure Error (S : String);
   function Error (S : String) return Boolean;
   procedure Check (Success : Boolean; Name : String);
   procedure Debug (S : String);
   procedure Debug (S : String; Val : Integer);
   procedure Timing (Item : String; Duration : Integer; Unit : String);
   function Test_Timer return Boolean;
   function Test_Context_Switch return Boolean;
   function Test_Alarm_Interrupts return Boolean;
   function Test_Data return Boolean;
   function Test_FPU_Switch return Boolean;
   function Test_Floating_Point return Boolean;

   function Ticks_To_Micros (Ticks : Timer_Interval) return Natural is
     (Natural (Ticks * (1E9 / Timer_Interval (Ticks_Per_Second)) / 1000));

   -----------
   -- Error --
   -----------

   procedure Error (S : String) is
   begin
      Put ("*** ");
      Put (S);
      Put (" ***");
      New_Line;
      Errors := True;
   end Error;

   function Error (S : String) return Boolean is
   begin
      New_Line;
      Error (S);
      return False;
   end Error;

   -----------
   -- Check --
   -----------

   procedure Check (Success : Boolean; Name : String) is
   begin
      if not Errors then
         Put ("  * check ");
         Put (Name);
         Errors := not Success;
         Put_Line (if Errors then ", ERROR" else ", OK");
      end if;
   end Check;

   -----------
   -- Debug --
   -----------

   procedure Debug (S : String) is
   begin
      pragma Debug (Put ("  . "));
      pragma Debug (Put_Line (S));
   end Debug;

   procedure Debug (S : String; Val : Integer) is
   begin
      pragma Debug (Put ("  . "));
      pragma Debug (Put (S));
      pragma Debug (Put (" = "));
      pragma Debug (Put_Dec (Val));
      pragma Debug (New_Line);
   end Debug;

   ------------
   -- Timing --
   ------------

   procedure Timing (Item : String; Duration : Integer; Unit : String) is
   begin
      Put ("=== ");
      Put (Item);
      Put (": ");
      Put_Dec (Duration);
      Put (' ');
      Put (Unit);
      New_Line;
   end Timing;

   -------------
   -- Put_Dec --
   -------------

   procedure Put_Dec (X : Integer; Exp : Natural := 0) is
   begin
      if X >= 10 then
         Put_Dec (X / 10, Exp + 1);
         if Exp + 1 in 3 | 6 | 9 then
               Put ('_');
         end if;
      elsif X < 0 then
         Put ('-');
         Put_Dec (-X, Exp);
         return;
      end if;
      Put (Character'Val (Character'Pos ('0') + X mod 10));
   end Put_Dec;

   ----------------
   -- Test_Timer --
   ----------------

   function Test_Timer return Boolean is
      TPS   : constant Timer_Interval := Timer_Interval (Ticks_Per_Second);
      T1,
      T2    : Timer_Interval;
      J     : Natural;

   begin
      Timing ("Clock tick duration", Ticks_To_Micros (1000), "ns");

      Check (Ticks_Per_Second >= 1E6 / 20,
        "tick duration less than 20 usec, see D.8(30)");
      --  As for now Time_Unit is equal to Ticks_Per_Second,

      Check (Max_Timer_Interval >= TPS / 4 * 3,
        "timer interval lasts at least 0.75s");
      --  For 2**31 timer intervals to last at least 50 years,
      --  a single interval should last about 0.75 seconds or longer.

      T1 := Read_Clock;

      --  Now lets see if the clock actually ticks OK. We try up to a
      --  million times: that should be just a few seconds at most on
      --  typical systems, but should take more than a millisecond.

      J := 0;
      loop
         T2 := Read_Clock;
         J := J + 1;
         exit when T2 - T1 >= TPS / 1000 or else J >= 1E6;
      end loop;

      Check (T2 /= T1, "clock is running");
      Check (T2 - T1 >= TPS / 1000, "clock counted up at least 1 msec");
      Check (T1 - T1 <  TPS / 500, "clock counted up less than 2 msec");
      Check (J > 50, "clock reading takes less than 20 usec");
      Check (J <= 200_000, "clock reading takes at least 5 ns");

      Timing ("Clock_Read duration",
         Ticks_To_Micros (1000 * (T2 - T1) / Timer_Interval (J)), "ns");

      return not Errors;
   end Test_Timer;

   -------------------------
   -- Test_Context_Switch --
   -------------------------

   function Test_Context_Switch return Boolean is
      function Yield (Thread : access Context_Buffer) return Integer;

      function Yield (Thread : access Context_Buffer) return Integer is
      begin
         Disable_Interrupts;
         First_Thread := Thread;
         Context_Switch;
         Enable_Interrupts (0);
         return Shared_Var;
      end Yield;

      Inc_Thread : constant access Context_Buffer := Test_Context (2)'Access;
      T1, T2 : Timer_Interval;

   begin
      Debug ("Initializing context");
      Initialize_Context (Inc_Thread,
        Program_Counter => Testthread'Address,
        Argument        => Shared_Var'Address,
        Stack_Pointer   => More_Init_Data (More_Init_Data'Last)'Address);

      Running_Thread := Test_Context (1)'Access;
      Initialize_Context (Running_Thread,
        Program_Counter => Testthread'Address,
        Argument        => Shared_Var'Address,
        Stack_Pointer   => More_Init_Data (More_Init_Data'Last)'Address);

      Shared_Var := 123;

      --  First do a dummy context switch with Running_Thread and First_Thread
      --  being equal. This should essentially just save/restore the context.
      Check (Yield (Running_Thread) = 123, "Context_Switch to self");
      Check (Yield (Inc_Thread) = 124,
         "Context_Switch to test thread increments shared var");
      Check (Yield (Inc_Thread) = 125, "repeated context switch");

      T1 := Read_Clock;

      for J in 1 .. 1000 loop
         Disable_Interrupts;
         First_Thread := Inc_Thread;
         Shared_Var := Shared_Var + 1;
         Context_Switch;
         Enable_Interrupts (0);
      end loop;
      T2 := Read_Clock;

      Timing ("Context_Switch duration", Ticks_To_Micros (T2 - T1) / 2, "ns");

      Check (Shared_Var = 2125,
         "context switches properly commit shared variables");

      return not Errors;
   end Test_Context_Switch;

   ---------------
   -- Test_Data --
   ---------------

   function Test_Data return Boolean is
     ((Constant_Data = Ident (Literal_Data)
         or else Error ("constant data has wrong value"))
             and then
      (Initialized_Data = Literal_Data
         or else Error ("constant data has wrong value"))
             and then
      (for all X of More_Init_Data => X = Literal_Data
         or else Error ("dynamically initialized data has wrong value"))
             and then
      (for all X of More_Const_Data => X = Literal_Data
         or else Error ("dynamically initialized constant has wrong value"))
             and then
      (for all X of Uninitialized_Data => X = 0
         or else Error ("uninitialized data not zeroed out")));

   ---------------------------
   -- Test_Alarm_Interrupts --
   ---------------------------

   function Test_Alarm_Interrupts return Boolean is
      Alarm_Int  : constant Interrupt_ID := Alarm_Interrupt_ID;
      Alarm_Prio : constant Interrupt_Priority
                      := Priority_Of_Interrupt (Alarm_Int);
      s  : constant Timer_Interval := Timer_Interval (Ticks_Per_Second);
      ms : constant Timer_Interval := Timer_Interval (Ticks_Per_Second / 1000);
      T0, T1, T2 : Timer_Interval;
   begin
      Debug ("Alarm_Interrupt_ID", Alarm_Int);
      Debug ("Priority_Of_Interrupt (Alarm_Interrupt_ID)", Alarm_Prio);

      Disable_Interrupts;
      Debug ("Installing interrupt handler");
      Install_Interrupt_Handler (Testhandler'Address, Alarm_Int, Alarm_Prio);

      Debug ("Testing enabling/disabling interrupts");
      Enable_Interrupts (System.Priority'Last);
      Disable_Interrupts;

      T1 := Read_Clock;
      for J in 1 .. 1_000 loop
         Enable_Interrupts (System.Priority'Last);
         Disable_Interrupts;
      end loop;
      T2 := Read_Clock;

      Timing ("Disable_Interrupts/Enable_Interrupts duration",
        Ticks_To_Micros (T2 - T1), "ns");

      Check (Last_Alarm = 0, "absence of spurious interrupts");
      Debug ("Testing 1 ms alarm");
      T0 := Read_Clock;
      Set_Alarm (1 * ms); -- time 1 ms
      Enable_Interrupts (Alarm_Prio - 1);

      for J in 1 .. 1E6 loop
         --  While the execution of the first alarm may take a long time due
         --  to the output it generates, the loop should not execute while
         --  handling the alarm, so it will only return with Last_Alarm = 0 if
         --  the alarm did not trigger.

         T1 := Read_Clock;
         exit when Last_Alarm /= 0 or T1 - T0 > 25 * ms;
      end loop;

      Check (Last_Alarm /= 0, "alarm interrupt occurred");
      Check (Last_Alarm - T0 >= 1 * ms, "delay is at least 1 ms");
      Check (Last_Alarm - T0 <= 25 * ms, "delay is at most 25 ms");

      Alarms := 1;

      --  Now spend two milliseconds setting alarms a millisecond into
      --  the future.  Each alarm should cancel the previous one, so an
      --  actual alarm should never trigger.

      T0 := Read_Clock;
      for J in 1 .. 1000 loop
         Disable_Interrupts;
         Set_Alarm (1 * ms);
         Enable_Interrupts (Alarm_Prio - 1);
      end loop;

      T1 := Read_Clock;

      --  Make sure to keep setting alarms for at least 2 ms
      loop
         Disable_Interrupts;
         Set_Alarm (1 * ms);
         Enable_Interrupts (Alarm_Prio - 1);
         exit when Read_Clock - T0 > 2 * ms;
      end loop;

      Disable_Interrupts;
      Check (Alarms = 1, "resetting the alarm cancels previous alarms");
      Timing ("Set_Alarm duration", Ticks_To_Micros (T1 - T0), "ns");

      while Read_Clock - T0 < 4 * ms loop
         null;
      end loop;

      Check (Alarms = 1, "disabling interrupts works");
      Enable_Interrupts (Alarm_Prio);
      Check (Alarms = 1, "interrupts do not interrupt task of same priority");
      Enable_Interrupts (Alarm_Prio - 1);
      Check (Alarms = 2, "enabling interrupts delivers pending interrupts");

      Disable_Interrupts;
      Set_Alarm (0);
      T0 := Read_Clock;
      Enable_Interrupts (Alarm_Prio - 1);

      Check (Alarms = 3, "alarm of minimal duration is immediate");

      if not Errors then
         --  The time of the alarm is computed right as first statement, so
         --  the latency should be reliable.
         Timing ("Interrupt latency",
            Ticks_To_Micros (1000 * (Last_Alarm - T0)), "ns");
      end if;

      --  In the following loop, we are testing alarms from 0 to 127 ticks,
      --  seven times for each value, as well as values 1 through 104. So the
      --  average wait is 7 * 128 * 63.5 ticks + 104 * 52.5, adding up to 62356
      --  ticks of delay. Anything else is wasted time. We should never have
      --  more than a millisecond of overhead for handling an alarm, so the
      --  following test allows for a total time of 62356 ticks + 1 sec to
      --  handle all delays.

      T0 := Read_Clock;
      Test_Small_Delays : for J in 1 .. 1000 loop
         Disable_Interrupts;
         Set_Alarm (Timer_Interval (J) mod 128);
         Enable_Interrupts (Alarm_Int - 1);

         while Alarms < J + 3 loop
            exit Test_Small_Delays when Read_Clock > T0 + 62356 + 1 * s;
         end loop;

      end loop Test_Small_Delays;
      T1 := Read_Clock;

      Check (Alarms = 1003, "missed alarms");
      Timing ("alarm setting/handling duration",
              Ticks_To_Micros (T1 - T0 - 62356), "ns");

      return not Errors;
   end Test_Alarm_Interrupts;

   ---------------------
   -- Test_FPU_Switch --
   ---------------------

   function Test_FPU_Switch return Boolean is
      function Yield (Thread : access Context_Buffer) return Float;

      function Yield (Thread : access Context_Buffer) return Float is
      begin
         Disable_Interrupts;
         First_Thread := Thread;
         Context_Switch;
         Enable_Interrupts (0);
         return Shared_Float;
      end Yield;

      Inc_Thread : constant access Context_Buffer := Test_Context (2)'Access;
      T1, T2 : Timer_Interval;

   begin
      Debug ("Initializing context");
      Initialize_Context (Inc_Thread,
        Program_Counter => Testfpu'Address,
        Argument        => Shared_Float'Address,
        Stack_Pointer   => More_Init_Data (More_Init_Data'Last)'Address);

      Running_Thread := Test_Context (1)'Access;

      Shared_Float := 123.0;

      --  First do a dummy context switch with Running_Thread and First_Thread
      --  being equal. This should essentially just save/restore the context.
      Check (Yield (Running_Thread) = 123.0, "FPU Context_Switch to self");
      Check (Yield (Inc_Thread) = 124.0,
         "FPU Context_Switch to test thread increments shared float");
      Check (Yield (Inc_Thread) = 125.0, "repeated FPU context switch");

      T1 := Read_Clock;
      for J in 1 .. 1000 loop
         Shared_Float := Shared_Float + 1.0;
         Disable_Interrupts;
         First_Thread := Inc_Thread;
         Context_Switch;
         Enable_Interrupts (0);
      end loop;
      T2 := Read_Clock;

      Timing ("Context_Switch duration between tasks using FPU",
         Ticks_To_Micros (T2 - T1) / 2, "ns");

      Check (Shared_Float = 2125.0,
         "context switches properly commit shared floating point variables");

      T1 := Read_Clock;
      for J in 1 .. 1000 loop
         Shared_Float := Shared_Float + 1.0;
         Disable_Interrupts;
         First_Thread := Inc_Thread;
         Context_Switch;
         Enable_Interrupts (0);
      end loop;
      T2 := Read_Clock;

      Timing ("Context_Switch betweens task using/not using FPU",
         Ticks_To_Micros (T2 - T1), "ns");

      return not Errors;
   end Test_FPU_Switch;

   -------------------------
   -- Test_Floating_Point --
   -------------------------

   function Test_Floating_Point return Boolean is
   begin
      Debug ("starting floating point test");

      Check (Shared_Float = 0.0, "check floating point instructions");
      return Test_FPU_Switch;
   end Test_Floating_Point;

begin -- TestBSP
   Put_Line ("Start TestBSP");

   Check (Initialized_Data /= Marker, "check against unexpected restart");
   --  An accidental jump to address 0 will lead to the program restarting.
   --  Catch that case right away, with a specific test.

   Initialize_Board;
   Check (True, "initialized board");

   System.BB.Interrupts.Initialize_Interrupts;
   Check (True, "initialized interrupts");

   Initialize_Floating_Point;
   Check (True, "initialized processor");

   Check ((if not Errors then Test_Data), "data initialization");

   --  Clobber initialized data
   Initialized_Data := Marker;

   Check ((if not Errors then Test_Timer), "all timer tests");
   Check ((if not Errors then Test_Context_Switch),
      "all context switch tests");
   Check ((if not Errors then Test_Alarm_Interrupts),
      "all alarm interrupt tests");
   Check ((if not Errors then Test_Floating_Point),
      "all float tests");

   Put_Line (if Errors then "FAILED" else "PASSED");
end TestBSP;
