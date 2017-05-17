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

--  This is the XtratuM version of this package

pragma Restrictions (No_Elaboration_Code);

with Interfaces.C;

with System.BB.Parameters;
with System.Machine_Code;

package body System.BB.Board_Support is
   use CPU_Primitives;
   use Interfaces.C;

   -----------------------
   -- Local Definitions --
   -----------------------

   XM_HW_CLOCK : constant := 0;
   --  Real-time clock

   type XM_Time_T is range -2 ** 63 .. 2 ** 63 - 1;
   for XM_Time_T'Size use 64;
   --  Time in XtratuM

   XM_VT_EXT_FIRST : constant := 16;
   --  First XtratuM extended interrupt

   XM_VT_EXT_HW_TIMER : constant := 0;
   --  Real-time timer interrupt (extended interrupt)

   HW_Timer_Unmasked : Boolean := False;
   --  Flag to know whether the timer IRQ has already been unmasked

   Flush_Register_Windows : constant Vector_Id := 16#83#;
   --  The trap number associated to the flush register windows (ta 3)

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Get_Time (Clock_Id : unsigned; Time : access XM_Time_T);
   pragma Import (C, Get_Time, "XM_get_time");
   --  Read clock

   procedure Set_Timer
     (Clock_Id : unsigned; AbsTime : XM_Time_T; Interval : XM_Time_T);
   pragma Import (C, Set_Timer, "XM_set_timer");
   --  Set hardware timer

   procedure Clear_IRQ_Mask (HwIrqsMask : unsigned; ExtIrqsMask : unsigned);
   pragma Import (C, Clear_IRQ_Mask, "XM_clear_irqmask");
   --  Unmask IRQs

   procedure Flush_Windows_Handler;
   --  Handler to install for the flush register windows trap (ta 3)

   ------------------------
   -- Alarm_Interrupt_ID --
   ------------------------

   function Alarm_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      --  This is an extended interrupt, identified by offset XM_VT_EXT_FIRST

      return XM_VT_EXT_FIRST + XM_VT_EXT_HW_TIMER;
   end Alarm_Interrupt_ID;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

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
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Poke_Interrupt;

   ---------------------------
   -- Flush_Windows_Handler --
   ---------------------------

   procedure Flush_Windows_Handler is
   begin
      --  This is the code for the hypercall XM_sparc_flush_regwin. We call it
      --  this way because this is a macro.

      System.Machine_Code.Asm
        ("mov 1, %%o0" & ASCII.LF & ASCII.HT & "ta 0xf1",
         Volatile => True, Clobber  => "o0");
   end Flush_Windows_Handler;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Install the trap handler for flushing register windows. This is
      --  needed for propagating exceptions and for getting tracebacks.

      Install_Trap_Handler
        (Service_Routine => Flush_Windows_Handler'Address,
         Vector          => Flush_Register_Windows,
         Synchronous     => True);
   end Initialize_Board;

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
      return 0;
   end Poke_Interrupt_ID;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

      --  Hardware interrupt

      if Interrupt < XM_VT_EXT_FIRST then
         return (Any_Priority (Interrupt) + Interrupt_Priority'First - 1);

      --  Extended interrupt

      else
         return System.Any_Priority'Last;
      end if;
   end Priority_Of_Interrupt;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
      XtratuM_Time : aliased XM_Time_T;

      pragma Suppress (Range_Check);
      --  Suppress this check so we can use a fast implementation for taking
      --  the lower part of the time (the 32 least significant bits) by simply
      --  ignoring the most significant part.

   begin
      Get_Time (XM_HW_CLOCK, XtratuM_Time'Access);

      --  Take the lower 32-bit

      return Timer_Interval (XtratuM_Time);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      XtratuM_Time : aliased XM_Time_T;

   begin
      --  Transform into absolute time

      Get_Time (XM_HW_CLOCK, XtratuM_Time'Access);
      Set_Timer (XM_HW_CLOCK, XtratuM_Time + XM_Time_T (Ticks), 0);

      if not HW_Timer_Unmasked then
         Clear_IRQ_Mask (0, 2 ** XM_VT_EXT_HW_TIMER);
         HW_Timer_Unmasked := True;
      end if;
   end Set_Alarm;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      null; --  No board-specific actions necessary
   end Set_Current_Priority;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      return Parameters.Clock_Frequency;
   end Ticks_Per_Second;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Primitives.Vector_Id)
      return System.BB.Interrupts.Interrupt_ID
   is
   begin
      --  The range corresponding to asynchronous traps is 16#11# .. 16#1F#,
      --  and extended interrupts are 16#E0# .. 16#FF#.

      pragma Assert (Vector in 16#11# .. 16#1F# | 16#E0# .. 16#FF#);

      if Vector in 16#11# .. 16#1F# then
         return System.BB.Interrupts.Interrupt_ID (Vector - 16#10#);
      else
         return System.BB.Interrupts.Interrupt_ID
                  (Vector - 16#E0# + XM_VT_EXT_FIRST);
      end if;
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
      Vec : constant Vector_Id :=
              (if Interrupt < XM_VT_EXT_FIRST
               then Vector_Id (Interrupt + 16#10#)
               else Vector_Id (Interrupt - XM_VT_EXT_FIRST + 16#E0#));
   begin
      Install_Trap_Handler (Handler, Vec);
   end Install_Interrupt_Handler;

end System.BB.Board_Support;
