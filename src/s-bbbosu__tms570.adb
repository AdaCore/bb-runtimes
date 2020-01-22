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
--                     Copyright (C) 2003-2020, AdaCore                     --
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

with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.Board_Support is
   use BB.Interrupts;

   -------------------------------
   -- Real-Time Interrupt (RTI) --
   -------------------------------

   RTI_Base     : constant Address := 16#FFFF_FC00#;

   type RTI_Registers_Type is record
     GCTRL        : Unsigned_32;
     TBCTRL       : Unsigned_32;
     CAPCTRL      : Unsigned_32;
     COMPCTRL     : Unsigned_32;

     FRC0         : Unsigned_32;
     UC0          : Unsigned_32;
     CPUC0        : Unsigned_32;
     Pad_1c       : Unsigned_32;

     CAFRC0       : Unsigned_32;
     CAUC0        : Unsigned_32;
     Pad_28       : Unsigned_32;
     Pad_2c       : Unsigned_32;

     FRC1         : Unsigned_32;
     UC1          : Unsigned_32;
     CPUC1        : Unsigned_32;
     Pad_3c       : Unsigned_32;

     --  Offset: 0x40
     CAFRC1       : Unsigned_32;
     CAUC1        : Unsigned_32;
     Pad_48       : Unsigned_32;
     Pad_4c       : Unsigned_32;

     COMP0        : Unsigned_32;
     UDCP0        : Unsigned_32;
     COMP1        : Unsigned_32;
     UDCP1        : Unsigned_32;

     COMP2        : Unsigned_32;
     UDCP2        : Unsigned_32;
     COMP3        : Unsigned_32;
     UDCP3        : Unsigned_32;

     TBLCOMP      : Unsigned_32;
     TBHCOMP      : Unsigned_32;
     Pad_78       : Unsigned_32;
     Pad_7c       : Unsigned_32;

     --  Offset: 0x80
     SETINTENA    : Unsigned_32;
     CLEARINTENA  : Unsigned_32;
     INTFLAG      : Unsigned_32;
     Pad_8c       : Unsigned_32;

     DWDCTRL      : Unsigned_32;
     DWDPRLD      : Unsigned_32;
     WDSTATUS     : Unsigned_32;
     WDKEY        : Unsigned_32;

     DWDCNTR      : Unsigned_32;
     WWDRXNCTRL   : Unsigned_32;
     WWDSIZECTRL  : Unsigned_32;
     INTCLRENABLE : Unsigned_32;

     COMP0CLR     : Unsigned_32;
     COMP1CLR     : Unsigned_32;
     COMP2CLR     : Unsigned_32;
     COMP3CLR     : Unsigned_32;
   end record;

   RTI : RTI_Registers_Type with Volatile, Import, Address => RTI_Base;

   RTI_Compare_Interrupt_3 : constant Interrupt_ID := 5;
   --  We use the compare unit 3, so the first counter and the first three
   --  compare units are available for use by the user.

   procedure Irq_Interrupt_Handler;
   pragma Export (C, Irq_Interrupt_Handler, "__gnat_irq_handler");
   procedure Fiq_Interrupt_Handler;
   pragma Export (C, Fiq_Interrupt_Handler, "__gnat_fiq_handler");
   --  Low-level interrupt handlers

   --------------------------------------
   -- Vectored Interrupt Manager (VIM) --
   --------------------------------------

   VIM_Base : constant Address := 16#FFFF_FE00#;

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;

   type VIM_Registers_Type is record
      IRQINDEX    : Unsigned_32;
      FIQINDEX    : Unsigned_32;
      Pad_08      : Unsigned_32;
      Pad_0c      : Unsigned_32;

      --  Register with one bit set for each interrupt that is an FIQ.
      --  Interrupt sources 32 and over are not supported as FIQ by this run
      --  time. These interrupts should be remapped to lower interrupt channels
      --  when required as FIQ. The FIQ_Ints must always include the bits set
      --  by NMI_Ints, as the hardware cannot mask these. Initialize_Boards
      --  will update this register.
      FIRQPR      : Unsigned_32_Array (0 .. 3);

      INTREQ      : Unsigned_32_Array (0 .. 3);

      --  Writing a bit mask to this register enables the interrupts
      REQENASET   : Unsigned_32_Array (0 .. 3);

      --  Offset 0x40
      --  Writing a bit mask to this register clears the interrupts
      REQENACLR   : Unsigned_32_Array (0 .. 3);

      --  Bit mask allowing corresponding interrupts to wake the processor
      WAKEENASET  : Unsigned_32_Array (0 .. 3);

      WAKEENACLR  : Unsigned_32_Array (0 .. 3);

      IRQVECREG   : Unsigned_32;
      FIQVECREG   : Unsigned_32;
      CAPEVT      : Unsigned_32;
      Pad_7c      : Unsigned_32;

      --  Offset 0x80
      CHANCTRL    : Unsigned_32_Array (0 .. 31);
   end record;

   VIM : VIM_Registers_Type with Volatile, Import, Address => VIM_Base;

   procedure Enable_Interrupt_Request (Interrupt : Interrupt_ID);
   --  Enable interrupt requests for the given interrupt

   ------------------------
   -- Interrupt_Handlers --
   ------------------------

   type Interrupt_Vector_Table is array (Interrupt_ID) of Address;
   Interrupt_Vectors : Interrupt_Vector_Table;
   pragma Volatile (Interrupt_Vectors);
   for Interrupt_Vectors'Address use 16#FFF8_2004#;

   FIQ_Prio  : constant Interrupt_Priority := Interrupt_Priority'Last;
   IRQ_Prio  : constant Interrupt_Priority := Interrupt_Priority'First;
   pragma Assert (FIQ_Prio = IRQ_Prio + 1);

   NMI_Ints  : constant Unsigned_32 := 3;
   --  Bitmap of unmaskable interrupts, namely interrupt channel 0 and 1

   FIQ_Masked : Boolean := False;
   --  Reflects whether FIQ interrupts are masked in the VIM or not

   procedure IRQ_Handler;
   pragma Import (Asm, IRQ_Handler, "__gnat_irq_trap");

   procedure FIQ_Handler;
   pragma Import (Asm, FIQ_Handler, "__gnat_fiq_trap");

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board
   is
      Dead : Unsigned_32 with Unreferenced;
   begin
      --  Disable all interrupts, except for NMIs

      VIM.REQENACLR (0)  := not NMI_Ints;
      VIM.WAKEENACLR (0) := not 0;
      VIM.REQENACLR (1)  := not 0;
      VIM.WAKEENACLR (1) := not 0;
      VIM.REQENACLR (2)  := not 0;
      VIM.WAKEENACLR (2) := not 0;
      if Interrupt_ID'Last > 96 then
         VIM.REQENACLR (3)  := not 0;
         VIM.WAKEENACLR (3) := not 0;
      end if;

      --  Initialize timer

      --  The counter needs to be disabled while programming it

      RTI.GCTRL       := 0;             --  Turn off timers
      RTI.TBCTRL      := 0;             --  Use RTIUC0 to clock counter 0
      RTI.CPUC1       := 16#ffff_ffff#; --  Program prescaler compare
      RTI.UC1         := 0;             --  Start prescaler at 0
      RTI.FRC1        := 0;             --  Start clock at 0

      RTI.CPUC0       := 1;             --  Set minimal prescalar for alarm
      RTI.COMPCTRL    := 0;             --  Use timer 0 for comparators
      RTI.INTFLAG     := 16#0000_700f#; --  Clear all pending interrupts
      RTI.CLEARINTENA := 16#0007_0f0f#; --  Disable all interrupts
      RTI.SETINTENA   := 2**3;          --  Enable Interrupt for comparator 3
      RTI.GCTRL       := 2;             --  Turn timer/counter 1 on
   end Initialize_Board;

   package body Time is
      Alarm_Interrupt_ID : constant Interrupt_ID := RTI_Compare_Interrupt_3;
      --  Interrupt for the alarm

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time)
      is
         use BB.Time;
         Now      : constant BB.Time.Time := Read_Clock;
         Diff     : constant Unsigned_64  := (if Now < Ticks
                                              then Unsigned_64 (Ticks - Now)
                                              else 1);
      begin
         if Ticks = BB.Time.Time'Last then
            Clear_Alarm_Interrupt;
         end if;

         RTI.INTFLAG := 2**3; --  Clear any pending alarms
         RTI.GCTRL   := 2;    --  Turn off timer/counter 0
         RTI.UC0     := 0;    --  Start at 0
         RTI.FRC0    := 0;

         if Diff < (2 ** 33 - 1) then
            RTI.CPUC0 := 1; --  Minimal prescaler: RTICLK / 2
            RTI.COMP3 := Unsigned_32 ((Diff + 1) / 2);
         else
            --  Raise an alarm around the target time: it'll be too early but
            --  s-bbtime will then re-set it and this time allowing a fine
            --  grain delay
            RTI.CPUC0 := 16#FFFF_FFFF#;
            RTI.COMP3 := Unsigned_32 (Shift_Right (Diff, 32));
         end if;

         RTI.GCTRL := 3;      --  Enable timer 0 and 1
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time
      is
         Lo, Hi : Unsigned_32;
      begin
         --  According to TMS570 manual (RTI 17.2.1 Counter and Capture Read
         --  Consistency), the free running counter must be read first.
         Hi := RTI.FRC1;
         Lo := RTI.UC1;
         return BB.Time.Time
           (Unsigned_64 (Lo) or Shift_Left (Unsigned_64 (Hi), 32));
      end Read_Clock;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         RTI.GCTRL := 2;      --  Turn off timer/counter 0
         RTI.INTFLAG := 2**3;
      end Clear_Alarm_Interrupt;

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
   end Time;

   ---------------------------
   -- Irq_Interrupt_Handler --
   ---------------------------

   procedure Irq_Interrupt_Handler
   is
      Id : constant Unsigned_32 := VIM.IRQINDEX and 16#FF#;
   begin
      if Id = 0 then
         --  Spurious interrupt
         return;
      end if;

      Interrupt_Wrapper (Interrupt_ID (Id - 1));
   end Irq_Interrupt_Handler;

   ---------------------------
   -- Fiq_Interrupt_Handler --
   ---------------------------

   procedure Fiq_Interrupt_Handler
   is
      Id : constant Unsigned_32 := VIM.FIQINDEX and 16#FF#;
   begin
      if Id = 0 then
         --  Spurious interrupt
         return;
      end if;

      Interrupt_Wrapper (Interrupt_ID (Id - 1));
   end Fiq_Interrupt_Handler;

   ------------------------------
   -- Enable_Interrupt_Request --
   ------------------------------

   procedure Enable_Interrupt_Request (Interrupt : Interrupt_ID) is
      Reg : constant Natural := Interrupt / 32;
      Bit : constant Unsigned_32 := Shift_Left (1, Interrupt mod 32);
      --  Many VIM registers use 3 words of 32 bits each to serve as a bitmap
      --  for all interrupt channels. Regofs indicates register offset (0..2),
      --  and Regbit indicates the mask required for addressing the bit.

   begin
      VIM.REQENASET (Reg) := Bit;
      VIM.WAKEENASET (Reg) := Bit;
   end Enable_Interrupt_Request;

   package body Multiprocessors is separate;

   package body Interrupts is
      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : Interrupt_ID)
        return System.Any_Priority
      is
         (if Interrupt <= 31 and then (VIM.FIRQPR (0) and 2**Interrupt) /= 0
          then FIQ_Prio
         else IRQ_Prio);

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer)
      is
      begin
         --  On the TMS570, FIQs cannot be masked by the processor. So, we need
         --  to disable them at the controller when required.

         if (Priority = FIQ_Prio) xor FIQ_Masked then
            if Priority = FIQ_Prio then
               VIM.REQENACLR (0) := VIM.FIRQPR (0);
               FIQ_Masked := True;

            else
               VIM.REQENASET (0) := VIM.FIRQPR (0);
               FIQ_Masked := False;
            end if;
         end if;
      end Set_Current_Priority;

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
         Hw_Prio : constant Any_Priority := Priority_Of_Interrupt (Interrupt);

      begin
         --  While we could directly have installed fixed IRQ and FIQ handlers,
         --  this would have required that all IRQ and FIQ handlers go through
         --  the Ravenscar run time, which is a bit of a limitation. By using
         --  the vector capability of the interrupt handler, it is possible to
         --  handle some interrupts directly for best performance.

         case Interrupt_Priority (Hw_Prio) is
            when IRQ_Prio =>
               Interrupt_Vectors (Interrupt) := IRQ_Handler'Address;

            when FIQ_Prio =>
               Interrupt_Vectors (Interrupt) := FIQ_Handler'Address;
         end case;

         Enable_Interrupt_Request (Interrupt);
      end Install_Interrupt_Handler;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         Asm ("wfi", Volatile => True);
      end Power_Down;
   end Interrupts;
end System.BB.Board_Support;
