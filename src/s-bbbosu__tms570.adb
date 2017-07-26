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

with System.BB.Parameters;

package body System.BB.Board_Support is
   use CPU_Primitives, BB.Interrupts;

   -------------------------------
   -- Real-Time Interrupt (RTI) --
   -------------------------------

   RTI_Base     : constant Address := 16#FFFF_FC00#;

   RTIGCTRL     : constant Address := RTI_Base + 16#00#;
   RTICOMPCTRL  : constant Address := RTI_Base + 16#0C#;

   RTIFRC1      : constant Address := RTI_Base + 16#30#;
   RTIUC1       : constant Address := RTI_Base + 16#34#;
   RTICPUC1     : constant Address := RTI_Base + 16#38#;

   RTICOMP3     : constant Address := RTI_Base + 16#68#;

   RTISETINTENA : constant Address := RTI_Base + 16#80#;
   RTIINTFLAG   : constant Address := RTI_Base + 16#88#;

   RTI_Compare_Interrupt_3 : constant Interrupt_ID := 5;
   --  We use the compare unit 3, so the first counter and the first three
   --  compare units are available for use by the user.

   -------------------------
   -- Software Interrupts --
   -------------------------

   --  As result of using a single counter and comparator for alarms, it is
   --  possible to miss the value to compare against, so the alarm interrupt
   --  will not be given. For this reason, it is necessary to use a software
   --  generated interrupt.

   --  The TMS570 has 4 registers, SSIR1 to SSIR4, for generating software
   --  interrupts. Reading the SSIVEC register yields the highest active
   --  software interrupt, with its associated data byte, and clears it.
   --  In order to also allow user handlers for this interrupt and to avoid
   --  accidentally clearing those, the runtime uses the SSIR1 register. This
   --  way, the SSIF register tells reliably if the request is ours or not.

   --  The data byte is interpreted as Interrupt_ID to handle. User programs
   --  can generate arbitrary interrupts by writing the value SSIR1_Key + X,
   --  where requested X is the Interrupt_ID. The software interrupt is
   --  automatically cleared.

   SSIR1 : Word;
   pragma Volatile (SSIR1);
   for SSIR1'Address use 16#FFFF_FFB0#;
   --  Register to generate software interrupts

   SSIR1_Key : constant Word := 16#7500#;
   --  Value to write to SSIR1 to trigger an interrupt

   SSIF : Word;
   pragma Volatile (SSIF);
   for SSIF'Address use 16#FFFF_FFF8#;
   --  Software interrupt flag register. The bits 0 .. 3 are used for
   --  SSIR1 .. SSIR4 respectively.

   SSIFLAG1 : constant Word := 2**0;
   --  Bit in the SSIF register corresponding to SSIR4

   SSIVEC : Word;
   pragma Volatile (SSIVEC);
   for SSIVEC'Address use 16#FFFF_FFF4#;
   --  Reading returns 256 * D + N, where N is the number of the SSIR register
   --  causing the interrupt (1 .. 4) and D is the data byte used. Returns zero
   --  if no software interrupt is pending.

   Software_Interrupt : constant Interrupt_ID := 21;
   --  Interrupt ID for software interrupts

   procedure Generate_Software_Interrupt (ID : Interrupt_ID);
   --  Generate an interrupt request corresponding to the given ID

   function Get_Software_Interrupt return Interrupt_ID;
   --  Return the Interrupt_ID of the pending SSIR1 software interrupt,
   --  or 0 when no such software interrupt is pending.

   procedure Irq_Interrupt_Handler;
   procedure Fiq_Interrupt_Handler;
   --  Low-level interrupt handlers

   --------------------------------------
   -- Vectored Interrupt Manager (VIM) --
   --------------------------------------

   VIM_Base : constant Address := 16#FFFF_FE00#;

   IRQINDEX : constant Address := VIM_Base + 16#00#;
   FIQINDEX : constant Address := VIM_Base + 16#04#;

   --  The addresses below refer to the first word of a three-word bitmap
   --  with one bit per interrupt channel.

   FIRQPR0 : constant Address := VIM_Base + 16#10#;
   --  Register with one bit set for each interrupt that is an FIQ interrupt.
   --  Interrupt sources 32 and over are not supported as FIQ by this run time.
   --  These interrupts should be remapped to lower interrupt channels when
   --  required as FIQ. The FIQ_Ints must always include the bits set by
   --  NMI_Ints, as the hardware cannot mask these. Initialize_Boards will
   --  update this register

   REQENASET0 : constant Address := VIM_Base + 16#30#;
   --  Writing a bit mask to this register enables the corresponding interrupts

   REQENACLR0 : constant Address := VIM_Base + 16#40#;
   REQENACLR1 : constant Address := VIM_Base + 16#44#;
   REQENACLR2 : constant Address := VIM_Base + 16#48#;
   --  Writing a bit mask to this register clears the corresponding interrupts

   WAKEENASET0 : constant Address := VIM_Base + 16#50#;
   --  Bit mask allowing corresponding interrupts to wake the processor

   procedure Enable_Interrupt_Request (Interrupt : Interrupt_ID);
   --  Enable interrupt requests for the given interrupt

   function Index_To_Interrupt (Index : Word) return Interrupt_ID is
     (case Index is
      when 0 => 0,
      when 1 => Interrupt_ID (1),
      when Word (Software_Interrupt + 1) => Get_Software_Interrupt,
      when others => Interrupt_ID (Index - 1));
   --  The IRQINDEX and FIQINDEX registers return the index into a vector table
   --  that starts with a dummy "phantom" entry, so the VIM Interrupt Channel
   --  is generally 1 less. While we the mapping from channel to Interrupt_ID
   --  is generally direct, we map Channel 0 to Interrupt_ID (1), to avoid
   --  confusion with No_Interrupt. This works out fine since VIM channel 1
   --  is reserved.

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

   NMI_Ints  : constant Word := 3;
   --  Bitmap of unmaskable interrupts, namely interrupt channel 0 and 1

   FIQ_Masked : Boolean := False;
   --  Reflects wether FIQ interrupts are masked in the VIM or not

   procedure IRQ_Handler;
   pragma Import (Asm, IRQ_Handler, "__gnat_irq_trap");

   procedure FIQ_Handler;
   pragma Import (Asm, FIQ_Handler, "__gnat_fiq_trap");

   --  Local utility functions

   function Shift_Left (W : Word; Amount : Natural) return Word
      with Import, Convention => Intrinsic;
   --  Efficiently compute W / 2**Amount

   function  Read  (Addr : Address) return Word with Inline;
   procedure Write (Addr : Address; Val : Word) with Inline;
   --  General functions to read/write from/to specific memory locations

   ----------------------------
   -- Get_Software_Interrupt --
   ----------------------------

   function Get_Software_Interrupt return Interrupt_ID is
     (if (SSIF and SSIFLAG1) = 0 then 0 else Interrupt_ID (SSIVEC / 256));
   --  Return the interrupt for the handler to call. This value is written
   --  as data byte to the SSIR1 register, and must be a valid Interrupt_ID.

   ----------
   -- Read --
   ----------

   function Read (Addr : Address) return Word is
      R : Word;
      for R'Address use Addr;
      pragma Volatile (R);
   begin
      return R;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Addr : Address; Val : Word) is
      R : Word;
      for R'Address use Addr;
      pragma Volatile (R);
   begin
      R := Val;
   end Write;

   ---------------------------------
   -- Generate_Software_Interrupt --
   ---------------------------------

   procedure Generate_Software_Interrupt (ID : Interrupt_ID) is
   begin
      SSIR1 := SSIR1_Key + Word (ID);
   end Generate_Software_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Disable all interrupts, except for NMIs

      Write (REQENACLR0, not NMI_Ints);
      Write (REQENACLR1, not 0);
      Write (REQENACLR2, not 0);

      --  Initialize timer

      --  The counter needs to be disabled while programming it

      Write (RTIGCTRL, Read (RTIGCTRL) and not 2); -- Turn off timer/counter 1
      Write (RTICPUC1, Parameters.Prescaler - 1);  -- Program prescaler compare
      Write (RTIUC1, 0);                           -- Start prescaler at 0
      Write (RTIFRC1, 0);                          -- Start clock at 0
      Write (RTICOMPCTRL, 2**12);
      Write (RTIINTFLAG, 2**3);
      Write (RTIGCTRL, Read (RTIGCTRL) or 2);      -- Turn timer/counter 1 on
      Write (RTISETINTENA, 2**3);                  -- Enable Interrupts

      --  Install low-level interrupt handler

      Install_Trap_Handler (Irq_Interrupt_Handler'Address, 5);
      Install_Trap_Handler (Fiq_Interrupt_Handler'Address, 6);

      --  Allow the generation of software interrupts

      Interrupt_Vectors (Software_Interrupt) := IRQ_Handler'Address;
      Enable_Interrupt_Request (Software_Interrupt);
   end Initialize_Board;

   package body Time is
      Alarm_Interrupt_ID : constant Interrupt_ID := RTI_Compare_Interrupt_3;
      --  Interrupt for the alarm

      ------------------------
      -- Max_Timer_Interval --
      ------------------------

      function Max_Timer_Interval return Timer_Interval is (2**32 - 1);

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : Timer_Interval) is
         Now     : constant Timer_Interval := Timer_Interval (Read_Clock);
         Alarm   : constant Timer_Interval := Now + Ticks;
         Elapsed : Timer_Interval;

      begin
         Write (RTIINTFLAG, 2**3); --  Clear any pending alarms
         Write (RTICOMP3, Word (Alarm));
         Elapsed := Timer_Interval (Read_Clock) - Now;

         if Elapsed >= Ticks and then (Read (RTIINTFLAG) and 2**3) = 0 then
            Generate_Software_Interrupt (RTI_Compare_Interrupt_3);
         end if;
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
         (BB.Time.Time (Read (RTIFRC1)));

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         Write (RTIINTFLAG, 2**3);
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

   procedure Irq_Interrupt_Handler is
      Id : constant Interrupt_ID := Index_To_Interrupt (Read (IRQINDEX));
   begin
      Interrupt_Wrapper (Id);
   end Irq_Interrupt_Handler;

   ---------------------------
   -- Fiq_Interrupt_Handler --
   ---------------------------

   procedure Fiq_Interrupt_Handler is
      Id : constant Interrupt_ID := Index_To_Interrupt (Read (FIQINDEX));
   begin
      Interrupt_Wrapper (Id);
   end Fiq_Interrupt_Handler;

   ------------------------------
   -- Enable_Interrupt_Request --
   ------------------------------

   procedure Enable_Interrupt_Request (Interrupt : Interrupt_ID) is
      Regofs : constant Address := Address (Interrupt) / 32 * 4;
      Regbit : constant Word := Shift_Left (1, Interrupt mod 32);
      --  Many VIM registers use 3 words of 32 bits each to serve as a bitmap
      --  for all interrupt channels. Regofs indicates register offset (0..2),
      --  and Regbit indicates the mask required for addressing the bit.

   begin
      Write (REQENASET0 + Regofs, Regbit);
      Write (WAKEENASET0 + Regofs, Regbit);
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
         (if Interrupt <= 31 and then (Read (FIRQPR0) and 2**Interrupt) /= 0
          then FIQ_Prio
         else IRQ_Prio);

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         --  On the TMS570, FIQs cannot be masked by the processor. So, we need
         --  to disable them at the controller when required.

         if (Priority = FIQ_Prio) xor FIQ_Masked then
            if Priority = FIQ_Prio then
               Write (REQENACLR0, Read (FIRQPR0));
               FIQ_Masked := True;

            else
               Write (REQENASET0, Read (FIRQPR0));
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
         null;
      end Power_Down;
   end Interrupts;
end System.BB.Board_Support;
