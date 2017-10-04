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
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with System.Machine_Code;
with System.BB.CPU_Primitives.Multiprocessors;
with System.BB.Parameters; use System.BB.Parameters;
with Interfaces;           use Interfaces;
with Interfaces.AArch64;   use Interfaces.AArch64;
with Interfaces.Raspberry_Pi;

package body System.BB.Board_Support is
   use BB.Interrupts;
   use System.Machine_Code;
   use System.Multiprocessors;
   use Interfaces.Raspberry_Pi;

   procedure IRQ_Handler;
   pragma Export (C, IRQ_Handler, "__gnat_irq_handler");
   --  Low-level interrupt handler

   procedure Initialize_CPU_Devices;
   pragma Export (C, Initialize_CPU_Devices, "__gnat_initialize_cpu_devices");
   --  Per CPU device initialization

   procedure Set_CNTP_CVAL (Val : Unsigned_64)
     with Inline_Always;
   --  Set CNTP_CVAL_EL0 or CNTHP_CVAL_EL2

   procedure Set_CNTP_CTL (Val : Unsigned_32)
     with Inline_Always;
   --  Set CNTP_CTL_EL0 or CNTP_CTL_EL2

   ------------------
   -- Set_CNTP_CTL --
   ------------------

   procedure Set_CNTP_CTL (Val : Unsigned_32) is
   begin
      case Runtime_EL is
         when 1 =>
            Set_CNTP_CTL_EL0 (Val);
         when 2 =>
            Set_CNTHP_CTL_EL2 (Val);
      end case;
   end Set_CNTP_CTL;

   -------------------
   -- Set_CNTP_CVAL --
   -------------------

   procedure Set_CNTP_CVAL (Val : Unsigned_64) is
   begin
      case Runtime_EL is
         when 1 =>
            Set_CNTP_CVAL_EL0 (Val);
         when 2 =>
            Set_CNTHP_CVAL_EL2 (Val);
      end case;
   end Set_CNTP_CVAL;

   ----------------------------
   -- Initialize_CPU_Devices --
   ----------------------------

   procedure Initialize_CPU_Devices is
   begin
      --  Disable CNTP and mask.
      Set_CNTP_CTL (2);

      --  Disable CNTV and mask.
      Set_CNTV_CTL_EL0 (2);
   end Initialize_CPU_Devices;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      Discard : Unsigned_32;
   begin
      --  Timer: set prescalar to 1, using crystal (19.2 Mhz)
      Local_Registers.Core_Timer_Prescaler := 16#8000_0000#;
      Local_Registers.Control := 0;

      --  Read MS to unlock the LS.
      Discard := Local_Registers.Core_Timer_MS;

      --  Initialize (LS then MS)
      Local_Registers.Core_Timer_LS := 0;
      Local_Registers.Core_Timer_MS := 0;

      --  GPU interrupt routing to core 1
      Local_Registers.GPU_Int_Routing := 2#00_00#;

      --  Disable PMU ints
      Local_Registers.PMU_Int_Routing_Clr := 2#1111_1111#;

      --  Core timers to IRQ
      for I in Core_Unsigned_32'Range loop
         Local_Registers.Cores_Timer_Int_Ctr (I) := 2#0000_1111#;
      end loop;

      --  Mailbox to IRQ
      for I in Core_Unsigned_32'Range loop
         Local_Registers.Cores_Mailboxes_Int_Ctr (I) := 2#0000_1111#;
      end loop;

      Initialize_CPU_Devices;
   end Initialize_Board;

   package body Time is

      Alarm_Interrupt_ID : constant BB.Interrupts.Interrupt_ID :=
                             (case Runtime_EL is when 1 => 1, when 2 => 2);
      --  Non-secure counter (CNTPNSIRQ)

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time) is
      begin
         --  Set CNTP_CVAL_EL
         Set_CNTP_CVAL (Unsigned_64 (Ticks));

         --  Set CNTP_CTL (enable and unmask)
         Set_CNTP_CTL (1);
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
      begin
         --  Read CNTPCT
         return BB.Time.Time (Get_CNTPCT_EL0);
      end Read_Clock;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler (Handler : Interrupt_Handler) is
      begin
         --  Attach interrupt handler
         BB.Interrupts.Attach_Handler
           (Handler,
            Alarm_Interrupt_ID,
            Interrupts.Priority_Of_Interrupt (Alarm_Interrupt_ID));
      end Install_Alarm_Handler;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is
      begin
         --  Disable and mask
         Set_CNTP_CTL (2);
      end Clear_Alarm_Interrupt;
   end Time;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler
   is
      This_CPU  : constant CPU := Multiprocessors.Current_CPU;

      Src       : constant Unsigned_32 :=
                    Local_Registers.Cores_IRQ_Source (Natural (This_CPU));
      --  Read interrupt source from Core_IRQ_Source

      Pending   : Unsigned_32;
      IRQ       : Interrupt_ID;
      Base      : Unsigned_32;

      subtype Basic_IRQ is Interrupt_ID range 0 .. 11;

      function CTZ (X : Unsigned_32) return Unsigned_32
        with Import, Convention => Intrinsic, External_Name => "__builtin_ctz";
      --  Returns the number of leading zeros in X, starting at the least
      --  significant position. X must not be zero.

   begin

      if Src = 0 then
         --  No interrupt
         return;
      end if;

      --  Retrieve the IRQ number
      Base := CTZ (Src);

      if Base = 8 then
         --  GPU interrupt: retrieve the source IRQ
         IRQ := Basic_IRQ'Last + 1;
         Pending := Arm_Interrupts.Irq_Pending_1;

         if Pending = 0 then
            Pending := Arm_Interrupts.Irq_Pending_2;
            IRQ := IRQ + 32;
         end if;

         if Pending = 0 then
            return;
         end if;

         IRQ := IRQ + Interrupt_ID (CTZ (Pending));

         Interrupt_Wrapper (IRQ);

      else
         Interrupt_Wrapper (Interrupt_ID (Base));
      end if;
   end IRQ_Handler;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
         Int_Num        : Natural;
         Reg_Value      : Unsigned_32;
         GPU_Int_Base_1 : constant := 12;
         GPU_Int_Base_2 : constant := 44;
      begin
         if Interrupt < 12 then
            return;

         elsif Interrupt < GPU_Int_Base_2 then
            Int_Num := Natural (Interrupt) - GPU_Int_Base_1;
            Reg_Value := 2 ** Int_Num;
            Arm_Interrupts.Enable_Irq_1 := Reg_Value;

         else
            Int_Num := Natural (Interrupt) - GPU_Int_Base_2;
            Reg_Value := 2 ** Int_Num;
            Arm_Interrupts.Enable_Irq_2 := Reg_Value;
         end if;
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
      is
         pragma Unreferenced (Interrupt);
      begin
         return Interrupt_Priority'Last;  --  IRQ
      end Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         null;
      end Set_Current_Priority;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         Asm ("wfi", Volatile => True);
      end Power_Down;
   end Interrupts;

   package body Multiprocessors is
      Poke_Interrupt : constant Interrupt_ID := 7;
      --  Use mailbox 3

      procedure Start_Slave_Cpu
        with Import, External_Name => "__start_slave_cpu";
      --  Entry point (in crt0) for a slave cpu

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID);
      --  Handler for the Poke interrupt

      function MPIDR return Unsigned_32 with Inline_Always;
      --  Return current value of the MPIDR register

      --------------------
      -- Number_Of_CPUs --
      --------------------

      function Number_Of_CPUs return CPU is
      begin
         return CPU'Last;
      end Number_Of_CPUs;

      -----------
      -- MPIDR --
      -----------

      function MPIDR return Unsigned_32 is
         R : Unsigned_32;
      begin
         Asm ("mrs %0, mpidr_el1",
              Outputs => Unsigned_32'Asm_Output ("=r", R),
              Volatile => True);
         return R and 3;
      end MPIDR;

      -----------------
      -- Current_CPU --
      -----------------

      function Current_CPU return CPU is

         --  Get CPU Id from bits 1:0 from the MPIDR register

         (if CPU'Last = 1 then 1 else CPU ((MPIDR and 3) + 1));

      --------------
      -- Poke_CPU --
      --------------

      procedure Poke_CPU (CPU_Id : CPU) is
      begin
         Local_Registers.Cores_Mailboxes_Write_Set (Natural (CPU_Id), 3) := 1;
      end Poke_CPU;

      --------------------
      -- Start_All_CPUs --
      --------------------

      procedure Start_All_CPUs is
      begin
         BB.Interrupts.Attach_Handler
           (Poke_Handler'Access, Poke_Interrupt, Interrupt_Priority'Last);

         --  Disable warnings for non-SMP case
         pragma Warnings (Off, "loop range is null*");

         for CPU_Id in CPU'First + 1 .. CPU'Last loop
            Local_Registers.Cores_Mailboxes_Write_Set (Natural (CPU_Id), 3) :=
              Unsigned_32 (Start_Slave_Cpu'Address);
         end loop;

         --  Send event
         Asm ("sev", Volatile => True);

         pragma Warnings (On, "loop range is null*");
      end Start_All_CPUs;

      ------------------
      -- Poke_Handler --
      ------------------

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID) is
         This_CPU : constant CPU := Multiprocessors.Current_CPU;
      begin
         --  Make sure we are handling the right interrupt
         pragma Assert (Interrupt = Poke_Interrupt);

         --  Clear bit in mailbox
         Local_Registers.Cores_Mailboxes_Read_Clr (Natural (This_CPU), 3) := 1;

         System.BB.CPU_Primitives.Multiprocessors.Poke_Handler;
      end Poke_Handler;
   end Multiprocessors;

end System.BB.Board_Support;
