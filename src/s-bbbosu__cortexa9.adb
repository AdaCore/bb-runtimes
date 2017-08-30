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

with Interfaces; use Interfaces;
with System.Machine_Code;
with System.BB.CPU_Primitives.Multiprocessors;

package body System.BB.Board_Support is
   use BB.Interrupts;

   procedure IRQ_Handler;
   pragma Export (Ada, IRQ_Handler, "__gnat_irq_handler");

   procedure FIQ_Handler;
   pragma Export (Ada, FIQ_Handler, "__gnat_fiq_handler");
--  Low-level interrupt handler

   procedure Initialize_CPU_Devices;
   pragma Export (C, Initialize_CPU_Devices, "__gnat_initialize_cpu_devices");
   --  Per CPU device initialization

   MPCore_Base : constant := 16#F8F0_0000#;

   -----------
   -- Timer --
   -----------

   Global_Timer_Base : constant := MPCore_Base + 16#200#;

   Global_Timer_Counter0         : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#00#;
   Global_Timer_Counter1         : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#04#;
   Global_Timer_Control          : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#08#;
   Global_Timer_Interrupt_Status : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#0C#;
   Global_Timer_Comparator0      : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#10#;
   Global_Timer_Comparator1      : Unsigned_32
     with Import, Volatile, Address => Global_Timer_Base + 16#14#;

   ----------------
   -- Interrupts --
   ----------------

   type PRI is mod 2**8;
   --  Type for GIC interrupt priorities. Note that 0 is the highest priority,
   --  which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest. We assume the PRIGROUP
   --  setting is such that the 4 most significant bits determine the priority
   --  group used for preemption. However, if less bits are implemented, this
   --  should still work.

   function To_PRI (P : Integer) return PRI;
   pragma Inline (To_PRI);
   --  Return the PRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.
   --  ??? The compiler crashes if this is an expression function.

   function To_PRI (P : Integer) return PRI is
   begin
      if P not in Interrupt_Priority then
         return 255;
      else
         return PRI (Interrupt_Priority'Last - P) * 16;
      end if;
   end To_PRI;

   function To_Priority (P : PRI) return Interrupt_Priority;
   pragma Inline (To_Priority);
   --  Given an ARM interrupt priority (PRI value), determine the Ada priority
   --  While the value 0 is reserved for the kernel and has no Ada priority
   --  that represents it, Interrupt_Priority'Last is closest.
   --  ??? The compiler crashes if this is an expression function.

   function To_Priority (P : PRI) return Interrupt_Priority is
   begin
      if P = 0 then
         return Interrupt_Priority'Last;
      else
         return Interrupt_Priority'Last - Any_Priority'Base (P / 16);
      end if;
   end To_Priority;

   ICCICR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#100#;
   ICCPMR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#104#;
   ICCBPR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#108#;
   ICCIAR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#10c#;
   ICCEOIR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#110#;
   ICDDCR : Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#1000#;
   ICDISER : array (0 .. 2) of Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#1100#;
   ICDICER : array (0 .. 2) of Unsigned_32
       with Import, Volatile, Address => MPCore_Base + 16#1180#;
   ICDIPR : array (BB.Interrupts.Interrupt_ID) of PRI
     with Import, Volatile, Address => MPCore_Base + 16#1400#;
   ICDIPTR : array (0 .. 23) of Unsigned_32
     with Import, Volatile, Address => MPCore_Base + 16#1800#;
   ICDICFR : array (0 .. 5) of Unsigned_32
     with Import, Volatile, Address => MPCore_Base + 16#1c00#;

   ----------------------------
   -- Initialize_CPU_Devices --
   ----------------------------

   procedure Initialize_CPU_Devices is
   begin
      --  Make sure the Global timer IRQ is cleared
      Global_Timer_Interrupt_Status := 1;
      --  Then enable (prescaler = 0).
      --  Bits 1-3 are bancked per core
      Global_Timer_Control := 16#00_0_1#;

      --  Disable all SGI interrupts (banked register)
      ICDICER (0) := 16#ffff_ffff#;

      --  Enable forwarding to CPU (banked register)
      ICDDCR := 3;

      --  Enable private timer (banked register)
      ICDISER (0) := 2**27;

      --  Set prio of Poke (banked register)
      ICDIPR (0) := To_PRI (Interrupt_Priority'Last);

      --  Set prio of timer (banked register);
      ICDIPR (27) := To_PRI (Interrupt_Priority'Last);
   end Initialize_CPU_Devices;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Setup global timer
      --  First stop and clear
      Global_Timer_Control := 0;
      Global_Timer_Counter0 := 0;
      Global_Timer_Counter1 := 0;

      --  Initialize SPI of the mpcore.

      --  Disable SPI interrupts
      if False then
         --  Generate a data abort ???
         ICDICER (1 .. 2) := (others => 16#ffff_ffff#);
      else
         ICDICER (1) := 16#ffff_ffff#;
         ICDICER (2) := 16#ffff_ffff#;
      end if;

      --  Mask all interrupts
      ICCPMR := 0;

      --  Binary point register
      ICCBPR := 3;

      --  Route to IRQ
      ICCICR := 2#0011#;

      --  Level: 01: high level, 11: rising-edge
      --  See ug585 table 7.4 for the values.
      --  IRQs 47 - 32
      ICDICFR (2) := 2#01_01_01_01_01_01_11_01_01_01_01_00_01_01_11_11#;
      --  IRQs 63 - 48
      ICDICFR (3) := 2#01_01_01_01_01_01_01_01_11_01_01_01_01_01_01_01#;
      --  IRQs 79 - 64
      ICDICFR (4) := 2#01_11_01_01_01_01_01_01_01_01_01_01_01_01_01_01#;
      --  IRQs 95 - 80
      ICDICFR (5) := 2#00_00_00_01_01_01_01_01_01_01_01_01_01_01_01_01#;

      --  Target: always target cpu 0
      ICDIPTR  (8) := 16#01_01_01_01#;
      ICDIPTR  (9) := 16#01_01_01_01#;
      ICDIPTR (10) := 16#01_01_01_01#;
      ICDIPTR (11) := 16#01_01_01_01#;
      ICDIPTR (12) := 16#01_01_01_01#;
      ICDIPTR (13) := 16#01_01_01_01#;
      ICDIPTR (14) := 16#01_01_01_01#;
      ICDIPTR (15) := 16#01_01_01_01#;
      ICDIPTR (16) := 16#01_01_01_01#;
      ICDIPTR (17) := 16#01_01_01_01#;
      ICDIPTR (18) := 16#01_01_01_01#;
      ICDIPTR (19) := 16#01_01_01_01#;
      ICDIPTR (20) := 16#01_01_01_01#;
      ICDIPTR (21) := 16#01_01_01_01#;
      ICDIPTR (22) := 16#01_01_01_01#;
      ICDIPTR (23) := 16#01_01_01_01#;

      Initialize_CPU_Devices;
   end Initialize_Board;

   package body Time is

      Alarm_Interrupt_ID : constant BB.Interrupts.Interrupt_ID := 27;
      --  Use the global timer interrupt

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time)
      is
         use BB.Time;
         Lo : constant Unsigned_32 := Unsigned_32 (Ticks and 16#FFFF_FFFF#);
         Hi : constant Unsigned_32 :=
                Unsigned_32 (Shift_Right (Unsigned_64 (Ticks), 32));
      begin
         if Ticks = BB.Time.Time'Last then
            Clear_Alarm_Interrupt;

         else
            --  Set comparator using the 64-bit private timer comparator.
            --  See Cortex-A9 Technical Reference Manual §4.3.
            --  Requires revision >= r2p0, otherwise no exception is raised for
            --  past counter values.

            --  Clear the comp_enable bit
            Global_Timer_Control := Global_Timer_Control and not 2#010#;

            --  Write Lo/Hi comparator values
            Global_Timer_Comparator0 := Lo;
            Global_Timer_Comparator1 := Hi;

            --  Enable timer and IRQ
            Global_Timer_Control := 2#111#;
         end if;
      end Set_Alarm;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
         use BB.Time;
         Lo  : Unsigned_32;
         Hi  : Unsigned_32;
         Hi1 : Unsigned_32;

      begin
         --  We can't atomically read the 64-bits counter. So check that the
         --  32 MSB don't change.

         Hi := Global_Timer_Counter1;
         loop
            Lo := Global_Timer_Counter0;
            Hi1 := Global_Timer_Counter1;
            exit when Hi = Hi1;
            Hi := Hi1;
         end loop;

         return (BB.Time.Time (Hi) * 2 ** 32) + BB.Time.Time (Lo);
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
         Global_Timer_Control := 2#001#;
         Global_Timer_Interrupt_Status := 1;
      end Clear_Alarm_Interrupt;
   end Time;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler
   is
      Iar : constant Unsigned_32 := ICCIAR;
      Int_Id : constant Unsigned_32 := Iar and 16#3ff#;
   begin
      if Int_Id = 16#3ff# then
         --  Spurious interrupt
         return;
      end if;

      Interrupt_Wrapper (Interrupt_ID (Int_Id));

      --  Clear interrupt request
      ICCEOIR := Iar;
   end IRQ_Handler;

   -----------------
   -- FIQ_Handler --
   -----------------

   procedure FIQ_Handler is
   begin
      --  Not supported
      raise Program_Error;
   end FIQ_Handler;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority) is
      begin
         ICDIPR (Interrupt) := To_PRI (Prio);
         ICDISER (Interrupt / 32) := 2** (Interrupt mod 32);
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority is
      begin
         return To_Priority (ICDIPR (Interrupt));
      end Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         ICCPMR := Unsigned_32 (To_PRI (Priority));
      end Set_Current_Priority;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
         use System.Machine_Code;
      begin
         Asm ("wfi", Volatile => True);
      end Power_Down;
   end Interrupts;

   package body Multiprocessors is
      use System.Machine_Code;
      use System.Multiprocessors;

      ICDSGIR : Unsigned_32
        with Import, Volatile, Address => System'To_Address (16#F8f0_1f00#);

      Poke_Interrupt : constant Interrupt_ID := 0;
      --  Use SGI #0

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID);
      --  Handler for the Poke interrupt

      function MPIDR return Unsigned_32;
      --  Return current value of the MPIDR register

      procedure Start_CPU (CPU_Id : CPU);
      --  Start one cpu

      --  SCU configuration register
      SCU_Configuration : Unsigned_32
        with Address => 16#F8F0_0004#, Volatile, Import;

      --------------------
      -- Number_Of_CPUs --
      --------------------

      function Number_Of_CPUs return CPU is
         NCPUs : CPU;
      begin
         NCPUs := CPU (1 + (SCU_Configuration and 3));
         return NCPUs;
      end Number_Of_CPUs;

      -----------
      -- MPIDR --
      -----------

      function MPIDR return Unsigned_32 is
         R : Unsigned_32;
      begin
         Asm ("mrc p15,0,%0,c0,c0,5",
              Outputs => Unsigned_32'Asm_Output ("=r", R),
              Volatile => True);
         return R;
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
         --  There is no need to protect access to the register since the only
         --  operation applied to it is this assignment and it's always with
         --  the same value (Poke_Interrupt).

         --  No race condition possible here.

         ICDSGIR :=
           2**(16 + Natural (CPU_Id - 1)) + Unsigned_32 (Poke_Interrupt);
      end Poke_CPU;

      ---------------
      -- Start_CPU --
      ---------------

      procedure Start_CPU (CPU_Id : CPU) is
         procedure Kick_Cpu1;
         pragma Import (C, Kick_Cpu1, "__kick_cpu1");
         --  Start processor #1
      begin
         --  Cannot be true on one processor configuration
         pragma Warnings (Off, "condition*");
         if CPU_Id = 2 then
            Kick_Cpu1;
         end if;
         pragma Warnings (On, "condition*");
      end Start_CPU;

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
            Start_CPU (CPU_Id);
         end loop;

         pragma Warnings (On, "loop range is null*");
      end Start_All_CPUs;

      ------------------
      -- Poke_Handler --
      ------------------

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID) is
      begin
         --  Make sure we are handling the right interrupt

         pragma Assert (Interrupt = Poke_Interrupt);

         System.BB.CPU_Primitives.Multiprocessors.Poke_Handler;
      end Poke_Handler;
   end Multiprocessors;

end System.BB.Board_Support;
