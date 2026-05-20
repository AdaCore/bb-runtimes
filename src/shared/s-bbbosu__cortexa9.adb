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
--                     Copyright (C) 2003-2022, AdaCore                     --
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
with System.ARM_GIC;
with System.Machine_Code;
with System.BB.CPU_Primitives.Multiprocessors;
with System.BB.Parameters; use System.BB.Parameters;

package body System.BB.Board_Support is
   use BB.Interrupts;

   ----------------
   -- Interrupts --
   ----------------

   package GIC renames System.ARM_GIC;

   procedure IRQ_Handler is new GIC.IRQ_Handler (Interrupt_Wrapper);
   pragma Export (Ada, IRQ_Handler, "__gnat_irq_handler");

   procedure FIQ_Handler;
   pragma Export (Ada, FIQ_Handler, "__gnat_fiq_handler");
--  Low-level interrupt handler

   procedure Initialize_CPU_Devices;
   pragma Export (C, Initialize_CPU_Devices, "__gnat_initialize_cpu_devices");
   --  Per CPU device initialization

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

      GIC.Initialize_GICC;
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

      GIC.Initialize_GICD;

      --  Level: 01: high level, 11: rising-edge
      --  See ug585 table 7.4 for the values.
      GIC.Define_IRQ_Triggers
        ((
          --  IRQs 47 - 32
          2 => 2#01_01_01_01_01_01_11_01_01_01_01_00_01_01_11_11#,
          --  IRQs 63 - 48
          3 => 2#01_01_01_01_01_01_01_01_11_01_01_01_01_01_01_01#,
          --  IRQs 79 - 64
          4 => 2#01_11_01_01_01_01_01_01_01_01_01_01_01_01_01_01#,
          --  IRQs 95 - 80
          5 => 2#00_00_00_01_01_01_01_01_01_01_01_01_01_01_01_01#));

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
            --  See Cortex-A9 Technical Reference Manual 4.3.
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
            Interrupt_Priority'Last);
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
   -- FIQ_Handler --
   -----------------

   procedure FIQ_Handler is
   begin
      --  Not supported
      raise Program_Error;
   end FIQ_Handler;

   package body Interrupts is
      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
        renames GIC.Install_Interrupt_Handler;

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
        renames GIC.Priority_Of_Interrupt;

      procedure Set_Current_Priority (Priority : Integer)
        renames GIC.Set_Current_Priority;

      procedure Power_Down renames GIC.Power_Down;
   end Interrupts;

   package body Multiprocessors is
      use System.Machine_Code;
      use System.Multiprocessors;

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
         NCPUs : Unsigned_32;
      begin
         NCPUs :=
           Unsigned_32'Min
             (Unsigned_32 (CPU'Last), 1 + (SCU_Configuration and 3));

         return CPU (NCPUs);
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

         GIC.Poke_CPU (CPU_Id, Poke_Interrupt);
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
