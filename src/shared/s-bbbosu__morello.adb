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

--  This is the Morello + GIC-600 version of this package

with System.ARM_GIC;
with System.BB.CPU_Primitives.Multiprocessors;
with System.BB.Parameters;
with Interfaces;                 use Interfaces;
with Interfaces.AArch64;

package body System.BB.Board_Support is
   use BB.Interrupts, AArch64, Parameters;
   use System.Multiprocessors;

   -----------
   -- Timer --
   -----------

   Alarm_Interrupt_ID : constant BB.Interrupts.Interrupt_ID := 30;
   --  Phys. Non-secure timer

   ------------------
   -- Set_CNTP_CTL --
   ------------------

   procedure Set_CNTP_CTL (Val : Unsigned_32)
     renames Set_CNTP_CTL_EL0;

   -------------------
   -- Set_CNTP_CVAL --
   -------------------

   procedure Set_CNTP_CVAL (Val : Unsigned_64)
     renames Set_CNTP_CVAL_EL0;

   -------------
   -- GIC-600 --
   -------------

   package GIC renames System.ARM_GIC;

   procedure Initialize_CPU_Devices;
   pragma Export (C, Initialize_CPU_Devices, "__gnat_initialize_cpu_devices");
   --  Per CPU device initialization

   ----------------------------
   -- Initialize_CPU_Devices --
   ----------------------------

   procedure Initialize_CPU_Devices
   is
   begin
      --  Timer: using the non-secure physical timer
      --  at init, we disable both the physical and the virtual timers
      --  the physical timer is awakened when we need it.

      --  Disable CNTP signals and mask its IRQ.
      Set_CNTP_CTL (2#10#);

      --  Disable CNTV and mask.
      Set_CNTV_CTL_EL0 (2);

      --  Core-specific part of the GIC configuration:
      --  The GICC (CPU Interface) is banked for each CPU, so has to be
      --  configured each time.
      GIC.Initialize_ICC;
   end Initialize_CPU_Devices;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board
   is
   begin
      GIC.Initialize_GICD;

      Initialize_CPU_Devices;
   end Initialize_Board;

   package body Time is

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time)
      is
         use type BB.Time.Time;
      begin
         if Ticks = BB.Time.Time'Last then
            Clear_Alarm_Interrupt;
         else
            --  Set Timer comparator value
            Set_CNTP_CVAL (Unsigned_64 (Ticks));

            --  Set CNTP_CTL (enable and unmask)
            Set_CNTP_CTL (2#01#);
         end if;
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

      procedure Install_Alarm_Handler (Handler : Interrupt_Handler)
      is

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
         --  mask the IRQ and disable signals
         Set_CNTP_CTL (2#10#);
      end Clear_Alarm_Interrupt;
   end Time;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler is new GIC.IRQ_Handler
     (Interrupt_Wrapper => Interrupt_Wrapper);
   pragma Export (C, IRQ_Handler, "__gnat_irq_handler");
   --  Low-level interrupt handler

   package body Interrupts is

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority) renames GIC.Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority renames GIC.Priority_Of_Interrupt;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer)
        renames GIC.Set_Current_Priority;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down renames GIC.Power_Down;
   end Interrupts;

   package body Multiprocessors is
      Poke_Interrupt : constant Interrupt_ID := 0;
      --  Use SGI #0

      procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID);
      --  Handler for the Poke interrupt

      procedure Start
        with Import, External_Name => "__start";
      --  Entry point (in crt0) for a slave cpu

      procedure CPU_Wake_Up
        (CPU_Id        : CPU;
         Start_Address : System.Address);
      --  Reset and wake_up the specified CPU

      --------------------
      -- Number_Of_CPUs --
      --------------------

      function Number_Of_CPUs return CPU is (CPU'Last);

      -----------------
      -- Current_CPU --
      -----------------

      function Current_CPU return CPU renames GIC.Current_CPU;

      --------------
      -- Poke_CPU --
      --------------

      procedure Poke_CPU (CPU_Id : CPU)
      is
      begin
         GIC.Poke_CPU (CPU_Id, Poke_Interrupt);
      end Poke_CPU;

      -----------------
      -- CPU_Wake_Up --
      -----------------

      procedure CPU_Wake_Up
        (CPU_Id        : CPU;
         Start_Address : System.Address)
      is
         pragma Unreferenced (CPU_Id);
         pragma Unreferenced (Start_Address);
      begin
         null;
      end CPU_Wake_Up;

      --------------------
      -- Start_All_CPUs --
      --------------------

      procedure Start_All_CPUs is
      begin
         if System.BB.Parameters.Multiprocessor and then Number_Of_CPUs > 1
         then
            --  Note: this gets never called in the Uniprocessor case.
            BB.Interrupts.Attach_Handler
              (Poke_Handler'Access, Poke_Interrupt, Interrupt_Priority'Last);

            --  Disable warnings for non-SMP case
            pragma Warnings (Off, "loop range is null*");

            for CPU_Id in CPU'First + 1 .. Number_Of_CPUs loop
               CPU_Wake_Up (CPU_Id, Start'Address);
            end loop;

            pragma Warnings (On, "loop range is null*");
         end if;
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
