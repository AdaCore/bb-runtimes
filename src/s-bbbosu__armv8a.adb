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

--  This is the ARMv8-A + GICv2 version of this package

with Ada.Unchecked_Conversion;
with System.ARM_GIC;
with System.BB.CPU_Primitives.Multiprocessors;
with System.BB.Parameters;
with System.Machine_Code;
with Interfaces;                 use Interfaces;
with Interfaces.AArch64;

package body System.BB.Board_Support is
   use BB.Interrupts, AArch64, Parameters;
   use System.Machine_Code;
   use System.Multiprocessors;

   -----------
   -- Timer --
   -----------

   Alarm_Interrupt_ID : constant BB.Interrupts.Interrupt_ID := 30;
   --  Phys. Non-secure timer

   --  System time stamp generator
   IOU_SCNTRS_Base_Address : constant := 16#FF26_0000#;
   SCNTRS_CTRL : Unsigned_32
     with Volatile, Import, Address => IOU_SCNTRS_Base_Address;
   SCNTRS_FREQ : Unsigned_32
     with Volatile, Import, Address => IOU_SCNTRS_Base_Address + 16#20#;

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

   -----------
   -- GICv2 --
   -----------

   package GIC renames System.ARM_GIC;

   ---------
   -- APU --
   ---------

   package APU is
      type Power_Down_Array is array (CPU) of Boolean with Pack;
      type UInt12 is mod 2 ** 12 with Size => 12;
      type UInt14 is mod 2 ** 14 with Size => 14;

      type POWERCTL_Register is record
         CPUPWRDWNREQ : Power_Down_Array;
         Unused_4_15  : UInt12;
         L2FLUSH_REQ  : Boolean;
         CLREXMONREQ  : Boolean;
         Unused_18_31 : UInt14;
      end record with Volatile_Full_Access, Size => 32;

      for POWERCTL_Register use record
         CPUPWRDWNREQ at 0 range 0 .. 3;
         Unused_4_15  at 0 range 4 .. 15;
         L2FLUSH_REQ  at 0 range 16 .. 16;
         CLREXMONREQ  at 0 range 17 .. 17;
         Unused_18_31 at 0 range 18 .. 31;
      end record;

      type APU_Registers is record
         ERR_CTRL     : Unsigned_32;
         ISR          : Unsigned_32;
         IMR          : Unsigned_32;
         IEN          : Unsigned_32;
         IDS          : Unsigned_32;
         CONFIG_0     : Unsigned_32;
         CONFIG_1     : Unsigned_32;
         RVBAR_ADDR0L : Unsigned_32;
         RVBAR_ADDR0H : Unsigned_32;
         RVBAR_ADDR1L : Unsigned_32;
         RVBAR_ADDR1H : Unsigned_32;
         RVBAR_ADDR2L : Unsigned_32;
         RVBAR_ADDR2H : Unsigned_32;
         RVBAR_ADDR3L : Unsigned_32;
         RVBAR_ADDR3H : Unsigned_32;
         ACE_CTRL     : Unsigned_32;
         SNOOP_CTRL   : Unsigned_32;
         PWRCTL       : POWERCTL_Register;
         PWRSTAT      : Unsigned_32;
      end record with Volatile;

      for APU_Registers use record
         ERR_CTRL     at 16#00# range 0 .. 31;
         ISR          at 16#10# range 0 .. 31;
         IMR          at 16#14# range 0 .. 31;
         IEN          at 16#18# range 0 .. 31;
         IDS          at 16#1C# range 0 .. 31;
         CONFIG_0     at 16#20# range 0 .. 31;
         CONFIG_1     at 16#24# range 0 .. 31;
         RVBAR_ADDR0L at 16#40# range 0 .. 31;
         RVBAR_ADDR0H at 16#44# range 0 .. 31;
         RVBAR_ADDR1L at 16#48# range 0 .. 31;
         RVBAR_ADDR1H at 16#4C# range 0 .. 31;
         RVBAR_ADDR2L at 16#50# range 0 .. 31;
         RVBAR_ADDR2H at 16#54# range 0 .. 31;
         RVBAR_ADDR3L at 16#58# range 0 .. 31;
         RVBAR_ADDR3H at 16#5C# range 0 .. 31;
         ACE_CTRL     at 16#60# range 0 .. 31;
         SNOOP_CTRL   at 16#80# range 0 .. 31;
         PWRCTL       at 16#90# range 0 .. 31;
         PWRSTAT      at 16#94# range 0 .. 31;
      end record;

      Regs : APU_Registers with Import, Address => 16#FD5C_0000#;

   end APU;

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
      --  the pysical timer is awaken when we need it.

      --  Disable CNTP signals and mask its IRQ.
      Set_CNTP_CTL (2#10#);

      --  Disable CNTV and mask.
      Set_CNTV_CTL_EL0 (2);

      --  Core-specific part of the GIC configuration:
      --  The GICC (CPU Interface) is banked for each CPU, so has to be
      --  configured each time.
      GIC.Initialize_GICC;
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

      function MPIDR return Unsigned_32 with Inline_Always;
      --  Return current value of the MPIDR register

      procedure CPU_Wake_Up
        (CPU_Id        : CPU;
         Start_Address : System.Address);
      --  Reset and wake_up the specified CPU

      Calculated_Number_Of_CPUs : Unsigned_32 := 0;

      --------------------
      -- Number_Of_CPUs --
      --------------------

      function Number_Of_CPUs return CPU is
         R : Unsigned_32;
      begin
         if Calculated_Number_Of_CPUs = 0 then
            pragma Warnings (Off, "condition is always *");
            if Parameters.Max_Number_Of_CPUs = 1 then
               Calculated_Number_Of_CPUs := 1;

            else
               Asm ("mrs %0, S3_1_c11_c0_2",
                    Outputs => Unsigned_32'Asm_Output ("=r", R),
                    Volatile => True);
               R := Shift_Right (R, 24) and 3;

               Calculated_Number_Of_CPUs :=
                 Unsigned_32'Min (R + 1, Parameters.Max_Number_Of_CPUs);
            end if;
            pragma Warnings (On, "condition is always *");
         end if;

         return CPU (Calculated_Number_Of_CPUs);
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
         function To_U64 is new Ada.Unchecked_Conversion
           (System.Address, Unsigned_64);

         RST_FPD_APU : Unsigned_32
           with Volatile, Import, Address => 16#FD1A0104#;

         Addr   : constant Unsigned_64 := To_U64 (Start_Address);
         Addr_L : constant Unsigned_32 := Unsigned_32 (Addr and 16#FFFF_FFFF#);
         Addr_H : constant Unsigned_32 := Unsigned_32 (Shift_Right (Addr, 32));

         Pwron_Rst_Mask : constant Unsigned_32 :=
                            Shift_Left (1, Natural (CPU_Id) + 9);
         Rst_Mask       : constant Unsigned_32 :=
                            Shift_Left (1, Natural (CPU_Id) - 1);
         Rst_Value      : Unsigned_32;

      begin
         --  Make sure the target CPU is powered
         APU.Regs.PWRCTL.CPUPWRDWNREQ (CPU_Id) := False;

         --  Assert the power-on-reset
         Rst_Value := RST_FPD_APU;
         if (Rst_Value and Pwron_Rst_Mask) = 0 then
            Rst_Value := Rst_Value or Pwron_Rst_Mask;
            RST_FPD_APU := Rst_Value;
         end if;

         --  Set the APU start address
         case Natural (CPU_Id) is
         when 1 =>
            APU.Regs.RVBAR_ADDR0L := Addr_L;
            APU.Regs.RVBAR_ADDR0H := Addr_H;
         when 2 =>
            APU.Regs.RVBAR_ADDR1L := Addr_L;
            APU.Regs.RVBAR_ADDR1H := Addr_H;
         when 3 =>
            APU.Regs.RVBAR_ADDR2L := Addr_L;
            APU.Regs.RVBAR_ADDR2H := Addr_H;
         when 4 =>
            APU.Regs.RVBAR_ADDR3L := Addr_L;
            APU.Regs.RVBAR_ADDR3H := Addr_H;
         when others =>
            raise Program_Error;
         end case;

         --  Release the reset line if needed
         if (Rst_Value and Rst_Mask) = Rst_Mask then
            Rst_Value := Rst_Value and not Rst_Mask;
            RST_FPD_APU := Rst_Value;
         end if;

         --  Release the power-on-reset line
         RST_FPD_APU := Rst_Value and not Pwron_Rst_Mask;
      end CPU_Wake_Up;

      --------------------
      -- Start_All_CPUs --
      --------------------

      procedure Start_All_CPUs is
      begin
         if not System.BB.Parameters.Multiprocessor
           or else Number_Of_CPUs = 1
         then
            --  Nothing to do
            return;
         end if;

         --  Note: this gets never called in the Uniprocessor case.
         BB.Interrupts.Attach_Handler
           (Poke_Handler'Access, Poke_Interrupt, Interrupt_Priority'Last);

         --  Disable warnings for non-SMP case
         pragma Warnings (Off, "loop range is null*");

         for CPU_Id in CPU'First + 1 .. Number_Of_CPUs loop
            CPU_Wake_Up (CPU_Id, Start'Address);
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
