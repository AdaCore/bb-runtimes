------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This version supports the MPC5200B. Nested interrupts are not supported and
--  critical interrupts are routed to core_int.

with System.BB.Board_Parameters;
with System.BB.CPU_Specific;
with System.Machine_Code;

pragma Warnings (off);
--  Vectors and priorities are defined in Ada.Interrupts.Names, which is not
--  preelaborated. Ignore this issue as we only reference static constants.

with Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

pragma Warnings (on);

with Interfaces; use Interfaces;

package body System.BB.Board_Support is
   use System.BB.Board_Parameters;
   use System.BB.Interrupts;

   pragma Warnings (Off, "*is not referenced*");
   --  Disable warnings for unused register or register components

   --  CPU Registers

   type Hardware_Implementation_Register_0 is record
      Enable_Core_MCP                    : Boolean;
      Enable_Address_Parity_Check        : Boolean;
      Enable_Data_Parity_Check           : Boolean;
      SBCLK                              : Boolean;
      ECLK                               : Boolean;
      Disable_Precharge                  : Boolean;
      Doze                               : Boolean;
      Nap                                : Boolean;
      Sleep                              : Boolean;
      Dynamic_Power_Management           : Boolean;
      Instruction_Cache_Enable           : Boolean;
      Data_Cache_Enable                  : Boolean;
      Instruction_Cache_Lock             : Boolean;
      Data_Cache_Lock                    : Boolean;
      Instruction_Cache_Flash_Invalidate : Boolean;
      Data_Cache_Flash_Invalidate        : Boolean;
      IFEM                               : Boolean;
      FBIOB                              : Boolean;
      Address_Broadcast_Enable           : Boolean;
      NOOPTI                             : Boolean;
   end record;

   for Hardware_Implementation_Register_0 use record
      Enable_Core_MCP                    at 0 range 0  .. 0;
      Enable_Address_Parity_Check        at 0 range 2  .. 2;
      Enable_Data_Parity_Check           at 0 range 3  .. 3;
      SBCLK                              at 0 range 4  .. 4;
      ECLK                               at 0 range 6  .. 6;
      Disable_Precharge                  at 0 range 7  .. 7;
      Doze                               at 0 range 8  .. 8;
      Nap                                at 0 range 9  .. 9;
      Sleep                              at 0 range 10 .. 10;
      Dynamic_Power_Management           at 0 range 11 .. 11;
      Instruction_Cache_Enable           at 0 range 16 .. 16;
      Data_Cache_Enable                  at 0 range 17 .. 17;
      Instruction_Cache_Lock             at 0 range 18 .. 18;
      Data_Cache_Lock                    at 0 range 19 .. 19;
      Instruction_Cache_Flash_Invalidate at 0 range 20 .. 20;
      Data_Cache_Flash_Invalidate        at 0 range 21 .. 21;
      IFEM                               at 0 range 24 .. 24;
      FBIOB                              at 0 range 27 .. 27;
      Address_Broadcast_Enable           at 0 range 28 .. 28;
      NOOPTI                             at 0 range 31 .. 31;
   end record;

   --  Local Interrupt Types

   subtype Peripheral_ID is Interrupt_ID range 0 .. 23;
   subtype Main_ID is Interrupt_ID range 0 .. 16;
   subtype Critical_ID is Interrupt_ID range 0 .. 3;
   subtype IRQ_Number is Interrupt_ID range 0 .. 3;
   --  Hardware interrupt numbers

   First_Peripheral_ID : constant Interrupt_ID :=
     Interrupt_ID (Ada.Interrupts.Names.Peripheral_Interrupt_ID'First);
   First_Main_ID : constant Interrupt_ID :=
     Interrupt_ID (Ada.Interrupts.Names.Main_Interrupt_ID'First);
   First_Critical_ID : constant Interrupt_ID :=
     Interrupt_ID (Ada.Interrupts.Names.Critical_Interrupt_ID'First);
   --  First interrupt ID of each of the Ada.Interrupt priority ranges

   --  Interrupt Controller ICTL Registers
   --  See MPC5200B User's Manual, Section 7.2

   type Peripheral_Mask is array (Peripheral_ID) of Boolean with Pack;

   type ICTL_Peripheral_Interrupt_Mask is record
      Mask : Peripheral_Mask;
   end record with Size => 32;

   for ICTL_Peripheral_Interrupt_Mask use record
      Mask at 0 range 0 .. 23;
   end record;

   type IRQ_Set is array (IRQ_Number) of Boolean with Pack;

   type ICTL_External_Enable_and_External_Types is record
      Master_External_Enable : Boolean;
      IRQ_Enable             : IRQ_Set;
      Critical_Enable        : Boolean;
   end record;

   for ICTL_External_Enable_and_External_Types use record
      Master_External_Enable at 0 range 19 .. 19;
      IRQ_Enable             at 0 range 20 .. 23;
      Critical_Enable        at 0 range 31 .. 31;
   end record;

   type Critical_Priority is mod 2 ** 2;
   type Critical_Priorities is array (Critical_ID) of
     Critical_Priority with Pack;

   type Main_Mask is array (Main_ID) of Boolean with Pack;

   type ICTL_Critical_Priority_And_Main_Interrupt_Mask is record
      Critical_Interrupt  : Critical_Priorities;
      Main_Interrupt_Mask : Main_Mask;
   end record;

   for ICTL_Critical_Priority_And_Main_Interrupt_Mask use record
      Critical_Interrupt  at 0 range 0 .. 7;
      Main_Interrupt_Mask at 0 range 15 .. 31;
   end record;

   type ICTL_Status_Encoded is record
      Peripheral_Interrupt_Present : Boolean;
      Peripheral_Interrupt         : Peripheral_ID;
      Main_Interrupt_Present       : Boolean;
      Main_Interrupt               : Main_ID;
      Critical_Interrupt_Present   : Boolean;
      Critical_Interrupt           : Critical_ID;
      Critical_Enable_Bar          : Boolean;
   end record;

   for ICTL_Status_Encoded use record
      Peripheral_Interrupt_Present at 0 range 2 .. 2;
      Peripheral_Interrupt         at 0 range 3 .. 7;
      Main_Interrupt_Present       at 0 range 10 .. 10;
      Main_Interrupt               at 0 range 11 .. 15;
      Critical_Interrupt_Present   at 0 range 21 .. 21;
      Critical_Interrupt           at 0 range 22 .. 23;
      Critical_Enable_Bar          at 0 range 31 .. 31;
   end record;

   ICTL_Peripheral_Interrupt_Mask_Register : ICTL_Peripheral_Interrupt_Mask
     with Volatile_Full_Access, Address => System'To_Address (MBAR + 16#0500#);

   ICTL_External_Enable_and_External_Types_Register :
     ICTL_External_Enable_and_External_Types
     with Volatile_Full_Access,
       Address => System'To_Address (MBAR + 16#0510#);

   ICTL_Critical_Priority_And_Main_Interrupt_Mask_Register :
     ICTL_Critical_Priority_And_Main_Interrupt_Mask
     with Volatile_Full_Access,
       Address => System'To_Address (MBAR + 16#0514#);

   ICTL_Status_Encoded_Register : ICTL_Status_Encoded
     with Volatile_Full_Access, Address => System'To_Address (MBAR + 16#0524#);

   --  XLB Arbiter Register
   --  See MPC5200B User's Manual, Section 16

   type Master is range 0 .. 7;
   type Parking is (No_Parking, Reserved, Recent, Programmed);

   type Arbiter_Configuration is record
      Pipeline_Disable               : Boolean;
      BestComm_Snooping_Disable      : Boolean;
      Snoop_Enable                   : Boolean;
      Force_Write_With_Flush         : Boolean;
      Timebase_Enable                : Boolean;
      Minimum_Wait_State             : Boolean;
      Select_Parked_Master           : Master;
      Parking_Mode                   : Parking;
      Bus_Activity_Time_Out_Enable   : Boolean;
      Data_Tenure_Time_Out_Enable    : Boolean;
      Address_Tenure_Time_Out_Enable : Boolean;
   end record;

   for Arbiter_Configuration use record
      Pipeline_Disable               at 0 range 0 .. 0;
      BestComm_Snooping_Disable      at 0 range 15 .. 15;
      Snoop_Enable                   at 0 range 16 .. 16;
      Force_Write_With_Flush         at 0 range 17 .. 17;
      Timebase_Enable                at 0 range 18 .. 18;
      Minimum_Wait_State             at 0 range 20 .. 20;
      Select_Parked_Master           at 0 range 21 .. 23;
      Parking_Mode                   at 0 range 25 .. 26;
      Bus_Activity_Time_Out_Enable   at 0 range 28 .. 28;
      Data_Tenure_Time_Out_Enable    at 0 range 29 .. 29;
      Address_Tenure_Time_Out_Enable at 0 range 30 .. 30;
   end record;

   Arbiter_Configuration_Register : Arbiter_Configuration
     with Volatile_Full_Access, Address => System'To_Address (MBAR + 16#1F40#);

   pragma Warnings (On, "*is not referenced*");

   procedure Interrupt_Handler;
   --  Called by low-level handler in case of external interrupt

   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Implementation of Time.Clear_Alarm_Interrupt

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      use System.Machine_Code;

      HID0 : Hardware_Implementation_Register_0;
   begin
      --  Enable Time Base Clock

      Arbiter_Configuration_Register.Timebase_Enable := True;

      --  Route Critical Interrupts to External Interrupts

      ICTL_External_Enable_and_External_Types_Register :=
        (Critical_Enable => True,
         Master_External_Enable => True,
         IRQ_Enable => (others => False));

      --  Install hanlders

      CPU_Specific.Install_Exception_Handler
        (Interrupt_Handler'Address, CPU_Specific.External_Interrupt_Excp);
      CPU_Specific.Install_Exception_Handler
        (Interrupt_Handler'Address, CPU_Specific.System_Management_Excp);

      --  Configure HID0 enable the Doze power management mode when we sleep
      --  and enable the instruction caches.

      Asm
        ("mfspr %0, 1008",
         Outputs  =>
           Hardware_Implementation_Register_0'Asm_Output ("=r", HID0),
         Volatile => True);

      HID0.Doze := True;
      HID0.Instruction_Cache_Enable := True;

      --  Data and instruction sync instructions ensure that no memory is
      --  accessed when we enable the cache.

      Asm ("sync", Volatile => True);
      Asm ("isync", Volatile => True);

      Asm
        ("mtspr 1008, %0",
         Inputs   =>
           Hardware_Implementation_Register_0'Asm_Input ("r", HID0),
         Volatile => True);

   end Initialize_Board;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  The hardware automatically clears the decrementer exception
      null;
   end Clear_Alarm_Interrupt;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      Interrupt_Status : ICTL_Status_Encoded :=
         ICTL_Status_Encoded_Register;
      --  Local copy of the ICTL_Status_Encoded_Register so that we do not have
      --  the overhead of reading the volatile register on each component
      --  access.

      Interrupt : Interrupt_ID;
      --  Interrupt to handle

   begin
      --  Since the SIU Interrupt Controller does not really supported nested
      --  interrupts, we check for any new interrupts at the end of the handler
      --  and service them to reduce the overhead of returning from the
      --  handler.

      loop
         if Interrupt_Status.Critical_Interrupt_Present then
            --  Convert the hardware Critical Interrupt to an Interrupt ID. If
            --  the Critical Interrupt is HI_INT then a Peripheral Interrupt
            --  has occurred and we look up the corresponding Peripheral
            --  Interrupt.

            Interrupt :=
              First_Critical_ID + Interrupt_Status.Critical_Interrupt;

            if Interrupt = Interrupt_ID (HI_INT) then
               Interrupt :=
                 First_Peripheral_ID + Interrupt_Status.Peripheral_Interrupt;
            end if;

         elsif Interrupt_Status.Main_Interrupt_Present then
            --  Convert the hardware Main Interrupt to an Interrupt ID. If the
            --  Main Interrupt is LO_INT then a Peripheral Interrupt has
            --  occurred and we look up the corresponding Peripheral Interrupt.

            Interrupt :=
              First_Main_ID + Interrupt_Status.Main_Interrupt;

            if Interrupt = Interrupt_ID (LO_INT) then
               Interrupt :=
                 First_Peripheral_ID + Interrupt_Status.Peripheral_Interrupt;
            end if;

         else
            --  Spurious interrupt, just return.

            return;
         end if;

         BB.Interrupts.Interrupt_Wrapper (Interrupt);

         --  Force the Interrupt Controller to reevaluate it's interrupts

         Interrupt_Status.Peripheral_Interrupt_Present := True;
         Interrupt_Status.Main_Interrupt_Present := True;
         Interrupt_Status.Critical_Interrupt_Present := True;

         ICTL_Status_Encoded_Register := Interrupt_Status;

         --  Update our local copy of the ICTL_Status_Encoded_Register

         Interrupt_Status := ICTL_Status_Encoded_Register;

         --  Exit if they're are no more remaining interrupts to service

         exit when not
           (Interrupt_Status.Peripheral_Interrupt_Present or
            Interrupt_Status.Main_Interrupt_Present or
            Interrupt_Status.Critical_Interrupt_Present);
      end loop;
   end Interrupt_Handler;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
      begin
         --  Unmask interrupts in the controller

         case Ada.Interrupts.Interrupt_ID (Interrupt) is
            when Peripheral_Interrupt_ID =>
               ICTL_Peripheral_Interrupt_Mask_Register.Mask
                 (Interrupt - First_Peripheral_ID) := False;
               ICTL_Critical_Priority_And_Main_Interrupt_Mask_Register.
                 Main_Interrupt_Mask
                   (Interrupt_ID (LO_INT) - First_Main_ID) := False;

            when Main_Interrupt_ID =>
               ICTL_Critical_Priority_And_Main_Interrupt_Mask_Register.
                 Main_Interrupt_Mask
                   (Interrupt - First_Main_ID) := False;

            when Critical_Interrupt_ID =>
               --  Critical interrupt are not maskable, so nothing to do here
               null;
         end case;
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
         return Interrupt_Priority'First;
      end Priority_Of_Interrupt;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
         use System.Machine_Code;

         MSR  : CPU_Specific.Machine_State_Register;
      begin
         --  Power down the core using the PowerPC power management feature

         --  Read MSR and set POW/WE bit

         Asm ("mfmsr %0",
              Outputs  =>
                CPU_Specific.Machine_State_Register'Asm_Output ("=r", MSR),
              Volatile => True);
         MSR.Power_Management_Enable := True;

         Asm ("sync", Volatile => True);

         --  Set MSR

         Asm ("mtmsr %0",
              Inputs   =>
                CPU_Specific.Machine_State_Register'Asm_Input ("r", MSR),
              Volatile => True);

         Asm ("isync", Volatile => True);
      end Power_Down;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         null;
      end Set_Current_Priority;
   end Interrupts;

   package body Time is separate;

   package body Multiprocessors is separate;
end System.BB.Board_Support;
