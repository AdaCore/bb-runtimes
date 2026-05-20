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
--                     Copyright (C) 2003-2020, AdaCore                     --
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

--  This is the Xilinx XPS Interrupt Controller version of this package.

--  Note: nested interrupts are not supported with this controller. Nested
--  controllers are also not supported.

--  This package provides the interface between the runtime and the external
--  interrupt controller. If you are using a different interrupt controller on
--  the Virtex-5 you will need to modify the following:
--
--    * Replace the Interrupt Controller Registers section with register
--      definitions for your interrupt controller.
--
--    * Tailor the following procedures to your interrupt controller:
--        + Initialize_Board
--        + Interrupt_Handler
--        + Install_Interrupt_Handler
--        + Priority_Of_Interrupt
--        + Set_Current_Priority

with Interfaces; use Interfaces;

with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.BB.CPU_Specific;
with System.Machine_Code;

package body System.BB.Board_Support is

   -------------------------------------
   --  Interrupt Controller Registers --
   -------------------------------------

   --  This section defines the Interrupt Controller registers. Replace the
   --  definitions here for those that correspond to your target's interrupt
   --  controller if not using the XPS Interrupt Controller.

   pragma Warnings (Off, "*is not referenced*");
   --  Disable warnings for unused register or register components

   --  XPS_INTC_Base_Address : defined in System.BB.Board_Parameters
   INTC_Interrupt_Status_Offset            : constant := 16#00#;
   INTC_Interrupt_Pending_Offset           : constant := 16#04#;
   INTC_Interrupt_Enable_Offset            : constant := 16#08#;
   INTC_Interrupt_Acknowledge_Offset       : constant := 16#0C#;
   INTC_Set_Interrupt_Enable_Bits_Offset   : constant := 16#10#;
   INTC_Clear_Interrupt_Enable_Bits_Offset : constant := 16#14#;
   INTC_Interrupt_Vector_Offset            : constant := 16#18#;
   INTC_Master_Enable_Offset               : constant := 16#1C#;

   subtype INTC_Interrupt_ID is BB.Interrupts.Interrupt_ID
     range BB.Interrupts.Interrupt_ID'First ..
       BB.Interrupts.Interrupt_ID'First + 31;
   --  Xilinx XPS Interrupt controller support 32 interrupts

   type Interrupt_Registers is array (INTC_Interrupt_ID) of Boolean
      with Pack, Size => 32;

   type INTC_Master_Enable is record
      Hardware_Interrupt_Enable : Boolean;
      Master_Interrupt_Enable   : Boolean;
   end record with Size => 32;

   for INTC_Master_Enable use record
      Hardware_Interrupt_Enable at 0 range 30 .. 30;
      Master_Interrupt_Enable   at 0 range 31 .. 31;
   end record;

   --  For the XPS Interrupt Controller registers where each bit maps to an
   --  interrupt we use Unsigned_32 instead of a packed array indexed by
   --  INTC_Interrupt_ID because the controller orders these registers with
   --  INT(0) in bit 31 and it's a hassel to convert to this bit endianness.
   --  Instead, we'll bit shifts ourselves instead of relying on high-level Ada
   --  code.

   INTC_Interrupt_Status_Register : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Interrupt_Status_Offset);

   INTC_Interrupt_Pending_Register : Interrupt_Registers
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Interrupt_Pending_Offset);

   INTC_Interrupt_Acknowledge_Register : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Interrupt_Acknowledge_Offset);

   INTC_Interrupt_Enable_Register : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Interrupt_Enable_Offset);

   INTC_Set_Interrupt_Enable_Bits_Register : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Set_Interrupt_Enable_Bits_Offset);

   INTC_Clear_Interrupt_Enable_Bits_Register : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Clear_Interrupt_Enable_Bits_Offset);

   INTC_Interrupt_Vector_Register : INTC_Interrupt_ID
     with Volatile_Full_Access, Size => 32,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Interrupt_Vector_Offset);

   INTC_Master_Enable_Register : INTC_Master_Enable
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (XPS_INTC_Base_Address + INTC_Master_Enable_Offset);

   pragma Warnings (On, "*is not referenced*");

   --  Package procedures

   procedure Interrupt_Handler;
   --  Called by low-level handler in case of external interrupt

   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Implementation of Time.Clear_Alarm_Interrupt

   ----------------------
   -- Initialize_Board --
   ----------------------

   --  For the Virtex-5 runtime this procedure is used to configure the
   --  interrupt controller and install the runtime's external interrupt
   --  handler. If using a different interrupt controller only replace the
   --  XPS specific code.

   procedure Initialize_Board is
   begin
      --  Install the runtime's External Interrupt handler into the runtime's
      --  Exception Handler table.

      CPU_Specific.Install_Exception_Handler
        (Interrupt_Handler'Address, CPU_Specific.External_Interrupt);

      --  Configure the interrupt controller. For the XPS controller this is
      --  simple: all we need to do is clear any pending interrupts and
      --  then enable the interrupt controller.

      --  If not using the the XPS controller, replace the following code
      --  with intialization code suitable for your interrupt controller.

      INTC_Interrupt_Acknowledge_Register := 16#FFFF_FFFF#;

      INTC_Master_Enable_Register :=
        (Hardware_Interrupt_Enable => True,
         Master_Interrupt_Enable   => True);
   end Initialize_Board;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   --  Clear the decrementer alarm

   procedure Clear_Alarm_Interrupt is
      use System.Machine_Code;
   begin
      --  Clear TSR[DIS] on Book-E CPUs

      Asm ("mtspr 336,%0",
           Inputs => Unsigned_32'Asm_Input ("r", 2 ** (63 - 36)),
           Volatile => True);
   end Clear_Alarm_Interrupt;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   --  Interrupt_Handler performs four operations:
   --
   --    1. It retrieves the Interrupt ID (or interrupt number) from the
   --       interrupt controller.
   --    2. Calls BB.Interrupts.Interrupt_Wrapper with the Interrupt ID
   --    3. Clears/acknowledge the interrupt in the interrupt controller.
   --    4. Informs the interrupt controller the interrupt has been handled
   --
   --  Depending on the interrupt controller, not all these steps may be used,
   --  and the order may differ (for example, some controllers consider reading
   --  the Interrupt ID from controller as an acknowledgement while others
   --  do not require the end of the interrupt to be signaled).

   procedure Interrupt_Handler is
      Interrupt : constant System.BB.Interrupts.Interrupt_ID :=
        INTC_Interrupt_Vector_Register;
      --  Retrieve the ID of the interrupt that raised the interrupt
   begin
      --  Call the runtime interrupt wrapper passing it the Interrupt ID of
      --  the external interrupt. Do not change this.
      BB.Interrupts.Interrupt_Wrapper (Interrupt);

      --  For the XPS Interrupt Controller we can only acknowledge the
      --  interrupt after the user clears the interrupt source. Otherwise a new
      --  interrupt will be raised.
      INTC_Interrupt_Acknowledge_Register := Shift_Left (1, Interrupt);
   end Interrupt_Handler;

   package body Interrupts is

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      --  For the Virtex-5 this procedure does not install any handler per se;
      --  instead we use it to enable the interrupt in the interrupt
      --  controller. If using a different interrupt controller, replace the
      --  body with code that will enable the interrupt for the Interrupt ID
      --  passed to the procedure. If your interrupt controller supports
      --  setting the priority of the interrupt, do that here too.

      procedure Install_Interrupt_Handler
        (Interrupt : System.BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         pragma Unreferenced (Prio);
      begin
         INTC_Set_Interrupt_Enable_Bits_Register := Shift_Left (1, Interrupt);
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      --  Nested interrupts are not supported on this controller and so only
      --  a single interrupt priority is supported. However, if your
      --  interrupt controller supports nested interrupts and interrupt
      --  priorities, modify the body to retrive the priority for the given
      --  interrupt. To support nested interrupts, your interrupt controller
      --  needs to have a way to mask interrupts by priority (see
      --  Set_Current_Priority for details). Remember to update the number of
      --  interrupt priorities supported by your interrupt controller in the
      --  system.ads package by updating the Nbr_Interrupt_Priorities
      --  parameter.

      function Priority_Of_Interrupt
        (Interrupt : BB.Interrupts.Interrupt_ID) return System.Any_Priority
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

         MSR : CPU_Specific.Machine_State_Register;
      begin
         --  Power down the core using the PowerPC Book E Wait State feature

         --  Read MSR and set POW/WE bit

         Asm ("mfmsr %0",
              Outputs  =>
                CPU_Specific.Machine_State_Register'Asm_Output ("=r", MSR),
              Volatile => True);
         MSR.Wait_State_Enable := True;

         Asm ("msync", Volatile => True);

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

      --  Nested interrupts are not supported on this controller so there
      --  is nothing to do. However, if your interrupt controller supports
      --  masking interrupts by priority, modify the procedure to set your
      --  controller's interrupt priority mask.

      procedure Set_Current_Priority (Priority : Integer) is
      begin
         null;
      end Set_Current_Priority;
   end Interrupts;

   package body Time is separate;

   package body Multiprocessors is separate;

end System.BB.Board_Support;
