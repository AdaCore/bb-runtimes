------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
-- S Y S T E M . T I . V E C T O R E D _ I N T E R R U P T _ M A N A G E R  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with Interfaces.ARM_V7AR;
with System.BB.CPU_Primitives;
with System.Machine_Code;

package body System.TI.Vectored_Interrupt_Manager is
   use System.BB.CPU_Primitives, System.BB.Interrupts;
   use Interfaces, Interfaces.ARM_V7AR;
   use System.Machine_Code;

   ------------------------
   -- Interrupt Handlers --
   ------------------------

   procedure IRQ_Handler;
   pragma Import (Asm, IRQ_Handler, "__gnat_irq_trap");

   procedure IRQ_Interrupt_Handler;
   pragma Export (C, IRQ_Interrupt_Handler, "__gnat_irq_handler");
   --  Low-level interrupt handler

   procedure FIQ_Interrupt_Handler is null;
   pragma Export (C, FIQ_Interrupt_Handler, "__gnat_fiq_handler");
   --  This runtime does not provide a FIQ handler

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      SCTLR : Unsigned_32;
   begin
      --  Enable the Vector Interrupt Controller (VIC)

      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR or (2 ** 24);  -- VE bit
      CP15.Set_SCTLR (SCTLR);

      --  Reset the interrupt controller to its default state

      Disable_Interrupts;

      for Group in VIM_Interrupt_Group_Register'Range loop
         VIM_Interrupt_Group_Register (Group).Interrupt_Disable_Register :=
           (others => Disable);
         VIM_Interrupt_Group_Register (Group).Interrupt_Status_Register :=
           (others => Active_Clear);
         VIM_Interrupt_Group_Register (Group).Interrupt_Map_Register :=
           (others => IRQ);
         VIM_Interrupt_Group_Register (Group).Interrupt_Type_Register :=
           (others => Level);
      end loop;

      --  Acknowledge and clear pending interrupt requests

      declare
         Handler : Address with Volatile;
      begin
         Handler := VIM_IRQ_Vector_Register;
         Handler := VIM_FIQ_Vector_Register;

         VIM_IRQ_Vector_Register := Handler;
         VIM_FIQ_Vector_Register := Handler;
      end;
   end Initialize;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Interrupt : BB.Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      pragma Unreferenced (Prio);

      Group_ID : constant Interrupt_Group := VIM_Group_ID (Interrupt);
      --  Register interrupt group the interrupt belongs to

      Sub_ID : constant Interrupt_Group := VIM_Sub_ID (Interrupt);
      --  ID of the interrupt within the above group

      Interrupt_Group_Register renames
        VIM_Interrupt_Group_Register (Group_ID);

   begin
      --  While we could directly have installed a single fixed IRQ handler,
      --  this would have required all IRQ go through Ravenscar run time,
      --  which is a bit of a limitation for some applications. By using the
      --  vector capability of the interrupt handler, it is possible to
      --  handle some interrupts directly for best performance.

      VIM_Interrupt_Vector_Register (Interrupt) := IRQ_Handler'Address;

      Interrupt_Group_Register.Interrupt_Map_Register (Sub_ID) := IRQ;

      Interrupt_Group_Register.Interrupt_Enable_Register (Sub_ID) := Enable;
   end Install_Interrupt_Handler;

   ---------------------------
   -- IRQ_Interrupt_Handler --
   ---------------------------

   procedure IRQ_Interrupt_Handler is
      Active_Request : constant VIM_Interrupt_Request :=
        VIM_Active_IRQ_Register;

   begin
      if not Active_Request.Valid then
         --  Spurious interrupt
         return;
      end if;

      declare
         Group_ID : constant Interrupt_Group :=
           VIM_Group_ID (Active_Request.Number);
         Sub_ID   : constant Interrupt_Group :=
           VIM_Sub_ID (Active_Request.Number);

         IRQ_Group renames VIM_Interrupt_Group_Register (Group_ID);

         IRQ_Type : constant Interrupt_Type :=
           IRQ_Group.Interrupt_Type_Register (Sub_ID);
      begin

         --  As per TI AM64x TRM 6.2.3.5.8.1 clear the status register before
         --  the user clears the interrupt source if the IRQ type is set to
         --  pulse. Otherwise, clear after the user handler.

         if IRQ_Type = Pulse then
            IRQ_Group.Interrupt_Status_Register (Sub_ID) := Active_Clear;
         end if;

         Interrupt_Wrapper (Active_Request.Number);

         if IRQ_Type = Level then
            IRQ_Group.Interrupt_Status_Register (Sub_ID) := Active_Clear;
         end if;
      end;

      --  Clear the priority mask

      VIM_IRQ_Vector_Register := Null_Address;
   end IRQ_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   --  Only one interrupt priority level is supported

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority is (Interrupt_Priority'Last);

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   --  Nested interrupts are not supported on this controller so there
   --  is nothing to do.

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
end System.TI.Vectored_Interrupt_Manager;
