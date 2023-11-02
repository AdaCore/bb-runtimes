------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
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

with Ada.Unchecked_Conversion;
with System;
with System.BB.Interrupts;

package body GNAT.Interrupts is
   package VIM renames System.TI.Vectored_Interrupt_Manager;
   use System, System.BB.Interrupts, VIM;

   function To_Address is new
     Ada.Unchecked_Conversion (Direct_Interrupt_Handler, System.Address);

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Direct_Interrupt_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Kind        : VIM_Interrupt_Kind;
      Event_Type  : VIM_Interrupt_Type := VIM_Interrupt_Type'First;
      Prio        : VIM_Interrupt_Priority := VIM_Interrupt_Priority'Last;
      Enable      : Boolean := True)
   is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.

      Interrupt_Group_Register renames
        VIM_Interrupt_Group_Register (Group_ID);

   begin
      VIM_Interrupt_Vector_Register (ID) := To_Address (New_Handler);

      VIM_Priority_Register (ID).Priority := Prio;
      Interrupt_Group_Register.Interrupt_Map_Register (Sub_ID) := Kind;
      Interrupt_Group_Register.Interrupt_Type_Register (Sub_ID) := Event_Type;
      Interrupt_Group_Register.Interrupt_Enable_Register (Sub_ID) :=
        (if Enable then VIM.Enable else VIM.Disable);
   end Attach_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler (Interrupt : Ada.Interrupts.Interrupt_ID) is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Interrupt_Enable_Register (Sub_ID) := VIM.Disable;
      VIM_Interrupt_Vector_Register (ID) := Null_Address;
   end Detach_Handler;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Interrupt_Enable_Register (Sub_ID) := VIM.Disable;
   end Disable_Interrupt;

   -------------------------
   -- Clear_Priority_Mask --
   -------------------------

   procedure Clear_Priority_Mask is
   begin
      VIM_IRQ_Vector_Register := Null_Address;
   end Clear_Priority_Mask;

   ----------------------
   -- Clear_VIM_Status --
   ----------------------

   procedure Clear_VIM_Status (Interrupt : Ada.Interrupts.Interrupt_ID) is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Interrupt_Status_Register (Sub_ID) := Active_Clear;
   end Clear_VIM_Status;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Interrupt_Enable_Register (Sub_ID) := VIM.Enable;
   end Enable_Interrupt;

   ------------------------
   -- Interrupt_Priority --
   ------------------------

   function Interrupt_Priority
     (Interrupt : Ada.Interrupts.Interrupt_ID)
     return VIM_Interrupt_Priority
   is
      ID : constant Interrupt_ID := Interrupt_ID (Interrupt);
      --  The ID of Interrupt
   begin
      return VIM_Priority_Register (ID).Priority;
   end Interrupt_Priority;

   --------------------
   -- Interrupt_Type --
   --------------------

   function Interrupt_Type
     (Interrupt : Ada.Interrupts.Interrupt_ID)
     return VIM_Interrupt_Type
   is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      return VIM_Interrupt_Group_Register (Group_ID).
               Interrupt_Type_Register (Sub_ID);
   end Interrupt_Type;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean
   is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      return VIM_Interrupt_Group_Register (Group_ID).
               Interrupt_Enable_Register (Sub_ID) = Enable;
   end Is_Enabled;

   ------------------------------
   -- Raise_Software_Interrupt --
   ------------------------------

   procedure Raise_Software_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID)
   is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Raw_Status_Register (Sub_ID) := Active_Set;

   end Raise_Software_Interrupt;

   ----------------------------
   -- Set_Interrupt_Priority --
   ----------------------------

   procedure Set_Interrupt_Priority
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Prio      : VIM_Interrupt_Priority)
   is
      ID : constant Interrupt_ID := Interrupt_ID (Interrupt);
      --  The ID of Interrupt
   begin
      VIM_Priority_Register (ID).Priority := Prio;
   end Set_Interrupt_Priority;

   ------------------------
   -- Set_Interrupt_Type --
   ------------------------

   procedure Set_Interrupt_Type
     (Interrupt  : Ada.Interrupts.Interrupt_ID;
      Event_Type : VIM_Interrupt_Type)
   is
      ID       : constant Interrupt_ID := Interrupt_ID (Interrupt);
      Group_ID : constant Interrupt_Group := VIM_Group_ID (ID);
      Sub_ID   : constant Interrupt_Group := VIM_Sub_ID (ID);
      --  The ID of the interrupt, the register group it belongs to and its ID
      --  within that group.
   begin
      VIM_Interrupt_Group_Register (Group_ID).
        Interrupt_Type_Register (Sub_ID) := Event_Type;
   end Set_Interrupt_Type;
end GNAT.Interrupts;
