------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      G N A T . I N T E R R U P T S                       --
--                                                                          --
--                                  S p e c                                 --
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

--  Provides a high level interface to Interrupt controllers. Generally
--  interrupts are managed by the GNAT runtime and interrupt handlers are
--  attached through protected procedure handlers. This package provides access
--  to additional interrupt controller features that may not be exposed via
--  Ada, for example setting whether an interrupt is pulse or level.

--  This version targets TI's Vectored Interrupt Manager for the TI Keystone
--  Cortex-R5F. Specifically, for each VIM interrupt it allows:
--
--     * Enabling and disabling interrupts at the VIM;
--     * Raising a software interrupt (for testing);
--     * Setting the type of interrupt event (level (default) or pulse);
--     * Setting the priority of the interrupt at the VIM.
--
--  Note: The TI Keystone Cortex-R5F runtimes do not support nested priorities
--  and the VIM interrupt priorities are only used to determine which interrupt
--  has precedence to be serviced when there are multiple pending interrupts.
--
--  Additionally, GNAT.Interrupts support direct attachment of IRQ and FIQ
--  interrupt handlers to the VIM that bypasses the GNAT runtime. This allows
--  users to provide low latency handlers. Direct attachment of handlers come
--  with the following limitations:
--
--    * Nesting of interrupts is not supported;
--    * Interrupts must not be reenabled while in a handler;
--    * Handler *must* not make any runtime calls, including calling protected
--      subprograms;
--    * Handler should not use the FPU;
--    * Users are responsible for save and restoring register state, and
--      returning from the interrupt. This can be done by using the "interrupt"
--      machine attribute on the handler procedure;
--    * Users must follow TI's documentation on "Servicing IRQ Through Vector
--      Interface" for IRQ handlers and "Servicing FIQ" for FIQ handlers.
--      Use the provided procedures Clear_VIM_Status and Clear_Priority_Mask to
--      clear the interrupt status and VIM priority.

with Ada.Interrupts;
with System.TI.Vectored_Interrupt_Manager;

package GNAT.Interrupts is

   --------------------------------------
   -- Vectored Interrupt Manager Types --
   --------------------------------------

   subtype VIM_Interrupt_Priority is
     System.TI.Vectored_Interrupt_Manager.VIM_Interrupt_Priority;
   --  Interrupt priorities in the VIM

   subtype VIM_Interrupt_Type is
     System.TI.Vectored_Interrupt_Manager.Interrupt_Type;
   --  Type of interrupt event: Level, Pulse

   subtype VIM_Interrupt_Kind is
     System.TI.Vectored_Interrupt_Manager.Interrupt_Kind;
   --  Kind of interrupt: IRQ, FIQ

   type Direct_Interrupt_Handler is access procedure;
   --  Parameterless access to procedure type for the handler to attach
   --  directly.

   --------------------
   -- VIM Operations --
   --------------------

   procedure Enable_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID);
   procedure Disable_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID);
   --  Enable and disable the interrupt in the VIM

   function Is_Enabled (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean;
   --  Returns True if the interrupt is enabled in the VIM

   procedure Raise_Software_Interrupt
     (Interrupt : Ada.Interrupts.Interrupt_ID);
   --  Raise the named interrupt at the VIM. This sets the relevant bit in
   --  Raw Status/Set register (R5FSS_VIM_RAW_j)

   procedure Set_Interrupt_Type
     (Interrupt  : Ada.Interrupts.Interrupt_ID;
      Event_Type : VIM_Interrupt_Type);
   function Interrupt_Type
     (Interrupt : Ada.Interrupts.Interrupt_ID)
     return VIM_Interrupt_Type;
   --  Set and get the event type of interrupt, i.e.: Level or Pulse

   procedure Set_Interrupt_Priority
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Prio      : VIM_Interrupt_Priority);
   function Interrupt_Priority
     (Interrupt : Ada.Interrupts.Interrupt_ID)
     return VIM_Interrupt_Priority;
   --  Set and get the priority of the interrupt

   -------------------------------
   -- Direct Handler Attachment --
   -------------------------------

   procedure Attach_Handler
     (New_Handler : Direct_Interrupt_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Kind        : VIM_Interrupt_Kind;
      Event_Type  : VIM_Interrupt_Type := VIM_Interrupt_Type'First;
      Prio        : VIM_Interrupt_Priority := VIM_Interrupt_Priority'Last;
      Enable      : Boolean := True);
   --  Attach the IRQ or FIQ interrupt handler for Interrupt directly to the
   --  VIM, bypassing the GNAT runtime. Kind is either IRQ and FIQ, and must be
   --  specified. Optionally, the Event_Type (Level or Pulse) and priority of
   --  the interrupt in the VIM can be specified. By default, the interrupt
   --  will be enabled but can remain disabled by passing False to Enable.

   procedure Detach_Handler (Interrupt : Ada.Interrupts.Interrupt_ID);
   --  Detach the handler for Interrupt. This will disable the interrupt and
   --  set the vector address for this interrupt to Null_Address.

   procedure Clear_VIM_Status (Interrupt : Ada.Interrupts.Interrupt_ID);
   --  Clear the VIM interrupt status for Interrupt

   procedure Clear_Priority_Mask;
   --  Clear the VIM priority mask

end GNAT.Interrupts;
