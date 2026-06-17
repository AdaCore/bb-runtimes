------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . A R M _ G I C                      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
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

--  This is support package for the GIC600 interrupt controller
--  This controller complies with the ARM Generic Interrupt Controller
--  v3.0 specification.
--  This package provides the BSP support for IRQ handling for the ravenscar
--  kernel.

with Interfaces;
with System.BB.Interrupts;
with System.Multiprocessors;

package System.ARM_GIC is
   pragma Preelaborate;

   type ICFGR is array (Positive range <>) of Interfaces.Unsigned_32;

   function Current_CPU return System.Multiprocessors.CPU;
   --  Get the CPU value for the current CPU

   procedure Define_IRQ_Triggers (Config : ICFGR);
   --  Set the ICFGR GICD registers

   procedure Initialize_GICD;
   --  There's only one Distributor, so this needs to be configured only once,
   --  at startup.

   procedure Initialize_ICC;
   --  The CPU interface is CPU-specific so needs to be initialized for each
   --  core on an SMP system.

   generic
      with procedure Interrupt_Wrapper
        (Id : System.BB.Interrupts.Interrupt_ID);
   procedure IRQ_Handler;
   --  To be called directly by the IRQ trap.

   --  Below is the implementation of System.BB.Board_Support.Interrupts:

   procedure Install_Interrupt_Handler
     (Interrupt : System.BB.Interrupts.Interrupt_ID;
      Prio      : System.Interrupt_Priority);
   --  Enable an interrupt and configure it with the specified priority

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority;
   --  Get the priority of the specified interrupt

   procedure Set_Current_Priority (Priority : Integer);
   --  Set the current priority in the interrupt controller

   procedure Poke_CPU
     (CPU_Id         : System.Multiprocessors.CPU;
      Poke_Interrupt : System.BB.Interrupts.Interrupt_ID)
   with Inline;
   --  Used to poke a CPU on SMP systems

   procedure Power_Down;
   --  Power down the current CPU

end System.ARM_GIC;
