------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . R I S C V _ P L I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  This package defines an interface to an implementation of the standard
--  RISC-V PLIC (Platform Local Interrupt Controller).
--
--  The specifications follow the Privileged Architecture Version 1.10.

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;

package System.BB.RISCV_PLIC is
   pragma Preelaborate;

   package BBI renames System.BB.Interrupts;

   procedure Initialize;
   --  This procedure is called during board initialization to perform PLIC
   --  initialization. All interrupts should be disabled on return.

   function Pending return Boolean;
   --  Return True if an interrupt above the threshold is pending for the
   --  current hart.

   procedure Enable (Interrupt : BBI.Interrupt_ID);
   --  Enable an interrupt for the current hart

   function Claimed (Interrupt : BBI.Interrupt_ID) return Boolean
     with Ghost;
   --  Return True if the interrupt is claimed.
   --
   --  Note: This function is declared as ghost because it is only used for
   --  specification and doesn't correspond to a hardware feature of the PLIC.

   function Claim return BBI.Any_Interrupt_ID
     with Post => Claim'Result = BBI.No_Interrupt
                    or else
                  Claimed (Claim'Result);
   --  Claim an interrupt on the PLIC for the current hart and return its ID.
   --  Return No_Interrupt if there was no pending interrupts for the hart when
   --  the claim was serviced.
   --
   --  Note: According to the RISC-V Privileged Architecture Specification
   --  (version 1.10), it is possible to claim interrupts even if their
   --  priority is below the threshold. Doing this in a Ravenscar context will
   --  lead to priority inversion.

   procedure Complete (Interrupt : BBI.Interrupt_ID)
     with Pre  => Claimed (Interrupt),
          Post => not Claimed (Interrupt);
   --  Signal an interrupt completion for the current hart.

   procedure Set_Priority_Threshold (Priority : Integer);
   --  Set the interrupt priority threshold for the current hart.
   --
   --  Only interrupts with a priority strictly greater than the threshold will
   --  be able to trigger a trap.

   function Threshold return Integer;
   --  Return the priority threshold for the current hart

   procedure Set_Priority
     (Interrupt : System.BB.Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority);
   --  Set the priority of an interrupt for all harts

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority;
   --  Return the priority of an interrupt

end System.BB.RISCV_PLIC;
