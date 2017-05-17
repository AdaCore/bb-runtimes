------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P A R A M E T E R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

--  This package defines basic parameters used by the low level tasking system

--  This is the ERC32/LEON/LEON3 version of this package

pragma Restrictions (No_Elaboration_Code);

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Frequency : constant Natural;  --  Hertz
   pragma Import (Asm, Clock_Frequency, "clock_frequency");
   --  Frequency of the system clock

   ----------------
   -- Interrupts --
   ----------------

   --  The following are ERC32/LEON/LEON3 definitions and they cannot be
   --  modified.

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   Number_Of_Interrupt_ID : constant := 15;
   --  Number of interrupts in the SPARC architecture

   Trap_Vectors : constant := 256;
   --  The SPARC arquitecture supports 256 vectors

   ------------------------
   -- Context Management --
   ------------------------

   --  The SPARC processor needs to save:
   --
   --   * 18 integer registers of 32 bits (7 global, 8 output, PSR, Y, and WIM)
   --     for normal processing
   --
   --   * 33 floating point registers of 32 bits
   --
   --   * the number of register windows saved to the stack
   --     (the input and local registers are saved to the stack).
   --
   --   * interrupt nesting level corresponding to the task
   --
   --   * for LEON, to slots for the Cache Control Register
   --
   --  This would be 55 slots for LEON/LEON3 and 53 otherwise, but
   --  this need to be rounded up to an even number to allow more
   --  efficient access.
   --
   --  For LEON, we store the Cache Control Register to be able to keep the
   --  cache status per task. We keep the base Cache Control Register (which
   --  is not affected by automatic changes related to the freeze-on-interrupt
   --  capability) and the actual Cache Control Register (the one directly
   --  extracted from the hardware).

   Base_CCR_Context_Index  : constant := 53;
   CCR_Context_Index       : constant := 54;

   Context_Buffer_Capacity : constant := 56;

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 4 * 1024;  --  bytes
   --  Size of each of the interrupt stacks

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
