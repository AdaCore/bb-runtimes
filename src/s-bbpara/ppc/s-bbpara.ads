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

--  This package defines basic parameters used by the low level tasking system

with System.BB.Board_Parameters;

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Ticks_Per_Second : constant := Board_Parameters.Clock_Frequency;
   --  Frequency of the system clock

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 1 .. 128 + 8;
   --  Number of interrupts (for both the interrupt controller and core
   --  interrupts such as the decrementer). The runtime supports at most
   --  128 interrupts external interrupts and 8 non external. If you have
   --  more interrupts, you need to change the above constants. This static
   --  constant is used to declare a type, and the handler table.

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 8 * 1024;
   --  Size of each of the interrupt stacks in bytes

   Interrupt_Sec_Stack_Size : constant := 1024;
   --  Size of the secondary stack for interrupt handlers

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
