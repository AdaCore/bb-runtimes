------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                    Copyright (C) 2012-2020, AdaCore                      --
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

--  This package defines board parameters for the PolarFire SOC

with Interfaces;

package System.BB.Board_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Scale     : constant := 1;
   --  Scaling factor for clock frequency. This is used to provide a clock
   --  frequency that results in a definition of Time_Unit less than 20
   --  microseconds (as Ada RM D.8 (30) requires).

   Decrementer_Frequency : constant Positive := 1_000_000;
   --  Frequency of the system clock for the decrementer timer

   Clock_Frequency : constant Positive := Decrementer_Frequency * Clock_Scale;
   --  Scaled clock frequency

   CLINT_Base_Address    : constant := 16#0200_0000#;
   CLINT_Mtime_Offset    : constant := 16#BFF8#;
   CLINT_Mtimecmp_Offset : constant := 16#4008#; --  mtimecmp for hart 1

   Mtime_Base_Address : constant :=
     CLINT_Base_Address + CLINT_Mtime_Offset;
   --  Address of the memory mapped mtime register

   Mtimecmp_Base_Address : constant :=
     CLINT_Base_Address + CLINT_Mtimecmp_Offset;
   --  Address of the memory mapped mtimecmp register

   UART_Base_Address    : constant := 16#2000_0000#;

   --  Platform Level Interrupt Controller
   PLIC_Base_Address     : constant := 16#0C00_0000#;
   PLIC_Nbr_Of_Harts     : constant := 5;
   PLIC_Nbr_Of_Sources   : constant := 185;
   PLIC_Nbr_Of_Mask_Regs : constant := 6;
   PLIC_Hart_Id          : constant := 1;
   PLIC_Priority_Bits    : constant := 3;

   GDB_First_CPU_Id : constant Interfaces.Unsigned_32 := 1;
   pragma Export (C, GDB_First_CPU_Id, "__gnat_gdb_cpu_first_id");
   --  This value is used by GDB to know the hardware id of the first CPU used
   --  by the run-time. On this board CPU #0 (the monitor) is not used,
   --  therefore the first id is 1.

end System.BB.Board_Parameters;
