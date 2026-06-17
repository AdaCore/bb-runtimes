------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ P A R A M E T E R S           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                    Copyright (C) 2012-2019, AdaCore                      --
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

--  This package defines board parameters for the HiFive1

package System.BB.Board_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Clock_Scale     : constant := 2;
   --  Scaling factor for clock frequency. This is used to provide a clock
   --  frequency that results in a definition of Time_Unit less than 20
   --  microseconds (as Ada RM D.8 (30) requires).

   Decrementer_Frequency : constant Positive := 32_768;
   --  Frequency of the system clock for the decrementer timer

   Clock_Frequency : constant Positive := Decrementer_Frequency * Clock_Scale;
   --  Scaled clock frequency

   CLINT_Base_Address    : constant := 16#02000000#;
   CLINT_Mtime_Offset    : constant := 16#BFF8#;
   CLINT_Mtimecmp_Offset : constant := 16#4000#;

   Mtime_Base_Address : constant Address :=
     System'To_Address (CLINT_Base_Address + CLINT_Mtime_Offset);
   --  Address of the memory mapped mtime register

   Mtimecmp_Base_Address : constant Address :=
     System'To_Address (CLINT_Base_Address + CLINT_Mtimecmp_Offset);
   --  Address of the memory mapped mtimecmp register

end System.BB.Board_Parameters;
