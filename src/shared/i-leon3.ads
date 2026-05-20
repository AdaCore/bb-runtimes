------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       I N T E R F A C E S . L E O N 3                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

--  This package provides the appropriate mapping for the system registers.
--  This is a LEON3 specific  package, based on the UT699 LEON 3FT/SPARC V8
--  Micro-Processor Advanced Users Manual, dated March 2, 2009 from
--  www.aeroflex.com/LEON, referenced hereafter as AUM.

package Interfaces.Leon3 is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of record types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Reserved_2 is array (0 .. 1) of Boolean;
   for Reserved_2'Size use 2;
   pragma Pack (Reserved_2);

   type Reserved_3 is array (0 .. 2) of Boolean;
   for Reserved_3'Size use 3;
   pragma Pack (Reserved_3);

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_9 is array (0 .. 8) of Boolean;
   for Reserved_9'Size use 9;
   pragma Pack (Reserved_9);

   type Reserved_16 is array (0 .. 15) of Boolean;
   for Reserved_16'Size use 16;
   pragma Pack (Reserved_16);

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_21 is array (0 .. 20) of Boolean;
   for Reserved_21'Size use 21;
   pragma Pack (Reserved_21);

   type Reserved_22 is array (0 .. 21) of Boolean;
   for Reserved_22'Size use 22;
   pragma Pack (Reserved_22);

   type Reserved_23 is array (0 .. 22) of Boolean;
   for Reserved_23'Size use 23;
   pragma Pack (Reserved_23);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Reserved_25 is array (0 .. 24) of Boolean;
   for Reserved_25'Size use 25;
   pragma Pack (Reserved_25);

   type Reserved_27 is array (0 .. 26) of Boolean;
   for Reserved_27'Size use 27;
   pragma Pack (Reserved_27);

   --  Mapping between bits in a 32-bit register as used in the hardware
   --  documentation and bit order as used by Ada.

   --  This makes it easier to verify correctness against the AUM. Ranges will
   --  need to be reversed, but the compiler will check this.

   Bit00 : constant := 31; Bit01 : constant := 30; Bit02 : constant := 29;
   Bit03 : constant := 28; Bit04 : constant := 27; Bit05 : constant := 26;
   Bit06 : constant := 25; Bit07 : constant := 24; Bit08 : constant := 23;
   Bit09 : constant := 22; Bit10 : constant := 21; Bit11 : constant := 20;
   Bit12 : constant := 19; Bit13 : constant := 18; Bit14 : constant := 17;
   Bit15 : constant := 16; Bit16 : constant := 15; Bit17 : constant := 14;
   Bit18 : constant := 13; Bit19 : constant := 12; Bit20 : constant := 11;
   Bit21 : constant := 10; Bit22 : constant := 09; Bit23 : constant := 08;
   Bit24 : constant :=  7; Bit25 : constant := 06; Bit26 : constant := 05;
   Bit27 : constant :=  4; Bit28 : constant := 03; Bit29 : constant := 02;
   Bit30 : constant :=  1; Bit31 : constant := 00;

end Interfaces.Leon3;
