------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 I N T E R F A C E S . L E O N 3 . I R Q M P              --
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

pragma Restrictions (No_Elaboration_Code);

with System.Multiprocessors;
with System.BB.Board_Parameters;

package Interfaces.Leon3.Irqmp is
   pragma Preelaborate;

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of record types
   --  that have components of these types.

   --------------------------
   --  Interrupt Registers --
   --------------------------

   Irqmp_Base : constant := System.BB.Board_Parameters.Irqmp_Base;

   type Interrupt_Register is mod 2**32;
   for Interrupt_Register'Size use 32;
   pragma Suppress_Initialization (Interrupt_Register);
   pragma Volatile_Full_Access (Interrupt_Register);

   ------------------------------
   -- Interrupt Level Register --
   ------------------------------

   Interrupt_Level : Interrupt_Register;
   for Interrupt_Level'Address use System'To_Address (Irqmp_Base + 16#00#);

   --------------------------------
   -- Interrupt Pending Register --
   --------------------------------

   Interrupt_Pending : Interrupt_Register;
   for Interrupt_Pending'Address use System'To_Address (Irqmp_Base + 16#04#);

   -------------------------------
   -- Interrupt Force Registers --
   -------------------------------

   --  On single CPU Leon3 (e.g. UT699), there is only one interrupt force
   --  register, mapped at 16#8000_0208#. On SMP Leon3, there is one interrupt
   --  force register for each CPU, starting at 16#8000_0280#. The register at
   --  16#8000_0208# is an alias to 16#8000_0280#.

   type Interrupt_Registers_CPU_Array is
     array (System.Multiprocessors.CPU) of Interrupt_Register;
   for Interrupt_Registers_CPU_Array'Component_Size use 32;
   pragma Suppress_Initialization (Interrupt_Registers_CPU_Array);

   Interrupt_Force : Interrupt_Registers_CPU_Array;
   for Interrupt_Force'Address use
     System'To_Address (if System.BB.Board_Parameters.Max_Number_Of_CPUs > 1
                        then Irqmp_Base + 16#80# else Irqmp_Base + 16#08#);

   ------------------------------
   -- Interrupt Clear Register --
   ------------------------------

   Interrupt_Clear : Interrupt_Register;
   for Interrupt_Clear'Address use System'To_Address (Irqmp_Base + 16#0C#);

   ------------------------------
   -- Interrupt Mask Registers --
   ------------------------------

   Interrupt_Mask : Interrupt_Registers_CPU_Array;
   for Interrupt_Mask'Address use System'To_Address (Irqmp_Base + 16#40#);

   ------------------------------------
   -- Multiprocessor status register --
   ------------------------------------

   type Scalar_4 is mod 2**4;
   for Scalar_4'Size use 4;

   --  Bit array for CPU status. Previously this was a packed array of
   --  booleans, but was numbered in reverse order (as SPARC is big endian).
   type CPU_Status is mod 2**16;
   for CPU_Status'Size use 16;

   type Multiprocessor_Status_Register is record
      NCPUS : Scalar_4;
      --  Number of CPUs - 1

      Reserved : Reserved_8;

      EIRQ : Scalar_4;
      --  Interrupt number used for extended interrupts. 0 if extended
      --  interrupts are disabled.

      Status : CPU_Status;
      --  Power-down status of CPU [n]: 0 = power-down, 1 = running. Write
      --  with 1 to start processor n.
   end record;

   for Multiprocessor_Status_Register use record
      NCPUS    at 0 range Bit31 .. Bit28;
      Reserved at 0 range Bit27 .. Bit20;
      EIRQ     at 0 range Bit19 .. Bit16;
      Status   at 0 range Bit15 .. Bit00;
   end record;

   for Multiprocessor_Status_Register'Size use 32;
   pragma Suppress_Initialization (Multiprocessor_Status_Register);
   pragma Volatile_Full_Access (Multiprocessor_Status_Register);

   Multiprocessor_Status : Multiprocessor_Status_Register;
   for Multiprocessor_Status'Address use
       System'To_Address (Irqmp_Base + 16#10#);

   -----------------------------------------------
   --  Extended Interrupt Acknowledge Registers --
   -----------------------------------------------

   Extended_Interrupt_Acknowledge_Register : Interrupt_Registers_CPU_Array;
   for Extended_Interrupt_Acknowledge_Register'Address use
     System'To_Address (Irqmp_Base + 16#C0#);
end Interfaces.Leon3.Irqmp;
