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
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  This is the nRF52840 (ARMv7) version of this package

with System.BB.Board_Parameters;
with System.BB.MCU_Parameters;

package System.BB.Parameters is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.BB.Parameters);

   Clock_Frequency : constant := Board_Parameters.Main_Clock_Frequency;
   Ticks_Per_Second : constant := Clock_Frequency;

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Integer
     range -1 .. MCU_Parameters.Number_Of_Interrupts;
   --  Number of interrupts for the interrupt controller

   Trap_Vectors : constant := 17;
   --  While on this target there is little difference between interrupts
   --  and traps, we consider the following traps:
   --
   --    Name                        Nr
   --
   --    Reset_Vector                 1
   --    NMI_Vector                   2
   --    Hard_Fault_Vector            3
   --    Mem_Manage_Vector            4
   --    Bus_Fault_Vector             5
   --    Usage_Fault_Vector           6
   --    SVC_Vector                  11
   --    Debug_Mon_Vector            12
   --    Pend_SV_Vector              14
   --    Sys_Tick_Vector             15
   --    Interrupt_Request_Vector    16
   --
   --  These trap vectors correspond to different low-level trap handlers in
   --  the run time. Note that as all interrupt requests (IRQs) will use the
   --  same interrupt wrapper, there is no benefit to consider using separate
   --  vectors for each.

   Context_Buffer_Capacity : constant := 10;
   --  The context buffer contains registers r4 .. r11 and the SP_process
   --  (PSP). The size is rounded up to an even number for alignment

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 2 * 1024;
   --  Size of each of the interrupt stacks in bytes. While there nominally is
   --  an interrupt stack per interrupt priority, the entire space is used as a
   --  single stack.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   Has_FPU : constant Boolean := True;
   --  Set to true if core has a FPU

   Has_VTOR : constant Boolean := True;
   --  Set to true if core has a Vector Table Offset Register (VTOR).
   --  VTOR is implemented in Cortex-M0+, Cortex-M4 and above.

   Has_OS_Extensions : constant Boolean := True;
   --  Set to true if core has armv6-m OS extensions (PendSV, MSP, PSP,
   --  etc...). The OS extensions are optional for the Cortex-M1.

   Is_ARMv6m : constant Boolean := False;
   --  Set to true if core is an armv6-m (Cortex-M0, Cortex-M0+, Cortex-M1)

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
