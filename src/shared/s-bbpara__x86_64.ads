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

--  This package defines basic parameters used by the low level tasking system

--  This is the x86-64 version of this package

pragma Restrictions (No_Elaboration_Code);

package System.BB.Parameters is
   pragma Pure;

   --------------------
   -- Hardware clock --
   --------------------

   Ticks_Per_Second : constant := 1_000_000_000;
   --  On x86-64 we read the TSC and convert that time to nanoseconds

   Local_APIC_Frequency : constant := 0;
   TSC_Frequency        : constant := 0;
   --  The frequencies of the Local APIC and Time Stamp Clock (TSC) in Hertz.
   --  When set to zero the runtime will attempt to determine its value from
   --  the processor's internal registers following the guidelines provided by
   --  the Intel 64 and IA-32 Architectures Software Developer's Manual,
   --  Volume 3B, Section 18.7.3. Since the TSC clock source is implemented
   --  differently acros the different Intel chip families, on some certain
   --  processors the runtime may fail to either determine the TSC frequency or
   --  will set it incorrectly. In the former case the runtime will raise a
   --  Program_Error on boot, while for the latter always check to ensure the
   --  timing behaviour is as expected. In both cases you will need to manual
   --  set the Local_APIC_Frequency and TSC_Frequency constants above.
   pragma Compile_Time_Error
     ((Local_APIC_Frequency = 0 and TSC_Frequency /= 0) or
       (Local_APIC_Frequency /= 0 and TSC_Frequency = 0),
       "Local_APIC_Frequency and TSC_Frequency need to be both zero or both " &
       "non-zero");

   APIC_Timer_Divider : constant := 4;
   --  Aim for microsecond granularity for most clock sources

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 0 .. 255;
   --  Number of interrupts supported by the Local APIC

   IO_APIC_Base_Address : constant := 16#FEC0_0000#;
   --  Address of the I/O APIC

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Frame_Size : constant := 8 * 1024;  --  bytes
   --  Size of the interrupt stack used for handling an interrupt.

   Interrupt_Stack_Size : constant :=
     Interrupt_Stack_Frame_Size *
       (Interrupt_Priority'Last - Interrupt_Priority'First + 1);
   --  Total size of the interrupt stack per processor. Each processor
   --  allocates an individual interrupt stack frame for each priority level.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   Exception_Stack_Size : constant := 4096; -- bytes
   --  Size for each processor exception stack.

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

end System.BB.Parameters;
