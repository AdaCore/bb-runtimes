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

--  This is the TI AM64x/AM243x Cortex-R5 version of this package

package System.BB.Parameters is
   pragma Pure;
   pragma No_Elaboration_Code_All;

   ---------------------
   -- Hardware clocks --
   ---------------------

   HFOSC0_CLKOUT : constant := 25_000_000;
   --  Frequency of the HFOSC0_CLKOUT is 25MHz

   Timer_Frequency : constant := HFOSC0_CLKOUT;
   --  Frequency of the Timer used for scheduler alarms

   GTC_Frequency : constant := 225_000_000;
   --  Frequency of the Global Timebase Counter

   Ticks_Per_Second : constant := GTC_Frequency;

   ----------------
   -- Interrupts --
   ----------------

   --  These definitions are in this package in order to isolate target
   --  dependencies.

   subtype Interrupt_Range is Natural range 0 .. 255;
   --  Number of interrupts supported by the AM64x Cortex R5 VIM

   Trap_Vectors : constant := 7;
   --  ARMv7 has these traps:
   --    0   (at 16#0000#) Reset
   --    1   (at 16#0004#) Undefined Instruction (synchronous)
   --    2   (at 16#0008#) Supervisor Call (synchronous)
   --    3   (at 16#000C#) Abort - Prefetch (synchronous)
   --    4   (at 16#0010#) Abort - Data (asynchronous)
   --    5   (at 16#0014#) IRQ Trap (asynchronous)
   --    6   (at 16#0018#) FIQ Trap (asynchronous)

   Interrupt_Unmask_Priority : constant System.Interrupt_Priority :=
                                 System.Interrupt_Priority'Last;
   --  The priority under which we unmask interrupts.
   --  Useful when we use FIQ to simulate priorities on ARM.

   VIM_Base_Address : constant := 16#2FFF_0000#;
   --  Base address of the Vectored Interrupt Manager

   ------------
   -- Stacks --
   ------------

   Interrupt_Stack_Size : constant := 4096;
   --  Size of each of the interrupt stacks. Each processor has its own
   --  interrupt stack. In case interrupts are nested, the same stack is used.

   Interrupt_Sec_Stack_Size : constant := 128;
   --  Size of the secondary stack for interrupt handlers

   ----------
   -- CPUS --
   ----------

   Max_Number_Of_CPUs : constant := 1;
   --  Maximum number of CPUs avaialble on the target.

   Multiprocessor : constant Boolean := Max_Number_Of_CPUs /= 1;
   --  Are we on a multiprocessor board?

   ----------
   -- UART --
   ----------

   UART_Base_Address : constant := 16#0280_0000#;
   --  UART base address

   Number_Of_UART_Modules : constant := 6;
   --  Number of UART Modules on the SoC

   type UART_ID is mod Number_Of_UART_Modules;
   --  UART Modules on the SoC

   IO_Module : constant UART_ID := 0;
   --  UART Module used by System.Text_IO

end System.BB.Parameters;
