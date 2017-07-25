------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

--  This package contains the primitives which are dependent on the
--  underlying processor.

pragma Restrictions (No_Elaboration_Code);

with System;
with System.Storage_Elements;
with System.BB.CPU_Specific;

package System.BB.CPU_Primitives is
   pragma Preelaborate;

   ------------------------
   -- Context Management --
   ------------------------

   subtype Context_Buffer is System.BB.CPU_Specific.Context_Buffer;

   procedure Context_Switch;
   --  Perform the context switch between the running_thread and the first
   --  thread. The value of running_thread will be updated.

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address);
   --  Initialize a stack which spans BASE .. BASE + SIZE - 1. Set
   --  STACK_POINTER to the address to be used by the processor.

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address);
   pragma Inline (Initialize_Context);
   --  Initialize_Context inserts inside the context buffer the default
   --  values for each register. The values for the stack pointer, the
   --  program counter, and argument to be passed are provided as arguments.

   ---------------------------------
   -- Interrupt and Trap Handling --
   ---------------------------------

   procedure Install_Error_Handlers;
   --  Called at system initialization time to install a CPU specific trap
   --  handler, GNAT_Error_Handler, that converts synchronous traps to
   --  appropriate exceptions.

   procedure Disable_Interrupts;
   pragma Inline (Disable_Interrupts);
   --  All external interrupts (asynchronous traps) are disabled

   procedure Enable_Interrupts (Level : Integer);
   pragma Inline (Enable_Interrupts);
   --  Interrupts are enabled if they are above the value given by Level

   procedure Initialize_CPU;
   pragma Inline (Initialize_CPU);

end System.BB.CPU_Primitives;
