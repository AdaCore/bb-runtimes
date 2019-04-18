------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
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

--  This package implements RISC-V architecture specific support for the GNAT
--  Ravenscar run time.

with System.Multiprocessors;
with System.BB.Threads.Queues;
with System.BB.Board_Support;
with System.BB.CPU_Specific; use System.BB.CPU_Specific;

package body System.BB.CPU_Primitives is

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   type Context_Switch_Params is record
      Running_Thread_Address : Address;
      --  Address of the running thread entry for the current cpu

      First_Thread_Address : Address;
      --  Address of the first read thread for the current cpu
   end record;
   pragma Convention (C, Context_Switch_Params);
   pragma Suppress_Initialization (Context_Switch_Params);
   --  This record describe data that are passed from Pre_Context_Switch
   --  to Context_Switch. In the assembly code we take advantage of the ABI
   --  so that the data returned are in the registers of the incoming call.
   --  So there is no need to copy or to move the data between both calls.

   function Pre_Context_Switch return Context_Switch_Params;
   pragma Export (Asm, Pre_Context_Switch, "__gnat_pre_context_switch");
   --  The full context switch is split in 2 stages:
   --  - Pre_Context_Switch: adjust the current priority (but don't modify
   --    the MSTATUS.MIE bit), and return the running and first thread queue
   --    addresses.
   --  - The assembly routine (context_switch) which does the real context
   --    switch.
   --  When called from interrupt handler, the stack pointer is saved before
   --  and restore after the context switch. Therefore the context switch
   --  cannot allocate a frame but only assembly code can guarantee that. We
   --  also take advantage of this two stage call to extract queue pointers
   --  in the Ada code.

   ------------------------
   -- Pre_Context_Switch --
   ------------------------

   function Pre_Context_Switch return Context_Switch_Params is
      use System.BB.Threads.Queues;
      use System.BB.Threads;

      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;

      New_Priority : constant Integer :=
                       First_Thread_Table (CPU_Id).Active_Priority;
   begin
      --  Called with interrupts disabled

      --  Set interrupt priority. Unlike the SPARC implementation, the
      --  interrupt priority is not part of the context (not in a register).
      --  However full interrupt disabling is part of the context switch.

      if New_Priority < Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (New_Priority);
      end if;

      return (Running_Thread_Table (CPU_Id)'Address,
              First_Thread_Table (CPU_Id)'Address);
   end Pre_Context_Switch;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is

      procedure Context_Switch_Asm
        (Running_Thread_Table_Element_Address : System.Address;
         Ready_Thread_Table_Element_Address : System.Address);
      pragma Import (Asm, Context_Switch_Asm, "__gnat_context_switch");
      --  Real context switch in assembly code

      Params : Context_Switch_Params;
   begin
      --  First set priority and get pointers

      Params := Pre_Context_Switch;

      --  Then the real context switch

      Context_Switch_Asm (Params.Running_Thread_Address,
                          Params.First_Thread_Address);
   end Context_Switch;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      --  Clear the Machine Interrupt Enable (MIE) bit of the mstatus register
      Clear_Mstatus_Bits (Mstatus_MIE);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : Integer) is
   begin

      if Level /= System.Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (Level);

         --  Really enable interrupts

         --  Set the Machine Interrupt Enable (MIE) bit of the mstatus register
         Set_Mstatus_Bits (Mstatus_MIE);
      end if;
   end Enable_Interrupts;

   ----------------------
   -- Initialize_Stack --
   ----------------------

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address)
   is
      use System.Storage_Elements;

      Minimum_Stack_Size_In_Bytes : constant Integer_Address :=
                                      CPU_Specific.Stack_Alignment;

      Initial_SP : constant System.Address :=
                     To_Address
                       (To_Integer (Base + Size) -
                          Minimum_Stack_Size_In_Bytes);

   begin
      Stack_Pointer := Initial_SP;
   end Initialize_Stack;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      procedure Start_Thread_Asm;
      pragma Import (Asm, Start_Thread_Asm, "__gnat_start_thread");

      Initial_SP : Address;

   begin
      --  No need to initialize the context of the environment task

      if Program_Counter = Null_Address then
         return;
      end if;

      --  We cheat as we don't know the stack size nor the stack base

      Initialize_Stack (Stack_Pointer, 0, Initial_SP);

      Buffer.RA := Start_Thread_Asm'Address;
      Buffer.SP := Initial_SP;

      --  Use callee saved registers to make sure the values are loaded during
      --  the first context_switch.
      Buffer.S1 := Argument;
      Buffer.S2 := Program_Counter;
   end Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
   begin
      null;
   end Initialize_CPU;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      null;
   end Install_Error_Handlers;

end System.BB.CPU_Primitives;
