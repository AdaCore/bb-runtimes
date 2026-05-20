------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   SYSTEM.BB.BOARD_SUPPORT.MULTIPROCESSORS                --
--                                                                          --
--                                  B o d y                                 --
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

with Interfaces.RP2040;
with Interfaces.RP2040.PSM; use Interfaces.RP2040.PSM;
with Interfaces.RP2040.SIO; use Interfaces.RP2040.SIO;

with System.BB.CPU_Primitives.Multiprocessors;
with System.Machine_Code;
with System.Storage_Elements;

with System; use System;

separate (System.BB.Board_Support)

package body Multiprocessors is

   SIO_IRQ_PROC0 : constant := 15;
   --  SIO FIFO IRQ (Poke Handler) for core0

   SIO_IRQ_PROC1 : constant := 16;
   --  SIO FIFO IRQ (Poke Handler) for core1

   procedure Poke_Handler
     with Export => True,
     External_Name => "__gnat_poke_handler";

   procedure Drain_FIFO;
   procedure Push_FIFO_Blocking (Value : UInt32);
   procedure Pop_FIFO_Blocking (Value : out UInt32);
   procedure Reset_Core1;
   procedure Launch_Core1 (Entry_Point  : System.Address;
                           Stack        : System.Address;
                           Vector_Table : System.Address);

   procedure Core1_Entry;

   procedure Initialize_Slave (CPU_Id : System.Multiprocessors.CPU)
     with Import => True,
     External_Name => "__gnat_initialize_slave";

   -----------------
   -- core1 stack --
   -----------------

   Core1_Stack_Size : constant := 2048;

   Core1_Stack : aliased array (1 .. Core1_Stack_Size)
     of System.Storage_Elements.Storage_Element
     with Linker_Section => (".stack1"),
     Alignment => Standard'Maximum_Alignment;

   ------------------
   -- Vector table --
   ------------------

   Vector_Table : aliased UInt32
     with Import => True,
     Convention => C,
     External_Name => "__vectors";

   --------------------
   -- Number_Of_CPUs --
   --------------------

   function Number_Of_CPUs return CPU is (2);

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return System.Multiprocessors.CPU is
   begin
      --  CPUID reads as 0 for core0 and 1 for core1.
      return System.Multiprocessors.CPU (SIO_Periph.CPUID + 1);
   end Current_CPU;

   -----------------------
   -- Poke_Handler --
   -----------------------

   procedure Poke_Handler is
   begin
      Drain_FIFO;
      CPU_Primitives.Multiprocessors.Poke_Handler;
   end Poke_Handler;

   --------------
   -- Poke_CPU --
   --------------

   --  Poke the given CPU to signal that a rescheduling may be required
   procedure Poke_CPU (CPU_Id : System.Multiprocessors.CPU) is
   begin
      if CPU_Id /= Current_CPU then
         --  Write to the TX FIFO (mailbox) to generate an interrupt on core1
         SIO_Periph.FIFO_WR := 0;
      end if;
   end Poke_CPU;

   --------------------
   -- Start_All_CPUs --
   --------------------

   procedure Start_All_CPUs is
   begin
      --  Core1 must be put back to its reset state, otherwise it won't
      --  respond to the handshake protocol in Launch_Core1.
      Reset_Core1;

      --  Launch core1.
      Launch_Core1 (Entry_Point  => Core1_Entry'Address,
                    Stack        => Core1_Stack'Address + Core1_Stack_Size,
                    Vector_Table => Vector_Table'Address);

      --  Enable our Poke Handler interrupt to receive pokes from core1.

      Enable_Interrupt_Request (SIO_IRQ_PROC0, Interrupt_Priority'First);
   end Start_All_CPUs;

   ----------------
   -- Drain_FIFO --
   ----------------

   procedure Drain_FIFO is
      Rx : UInt32;
      pragma Unreferenced (Rx);

   begin
      while SIO_Periph.FIFO_ST.VLD /= 0 loop
         Rx := SIO_Periph.FIFO_RD;
      end loop;

      --  Clear sticky FIFO error flags
      SIO_Periph.FIFO_ST := (ROE    => 1,
                             WOF    => 1,
                             others => <>);
   end Drain_FIFO;

   ------------------------
   -- Push_FIFO_Blocking --
   ------------------------

   procedure Push_FIFO_Blocking (Value : UInt32) is
   begin
      loop
         exit when SIO_Periph.FIFO_ST.RDY = 1;
      end loop;
      SIO_Periph.FIFO_WR := Value;

      --  Send the event to the other core.
      Asm ("sev", Volatile => True);
   end Push_FIFO_Blocking;

   -----------------------
   -- Pop_FIFO_Blocking --
   -----------------------

   procedure Pop_FIFO_Blocking (Value : out UInt32) is
   begin
      loop
         exit when SIO_Periph.FIFO_ST.VLD = 1;
      end loop;
      Value := SIO_Periph.FIFO_RD;
   end Pop_FIFO_Blocking;

   -----------------
   -- Reset_Core1 --
   -----------------

   procedure Reset_Core1 is
   begin
      --  Hard reset core 1.
      PSM_Periph.FRCE_OFF := (proc   => (As_Array => True,
                                         Arr      => (0 => 0,
                                                      1 => 1)),
                              others => <>);

      --  Wait until core1 has been powered off.
      loop
         exit when PSM_Periph.FRCE_OFF.proc.Arr (1) = 1;
      end loop;

      --  Release core1 from reset.
      PSM_Periph.FRCE_OFF := (others => <>);
   end Reset_Core1;

   ------------------
   -- Launch_Core1 --
   ------------------

   procedure Launch_Core1 (Entry_Point  : System.Address;
                           Stack        : System.Address;
                           Vector_Table : System.Address) is

      --  Handshake protocol with the bootrom to launch core1.
      Command_Sequence : constant array (1 .. 6) of UInt32 :=
         (1 => 0,
          2 => 0,
          3 => 1,
          4 => UInt32 (Vector_Table),
          5 => UInt32 (Stack),
          6 => UInt32 (Entry_Point));

      Command  : UInt32;
      Response : UInt32;
      I : Positive := Command_Sequence'First;

   begin
      loop
         Command := Command_Sequence (I);

         --  Drain the FIFO before sending a 0.
         if Command = 0 then
            Drain_FIFO;

            --  Core1 may be waiting for FIFO space
            Asm ("sev", Volatile => True);
         end if;

         Push_FIFO_Blocking (Command);
         Pop_FIFO_Blocking (Response);

         --  Move to the next command on correct response or start over
         if Response = Command then
            I := I + 1;
         else
            I := Command_Sequence'First;
         end if;

         exit when I > Command_Sequence'Last;
      end loop;
   end Launch_Core1;

   -----------------
   -- Core1_Entry --
   -----------------

   --  Entry point for core1. This is called by the bootrom on core1
   --  after the handshake protocol is completed (see Launch_Core1).
   procedure Core1_Entry is
   begin
      --  Initialize stack pointers (e.g. PSP). This is needed before
      --  we can successfully context switch on core1.

      System.BB.CPU_Primitives.Initialize_CPU;

      --  Prevent FIFO interrupt from triggering before the slave
      --  is fully initialized.

      Disable_Interrupts;

      --  Enable user interrupts that were attached to core1
      --  during elaboration.

      IP        := IP_Core1;   --  Set priorities
      NVIC_ISER := ISER_Core1; --  Enable interrupts

      --  Enable FIFO interrupt to receive pokes from core0.

      Drain_FIFO;
      Enable_Interrupt_Request (SIO_IRQ_PROC1, Interrupt_Priority'First);

      --  Self-poke to trigger initial context switch to any runnable task.
      --  Note that interrupts are kept disabled until the slave is fully
      --  initialized and calls Power_Down, at which point interrupts
      --  are enabled and the poke will be serviced to trigger the context
      --  switch into a runnable task.

      Set_Pending_Interrupt (SIO_IRQ_PROC1);

      Initialize_Slave (Current_CPU);
   end Core1_Entry;

end Multiprocessors;
