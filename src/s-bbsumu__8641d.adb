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

with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.BB.CPU_Primitives.Multiprocessors;

separate (System.BB.Board_Support)

package body Multiprocessors is
   use System.Multiprocessors;

   Poke_Interrupt : constant  BB.Interrupts.Interrupt_ID := 1;
   --  Use interrupt #1

   FRR : Unsigned_32;
   for FRR'Address use 16#f804_1000#;
   pragma Import (Ada, FRR);
   --  ePIC's Feature Reporting Register

   MCM_PCR : Unsigned_32;
   for MCM_PCR'Address use System'To_Address (CCSRBAR + 16#1010#);
   pragma Import (Ada, MCM_PCR);
   --  Port configuration register

   procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID);
   --  Poke interrupt handler

   --------------------
   -- Number_Of_CPUs --
   --------------------

   function Number_Of_CPUs return CPU is
      NCPUs : CPU;
   begin
      NCPUs := CPU (Shift_Right (FRR and 16#00001f00#, 8) + 1);
      return NCPUs;
   end Number_Of_CPUs;

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return CPU is
      Cpu_Id : Unsigned_32;
      for Cpu_Id'Address use System'To_Address (CCSRBAR + 16#4_0090#);
      pragma Import (Ada, Cpu_Id);
      --  Per-CPU WHOAMI register
   begin
      return CPU_Range (Cpu_Id) + CPU'First;
   end Current_CPU;

   --------------
   -- Poke_CPU --
   --------------

   procedure Poke_CPU (CPU_Id : CPU) is
      Val : Unsigned_32;

      IPI : Unsigned_32;
      for IPI'Address use System'To_Address (CCSRBAR + 16#4_0040#);
      pragma Import (Ada, IPI);
      --  Per-CPU interproces interrupt dispatch register (IPIDR0)
   begin
      --  Send IPI0 to processor CPU_Id
      Val := Shift_Left (1, CPU_Range'Pos (CPU_Id - CPU'First));
      IPI := IPI or Val;
   end Poke_CPU;

   ------------------
   -- Poke_Handler --
   ------------------

   procedure Poke_Handler (Interrupt : BB.Interrupts.Interrupt_ID) is
      Bogus : Unsigned_32;
      pragma Unreferenced (Bogus);
      IACK  : Unsigned_32;
      for IACK'Address use System'To_Address (CCSRBAR + 16#4_00a0#);
      pragma Import (Ada, IACK);
      EOI   : Unsigned_32;
      for EOI'Address use System'To_Address (CCSRBAR + 16#4_00b0#);
      pragma Import (Ada, EOI);
   begin
      --  Make sure we are handling the right interrupt
      pragma Assert (Interrupt = Poke_Interrupt);

      --  Acknowledge the interrupt by reading the OpenPIC IACK register
      Bogus := IACK;

      CPU_Primitives.Multiprocessors.Poke_Handler;

      EOI := 0;
   end Poke_Handler;

   --------------------
   -- Start_All_CPUs --
   --------------------

   procedure Start_All_CPUs is
      Val : Unsigned_32;

      IPIVPR : Unsigned_32;
      for IPIVPR'Address use System'To_Address (CCSRBAR + 16#4_10a0#);
      pragma Import (Ada, IPIVPR);
      --  IPI0 vector/priority register

      PCTP : Unsigned_32;
      for PCTP'Address use System'To_Address (CCSRBAR + 16#6_1080#);
      pragma Import (Ada, PCTP);
      --  Processor 1 current task priority register

   begin
      BB.Interrupts.Attach_Handler
        (Poke_Handler'Access, Poke_Interrupt, Interrupt_Priority'First);
      PCTP := 0;

      --  Unask IPI, priority 15, vector #0
      IPIVPR := (IPIVPR or 16#000f_0000#) and (not 16#8000_0000#);

      --  Enable all CPUs
      Val := Shift_Left
        (Shift_Left (1, CPU_Range'Pos (Number_Of_CPUs)) - 1, 24);
      MCM_PCR := MCM_PCR or Val;
   end Start_All_CPUs;
end Multiprocessors;
