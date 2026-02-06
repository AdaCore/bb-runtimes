------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   SYSTEM.BB.BOARD_SUPPORT.MULTIPROCESSORS                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                     --
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

--  This is the PolarFire SoC SMP version of this package

with Interfaces; use Interfaces;

with System.BB.CPU_Primitives;
with System.BB.CPU_Specific;
with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System; use System;
with System.BB.Parameters;

separate (System.BB.Board_Support)

package body Multiprocessors is
  --  The PolarFire SoC has 5 RISC-V cores:
  --    - 1 E51 Monitor hart (mhartid 0) - not used by Ada bareboard runtime
  --    - 4 U54 Application harts (mhartid 1-4) - used by Ada bareboard runtime

   --------------------
   -- Number_Of_CPUs --
   --------------------

   function Number_Of_CPUs return System.Multiprocessors.CPU
   is (System.BB.Parameters.Max_Number_Of_CPUs);
   --  The runtime supports the 4 U54 application harts (mhartid 1-4)

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return System.Multiprocessors.CPU is
      Hart_Id : constant Hart_Id_Range := Hart_Id_Range (CPU_Specific.Mhartid);
   begin
      --  Map hardware hart IDs (1-4) to Ada CPU IDs (1-4)
      --  The E51 monitor core (hartid 0) is not used
      return System.Multiprocessors.CPU (Hart_Id);
   end Current_CPU;

   --------------
   -- Poke_CPU --
   --------------

   procedure Poke_CPU (CPU_Id : System.Multiprocessors.CPU) is
      use System.Multiprocessors;
      Hart_Id : constant Hart_Id_Range := Hart_Id_Range (CPU_Id);
   begin
      --  Trigger a machine software interrupt on the target hart
      --  by setting its MSIP register to 1
      if CPU_Id /= Current_CPU then
         CPU_Specific.CLINT_MSIP (Hart_Id) := 1;
      end if;
   end Poke_CPU;

   --------------------
   -- Start_All_CPUs --
   --------------------

   procedure Start_All_CPUs is
   begin
      --  The primary hart (mhartid 1) is already running and was started
      --  by the bootloader. We need to wake up the secondary harts (2-4).
      --
      --  On PolarFire SoC, the secondary harts are initially parked in
      --  a WFI (wait-for-interrupt) loop in the startup code. waiting for
      --  this poke. (See startup code)
      --  FIXME: Is there a chance that the secondary harts might miss this
      --  poke if they haven't entered WFI yet? If so, we might need a more
      --  robust synchronization mechanism.

      for CPU_Id in System.Multiprocessors.CPU range 2 .. Number_Of_CPUs loop
         Poke_CPU (CPU_Id);
      end loop;
   end Start_All_CPUs;

end Multiprocessors;
