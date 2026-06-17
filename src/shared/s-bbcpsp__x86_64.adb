------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
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
------------------------------------------------------------------------------

--  This package implements x86-64 architecture specific support for the GNAT
--  Ravenscar run time.

with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.CPU_Specific is

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator for Asm blocks

   Cached_CPU_Model : CPU_Model := No_Model;
   --  Cache a copy of the CPU's model information so we can quickly reterive
   --  it when required.

   ------------------
   -- My_CPU_Model --
   ------------------

   function My_CPU_Model return CPU_Model is
      --  CPUId Leaf 01H, Feature Information

      Feature_Info_Leaf : constant Unsigned_32 := 16#01#;
      Features_EAX      : Feature_Information_EAX;
   begin
      if Cached_CPU_Model = No_Model then
         Asm ("cpuid",
              Inputs  => Unsigned_32'Asm_Input ("a", Feature_Info_Leaf),
              Outputs =>
                Feature_Information_EAX'Asm_Output ("=a", Features_EAX),
              Volatile => True);
         Cached_CPU_Model.Family := Features_EAX.Family;
         Cached_CPU_Model.Model  :=
           Intel_Model_Number
             (Shift_Left (Unsigned_8 (Features_EAX.Extended_Model), 4) +
              Unsigned_8 (Features_EAX.Model));
      end if;

      return Cached_CPU_Model;
   end My_CPU_Model;

   --------------------
   -- Read_TSC --
   --------------------

   function Read_TSC return Unsigned_64 is
      Raw_Count : Unsigned_64;
   begin
      Asm
        ("rdtsc"                                                         & NL &
         "shlq   $32,   %%rdx"                                           & NL &
         "orq    %%rdx, %%rax",
         Outputs  => Unsigned_64'Asm_Output ("=a", Raw_Count),
         Clobber  => "rdx",
         Volatile => True);
      return Raw_Count;
   end Read_TSC;

end System.BB.CPU_Specific;
