------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       I N T E R F A C E S . C A C H E                    --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                      Copyright (C) 2016-2018, AdaCore                    --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;
use System;

package body Interfaces.Cache is
   use System.Storage_Elements;

   --  Binding of aarch64 instructions:

   procedure DC_CIVAC (Addr : System.Address) with Inline_Always;
   procedure DC_IVAC (Addr : System.Address) with Inline_Always;
   procedure DSB with Inline_Always;

   procedure Set_CSSELR_EL1 (Val : Unsigned_32) with Inline_Always;

   --  Utility

   Cache_Line : constant := 64;

   function Align_To_Cache_Line
     (Addr : System.Address)
      return System.Address is
        (Addr - Storage_Offset (Addr mod Cache_Line));
   --  Align an address to the start of cache line that contains the address

   --  Intermediate subprograms

   procedure DCache_Invalidate_Line (Addr : System.Address) with Inline_Always;
   procedure DCache_Flush_Line (Addr : System.Address) with Inline_Always;

   --------------
   -- DC_CIVAC --
   --------------

   procedure DC_CIVAC (Addr : System.Address) is
   begin
      Asm ("dc civac, %0",
           Inputs => System.Address'Asm_Input ("r", Addr),
           Volatile => True);
   end DC_CIVAC;

   -------------
   -- DC_IVAC --
   -------------

   procedure DC_IVAC (Addr : System.Address) is
   begin
      Asm ("dc ivac, %0",
           Inputs => System.Address'Asm_Input ("r", Addr),
           Volatile => True);
   end DC_IVAC;

   ---------
   -- DSB --
   ---------

   procedure DSB is
   begin
      Asm ("dsb sy", Volatile => True);
   end DSB;

   --------------------
   -- Set_CSSELR_EL1 --
   --------------------

   procedure Set_CSSELR_EL1 (Val : Unsigned_32) is
   begin
      Asm ("msr csselr_el1, %0",
           Inputs   => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_CSSELR_EL1;

   ----------------------------
   -- DCache_Invalidate_Line --
   ----------------------------

   procedure DCache_Invalidate_Line (Addr : System.Address)
   is
   begin
      --  Select L1 D-cache
      Set_CSSELR_EL1 (0);
      DC_IVAC (Addr);
      DSB;
      --  Select L2 D-cache
      Set_CSSELR_EL1 (2);
      DC_IVAC (Addr);
      DSB;
   end DCache_Invalidate_Line;

   -----------------------
   -- DCache_Flush_Line --
   -----------------------

   procedure DCache_Flush_Line (Addr : System.Address)
   is
   begin
      --  Select L1 D-cache
      Set_CSSELR_EL1 (0);
      DC_CIVAC (Addr);
      DSB;
      --  Select L2 D-cache
      Set_CSSELR_EL1 (2);
      DC_CIVAC (Addr);
      DSB;
   end DCache_Flush_Line;

   --------------------------------
   -- Dcache_Invalidate_By_Range --
   --------------------------------

   procedure Dcache_Invalidate_By_Range
     (Start  : System.Address;
      Len    : System.Storage_Elements.Storage_Count)
   is
      Tmp_Addr   : System.Address := Align_To_Cache_Line (Start);
      --  Start address aligned on a cache line
      End_Addr   : constant System.Address := Start + Len;
      Tmp_End    : constant System.Address := Align_To_Cache_Line (End_Addr);
      --  End address aligned on a cache line

   begin
      if Len = 0 then
         return;
      end if;

      --  Mask IRQs/FIQs during cache maintenance
      Asm ("msr DAIFSet, #3", Volatile => True);

      --  If the cache lines span outside the range, flush instead of
      --  invalidate, else we could lose neighbouring data
      if Tmp_Addr /= Start then
         DCache_Flush_Line (Tmp_Addr);
         Tmp_Addr := Tmp_Addr + Cache_Line;
      end if;
      if Tmp_End /= End_Addr and then End_Addr > Tmp_Addr then
         DCache_Flush_Line (Tmp_End);
      end if;

      while Tmp_Addr < Tmp_End loop
         DCache_Invalidate_Line (Tmp_Addr);
         Tmp_Addr := Tmp_Addr + Cache_Line;
      end loop;

      --  Unmask interrupts
      Asm ("msr DAIFClr, #3", Volatile => True);
   end Dcache_Invalidate_By_Range;

   ---------------------------
   -- Dcache_Flush_By_Range --
   ---------------------------

   procedure Dcache_Flush_By_Range
     (Start : System.Address;
      Len   : System.Storage_Elements.Storage_Count)
   is
      Tmp_Addr   : System.Address := Align_To_Cache_Line (Start);
      End_Addr   : constant System.Address := Start + Len;

   begin
      if Len = 0 then
         return;
      end if;

      --  Mask IRQs/FIQs during cache maintenance
      Asm ("msr DAIFSet, #3", Volatile => True);

      while Tmp_Addr < End_Addr loop
         DCache_Flush_Line (Tmp_Addr);
         Tmp_Addr := Tmp_Addr + Cache_Line;
      end loop;

      --  Unmask interrupts
      Asm ("msr DAIFClr, #3", Volatile => True);
   end Dcache_Flush_By_Range;
end Interfaces.Cache;
