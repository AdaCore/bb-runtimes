------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    I N T E R F A C E S . A R M _ V 7 A R                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                      Copyright (C) 2016-2017, AdaCore                    --
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

--  Low level operations for ARM V7A and ARM V7R cores (cache maintainance,
--  access to control registers...)

with System;
with System.Storage_Elements;

package Interfaces.ARM_V7AR is
   --  pragma No_Elaboration_Code_All;
   --  Not yet ready as Storage_Elements doesn't have the pragma

   pragma Preelaborate;

   --  Access to CP15 registers

   package CP15 is
      function Get_CLIDR return Unsigned_32
        with Inline_Always;
      --  Get CLIDR

      procedure DCCIMVAC (Mva : System.Address)
        with Inline_Always;
      --  Data cache clean and invalidate by va

      function Get_SCTLR return Unsigned_32
        with Inline_Always;
      procedure Set_SCTLR (V : Unsigned_32)
        with Inline_Always;
      --  Get/Set SCTLR

      SCTLR_C : constant := 16#0004#;
      SCTLR_I : constant := 16#1000#;

      function Get_CCSIDR return Unsigned_32
        with Inline_Always;
      --  Get CCSIDR

      procedure Set_CSSELR (V : Unsigned_32)
        with Inline_Always;
      --  Set CSSELR
   end CP15;

   --  Memory barriers

   package Barriers is
      procedure DSB
        with Inline_Always;
      --  Data synchronization barrier

      procedure ISB
        with Inline_Always;
      --  Instruction synchronization barrier
   end Barriers;

   --  Cache maintenance

   package Cache is
      procedure Dcache_Flush_By_Range
        (Start : System.Address;
         Len   : System.Storage_Elements.Storage_Count);
      --  Clean and invalidate all location between START and START + LEN - 1
   end Cache;
end Interfaces.ARM_V7AR;
