------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    I N T E R F A C E S . A R M _ V 7 A R                 --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--                      Copyright (C) 2016-2020, AdaCore                    --
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
--  @design to generate assembly code.

package body Interfaces.ARM_V7AR is
   use System;

   package body CP15 is

      ---------------
      -- Get_CLIDR --
      ---------------

      function Get_CLIDR return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#1,%0,c0,c0,#1",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_CLIDR;

      ----------------
      -- Get_CCSIDR --
      ----------------

      function Get_CCSIDR return Unsigned_32 is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#1,%0,c0,c0,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_CCSIDR;

      ----------------
      -- Set_CSSELR --
      ----------------

      procedure Set_CSSELR (V : Unsigned_32) is
      begin
         Asm ("mcr p15,#2,%0,c0,c0,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_CSSELR;

      ---------------
      -- Get_SCTLR --
      ---------------

      function Get_SCTLR return Unsigned_32 is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c1,c0,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_SCTLR;

      ---------------
      -- Set_SCTLR --
      ---------------

      procedure Set_SCTLR (V : Unsigned_32) is
      begin
         Asm ("mcr p15,#0,%0,c1,c0,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_SCTLR;

      ---------------
      -- Get_ACTLR --
      ---------------

      function Get_ACTLR return Unsigned_32 is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c1,c0,#1",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_ACTLR;

      ---------------
      -- Set_ACTLR --
      ---------------

      procedure Set_ACTLR (V : Unsigned_32) is
      begin
         Asm ("mcr p15,#0,%0,c1,c0,#1",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_ACTLR;

      ---------------
      -- Get_MPUIR --
      ---------------

      function Get_MPUIR return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c0,c0,#4",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_MPUIR;

      ---------------------------------
      -- Get_MPU_Region_Base_Address --
      ---------------------------------

      function Get_MPU_Region_Base_Address return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c6,c1,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_MPU_Region_Base_Address;

      ---------------------------------
      -- Set_MPU_Region_Base_Address --
      ---------------------------------

      procedure Set_MPU_Region_Base_Address (V : Unsigned_32)
      is
      begin
         Asm ("mcr p15,#0,%0,c6,c1,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_MPU_Region_Base_Address;

      ------------------------------------
      -- Get_MPU_Region_Size_And_Enable --
      ------------------------------------

      function Get_MPU_Region_Size_And_Enable return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c6,c1,#2",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_MPU_Region_Size_And_Enable;

      ------------------------------------
      -- Set_MPU_Region_Size_And_Enable --
      ------------------------------------

      procedure Set_MPU_Region_Size_And_Enable (V : Unsigned_32)
      is
      begin
         Asm ("mcr p15,#0,%0,c6,c1,#2",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_MPU_Region_Size_And_Enable;

      -----------------------------------
      -- Get_MPU_Region_Access_Control --
      -----------------------------------

      function Get_MPU_Region_Access_Control return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c6,c1,#4",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_MPU_Region_Access_Control;

      -----------------------------------
      -- Set_MPU_Region_Access_Control --
      -----------------------------------

      procedure Set_MPU_Region_Access_Control (V : Unsigned_32)
      is
      begin
         Asm ("mcr p15,#0,%0,c6,c1,#4",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_MPU_Region_Access_Control;

      ---------------------------
      -- Set_MPU_Region_Number --
      ---------------------------

      procedure Set_MPU_Region_Number (V : Unsigned_32)
      is
      begin
         Asm ("mcr p15,#0,%0,c6,c2,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_MPU_Region_Number;

      -------------
      -- DCIMVAC --
      -------------

      procedure DCIMVAC (Mva : Address) is
      begin
         Asm ("mcr p15,#0,%0,c7,c6,#1",
              Inputs => Address'Asm_Input ("r", Mva),
              Volatile => True);
      end DCIMVAC;

      --------------
      -- DCCIMVAC --
      --------------

      procedure DCCIMVAC (Mva : Address) is
      begin
         Asm ("mcr p15,#0,%0,c7,c14,#1",
              Inputs => Address'Asm_Input ("r", Mva),
              Volatile => True);
      end DCCIMVAC;

      --------------
      -- Get_PMCR --
      --------------

      function Get_PMCR return Unsigned_32
      is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c9,c12,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_PMCR;

      --------------
      -- Set_PMCR --
      --------------

      procedure Set_PMCR (V : Unsigned_32)
      is
      begin
         Asm ("mcr p15,#0,%0,c9,c12,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_PMCR;

   end CP15;

   package body Barriers is
      ---------
      -- DSB --
      ---------

      procedure DSB is
      begin
         Asm ("dsb", Volatile => True);
      end DSB;

      ---------
      -- DMB --
      ---------

      procedure DMB is
      begin
         Asm ("dmb", Volatile => True);
      end DMB;

      ---------
      -- ISB --
      ---------

      procedure ISB is
      begin
         Asm ("isb", Volatile => True);
      end ISB;
   end Barriers;

   package body Cache is

      -----------------------
      -- Invalidate_DCache --
      -----------------------

      procedure Invalidate_DCache
      is
      begin
         Asm ("mcr p15,#0,%0,c15,c5,#0",
              Inputs => Unsigned_32'Asm_Input ("r", 0),
              Volatile => True);
      end Invalidate_DCache;

      procedure Invalidate_ICache
      is
      begin
         Asm ("mcr p15,#0,%0,c7,c5,#0",
              Inputs => Unsigned_32'Asm_Input ("r", 0),
              Volatile => True);
      end Invalidate_ICache;
   end Cache;
end Interfaces.ARM_V7AR;
