------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    I N T E R F A C E S . A R M _ V 7 A R                 --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--                         Copyright (C) 2016, AdaCore                      --
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

package body Interfaces.ARM_V7AR is
   use System;
   use System.Storage_Elements;

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

      --------------
      -- DCCIMVAC --
      --------------

      procedure DCCIMVAC (Mva : Address) is
      begin
         Asm ("mcr p15,#0,%0,c7,c14,#1",
              Inputs => Address'Asm_Input ("r", Mva),
              Volatile => True);
      end DCCIMVAC;

      function Get_SCTLR return Unsigned_32 is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#0,%0,c1,c0,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_SCTLR;

      procedure Set_SCTLR (V : Unsigned_32) is
      begin
         Asm ("mcr p15,#0,%0,c1,c0,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_SCTLR;

      function Get_CCSIDR return Unsigned_32 is
         Res : Unsigned_32;
      begin
         Asm ("mrc p15,#1,%0,c0,c0,#0",
              Outputs => Unsigned_32'Asm_Output ("=r", Res),
              Volatile => True);
         return Res;
      end Get_CCSIDR;

      procedure Set_CSSELR (V : Unsigned_32) is
      begin
         Asm ("mcr p15,#2,%0,c0,c0,#0",
              Inputs => Unsigned_32'Asm_Input ("r", V),
              Volatile => True);
      end Set_CSSELR;
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
      -- ISB --
      ---------

      procedure ISB is
      begin
         Asm ("isb", Volatile => True);
      end ISB;
   end Barriers;

   package body Cache is
      ---------------------------
      -- Dcache_Flush_By_Range --
      ---------------------------

      procedure Dcache_Flush_By_Range (Start : Address; Len : Storage_Count)
      is
         Line_Size : constant := 16;
         Line_Off : Storage_Count;
         Off : Storage_Count;
         Addr : Address;
      begin
         Line_Off := Start mod Line_Size;
         Addr := Start - Line_Off;
         Off := 0;
         loop
            CP15.DCCIMVAC (Addr);
            Off := Off + Line_Size;
            exit when Off > Len + Line_Off;
            Addr := Addr + Line_Size;
         end loop;

         Barriers.DSB;
      end Dcache_Flush_By_Range;
   end Cache;
end Interfaces.ARM_V7AR;
