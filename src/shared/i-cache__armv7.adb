------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       I N T E R F A C E S . C A C H E                    --
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

with Interfaces.ARM_V7AR; use Interfaces.ARM_V7AR;
with System; use System;

package body Interfaces.Cache is
   use System.Storage_Elements;

   procedure Dcache_Invalidate_By_Range (Start : Address; Len : Storage_Count)
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
         CP15.DCIMVAC (Addr);
         Off := Off + Line_Size;
         exit when Off > Len + Line_Off;
         Addr := Addr + Line_Size;
      end loop;

      Barriers.DSB;
   end Dcache_Invalidate_By_Range;

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
end Interfaces.Cache;
