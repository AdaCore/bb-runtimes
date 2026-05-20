------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      Copyright (C) 2016-2021, AdaCore                    --
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
------------------------------------------------------------------------------

with Interfaces;              use Interfaces;
with Interfaces.ARM_V7AR;     use Interfaces.ARM_V7AR;
with System.MPU_Definitions;  use System.MPU_Definitions;
with System.Board_Parameters; use System.Board_Parameters;

package body System.MPU_Init is

   --------------
   -- MPU_Init --
   --------------

   procedure MPU_Init
   is
      SCTLR                    : Unsigned_32;
      MPUIR                    : Unsigned_32;
      Num_Rgn                  : Unsigned_32;

   begin
      --  Get the number of MPU regions
      MPUIR := CP15.Get_MPUIR;
      Num_Rgn := Shift_Right (MPUIR and 16#FF00#, 8);

      --  Configure memory regions.

      for Index in MPU_Config'Range loop
         declare
            C : MPU_Region_Configuration renames MPU_Config (Index);
         begin
            CP15.Set_MPU_Region_Number (Index);
            CP15.Set_MPU_Region_Base_Address (C.Base_Address);
            CP15.Set_MPU_Region_Size_And_Enable (As_W32 (C.Size_And_Enable));
            CP15.Set_MPU_Region_Access_Control (As_W32 (C.Access_Control));
         end;
      end loop;

      --  Disable the unused regions

      for Index in MPU_Config'Last + 1 .. Num_Rgn - 1 loop
         CP15.Set_MPU_Region_Number (Index);
         CP15.Set_MPU_Region_Base_Address    (16#0000_0000#);
         CP15.Set_MPU_Region_Size_And_Enable (0);
         CP15.Set_MPU_Region_Access_Control  (0);
      end loop;

      --  Enable MPU (bit 0 of SCTLR)
      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR or 1;
      Barriers.DSB;
      CP15.Set_SCTLR (SCTLR);
      Barriers.ISB;
   end MPU_Init;

end System.MPU_Init;
