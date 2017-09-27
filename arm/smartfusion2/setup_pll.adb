------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

pragma Restrictions (No_Elaboration_Code);

with Interfaces;
with Interfaces.SF2.System_Registers; use Interfaces.SF2.System_Registers;

procedure Setup_Pll
is
   SysReg : System_Registers_Peripheral renames System_Registers_Periph;

begin
   --  Wait for fabric PLL to lock
   loop
      exit when SysReg.MSSDDR_PLL_STATUS.FAB_PLL_LOCK;
   end loop;

   --  Negate MPLL bypass
   SysReg.MSSDDR_PLL_STATUS_HIGH_CR.FACC_PLL_BYPASS := False;

   --  Wait for MPLL to lock
   loop
      exit when SysReg.MSSDDR_PLL_STATUS.MPLL_LOCK;
   end loop;

   --  Switch FACC from standby to run mode
   SysReg.MSSDDR_FACC1_CR.FACC_GLMUX_SEL := False;

   --  Negate FPGA_SOFTRESET to de-assert MSS_RESET_N_M2F in the fabric
   SysReg.SOFT_RESET_CR.FPGA_SOFTRESET := False;
end Setup_Pll;
