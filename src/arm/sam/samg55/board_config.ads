------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    Copyright (C) 2013-2016, AdaCore                      --
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

with Interfaces.SAM;     use Interfaces.SAM;
with Interfaces.SAM.PMC;

package Board_Config is
   pragma No_Elaboration_Code_All;
   --  This package is used before elaboration

   type Master_Clock_Sources is
     (Internal_32k_RC,
      External_32k_XTAL,
      External_32k_BYPASS,
      Internal_8M_RC,
      Internal_16M_RC,
      Internal_24M_RC,
      External_XTAL,
      External_BYPASS,
      PLLA,
      PLLB);
   subtype Oscillators is Master_Clock_Sources
     range Internal_32k_RC .. External_BYPASS;
   subtype PLL_Sources is Master_Clock_Sources
     range Internal_32k_RC .. External_32k_BYPASS;

   External_Oscillator_Startup_Time : constant Byte := 0;

   Master_Source    : constant Master_Clock_Sources := PLLA;
   Master_Prescaler : constant Interfaces.SAM.PMC.PMC_MCKR_PRES_Field :=
     Interfaces.SAM.PMC.Clk_1;

   PLLA_Enable      : constant Boolean := True;
   PLLA_Source      : constant PLL_Sources := External_32k_XTAL;
   PLLA_Mul         : constant UInt12 := 3662; --  120 MHz

   PLLB_Enable      : constant Boolean := True;
   PLLB_Source      : constant PLL_Sources := External_32k_XTAL;
   PLLB_Mul         : constant UInt11 := 1465; -- 48 MHz

end Board_Config;
