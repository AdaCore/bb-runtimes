------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with Interfaces.SF2;                  use Interfaces.SF2;
with Interfaces.SF2.System_Registers; use Interfaces.SF2.System_Registers;

package body System.SF2 is

   function Get_RCOSC_25_50_MHz_Frequency return Natural;

   -----------------------------------
   -- Get_RCOSC_25_50_MHz_Frequency --
   -----------------------------------

   function Get_RCOSC_25_50_MHz_Frequency return Natural
   is
   begin
      if System_Registers_Periph.MSSDDR_PLL_STATUS.RCOSC_DIV2 then
         return 25_000_000;
      else
         return 50_000_000;
      end if;
   end Get_RCOSC_25_50_MHz_Frequency;

   -----------------------
   -- Get_System_Clocks --
   -----------------------

   function Get_System_Clocks return System_Clocks
   is
      Ret     : System_Clocks;
      Clk_Src : UInt3;
      Mux_0   : Natural;
      Mux_1   : Natural;
      Mux_2   : Natural;

   begin
      if not System_Registers_Periph.MSSDDR_FACC1_CR.CONTROLLER_PLL_INIT then
         if not System_Registers_Periph.MSSDDR_FACC1_CR.FACC_GLMUX_SEL then
            --  Clocked from MSS PLL.
            --  ??? Those need to be user-defined values
            Ret :=
              (Core_Clock => 142_000_000,
               PCLK0      =>  71_000_000,
               PCLK1      =>  71_000_000,
               PCLK2      => 355_000_000,
               FIC0       => 142_000_000,
               FIC1       => 142_000_000,
               FIC64      => 142_000_000);
         else
            --  Clocked by standby clock
            Clk_Src :=
              System_Registers_Periph.MSSDDR_FACC2_CR.FACC_STANDBY_SEL;

            if (Clk_Src and 2#001#) = 0 then
               --  MUX 0 output comes from RCOSC_25_50 MHz
               Mux_0 := Get_RCOSC_25_50_MHz_Frequency;
            else
               --  MUX 0 output comes from XTLOSC_CLK
               Mux_0 := 32_000;
            end if;

            if (Clk_Src and 2#010#) = 0 then
               --  MUX 1 output comes from RCOSC_1MHZ
               Mux_1 := 1_000_000;
            else
               --  CCC2ASCI Clock
               Mux_1 := 1_000_000;
            end if;

            if (Clk_Src and 2#100#) = 0 then
               Mux_2 := Mux_0;
            else
               Mux_2 := Mux_1;
            end if;

            Ret :=
              (Core_Clock => Mux_2,
               PCLK0      => Mux_2,
               PCLK1      => Mux_2,
               PCLK2      => 355_000_000,
               FIC0       => Mux_2,
               FIC1       => Mux_2,
               FIC64      => Mux_2);
         end if;
      else
         Mux_2 := Get_RCOSC_25_50_MHz_Frequency;

         Ret :=
           (Core_Clock => Mux_2,
            PCLK0      => Mux_2,
            PCLK1      => Mux_2,
            PCLK2      => 355_000_000,
            FIC0       => Mux_2,
            FIC1       => Mux_2,
            FIC64      => Mux_2);
      end if;

      return Ret;
   end Get_System_Clocks;

end System.SF2;
