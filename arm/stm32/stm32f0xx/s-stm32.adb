------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2020, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

with System.BB.Board_Parameters;

with Interfaces;            use Interfaces;
with Interfaces.STM32;      use Interfaces.STM32;
with Interfaces.STM32.RCC;  use Interfaces.STM32.RCC;

package body System.STM32 is

   package Param renames System.BB.Board_Parameters;

   HPRE_Presc_Table : constant array (AHB_Prescaler_Enum) of UInt32 :=
     (2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (APB_Prescaler_Enum) of UInt32 :=
     (2, 4, 8, 16);

   -------------------
   -- System_Clocks --
   -------------------

   function System_Clocks return RCC_System_Clocks
   is
      Source       : constant SYSCLK_Source :=
                      SYSCLK_Source'Val (RCC_Periph.CFGR.SWS);
      Result       : RCC_System_Clocks;

   begin
      case Source is

         --  HSI as source

         when SYSCLK_SRC_HSI =>
            Result.SYSCLK := HSICLK;

         --  HSE as source

         when SYSCLK_SRC_HSE =>
            Result.SYSCLK := Param.HSE_Clock_Frequency;

         --  PLL as source

         when SYSCLK_SRC_PLL =>
            declare
               PLLMUL : constant UInt32 :=
                 UInt32 (RCC_Periph.CFGR.PLLMUL) + 2;

               PREDIV : constant UInt32 :=
                 UInt32 (RCC_Periph.CFGR2.PREDIV + 1);

               PLLOUT : UInt32;

            begin
               case PLL_Source'Val (RCC_Periph.CFGR.PLLSRC) is
                  when PLL_SRC_HSI_2 =>
                     PLLOUT := (HSICLK / 2) * PLLMUL;
                  when PLL_SRC_HSI_PREDIV =>
                     PLLOUT := (HSICLK / PREDIV) * PLLMUL;
                  when PLL_SRC_HSE_PREDIV =>
                     PLLOUT := (Param.HSE_Clock_Frequency / PREDIV) * PLLMUL;
                  when PLL_SRC_HSI48_PREDIV =>
                     PLLOUT := (HSI48CLK / PREDIV) * PLLMUL;
               end case;

               Result.SYSCLK := PLLOUT;
            end;

         --  HSI48 as source

         when SYSCLK_SRC_HSI48 =>
            Result.SYSCLK := HSI48CLK;
      end case;

      declare
         function To_AHBP is new Ada.Unchecked_Conversion
           (CFGR_HPRE_Field, AHB_Prescaler);
         function To_APBP is new Ada.Unchecked_Conversion
           (CFGR_PPRE_Field, APB_Prescaler);

         HPRE      : constant AHB_Prescaler := To_AHBP (RCC_Periph.CFGR.HPRE);
         HPRE_Div  : constant UInt32 := (if HPRE.Enabled
                                         then HPRE_Presc_Table (HPRE.Value)
                                         else 1);
         PPRE      : constant APB_Prescaler := To_APBP (RCC_Periph.CFGR.PPRE);
         PPRE_Div : constant UInt32 := (if PPRE.Enabled
                                        then PPRE_Presc_Table (PPRE.Value)
                                        else 1);

      begin
         Result.HCLK := Result.SYSCLK / HPRE_Div;
         Result.PCLK := Result.HCLK   / PPRE_Div;

         if not PPRE.Enabled then
            Result.TIMCLK := Result.PCLK;
         else
            Result.TIMCLK := Result.PCLK * 2;
         end if;
      end;

      return Result;
   end System_Clocks;

end System.STM32;
