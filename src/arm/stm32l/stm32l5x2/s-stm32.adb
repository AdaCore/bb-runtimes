------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2021, Free Software Foundation, Inc.         --
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

with System.BB.Parameters;
with Interfaces.STM32;      use Interfaces.STM32;
with Interfaces.STM32.RCC;  use Interfaces.STM32.RCC;

package body System.STM32 is

   package Params renames System.BB.Parameters;

   subtype Divisor is UInt32;

   HPRE_Prescaler_Divisors : constant array (AHB_Prescalers) of Divisor :=
     (1, 2, 4, 8, 16, 64, 128, 256, 512);
     --  per RM0438 Rev 6 pg 353/2194

   PPRE_Prescaler_Divisors : constant array (APB_Prescalers) of Divisor :=
     (1, 2, 4, 8, 16);

   MSI_Range_Table : constant array (UInt4) of UInt32 :=
     (0      => 100_000,
      1      => 200_000,
      2      => 400_000,
      3      => 800_000,
      4      => 1_000_000,
      5      => 2_000_000,
      6      => 4_000_000,
      7      => 8_000_000,
      8      => 16_000_000,
      9      => 24_000_000,
      10     => 32_000_000,
      11     => 48_000_000,
      others => 0);

   procedure Get_All_But_SYSCLK (This : in out RCC_System_Clocks);

   -------------------
   -- System_Clocks --
   -------------------

   function System_Clocks return RCC_System_Clocks is
      Result : RCC_System_Clocks;
   begin
      case SYSCLK_Source'Enum_Val (RCC_Periph.CFGR.SWS) is
         when SYSCLK_SRC_MSI =>
            Result.SYSCLK := Params.MSI_Clock;
         when SYSCLK_SRC_HSI =>
            Result.SYSCLK := HSICLK;
         when SYSCLK_SRC_HSE =>
            Result.SYSCLK := Params.HSE_Clock;  -- note: HSE is optional
         when SYSCLK_SRC_PLL =>
            Result.SYSCLK := Computed_SYSCLK_From_PLL;
      end case;
      Get_All_But_SYSCLK (Result);
      return Result;
   end System_Clocks;

   ------------------------------
   -- Computed_SYSCLK_From_PLL --
   ------------------------------

   function Computed_SYSCLK_From_PLL return Frequency is
      PLLM      : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLM) + 1;
      PLLN      : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLN);
      PLLR      : constant UInt32 := (case RCC_Periph.PLLCFGR.PLLR is
                                        when 2#00# => 2,
                                        when 2#01# => 4,
                                        when 2#10# => 6,
                                        when 2#11# => 8);
      MSI_Value : UInt32;
      MSI_Index : UInt4;
      PLL_VCO   : UInt32;
      Result    : Frequency;
   begin
      if PLL_Source'Enum_Val (RCC_Periph.PLLCFGR.PLLSRC) = PLL_SRC_MSI then
         if not RCC_Periph.CR.MSIRGSEL then
            --  MSISRANGE from RCC_CSR applies

            MSI_Index := RCC_Periph.CSR.MSISRANGE;
         else
            --  MSIRANGE from RCC_CR applies

            MSI_Index := RCC_Periph.CR.MSIRANGE;
         end if;
         MSI_Value := MSI_Range_Table (MSI_Index);
      end if;

      --  See RM0438 Rev 6, pg 354/2194, section 9.8.4 RCC PLL configuration

      case PLL_Source'Enum_Val (RCC_Periph.PLLCFGR.PLLSRC) is
         when PLL_SRC_None =>
            PLL_VCO := 0;
         when PLL_SRC_HSE =>
            PLL_VCO := Params.HSE_Clock;
         when PLL_SRC_HSI =>
            PLL_VCO := HSICLK;
         when PLL_SRC_MSI =>
            PLL_VCO := MSI_Value;
      end case;

      if PLL_VCO = 0 then
         --  the PLL is not really selected as the SYSCLK source

         Result := 0;
      else
         Result := ((PLL_VCO / PLLM) * PLLN) / PLLR;
      end if;
      return Result;
   end Computed_SYSCLK_From_PLL;

   ------------------------
   -- Get_All_But_SYSCLK --
   ------------------------

   procedure Get_All_But_SYSCLK (This : in out RCC_System_Clocks) is

      HPRE : constant AHB_Prescalers :=
         AHB_Prescalers'Enum_Val (RCC_Periph.CFGR.HPRE);

      HPRE_Div : constant Divisor := HPRE_Prescaler_Divisors (HPRE);

      PPRE1 : constant APB_Prescalers :=
         APB_Prescalers'Enum_Val (RCC_Periph.CFGR.PPRE.Arr (1));

      PPRE1_Div : constant Divisor := PPRE_Prescaler_Divisors (PPRE1);

      PPRE2 : constant APB_Prescalers :=
         APB_Prescalers'Enum_Val (RCC_Periph.CFGR.PPRE.Arr (2));

      PPRE2_Div : constant Divisor := PPRE_Prescaler_Divisors (PPRE2);

   begin
      This.HCLK  := This.SYSCLK / HPRE_Div;
      This.PCLK1 := This.HCLK / PPRE1_Div;
      This.PCLK2 := This.HCLK / PPRE2_Div;

      if PPRE1 = RCC_HCLK_DIV1 then
         This.TIMCLK1 := This.PCLK1;
      else
         This.TIMCLK1 := This.PCLK1 * 2;
      end if;

      if PPRE2 = RCC_HCLK_DIV1 then
         This.TIMCLK2 := This.PCLK2;
      else
         This.TIMCLK2 := This.PCLK2 * 2;
      end if;
   end Get_All_But_SYSCLK;

end System.STM32;
