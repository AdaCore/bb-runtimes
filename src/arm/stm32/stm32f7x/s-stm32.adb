------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2016, Free Software Foundation, Inc.         --
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

with System.BB.Parameters;

with Interfaces;            use Interfaces;
with Interfaces.STM32;      use Interfaces.STM32;
with Interfaces.STM32.RCC;  use Interfaces.STM32.RCC;

package body System.STM32 is

   package Param renames System.BB.Parameters;

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
            Result.SYSCLK := Param.HSI_Clock;

         --  HSE as source

         when SYSCLK_SRC_HSE =>
            Result.SYSCLK := Param.HSE_Clock;

         --  PLL as source

         when SYSCLK_SRC_PLL =>
            declare
               Pllm   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLM);
               Plln   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLN);
               Pllp   : constant UInt32 := UInt32 (RCC_Periph.PLLCFGR.PLLP);
               Pllvco : UInt32;

            begin
               case PLL_Source'Val (RCC_Periph.PLLCFGR.PLLSRC) is
                  when PLL_SRC_HSE =>
                     Pllvco := (Param.HSE_Clock / Pllm) * Plln;
                  when PLL_SRC_HSI =>
                     Pllvco := (Param.HSI_Clock / Pllm) * Plln;
               end case;

               Result.SYSCLK := Pllvco / ((Pllp + 1) * 2);
            end;
      end case;

      declare
         function To_AHBP is new Ada.Unchecked_Conversion
           (CFGR_HPRE_Field, AHB_Prescaler);
         function To_APBP is new Ada.Unchecked_Conversion
           (CFGR_PPRE_Element, APB_Prescaler);

         HPRE      : constant AHB_Prescaler := To_AHBP (RCC_Periph.CFGR.HPRE);
         HPRE_Div  : constant UInt32 := (if HPRE.Enabled
                                         then HPRE_Presc_Table (HPRE.Value)
                                         else 1);
         PPRE1     : constant APB_Prescaler :=
                      To_APBP (RCC_Periph.CFGR.PPRE.Arr (1));
         PPRE1_Div : constant UInt32 := (if PPRE1.Enabled
                                         then PPRE_Presc_Table (PPRE1.Value)
                                         else 1);
         PPRE2     : constant APB_Prescaler :=
                      To_APBP (RCC_Periph.CFGR.PPRE.Arr (2));
         PPRE2_Div : constant UInt32 := (if PPRE2.Enabled
                                         then PPRE_Presc_Table (PPRE2.Value)
                                         else 1);

      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Div;
         Result.PCLK1 := Result.HCLK / PPRE1_Div;
         Result.PCLK2 := Result.HCLK / PPRE2_Div;

         --  Timer clocks
         --  See Dedicated clock cfg register documentation.
         if RCC_Periph.DKCFGR1.TIMPRE = 0 then
            --  Mode 0:
            --  If the APB prescaler (PPRE1, PPRE2 in the RCC_CFGR register)
            --  is configured to a division factor of 1, TIMxCLK = PCLKx.
            --  Otherwise, the timer clock frequencies are set to twice to the
            --  frequency of the APB domain to which the timers are connected :
            --  TIMxCLK = 2xPCLKx.
            if not PPRE1.Enabled then
               Result.TIMCLK1 := Result.PCLK1;
            else
               Result.TIMCLK1 := Result.PCLK1 * 2;
            end if;

            if not PPRE2.Enabled then
               Result.TIMCLK2 := Result.PCLK2;
            else
               Result.TIMCLK2 := Result.PCLK2 * 2;
            end if;

         else
            --  Mode 1:
            --  If the APB prescaler (PPRE1, PPRE2 in the RCC_CFGR register) is
            --  configured to a division factor of 1, 2 or 4, TIMxCLK = HCLK.
            --  Otherwise, the timer clock frequencies are set to four times
            --  to the frequency of the APB domain to which the timers are
            --  connected : TIMxCLK = 4xPCLKx.
            if PPRE1_Div in 1 .. 4 then
               Result.TIMCLK1 := Result.HCLK;
            else
               Result.TIMCLK1 := Result.PCLK1 * 4;
            end if;
            if PPRE2_Div in 1 .. 4 then
               Result.TIMCLK2 := Result.HCLK;
            else
               Result.TIMCLK2 := Result.PCLK1 * 4;
            end if;
         end if;
      end;

      return Result;
   end System_Clocks;

end System.STM32;
