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

pragma Ada_2012; -- To work around pre-commit check?
pragma Restrictions (No_Elaboration_Code);
pragma Suppress (All_Checks);

--  This initialization procedure mainly initializes the PLLs and
--  all derived clocks.

with System.STM32F4;       use System.STM32F4;
with System.STM32F4.RCC;   use System.STM32F4.RCC;
with System.BB.Parameters; use System.BB.Parameters;

procedure Setup_Pll is

   package RCC renames System.STM32F4.RCC;

   function "and" (Left, Right : Word) return Boolean is
     ((Left and Right) /= 0);

   procedure Initialize_Clocks;
   procedure Reset_Clocks;

   ------------------------------
   -- Clock Tree Configuration --
   ------------------------------

   HSE_Enabled     : constant Boolean := True;  -- use high-speed ext. clock
   HSE_Bypass      : constant Boolean := False; -- don't bypass ext. resonator
   LSI_Enabled     : constant Boolean := True;  -- use low-speed internal clock

   Activate_PLL    : constant Boolean := True;
   Activate_PLLI2S : constant Boolean := False;

   pragma Assert ((if Activate_PLL then HSE_Enabled),
                  "PLL only supported with external clock");

   pragma Assert (not Activate_PLLI2S, "not yet implemented");

   -----------------------
   -- Initialize_Clocks --
   -----------------------

   procedure Initialize_Clocks
   is
      MCU_ID_Cp   : constant MCU_ID_Register := MCU_ID;
      HSECLK      : constant Integer := Integer (HSE_Clock (MCU_ID_Cp.DEV_ID));

      -------------------------------
      -- Compute Clock Frequencies --
      -------------------------------

      PLLP_Value  : constant RCC.PLLP_Range := 2;
      --  Arbitrary fixed to a convenient value

      PLLCLKIN    : constant Integer := 1_000_000;
      PLLM_Value  : constant Integer  := HSECLK / PLLCLKIN;
      --  First divider M is set to produce a 1Mhz clock

      PLLN_Value  : constant Integer :=
                      (PLLP_Value * Clock_Frequency) / PLLCLKIN;
      --  Compute N to to generate the required frequency

      PLLVC0      : constant Integer := PLLCLKIN * PLLN_Value;
      PLLCLKOUT   : constant Integer := PLLVC0 / PLLP_Value;

      PLLQ_Value  : constant RCC.PLLQ_Range := 7;
      --  Arbitrary fixed

      PLLM        : constant UInt6 := UInt6 (PLLM_Value);
      PLLN        : constant UInt9 := UInt9 (PLLN_Value);
      PLLP        : constant UInt2 := UInt2 (PLLP_Value / 2 - 1);
      PLLQ        : constant UInt4 := UInt4 (PLLQ_Value);

      SW          : constant SYSCLK_Source :=
                      (if Activate_PLL
                       then SYSCLK_SRC_PLL
                       else SYSCLK_SRC_HSI);

      SYSCLK      : constant Integer := (if Activate_PLL
                                         then PLLCLKOUT
                                         else RCC.HSICLK);

      HCLK        : constant Integer :=
                      (if not AHB_PRE.Enabled
                       then SYSCLK
                       else
                         (case AHB_PRE.Value is
                             when RCC.DIV2   => SYSCLK / 2,
                             when RCC.DIV4   => SYSCLK / 4,
                             when RCC.DIV8   => SYSCLK / 8,
                             when RCC.DIV16  => SYSCLK / 16,
                             when RCC.DIV64  => SYSCLK / 64,
                             when RCC.DIV128 => SYSCLK / 128,
                             when RCC.DIV256 => SYSCLK / 256,
                             when RCC.DIV512 => SYSCLK / 512));
      PCLK1       : constant Integer :=
                      (if not APB1_PRE.Enabled
                       then HCLK
                       else
                         (case APB1_PRE.Value is
                             when RCC.DIV2  => HCLK / 2,
                             when RCC.DIV4  => HCLK / 4,
                             when RCC.DIV8  => HCLK / 8,
                             when RCC.DIV16 => HCLK / 16));
      PCLK2       : constant Integer :=
                      (if not APB2_PRE.Enabled
                       then HCLK
                       else
                         (case APB2_PRE.Value is
                             when RCC.DIV2  => HCLK / 2,
                             when RCC.DIV4  => HCLK / 4,
                             when RCC.DIV8  => HCLK / 8,
                             when RCC.DIV16 => HCLK / 16));

   begin

      --  Check configuration
      pragma Warnings (Off, "condition is always False");
      if PLLVC0 not in RCC.PLLVC0_Range
        or else
          PLLCLKOUT not in RCC.PLLOUT_Range
      then
         raise Program_Error with "Invalid clock configuration";
      end if;

      if SYSCLK /= Clock_Frequency then
         raise Program_Error with "Cannot generate requested clock";
      end if;

      if HCLK not in RCC.HCLK_Range
        or else
         PCLK1 not in RCC.PCLK1_Range
        or else
         PCLK2 not in RCC.PCLK2_Range
      then
         raise Program_Error with "Invalid AHB/APB prescalers configuration";
      end if;
      pragma Warnings (On, "condition is always False");

      --  PWR clock enable
      --  Reset the power interface

      RCC.Registers.APB1ENR := RCC.RCC_APB1ENR_PWR;

      --  PWR initialization
      --  Select higher supply power for stable operation at max. freq.
      --  (See the Symbol V12 line in table 14 of the STM32F407xx datasheet,
      --  and table 15 p79). On the stm32f4 discovery board, VDD is 3V.
      --  Voltage supply scaling only

      if MCU_ID_Cp.DEV_ID = DEV_ID_STM32F40xxx then
         PWR.CR := PWR_CR_VOS_HIGH_407;
      elsif MCU_ID_Cp.DEV_ID = DEV_ID_STM32F42xxx
        or else MCU_ID_Cp.DEV_ID = DEV_ID_STM32F46xxx
        or else MCU_ID_Cp.DEV_ID = DEV_ID_STM32F74xxx
      then
         PWR.CR := PWR_CR_VOS_HIGH_429;
      end if;

      --  Setup internal clock and wait for HSI stabilisation.
      --  The internal high speed clock is always enabled, because it is the
      --  fallback clock when the PLL fails.

      RCC.Registers.CR.HSION := True;

      loop
         exit when RCC.Registers.CR.HSIRDY;
      end loop;

      --  Configure high-speed external clock, if enabled

      if HSE_Enabled then
         RCC.Registers.CR.HSEON := True;
         RCC.Registers.CR.HSEBYP := HSE_Bypass;

         loop
            exit when RCC.Registers.CR.HSERDY;
         end loop;
      end if;

      --  Configure low-speed internal clock if enabled

      if LSI_Enabled then
         RCC.Registers.CSR := RCC.Registers.CSR or RCC.CSR.LSION;

         loop
            exit when RCC.Registers.CSR and RCC.CSR.LSIRDY;
         end loop;
      end if;

      --  Activate PLL if enabled

      if Activate_PLL then
         RCC.Registers.PLLCFGR :=
           (PLLM   => PLLM,
            PLLN   => PLLN,
            PLLP   => PLLP,
            PLLQ   => PLLQ,
            PLLSRC => PLL_SRC_HSE,
            others => <>);

         RCC.Registers.CR.PLLON := True;
         loop
            exit when RCC.Registers.CR.PLLRDY;
         end loop;
      end if;

      --  Configure flash
      --  Must be done before increasing the frequency, otherwise the CPU
      --  won't be able to fetch new instructions.

      FLASH.ACR := FLASH_ACR.LATENCY_5WS or FLASH_ACR.ICEN or FLASH_ACR.DCEN
        or FLASH_ACR.PRFTEN;

      --  Configure derived clocks

      RCC.Registers.CFGR :=
        (SW      => SW,
         HPRE    => AHB_PRE,
         PPRE1   => APB1_PRE,
         PPRE2   => APB2_PRE,
         RTCPRE  => 16#0#,
         I2SSRC  => I2SSEL_PLL,
         MCO1    => MC01SEL_HSI,
         MCO1PRE => MC0xPRE_DIV1,
         MCO2    => MC02SEL_SYSCLK,
         MCO2PRE => MC0xPRE_DIV5,
         others  => <>);

      if Activate_PLL then
         loop
            exit when RCC.Registers.CFGR.SWS = SYSCLK_SRC_PLL;
         end loop;

         --  Wait until voltage supply scaling has completed

         loop
            exit when PWR.CSR and PWR_CSR_VOSRDY;
         end loop;
      end if;

   end Initialize_Clocks;

   ------------------
   -- Reset_Clocks --
   ------------------

   procedure Reset_Clocks is
   begin
      --  Switch on high speed internal clock
      RCC.Registers.CR.HSION := True;

      --  Reset CFGR regiser
      RCC.Registers.CFGR := (others => <>);

      --  Reset HSEON, CSSON and PLLON bits
      RCC.Registers.CR.HSEON := False;
      RCC.Registers.CR.CSSON := False;
      RCC.Registers.CR.PLLON := False;

      --  Reset PLL configuration register
      RCC.Registers.PLLCFGR := (others => <>);

      --  Reset HSE bypass bit
      RCC.Registers.CR.HSEBYP := False;

      --  Disable all interrupts
      RCC.Registers.CIR := 0;
   end Reset_Clocks;

begin
   Reset_Clocks;
   Initialize_Clocks;
end Setup_Pll;
