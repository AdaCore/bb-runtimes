------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2015, Free Software Foundation, Inc.         --
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

--  This initialization procedure mainly initializes the PLLs and
--  all derived clocks.

with System.STM32F4; use System.STM32F4;
with System.STM32F4.RCC;
with System.BB.Parameters; use System.BB.Parameters;

procedure Setup_Pll is

   package RCC renames System.STM32F4.RCC;

   function "and" (Left, Right : Word) return Boolean is
     ((Left and Right) /= 0);

   procedure Reset (Register : in out Word; Mask : Word);
   procedure Set (Register : in out Word; Mask : Word);

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

   -----------
   -- Reset --
   -----------

   procedure Reset (Register : in out Word; Mask : Word) is
   begin
      Register := Register and not Mask;
   end Reset;

   ---------
   -- Set --
   ---------

   procedure Set (Register : in out Word; Mask : Word) is
   begin
      Register := Register or Mask;
   end Set;

   -----------------------
   -- Initialize_Clocks --
   -----------------------

   procedure Initialize_Clocks is

      HSECLK    : constant Integer := Integer (HSE_Clock (MCU_ID.DEV_ID));
      MCU_ID_Cp : constant MCU_ID_Register := MCU_ID;

      -------------------------------
      -- Compute Clock Frequencies --
      -------------------------------

      PLLP_Value  : constant RCC.PLLP_Range := 2;
      --  Arbitrary fixed to a convenient value

      PLLM_Value : constant Integer  := HSECLK / 1_000_000;
      PLLCLKIN   : constant Integer := HSECLK / PLLM_Value;
      --  First divider M is set to produce a 1Mhz clock

      PLLN_Value  : constant Integer :=
        (PLLP_Value * Clock_Frequency) / PLLCLKIN;
      --  Compute N to to generate the required frequency

      PLLVC0      : constant Integer := PLLCLKIN * PLLN_Value;
      PLLCLKOUT   : constant Integer := PLLVC0 / PLLP_Value;

      PLLQ_Value  : constant RCC.PLLQ_Range := 7;
      --  Arbitrary fixed

      PLLM     : constant Word := Word (PLLM_Value);
      PLLN     : constant Word := Word (PLLN_Value * 2**6);
      PLLP     : constant Word := Word ((PLLP_Value / 2 - 1) * 2**16);
      PLLQ     : constant Word := Word (PLLQ_Value * 2**24);

      SW       : constant := (if Activate_PLL then RCC.CFGR.SW_PLL
                              else RCC.CFGR.SW_HSI);

      SYSCLK   : constant Integer := (if Activate_PLL then PLLCLKOUT
                                      else RCC.HSICLK);

      HCLK     : constant Integer :=
        (case AHB_PRE is
            when RCC.CFGR.AHBPRE_DIV1   => SYSCLK / 1,
            when RCC.CFGR.AHBPRE_DIV2   => SYSCLK / 2,
            when RCC.CFGR.AHBPRE_DIV4   => SYSCLK / 4,
            when RCC.CFGR.AHBPRE_DIV8   => SYSCLK / 8,
            when RCC.CFGR.AHBPRE_DIV16  => SYSCLK / 16,
            when RCC.CFGR.AHBPRE_DIV64  => SYSCLK / 64,
            when RCC.CFGR.AHBPRE_DIV128 => SYSCLK / 128,
            when RCC.CFGR.AHBPRE_DIV256 => SYSCLK / 256,
            when RCC.CFGR.AHBPRE_DIV512 => SYSCLK / 512);

      PCLK1    : constant Integer :=
        (case APB1_PRE is
            when RCC.CFGR.APB1PRE_DIV1  => HCLK / 1,
            when RCC.CFGR.APB1PRE_DIV2  => HCLK / 2,
            when RCC.CFGR.APB1PRE_DIV4  => HCLK / 4,
            when RCC.CFGR.APB1PRE_DIV8  => HCLK / 8,
            when RCC.CFGR.APB1PRE_DIV16 => HCLK / 16);

      PCLK2    : constant Integer :=
        (case APB2_PRE is
            when RCC.CFGR.APB2PRE_DIV1  => HCLK / 1,
            when RCC.CFGR.APB2PRE_DIV2  => HCLK / 2,
            when RCC.CFGR.APB2PRE_DIV4  => HCLK / 4,
            when RCC.CFGR.APB2PRE_DIV8  => HCLK / 8,
            when RCC.CFGR.APB2PRE_DIV16 => HCLK / 16);

      AHB_PRE_Rep : constant Word :=
        Word (RCC.CFGR.AHB_PRE_Value'Enum_Rep (AHB_PRE));
      APB1_PRE_Rep : constant Word :=
        Word (RCC.CFGR.APB1_PRE_Value'Enum_Rep (APB1_PRE));
      APB2_PRE_Rep : constant Word :=
        Word (RCC.CFGR.APB2_PRE_Value'Enum_Rep (APB2_PRE));
   begin

      --  Check configuration
      if PLLCLKIN not in RCC.PLLIN_Range
        or else
          PLLVC0 not in RCC.PLLVC0_Range
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
      elsif MCU_ID_Cp.DEV_ID = DEV_ID_STM32F42xxx then
         PWR.CR := PWR_CR_VOS_HIGH_429;
      end if;

      --  Setup internal clock and wait for HSI stabilisation.
      --  The internal high speed clock is always enabled, because it is the
      --  fallback clock when the PLL fails.

      RCC.Registers.CR := RCC.Registers.CR or RCC.CR.HSION;

      loop
         exit when RCC.Registers.CR and RCC.CR.HSIRDY;
      end loop;

      --  Configure high-speed external clock, if enabled

      if HSE_Enabled then
         RCC.Registers.CR := RCC.Registers.CR or RCC.CR.HSEON
           or (if HSE_Bypass then RCC.CR.HSEBYP else 0);

         loop
            exit when RCC.Registers.CR and RCC.CR.HSERDY;
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
         RCC.Registers.PLLCFGR := RCC.PLLSRC_HSE or
           PLLQ or PLLP or PLLN or PLLM;

         Set (RCC.Registers.CR, RCC.CR.PLLON);
         loop
            exit when RCC.Registers.CR and RCC.CR.PLLRDY;
         end loop;
      end if;

      --  Configure flash
      --  Must be done before increasing the frequency, otherwise the CPU
      --  won't be able to fetch new instructions.

      FLASH.ACR := FLASH_ACR.LATENCY_5WS or FLASH_ACR.ICEN or FLASH_ACR.DCEN
        or FLASH_ACR.PRFTEN;

      --  Configure derived clocks

      RCC.Registers.CFGR :=
        --  AHB prescaler is 1, APB1 uses 4 and APB2 prescaler is 2
        AHB_PRE_Rep or APB1_PRE_Rep or APB2_PRE_Rep or
        --  Configure MC01 pin to have the HSI (high speed internal clock)
        RCC.CFGR.MCO1PRE_DIV1 or RCC.CFGR.MCO1SEL_HSI or
        --  Configure MCO2 pin to have SYSCLK / 5
        RCC.CFGR.MCO2PRE_DIV5 or RCC.CFGR.MCO2SEL_SYSCLK or
        --  Select system clock source
        SW;

      if Activate_PLL then
         loop
            exit when (RCC.Registers.CFGR and
                         (RCC.CFGR.SWS_HSE or RCC.CFGR.SWS_PLL))
              = RCC.CFGR.SWS_PLL;
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
      Set (RCC.Registers.CR, RCC.CR.HSION);

      --  Reset CFGR regiser
      RCC.Registers.CFGR := 0;

      --  Reset HSEON, CSSON and PLLON bits
      Reset (RCC.Registers.CR,
             RCC.CR.HSEON or RCC.CR.CSSON or RCC.CR.PLLON);

      --  Reset PLL configuration register
      RCC.Registers.PLLCFGR := 16#2400_3010#;

      --  Reset HSE bypass bit
      Reset (RCC.Registers.CR, RCC.CR.HSEBYP);

      --  Disable all interrupts
      RCC.Registers.CIR := 0;
   end Reset_Clocks;

begin
   Reset_Clocks;
   Initialize_Clocks;
end Setup_Pll;
