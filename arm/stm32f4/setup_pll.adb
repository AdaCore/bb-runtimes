------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2014, Free Software Foundation, Inc.         --
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
--  all derived clocks. For now it also initializes the first USART.
--  To be moved to s-textio, but needs clock info ???

with System.STM32F4; use System.STM32F4;
with System.BB.Parameters; use System.BB.Parameters;

procedure Setup_Pll is
   --  Local Subprograms

   function "and" (Left, Right : Word) return Boolean is
     ((Left and Right) /= 0);

   procedure Reset (Register : in out Word; Mask : Word);
   procedure Set (Register : in out Word; Mask : Word);

   procedure Initialize_Clocks;
   procedure Reset_Clocks;

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
      MCU_ID_Cp : constant MCU_ID_Register := MCU_ID;
   begin
      --  PWR clock enable
      --  Reset the power interface

      RCC.APB1ENR := RCC_APB1ENR_PWR;

      --  PWR initialization
      --  Select higher supply power for stable operation at max. freq.
      --  (See the Symbol V12 line in table 14 of the STM32F407xx datasheet,
      --  and table 15 p79). On the stm32f4 discovery board, VDD is 3V.
      --  Voltage supply scaling only

      if MCU_ID_Cp.DEV_ID = DEV_ID_STM32F407xx then
         PWR.CR := PWR_CR_VOS_HIGH_407;
      elsif MCU_ID_Cp.DEV_ID = DEV_ID_STM32F42xxx then
         PWR.CR := PWR_CR_VOS_HIGH_429;
      end if;

      --  Setup internal clock and wait for HSI stabilisation.
      --  The internal high speed clock is always enabled, because it is the
      --  fallback clock when the PLL fails.

      RCC.CR := RCC.CR or RCC_CR.HSION;

      loop
         exit when RCC.CR and RCC_CR.HSIRDY;
      end loop;

      --  Configure high-speed external clock, if enabled

      if Boolean'Val (HSE_Enabled) then
         RCC.CR := RCC.CR or RCC_CR.HSEON
           or (if Boolean'Val (HSE_Bypass) then RCC_CR.HSEBYP else 0);

         loop
            exit when RCC.CR and RCC_CR.HSERDY;
         end loop;
      end if;

      --  Configure low-speed internal clock if enabled

      if Boolean'Val (LSI_Enabled) then
         RCC.CSR := RCC.CSR or RCC_CSR.LSION;

         loop
            exit when RCC.CSR and RCC_CSR.LSIRDY;
         end loop;
      end if;

      --  Activate PLL if enabled

      if Boolean'Val (Activate_PLL) then
         RCC.PLLCFGR := PLLQ or PLLSRC_HSE or PLLP or PLLN or PLLM;
         Set (RCC.CR, RCC_CR.PLLON);

         loop
            exit when RCC.CR and RCC_CR.PLLRDY;
         end loop;
      end if;

      --  Configure flash
      --  Must be done before increasing the frequency, otherwise the CPU
      --  won't be able to fetch new instructions.

      FLASH.ACR := FLASH_ACR.LATENCY_5WS or FLASH_ACR.ICEN or FLASH_ACR.DCEN
        or FLASH_ACR.PRFTEN;

      --  Configure derived clocks

      RCC.CFGR :=
        --  AHB prescaler is 1, APB1 uses 4 and APB2 prescaler is 2
        HPRE or PPRE1 or PPRE2 or
        --  Configure MC01 pin to have the HSI (high speed internal clock)
        RCC_CFGR.MCO1PRE_DIV1 or RCC_CFGR.MCO1SEL_HSI or
        --  Configure MCO2 pin to have SYSCLK / 5
        RCC_CFGR.MCO2PRE_DIV5 or RCC_CFGR.MCO2SEL_SYSCLK or
        --  Select system clock source
        SW;

      if Boolean'Val (Activate_PLL) then
         loop
            exit when (RCC.CFGR and (RCC_CFGR.SWS_HSE or RCC_CFGR.SWS_PLL))
              = RCC_CFGR.SWS_PLL;
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
      Set (RCC.CR, RCC_CR.HSION);

      --  Reset CFGR regiser
      RCC.CFGR := 0;

      --  Reset HSEON, CSSON and PLLON bits
      Reset (RCC.CR, RCC_CR.HSEON or RCC_CR.CSSON or RCC_CR.PLLON);

      --  Reset PLL configuration register
      RCC.PLLCFGR := 16#2400_3010#;

      --  Reset HSE bypass bit
      Reset (RCC.CR, RCC_CR.HSEBYP);

      --  Disable all interrupts
      RCC.CIR := 0;
   end Reset_Clocks;
begin
   Reset_Clocks;
   Initialize_Clocks;
end Setup_Pll;
