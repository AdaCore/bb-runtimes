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

--  This procedure is tailored for the STM32L5xx boards. It configures the
--  system for the maximum clock frequency of 110MHz.

pragma Suppress (All_Checks);
--  The procedure is called before the Ada runtime is initialized, so suppress
--  any runtime checks.

with Interfaces.STM32;           use Interfaces, Interfaces.STM32;
with Interfaces.STM32.Flash;     use Interfaces.STM32.Flash;
with Interfaces.STM32.RCC;       use Interfaces.STM32.RCC;
with Interfaces.STM32.PWR;       use Interfaces.STM32.PWR;

with System.BB.MCU_Parameters;
with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.STM32;               use System.STM32;

procedure Setup_PLL is

   --  PLL parameters. STMCubeMX can be used to calculate these parameters for
   --  a given clock source and the desired output frequency. Here the PLLCLK
   --  and PPLQ is configured to 110 MHz while PLLP is configured 31.4 MHz

   PLLM : constant := 12;
   PLLN : constant := 55;
   PLLP : constant := 7;
   PLLQ : constant := 2;
   PLLR : constant := 2;

   SysClock_From_PLL  : constant := SYSCLK_Source'Enum_Rep (SYSCLK_SRC_PLL);
   RCC_PLL_Source_MSI : constant UInt2 := PLL_Source'Enum_Rep (PLL_SRC_MSI);

   procedure Configure_RCC_Clocks;
   --  Initialize the HCLK, SYSCLK, AHB and APB bus clocks. SYSCLK source is
   --  the PLL. AHB and APB bus clocks are at max speed.

   procedure Reset_Clocks;

   procedure Enable_PWR_Clock;

   procedure Disable_Backup_Domain_Protection;

   procedure Configure_RCC_LSE_Drive_Low;
   --  Set the External Low Speed oscillator (LSE) drive to Low

   procedure Enable_LSE;
   --  When configured in PLL-mode, the MSI automatically calibrates itself
   --  via the LSE to better than +/- 0.25% accuracy. We enable the LSE
   --  accordingly. See RM0438 Rev 6 pg 333/2194, section 9.3.3 "MSI
   --  clock" LSESYSEN is disabled.

   procedure Configure_HSI;
   --  Enable the HSI and apply the calibration default

   procedure Configure_MSI_To_Max_Speed;
   --  Set up the multispeed internal oscillator to 48MHz so that it can feed
   --  the main PLL to run the system at the maximum speed of 110 MHz. Sets the
   --  FLASH latency from the contant declared in BB.Board_Parameters.

   procedure Configure_PLL_From_MSI;
   --  Set up the PLL driven by the MSI internal oscillator to run the system
   --  at the maximum speed of 110 MHz

   procedure Enable_MSI_PLL_Mode;
   --  Enable MSI Auto calibration

   procedure Select_Output_Voltage_Scale0;
   --  Configure the main internal regulator output voltage for high
   --  performance

   procedure Configure_SYSCLK_From_PLL;
   --  Set the system clock source to the PLL

   procedure Configure_PCLK1_PCLK2;
   --  Configure both AHB and APB clocks to run at max speed

   procedure Await_PLL_Configuration_Complete;

   procedure Await_Voltage_Supply_Scaling_Complete;

   --------------------------
   -- Configure_RCC_Clocks --
   --------------------------

   procedure Configure_RCC_Clocks is
   begin
      --  To correctly read data from FLASH memory, the number of wait states
      --  (LATENCY) must be correctly programmed according to the frequency of
      --  the CPU clock (HCLK) and the supply voltage of the device.

      --    Increase the number of wait states if higher CPU frequency

      if FLASH_Latency > FLASH_Periph.ACR.LATENCY then
         FLASH_Periph.ACR.LATENCY := FLASH_Latency;
         pragma Assert (FLASH_Periph.ACR.LATENCY = FLASH_Latency);
      end if;

      --  The PLL must be ready before we can configure the system clock

      if not RCC_Periph.CR.PLLRDY then
         raise Program_Error;
      end if;

      if Computed_SYSCLK_From_PLL > 80_000_000 then
         --  Transition state management is required when selecting the PLL as
         --  SYSCLK source with a target frequency above 80Mhz. See RM0438 Rev
         --  6, pg 336/2194, section 9.3.9 "System clock (SYSCLK) selection".

         RCC_Periph.CFGR.HPRE := AHB_Prescalers'Enum_Rep (RCC_SYSCLK_DIV2);
      end if;

      Configure_SYSCLK_From_PLL;

      --  Configure HCLK prescalar for max speed (110MHz)

      RCC_Periph.CFGR.HPRE := AHB_Prescalers'Enum_Rep (RCC_SYSCLK_DIV1);

      --  Note that Configure_HCLK happens to set the HPRE back to
      --  RCC_SYSCLK_DIV1 in this configuration, thus ending the
      --  state transition management section

      Configure_PCLK1_PCLK2;
   end Configure_RCC_Clocks;

   -------------------------------
   -- Configure_SYSCLK_From_PLL --
   -------------------------------

   procedure Configure_SYSCLK_From_PLL is
   begin
      RCC_Periph.CFGR.SW := SysClock_From_PLL;

      --  Wait for clock source to be as requested

      loop
         exit when RCC_Periph.CFGR.SWS = SysClock_From_PLL;
      end loop;
   end Configure_SYSCLK_From_PLL;

   ---------------------------
   -- Configure_PCLK1_PCLK2 --
   ---------------------------

   procedure Configure_PCLK1_PCLK2 is
   begin
      --  In this specific BSP configuration, both APB1CLKDivider and
      --  APB2CLKDivider are RCC_HCLK_DIV1.

      RCC_Periph.CFGR.PPRE.Arr (1) := APB_Prescalers'Enum_Rep (RCC_HCLK_DIV1);
      RCC_Periph.CFGR.PPRE.Arr (2) := APB_Prescalers'Enum_Rep (RCC_HCLK_DIV1);
   end Configure_PCLK1_PCLK2;

   ---------------------------------
   -- Configure_RCC_LSE_Drive_Low --
   ---------------------------------

   procedure Configure_RCC_LSE_Drive_Low is
      RCC_LSE_Drive_Low : constant UInt2 := 0;
   begin
      Disable_Backup_Domain_Protection;

      --  Configure the External Low Speed oscillator (LSE) drive

      RCC_Periph.BDCR.LSEDRV := RCC_LSE_Drive_Low;
   end Configure_RCC_LSE_Drive_Low;

   ----------------------------
   -- Configure_PLL_From_MSI --
   ----------------------------

   procedure Configure_PLL_From_MSI is
   begin
      --  see RM0438 Rev 6, pg 334/2194 , section 9.3.5 PLL for the steps
      --  required to configure the PLL

      --  Disable the main PLL before configuring it

      RCC_Periph.CR.PLLON := False;

      loop
         exit when not RCC_Periph.CR.PLLRDY;
      end loop;

      RCC_Periph.PLLCFGR :=
        (PLLM    => PLLM - 1,      -- handle the encoding
         PLLN    => PLLN,
         PLLPDIV => PLLP,
         PLLQ    => PLLQ / 2 - 1,  -- handle the encoding
         PLLR    => PLLR / 2 - 1,  -- handle the encoding
         PLLSRC  => RCC_PLL_Source_MSI,
         others  => <>);

      --  Enable the main PLL

      RCC_Periph.CR.PLLON := True;

      --  Enable PLL System Clock output

      RCC_Periph.PLLCFGR.PLLREN := True;

      --  Wait until the PLL is ready

      loop
         exit when RCC_Periph.CR.PLLRDY;
      end loop;
   end Configure_PLL_From_MSI;

   -------------------------
   -- Enable_MSI_PLL_Mode --
   -------------------------

   procedure Enable_MSI_PLL_Mode is
   begin
      --  MSIPLLEN must be enabled after LSE is enabled (LSEON enabled) and
      --  ready (LSERDY set by hardware). There is a hardware protection to
      --  avoid enabling MSIPLLEN if LSE is not ready. This bit is cleared by
      --  hardware when LSE is disabled (LSEON = 0) or when the Clock Security
      --  System on LSE detects a LSE failure (refer to RCC_CSR register in
      --  RM0438 Rev 6 pg 350/2194).

      if RCC_Periph.BDCR.LSECSSD -- failure detected on LSE
         or else not RCC_Periph.BDCR.LSEON
         or else not RCC_Periph.BDCR.LSERDY
      then
         raise Program_Error;
      end if;

      RCC_Periph.CR.MSIPLLEN := True;
   end Enable_MSI_PLL_Mode;

   --------------------------------
   -- Configure_MSI_To_Max_Speed --
   --------------------------------

   procedure Configure_MSI_To_Max_Speed is
      --  RM0438, section 9.3 "Clocks" says that the MSI is used as system
      --  clock source after startup from reset, configured at 4 MHz. We
      --  reconfigure it to 48MHz.

      MSI_Range_11 : constant := 2#1011#;
      --  48 MHz. See RM0438 Rev 6, page 349/2194

      MSI_Range_From_RCC_CR : constant Boolean := True;
      --  MSI Range is provided by MSIRANGE[3:0] in the RCC_CR register, as per
      --  RM0438 Rev 6 pg 349/2194. It can also be set by another register but
      --  not to 48MHz.

      RCC_MSI_Calibration_Default : constant Byte := 0;
   begin
      --  Note: Warning: MSIRANGE can be modified when MSI is OFF (MSION=0)
      --  or when MSI is ready (MSIRDY=1). MSIRANGE must NOT be modified when
      --  MSI is ON and NOT ready (MSION=1 and MSIRDY=0). We document the
      --  requirement with an assertion.

      pragma Assert (not RCC_Periph.CR.MSION or else RCC_Periph.CR.MSIRDY);

      --  To correctly read data from FLASH memory, the number of wait states
      --  (latency) must be correctly programmed according to the frequency of
      --  the CPU clock and the supply voltage of the device.
      --
      --  See RM0438 Rev 6, pg 180/2194, "Increasing the CPU frequency" for the
      --  steps required
      --
      --  We are executing at powerup, at which point the MSI clock is at 4MHz
      --  with zero wait states, and we are setting it to 48MHz.
      --
      --  Therefore, we first increase the number of wait states, if necessary:

      FLASH_Periph.ACR.LATENCY := FLASH_Latency;

      --  Select the Multiple Speed oscillator (MSI) clock range

      RCC_Periph.CR.MSIRGSEL := MSI_Range_From_RCC_CR;
      RCC_Periph.CR.MSIRANGE := MSI_Range_11;

      --  Finally adjust the MSI calibration value

      RCC_Periph.ICSCR.MSITRIM := RCC_MSI_Calibration_Default;

      --  Check that the new number of wait states is taken into account. We
      --  document the requirement with an assertion.

      pragma Assert (FLASH_Periph.ACR.LATENCY = FLASH_Latency);
   end Configure_MSI_To_Max_Speed;

   --------------------------------------
   -- Disable_Backup_Domain_Protection --
   --------------------------------------

   procedure Disable_Backup_Domain_Protection is
      --  Note that when the "Disable Backup domain write Protection" bit is
      --  set, access is enabled.
   begin
      PWR_Periph.CR1.DBP := True;

      --  Wait for protection to be disabled

      loop
         exit when PWR_Periph.CR1.DBP;
      end loop;
   end Disable_Backup_Domain_Protection;

   ----------------
   -- Enable_LSE --
   ----------------

   procedure Enable_LSE is
   begin
      if not PWR_Periph.CR1.DBP then
         Disable_Backup_Domain_Protection;
      end if;

      RCC_Periph.BDCR.LSEON := True;
      loop
         exit when RCC_Periph.BDCR.LSERDY;
      end loop;

      RCC_Periph.BDCR.LSESYSEN := False;

      --  Wait until LSESYSRDY is cleared

      loop
         exit when not RCC_Periph.BDCR.LSESYSRDY;
      end loop;
   end Enable_LSE;

   -------------------
   -- Configure_HSI --
   -------------------

   procedure Configure_HSI is
      RCC_HSICALIBRATION_DEFAULT : constant UInt7 := 40;
   begin
      --  Enable the Internal High Speed oscillator

      RCC_Periph.CR.HSION := True;

      --  Wait till HSI is ready

      loop
         exit when RCC_Periph.CR.HSIRDY;
      end loop;

      --  Adjust the Internal High Speed oscillator (HSI) calibration value

      RCC_Periph.ICSCR.HSITRIM := RCC_HSICALIBRATION_DEFAULT;
   end Configure_HSI;

   -------------------------------------------------
   -- Configure_Internal_Regulator_Output_Voltage --
   -------------------------------------------------

   procedure Select_Output_Voltage_Scale0 is
      PWR_REGULATOR_VOLTAGE_SCALE0 : constant UInt2 := 0;
      --  Allow 110MHz. At power-on reset or a system reset, the main regulator
      --  voltage Range 2 is selected by default. The system clock is limited
      --  to 110 MHz in Range 0 mode, 80 MHz in Range 1 mode, 26 MHz in Range
      --  2 mode, therefore we want Range 0.

   begin
      --  VOS shall not be changed in Low Power Mode, or if Low Power Mode is
      --  requested but not yet established.

      pragma Assert (PWR_Periph.SR1.SMPSHPRDY and  -- High-power mode ready
                     not PWR_Periph.CR4.SMPSLPEN); -- Low-power mode enabled

      PWR_Periph.CR1.VOS := PWR_REGULATOR_VOLTAGE_SCALE0;
      Await_VOSF_Cleared : declare
         At_Least_Once : Boolean := False;
      begin
         loop
            exit when At_Least_Once and then PWR_Periph.SR2.VOSF;
            At_Least_Once := True;
         end loop;
      end Await_VOSF_Cleared;
   end Select_Output_Voltage_Scale0;

   ------------------
   -- Reset_Clocks --
   ------------------

   procedure Reset_Clocks is
   begin
      --  Switch on high speed internal clock

      RCC_Periph.CR.HSION := True;

      --  Reset CFGR register

      RCC_Periph.CFGR := (others => <>);

      --  Reset HSEON, CSSON and PLLON bits

      RCC_Periph.CR.HSEON := False;
      RCC_Periph.CR.CSSON := False;
      RCC_Periph.CR.PLLON := False;

      --  Reset PLL configuration register

      RCC_Periph.PLLCFGR := (others => <>);

      --  Reset HSE bypass bit

      RCC_Periph.CR.HSEBYP := False;

      --  Disable all interrupts

      RCC_Periph.CIER := (others => <>);
   end Reset_Clocks;

   ----------------------
   -- Enable_PWR_Clock --
   ----------------------

   procedure Enable_PWR_Clock is
      Temp : Boolean
        with Volatile, Unreferenced; -- Ensure the compiler actually reads it
   begin
      RCC_Periph.APB1ENR1.PWREN := True;

      --  As per __HAL_RCC_PWR_CLK_ENABLE()

      Temp := RCC_Periph.APB1ENR1.PWREN;
   end Enable_PWR_Clock;

   --------------------------------------
   -- Await_PLL_Configuration_Complete --
   --------------------------------------

   procedure Await_PLL_Configuration_Complete is
   begin
      loop
         exit when RCC_Periph.CFGR.SWS = SysClock_From_PLL;
      end loop;
   end Await_PLL_Configuration_Complete;

   -------------------------------------------
   -- Await_Voltage_Supply_Scaling_Complete --
   -------------------------------------------

   procedure Await_Voltage_Supply_Scaling_Complete is
   begin
      loop
         exit when System.BB.MCU_Parameters.Is_PWR_Stabilized;
      end loop;
   end Await_Voltage_Supply_Scaling_Complete;

begin
   Reset_Clocks;
   Enable_PWR_Clock;

   --  Reset the power interface

   RCC_Periph.APB1RSTR1.PWRRST := True;
   RCC_Periph.APB1RSTR1.PWRRST := False;

   Select_Output_Voltage_Scale0;
   Configure_RCC_LSE_Drive_Low;

   --  Configure the MSI, HSI, LSE, and PLL. The PLL is clocked from MSI, which
   --  is configured to run at 48 MHz. The PLL outputs PLLCLK and PPLQ are
   --  configured to 110 MHz while PLLP is configured 31.4 MHz.

   Configure_MSI_To_Max_Speed;
   Configure_HSI;
   Enable_LSE;
   Configure_PLL_From_MSI;

   Configure_RCC_Clocks;
   Enable_MSI_PLL_Mode;
   Await_PLL_Configuration_Complete;
   Await_Voltage_Supply_Scaling_Complete;
end Setup_PLL;
