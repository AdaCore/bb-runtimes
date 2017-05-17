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

--  This file provides register definitions for the STM32Fx (ARM Cortex M4/7)
--  microcontrollers from ST Microelectronics.

with Interfaces.STM32;

package System.STM32 is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.STM32);

   subtype Frequency is Interfaces.STM32.UInt32;

   type RCC_System_Clocks is record
      SYSCLK  : Frequency;
      HCLK    : Frequency;
      PCLK1   : Frequency;
      PCLK2   : Frequency;
      TIMCLK1 : Frequency;
      TIMCLK2 : Frequency;
   end record;

   function System_Clocks return RCC_System_Clocks;

   --  MODER constants
   subtype GPIO_MODER_Values is Interfaces.STM32.UInt2;
   Mode_IN  : constant GPIO_MODER_Values := 0;
   Mode_OUT : constant GPIO_MODER_Values := 1;
   Mode_AF  : constant GPIO_MODER_Values := 2;
   Mode_AN  : constant GPIO_MODER_Values := 3;

   --  OTYPER constants
   subtype GPIO_OTYPER_Values is Interfaces.STM32.Bit;
   Push_Pull  : constant GPIO_OTYPER_Values := 0;
   Open_Drain : constant GPIO_OTYPER_Values := 1;

   --  OSPEEDR constants
   subtype GPIO_OSPEEDR_Values is Interfaces.STM32.UInt2;
   Speed_2MHz   : constant GPIO_OSPEEDR_Values := 0; -- Low speed
   Speed_25MHz  : constant GPIO_OSPEEDR_Values := 1; -- Medium speed
   Speed_50MHz  : constant GPIO_OSPEEDR_Values := 2; -- Fast speed
   Speed_100MHz : constant GPIO_OSPEEDR_Values := 3; -- High speed

   --  PUPDR constants
   subtype GPIO_PUPDR_Values is Interfaces.STM32.UInt2;
   No_Pull   : constant GPIO_PUPDR_Values := 0;
   Pull_Up   : constant GPIO_PUPDR_Values := 1;
   Pull_Down : constant GPIO_PUPDR_Values := 2;

   --  AFL constants
   AF_USART1  : constant Interfaces.STM32.UInt4 := 7;
   AF_USART6  : constant Interfaces.STM32.UInt4 := 8;

   type MCU_ID_Register is record
      DEV_ID   : Interfaces.STM32.UInt12;
      Reserved : Interfaces.STM32.UInt4;
      REV_ID   : Interfaces.STM32.UInt16;
   end record with Pack, Size => 32;

   --  RCC constants

   type PLL_Source is
     (PLL_SRC_HSI,
      PLL_SRC_HSE)
     with Size => 1;

   type SYSCLK_Source is
     (SYSCLK_SRC_HSI,
      SYSCLK_SRC_HSE,
      SYSCLK_SRC_PLL)
     with Size => 2;

   type AHB_Prescaler_Enum is
     (DIV2,  DIV4,   DIV8,   DIV16,
      DIV64, DIV128, DIV256, DIV512)
     with Size => 3;

   type AHB_Prescaler is record
      Enabled : Boolean := False;
      Value   : AHB_Prescaler_Enum := AHB_Prescaler_Enum'First;
   end record with Size => 4;

   for AHB_Prescaler use record
      Enabled at 0 range 3 .. 3;
      Value   at 0 range 0 .. 2;
   end record;

   AHBPRE_DIV1 : constant AHB_Prescaler := (Enabled => False, Value => DIV2);

   type APB_Prescaler_Enum is
     (DIV2,  DIV4,  DIV8,  DIV16)
     with Size => 2;

   type APB_Prescaler is record
      Enabled : Boolean;
      Value   : APB_Prescaler_Enum;
   end record with Size => 3;

   for APB_Prescaler use record
      Enabled at 0 range 2 .. 2;
      Value   at 0 range 0 .. 1;
   end record;

   type I2S_Clock_Selection is
     (I2SSEL_PLL,
      I2SSEL_CKIN)
     with Size => 1;

   type MC01_Clock_Selection is
     (MC01SEL_HSI,
      MC01SEL_LSE,
      MC01SEL_HSE,
      MC01SEL_PLL)
     with Size => 2;

   type MC02_Clock_Selection is
     (MC02SEL_SYSCLK,
      MC02SEL_PLLI2S,
      MC02SEL_HSE,
      MC02SEL_PLL)
     with Size => 2;

   type MC0x_Prescaler is
     (MC0xPRE_DIV1,
      MC0xPRE_DIV2,
      MC0xPRE_DIV3,
      MC0xPRE_DIV4,
      MC0xPRE_DIV5)
     with Size => 3;
   for MC0x_Prescaler use
     (MC0xPRE_DIV1 => 0,
      MC0xPRE_DIV2 => 2#100#,
      MC0xPRE_DIV3 => 2#101#,
      MC0xPRE_DIV4 => 2#110#,
      MC0xPRE_DIV5 => 2#111#);

   --  Constants for RCC CR register

   subtype PLLM_Range is Integer range 2 .. 63;
   subtype PLLN_Range is Integer range 50 .. 432;
   subtype PLLP_Range is Integer range 2 .. 8
     with Static_Predicate => (case PLLP_Range is
                                 when 2 | 4 | 6 | 8 => True,
                                 when others => False);
   subtype PLLQ_Range is Integer range 2 .. 15;

   subtype HSECLK_Range is Integer range   1_000_000 ..  26_000_000;
   subtype PLLIN_Range  is Integer range     950_000 ..   2_000_000;
   subtype PLLVC0_Range is Integer range 192_000_000 .. 432_000_000;
   subtype PLLOUT_Range is Integer range  24_000_000 .. 216_000_000;
   subtype SYSCLK_Range is Integer range           1 .. 216_000_000;
   subtype HCLK_Range   is Integer range           1 .. 216_000_000;
   subtype PCLK1_Range  is Integer range           1 ..  54_000_000;
   subtype PCLK2_Range  is Integer range           1 .. 108_000_000;
   subtype SPII2S_Range is Integer range           1 ..  37_500_000;
   pragma Unreferenced (SPII2S_Range);

   --  These internal low and high speed clocks are fixed (do not modify)

   HSICLK : constant := 16_000_000;
   LSICLK : constant :=     32_000;

   MCU_ID : MCU_ID_Register with Volatile,
                                 Address => System'To_Address (16#E004_2000#);
   --  Only 32-bits access supported (read-only)

   DEV_ID_STM32F40xxx : constant := 16#413#; --  STM32F40xxx/41xxx
   DEV_ID_STM32F42xxx : constant := 16#419#; --  STM32F42xxx/43xxx
   DEV_ID_STM32F46xxx : constant := 16#434#; --  STM32F469xx/479xx
   DEV_ID_STM32F74xxx : constant := 16#449#; --  STM32F74xxx/75xxx

end System.STM32;
