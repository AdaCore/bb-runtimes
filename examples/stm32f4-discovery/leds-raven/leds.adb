------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2013, Free Software Foundation, Inc.           --
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

pragma Warnings (Off);
with System.STM32F4; use System.STM32F4;
pragma Warnings (On);
with Ada.Real_Time; use Ada.Real_Time;

procedure Leds is
   --  Bit definitions for RCC AHB1ENR register
   RCC_AHB1ENR_GPIOD    : constant Word := 16#08#;
   RCC_AHB1ENR_GPIOA    : constant Word := 16#01#;

   GPIOD_Base           : constant := AHB1_Peripheral_Base + 16#0C00#;
   GPIOA_Base           : constant := AHB1_Peripheral_Base + 16#0000#;

   GPIOD : GPIO_Registers with Volatile,
                             Address => System'To_Address (GPIOD_Base);
   pragma Import (Ada, GPIOD);

   GPIOA : GPIO_Registers with Volatile,
                             Address => System'To_Address (GPIOA_Base);
   pragma Import (Ada, GPIOA);

   Period : constant Time_Span := Milliseconds (200);
   Next_Start : Time := Clock;

   type Idx_Type is mod 4;
   Idx : Idx_Type := 0;
   Masks : constant array (Idx_Type) of Word :=
     (16#1_000#, 16#2_000#, 16#4_000#, 16#8_000#);
   Clockwise : Boolean := True;
begin
   --  Enable clock for GPIO-D (leds) and GPIO-A (button)

   RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOD or RCC_AHB1ENR_GPIOA;

   --  Configure PD12-15 (leds) and PA0 (Button)
   declare
      use GPIO;
   begin
      GPIOD.MODER   (12 .. 15) := (others => Mode_OUT);
      GPIOD.OTYPER  (12 .. 15) := (others => Type_PP);
      GPIOD.OSPEEDR (12 .. 15) := (others => Speed_100MHz);
      GPIOD.PUPDR   (12 .. 15) := (others => No_Pull);

      GPIOA.MODER   (0) := Mode_IN;
      GPIOA.PUPDR   (0) := No_Pull;
   end;

   loop
      --  Off
      GPIOD.BSRR := Masks (Idx) * 2**16;

      if Clockwise then
         Idx := Idx + 1;
      else
         Idx := Idx - 1;
      end if;

      --  On
      GPIOD.BSRR := Masks (Idx);

      if (GPIOA.IDR and 1) /= 0 then
         Clockwise := not Clockwise;
      end if;

      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end loop;
end Leds;
