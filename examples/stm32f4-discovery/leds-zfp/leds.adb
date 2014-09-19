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

procedure Leds is
   --  Bit definitions for RCC AHB1ENR register
   RCC_AHB1ENR_GPIOD    : constant Word := 16#08#;

   GPIOD_Base           : constant := AHB1_Peripheral_Base + 16#0C00#;

   GPIOD : GPIO_Registers with Volatile,
                             Address => System'To_Address (GPIOD_Base);
   pragma Import (Ada, GPIOD);

   procedure Wait is
   begin
      for I in 1 .. 16#f_ffff# loop
         null;
      end loop;
   end Wait;
begin
   --  Enable clock for GPIO-D

   RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOD;

   --  Configure PD12-15
   declare
      use GPIO;
   begin
      GPIOD.MODER   (12 .. 15) := (others => Mode_OUT);
      GPIOD.OTYPER  (12 .. 15) := (others => Type_PP);
      GPIOD.OSPEEDR (12 .. 15) := (others => Speed_100MHz);
      GPIOD.PUPDR   (12 .. 15) := (others => No_Pull);
   end;

   loop
      --  On

      GPIOD.BSRR := 16#f000#;
      Wait;

      --  Off
      GPIOD.BSRR := 16#f000_0000#;
      Wait;
   end loop;
end Leds;
