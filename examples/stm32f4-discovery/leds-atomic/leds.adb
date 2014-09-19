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

with Ada.Interrupts.Names;
with Ada.Real_Time; use Ada.Real_Time;

package body Leds is
   protected Button is
      pragma Interrupt_Priority;

   private
      procedure Handler;
      pragma Attach_Handler (Handler, Ada.Interrupts.Names.EXTI0_Interrupt);
      --  Interrupt handler

      Last_Time : Time := Clock;
   end Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   protected body Button is
      procedure Handler is
         Now : constant Time := Clock;
      begin
         --  Clear interrupt
         EXTI.PR (0) := 1;

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            --  Change the direction
            Direction := not Direction;
            Last_Time := Now;
         end if;
      end Handler;
   end Button;

begin
   --  Enable clock for GPIO-D (leds) and GPIO-A (button)

   RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOD or RCC_AHB1ENR_GPIOA;

   --  And for SYSCFGEN

   RCC.APB2ENR := RCC.APB2ENR or RCC_APB2ENR_SYSCFGEN;

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

   --  Select PA for EXTI0

   SYSCFG.EXTICR1 (0) := 0;

   --  Interrupt on rising edge
   EXTI.FTSR (0) := 0;
   EXTI.RTSR (0) := 1;
   EXTI.IMR (0) := 1;
end Leds;
