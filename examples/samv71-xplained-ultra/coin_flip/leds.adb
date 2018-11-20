------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

with Ada.Numerics.Discrete_Random;
with Ada.Real_Time; use Ada.Real_Time;

with Interfaces.SAM; use Interfaces.SAM;
with Interfaces.SAM.PMC; use Interfaces.SAM.PMC;
with Interfaces.SAM.PIO; use Interfaces.SAM.PIO;
with System.SAMV71; use System.SAMV71;

procedure Leds is

   LED0 : constant := 2 ** 23;  --  PA23
   LED1 : constant := 2 ** 9;   --  PC09

   procedure Wait (Period : Time_Span)
   is
   begin
      delay until (Period + Clock);
   end Wait;

   procedure Flip_Coin
   is
      type Coin is (Heads, Tails);

      package Random_Coin is new Ada.Numerics.Discrete_Random (Coin);
      use Random_Coin;
      G : Generator;
   begin
      Reset (G);
      loop
         --  simulate flipping
         for I in 1 .. 10 loop
            PIOA_Periph.PIO_SODR.Val := LED0;
            PIOC_Periph.PIO_CODR.Val := LED1;
            Wait (Period => Milliseconds (50));
            PIOA_Periph.PIO_CODR.Val := LED0;
            PIOC_Periph.PIO_SODR.Val := LED1;
            Wait (Period => Milliseconds (50));
         end loop;

         --  Clear LEDS and delay
         PIOA_Periph.PIO_SODR.Val := LED0;
         PIOC_Periph.PIO_SODR.Val := LED1;
         Wait (Period => Milliseconds (300));

         --  check result
         case Random (G) is
            when Heads =>
               PIOA_Periph.PIO_SODR.Val := LED0;
               PIOC_Periph.PIO_CODR.Val := LED1;
            when Tails =>
               PIOA_Periph.PIO_CODR.Val := LED0;
               PIOC_Periph.PIO_SODR.Val := LED1;
         end case;

         --  delay and repeat
         Wait (Period => Milliseconds (2000));
      end loop;
   end Flip_Coin;

begin
   --  Enable clock for GPIO-A and GPIO-C
   PMC_Periph.PMC_PCER0.PID.Val := 2 ** PIOA_ID + 2 ** PIOC_ID;

   --  PIO Enable
   PIOA_Periph.PIO_PER.Val := LED0;
   PIOC_Periph.PIO_PER.Val := LED1;

   PIOA_Periph.PIO_OER.Val := LED0;
   PIOC_Periph.PIO_OER.Val := LED1;

   PIOA_Periph.PIO_CODR.Val := LED0;
   PIOC_Periph.PIO_CODR.Val := LED1;

   PIOA_Periph.PIO_MDDR.Val := LED0;
   PIOC_Periph.PIO_MDDR.Val := LED1;

   Flip_Coin;
end Leds;
