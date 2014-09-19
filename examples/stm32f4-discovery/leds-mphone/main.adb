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

with Leds; use Leds;
with Mphone; use Mphone;

procedure Main is
   Mask : Word;
   Prev_Mask : Word;
   Spd : Natural;
begin
   Prev_Mask := 0;

   loop
      Spd := Get_Spd_Sample;
      case Spd is
         when 0 .. 4 * 4 =>
            Mask := 16#0_000#;
         when 5 * 5 .. 7 * 7 =>
            Mask := 16#1_000#;
         when 8 * 8 .. 10 * 10 =>
            Mask := 16#3_000#;
         when 11 * 11 .. 13 * 13 =>
            Mask := 16#7_000#;
         when 14 * 14 .. 16 * 16 =>
            Mask := 16#f_000#;
         when others =>
            Mask := 16#0_000#;
      end case;

      if Mask /= Prev_Mask then
         --  Off
         GPIOD.BSRR := 16#f_000# * 2**16;

         --  On
         GPIOD.BSRR := Mask;

         Prev_Mask := Mask;
      end if;

      if False then
         --  Off
         GPIOD.BSRR := 16#f_000# * 2**16;

         Spd := Get_Spd_Sample;

         --  On
         GPIOD.BSRR := 16#f_000#;
      end if;
   end loop;
end Main;
