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

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.SAM; use Interfaces.SAM;
with Interfaces.SAM.PMC; use Interfaces.SAM.PMC;
with Interfaces.SAM.PIO; use Interfaces.SAM.PIO;
with System.SAMV71; use System.SAMV71;

procedure Leds is

   LED0 : constant := 2 ** 23;  --  PA23
   LED1 : constant := 2 ** 9;   --  PC09

   procedure Wait is
   begin
      for I in 1 .. (2 ** 24 - 1) loop
         null;
      end loop;
   end Wait;

begin
   Put_Line ("Starting led toggle loop...");

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

   loop
      PIOA_Periph.PIO_SODR.Val := LED0;
      PIOC_Periph.PIO_CODR.Val := LED1;
      Wait;

      PIOA_Periph.PIO_CODR.Val := LED0;
      PIOC_Periph.PIO_SODR.Val := LED1;
      Wait;
   end loop;
end Leds;
