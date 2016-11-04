------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

package System.SF2.GPIO is

   subtype GPIO_Num is Integer range 0 .. 31;

   type GPIO_Mode is
     (Input_Mode,
      Output_Mode,
      In_Out_Mode);

   type GPIO_Interrupt_Mode is
     (IRQ_Level_High,
      IRQ_Level_Low,
      IRQ_Edge_Positive,
      IRQ_Edge_Negative,
      IRQ_Edge_Both);

   procedure GPIO_Init;

   procedure GPIO_Config
     (Num  : GPIO_Num;
      Mode : GPIO_Mode);

   procedure GPIO_Config_Interrupt
     (Num   : GPIO_Num;
      Event : GPIO_Interrupt_Mode;
      Mode  : GPIO_Mode := Input_Mode);

   procedure Set (Num : GPIO_Num)
     with Inline_Always;

   procedure Clear (Num : GPIO_Num)
     with Inline_Always;

   function Set (Num : GPIO_Num) return Boolean
     with Inline_Always;

   function Interrupt_Name
     (Num : GPIO_Num) return Ada.Interrupts.Interrupt_ID;

private

   function Interrupt_Name
     (Num : GPIO_Num) return Ada.Interrupts.Interrupt_ID
   is (case Num is
          when 0 => Ada.Interrupts.Names.GPIO_INT_0_Interrupt,
          when 1 => Ada.Interrupts.Names.GPIO_INT_1_Interrupt,
          when 2 => Ada.Interrupts.Names.GPIO_INT_2_Interrupt,
          when 3 => Ada.Interrupts.Names.GPIO_INT_3_Interrupt,
          when 4 => Ada.Interrupts.Names.GPIO_INT_4_Interrupt,
          when 5 => Ada.Interrupts.Names.GPIO_INT_5_Interrupt,
          when 6 => Ada.Interrupts.Names.GPIO_INT_6_Interrupt,
          when 7 => Ada.Interrupts.Names.GPIO_INT_7_Interrupt,
          when 8 => Ada.Interrupts.Names.GPIO_INT_8_Interrupt,
          when 9 => Ada.Interrupts.Names.GPIO_INT_9_Interrupt,
          when 10 => Ada.Interrupts.Names.GPIO_INT_10_Interrupt,
          when 11 => Ada.Interrupts.Names.GPIO_INT_11_Interrupt,
          when 12 => Ada.Interrupts.Names.GPIO_INT_12_Interrupt,
          when 13 => Ada.Interrupts.Names.GPIO_INT_13_Interrupt,
          when 14 => Ada.Interrupts.Names.GPIO_INT_14_Interrupt,
          when 15 => Ada.Interrupts.Names.GPIO_INT_15_Interrupt,
          when 16 => Ada.Interrupts.Names.GPIO_INT_16_Interrupt,
          when 17 => Ada.Interrupts.Names.GPIO_INT_17_Interrupt,
          when 18 => Ada.Interrupts.Names.GPIO_INT_18_Interrupt,
          when 19 => Ada.Interrupts.Names.GPIO_INT_19_Interrupt,
          when 20 => Ada.Interrupts.Names.GPIO_INT_20_Interrupt,
          when 21 => Ada.Interrupts.Names.GPIO_INT_21_Interrupt,
          when 22 => Ada.Interrupts.Names.GPIO_INT_22_Interrupt,
          when 23 => Ada.Interrupts.Names.GPIO_INT_23_Interrupt,
          when 24 => Ada.Interrupts.Names.GPIO_INT_24_Interrupt,
          when 25 => Ada.Interrupts.Names.GPIO_INT_25_Interrupt,
          when 26 => Ada.Interrupts.Names.GPIO_INT_26_Interrupt,
          when 27 => Ada.Interrupts.Names.GPIO_INT_27_Interrupt,
          when 28 => Ada.Interrupts.Names.GPIO_INT_28_Interrupt,
          when 29 => Ada.Interrupts.Names.GPIO_INT_29_Interrupt,
          when 30 => Ada.Interrupts.Names.GPIO_INT_30_Interrupt,
          when 31 => Ada.Interrupts.Names.GPIO_INT_31_Interrupt);

end System.SF2.GPIO;
