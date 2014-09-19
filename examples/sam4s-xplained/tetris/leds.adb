------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
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
with System.SAM4S; use System.SAM4S;
pragma Warnings (On);

package body Leds is
   Led_Pin_C  : constant := 2 ** 23; -- PC23
   Led1_Pin_C : constant := 2 ** 20; -- PC20
   Led2_Pin_A : constant := 2 ** 16; -- PA16
   Led3_Pin_C : constant := 2 ** 22; -- PC22;

   But_Pin_A  : constant := 2 ** 2;  -- PA2
   But1_Pin_A : constant := 2 ** 0;  -- PA0
   But2_Pin_C : constant := 2 ** 29; -- PC29
   But3_Pin_C : constant := 2 ** 30; -- PC30

   function Button1_Pressed return Boolean is
   begin
      return (PIOA.PDSR and But1_Pin_A) = 0;
   end Button1_Pressed;

   function Button2_Pressed return Boolean is
   begin
      return (PIOC.PDSR and But2_Pin_C) = 0;
   end Button2_Pressed;

   function Button3_Pressed return Boolean is
   begin
      return (PIOC.PDSR and But3_Pin_C) = 0;
   end Button3_Pressed;

   function Button_Pressed return Boolean is
   begin
      return (PIOA.PDSR and But_Pin_A) = 0;
   end Button_Pressed;

   procedure Set_Led (On : Boolean) is
   begin
      if On then
         PIOC.CODR := Led_Pin_C;
      else
         PIOC.SODR := Led_Pin_C;
      end if;
   end Set_Led;

   procedure Set_Led1 (On : Boolean) is
   begin
      if On then
         PIOC.CODR := Led1_Pin_C;
      else
         PIOC.SODR := Led1_Pin_C;
      end if;
   end Set_Led1;

   procedure Set_Led2 (On : Boolean) is
   begin
      if On then
         PIOA.CODR := Led2_Pin_A;
      else
         PIOA.SODR := Led2_Pin_A;
      end if;
   end Set_Led2;

   procedure Set_Led3 (On : Boolean) is
   begin
      if On then
         PIOC.CODR := Led3_Pin_C;
      else
         PIOC.SODR := Led3_Pin_C;
      end if;
   end Set_Led3;

   procedure Init is
   begin
      --  Enable clock for GPIO-A and GPIO-C

      PMC.PMC_PCER0 := 2 ** PIOA_ID + 2 ** PIOC_ID;

      --  Configure Led

      PIOC.PER := Led_Pin_C + Led1_Pin_C + Led3_Pin_C
        + But2_Pin_C + But3_Pin_C;
      PIOC.OER := Led_Pin_C + Led1_Pin_C + Led3_Pin_C;
      PIOC.CODR := Led_Pin_C + Led1_Pin_C + Led3_Pin_C;
      PIOC.MDDR := Led_Pin_C + Led1_Pin_C + Led3_Pin_C;
      PIOC.PUER := But2_Pin_C + But3_Pin_C;

      PIOA.PER := Led2_Pin_A + But_Pin_A + But1_Pin_A;
      PIOA.OER := Led2_Pin_A;
      PIOA.CODR := Led2_Pin_A;
      PIOA.MDDR := Led2_Pin_A;
      PIOA.PUER := But_Pin_A + But1_Pin_A;
   end Init;
end Leds;
