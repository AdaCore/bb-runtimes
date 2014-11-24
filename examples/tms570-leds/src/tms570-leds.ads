------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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

--  This file provides declarations for the user LEDs on the TMS570LS31
--  board from Texas Instruments.

with TMS570.GPIO;   use TMS570.GPIO;
with System;

package TMS570.LEDs is
   pragma Elaborate_Body;

   type User_LED is (Right_Top, Right, Left_Top, Bottom,
                     Right_Bottom, Left, Left_Bottom, Top);

   for User_LED use
     (Right_Top    => GPIO_Pin_0,
      Right        => GPIO_Pin_5,
      Left_Top     => GPIO_Pin_17,
      Bottom       => GPIO_Pin_18,
      Right_Bottom => GPIO_Pin_25,
      Left         => GPIO_Pin_27,
      Left_Bottom  => GPIO_Pin_29,
      Top          => GPIO_Pin_31);

   --  As a result of the representation clause, avoid iterating directly over
   --  the type since that will require an implicit lookup in the generated
   --  code of the loop.  Such usage seems unlikely so this direct
   --  representation is reasonable, and efficient.

   for User_LED'Size use Word'Size;
   --  we convert the LED values to Word values in order to write them to
   --  the registers, so the size must be the same

   procedure On  (This : User_LED);
   pragma Inline (On);
   procedure Off (This : User_LED);
   pragma Inline (Off);

private

   HET_Port : GPIO_Register;
   pragma Volatile (HET_Port);
   for HET_Port'Address use System'To_Address (16#FFF7_B84C#);  -- HET Port 1
   pragma Import (Ada, HET_Port);

end TMS570.LEDs;
