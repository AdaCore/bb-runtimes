------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Synchronous_Task_Control;
with Video; use Video;

package Mandel is
   pragma Elaborate_Body;
   Nbr_Tasks : constant Natural := 4;

   type Mandel_Screen is record
      X, Y : Natural;
      Width, Height : Natural;
   end record;

   Regions : array (1 .. 4) of Mandel_Screen :=
     (1 => (0, 0, Width / 2, Height / 2),
      2 =>  (Width / 2, 0, Width / 2, Height / 2),
      3 =>  (0, Height / 2, Width / 2, Height / 2),
      4 =>  (Width / 2, Height / 2, Width / 2, Height / 2));

   type Suspension_Array is array (1 .. Nbr_Tasks) of
     Ada.Synchronous_Task_Control.Suspension_Object;

   Starts : Suspension_Array;
   Wait : Suspension_Array;

   Times : array (1 .. Nbr_Tasks) of Time_Span;
end Mandel;
