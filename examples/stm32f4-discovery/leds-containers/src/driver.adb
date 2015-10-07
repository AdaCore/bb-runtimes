------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2015, Free Software Foundation, Inc.           --
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

with LEDs;          use LEDs;
with Button;        use Button;
with Ada.Real_Time; use Ada.Real_Time;

with Ada.Containers.Doubly_Linked_Lists;

package body Driver is

   package LED_Lists is new Ada.Containers.Doubly_Linked_Lists (User_LED);
   use LED_Lists;

   Pattern : List;

   task body Controller is
      Period     : constant Time_Span := Milliseconds (70);  -- arbitrary
      Next_Start : Time := Clock;
      Index      : Cursor;
   begin
      Pattern.Append (Orange);
      Pattern.Append (Red);
      Pattern.Append (Blue);
      Pattern.Append (Green);

      Index := Pattern.First;
      loop
         Off (Element (Index));

         if Button.Current_Direction = Clockwise then
            if Index = Pattern.Last then
               Index := Pattern.First;
            else
               Index := Next (Index);
            end if;
         else  -- going the other direction
            if Index = Pattern.First then
               Index := Pattern.Last;
            else
               Index := Previous (Index);
            end if;
         end if;

         On (Element (Index));

         Next_Start := Next_Start + Period;
         delay until Next_Start;
      end loop;
   end Controller;

end Driver;
