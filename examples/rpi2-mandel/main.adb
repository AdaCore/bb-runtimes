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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Video; use Video;
with Mandel; use Mandel;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

procedure Main is
   T : Time;
begin
   Put_Line ("*******");

   Init_Video;

   Put_Line ("Waiting for screen...");

   --  Wait for screen on.
   T := Clock + Seconds (1);
   delay until T;

   Put_Line ("Starting...");

   loop
      --  Start tasks.
      for I in 1 .. Nbr_Tasks loop
         Set_True (Starts (I));
      end loop;

      --  Wait until tasks are completed.
      for I in 1 .. Nbr_Tasks loop
         Suspend_Until_True (Wait (I));
      end loop;

      --  Display time.
      for I in Times'Range loop
         Put ("CPU#");
         Put (Natural'Image (I));
         Put (":");
         Put (Natural'Image (Times (I) / Milliseconds (1)));
         Put_Line (" ms");
      end loop;
      New_Line;

      delay until Clock + Seconds (5);
   end loop;
end Main;
