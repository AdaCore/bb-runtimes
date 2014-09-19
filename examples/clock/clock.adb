------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

procedure Clock is
   procedure Disp_Digit (N : Natural) is
   begin
      Put (Character'Val (Character'Pos ('0') + N));
   end Disp_Digit;

   procedure Disp_2digits (N : Natural)
   is
   begin
      Disp_Digit (N / 10);
      Disp_Digit (N mod 10);
   end Disp_2digits;

   procedure Disp_Hours (N : Natural) is
   begin
      if N > 10 then
         Disp_Hours (N / 10);
      else
         Disp_Digit (N);
      end if;
   end Disp_Hours;

   T0 : Time := Clock;
   Clk : Time := T0;
   One_Sec : constant Time_Span := Milliseconds (1_000);
   Sec : Natural := 0;
   Min : Natural := 0;
   Hour : Natural := 0;
begin
   Put_Line ("Start of test");
   loop
      --  Disp clock.
      New_Line;
      Disp_Hours (Hour);
      Put (':');
      Disp_2digits (Min);
      Put (':');
      Disp_2digits (Sec);

      Clk := Clk + One_Sec;
      delay until Clk;

      if abs (Clk - Clock) > One_Sec then
         Put (" DRIFT!");
      end if;
      Sec := Sec + 1;
      if Sec = 60 then
         Sec := 0;
         Min := Min + 1;
         if Min = 60 then
            Min := 0;
            Hour := Hour + 1;
         end if;
      end if;

   end loop;
end Clock;
