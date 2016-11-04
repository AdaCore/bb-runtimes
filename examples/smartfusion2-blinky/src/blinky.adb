------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;

pragma Warnings (Off);
with System.SF2.GPIO;          use System.SF2.GPIO;
pragma Warnings (On);

procedure Blinky
is
   DS3 : constant GPIO_Num := 1;
   DS4 : constant GPIO_Num := 2;

   Dot_Time : constant Time_Span := Milliseconds (50);
   Dash_Time : constant Time_Span := 3 * Dot_Time;

   ---------
   -- Dot --
   ---------

   procedure Dot is
   begin
      Set (DS3);
      Set (DS4);
      delay until Clock + Dot_Time;
      Clear (DS3);
      Clear (DS4);
      delay until Clock + Dot_Time;
   end Dot;

   ----------
   -- Dash --
   ----------

   procedure Dash is
   begin
      Set (DS3);
      Set (DS4);
      delay until Clock + Dash_Time;
      Clear (DS3);
      Clear (DS4);
      delay until Clock + Dot_Time;
   end Dash;

   -----------
   -- Morse --
   -----------

   procedure Morse (S : String) is
   begin
      for J in S'Range loop
         case S (J) is
            when 'a' => Dot; Dash;
            when 'b' => Dash; Dot; Dot; Dot;
            when 'c' => Dash; Dot; Dash; Dot;
            when 'd' => Dash; Dot; Dot;
            when 'e' => Dot;
            when 'f' => Dot; Dot; Dash; Dot;
            when 'g' => Dash; Dash; Dot;
            when 'h' => Dot; Dot; Dot; Dot;
            when 'i' => Dot; Dot;
            when 'j' => Dot; Dash; Dash; Dash;
            when 'k' => Dash; Dot; Dash;
            when 'l' => Dot; Dash; Dot; Dot;
            when 'm' => Dash; Dash;
            when 'n' => Dash; Dot;
            when 'o' => Dash; Dash; Dash;
            when 'p' => Dot; Dash; Dash; Dot;
            when 'q' => Dash; Dash; Dot; Dash;
            when 'r' => Dot; Dash; Dot;
            when 's' => Dot; Dot; Dot;
            when 't' => Dash;
            when 'u' => Dot; Dot; Dash;
            when 'v' => Dot; Dot; Dot; Dash;
            when 'w' => Dot; Dash; Dash;
            when 'x' => Dash; Dot; Dot; Dash;
            when 'y' => Dash; Dot; Dash; Dash;
            when 'z' => Dash; Dash; Dot; Dot;
            when '1' => Dot; Dash; Dash; Dash; Dash;
            when '2' => Dot; Dot; Dash; Dash; Dash;
            when '3' => Dot; Dot; Dot; Dash; Dash;
            when '4' => Dot; Dot; Dot; Dot; Dash;
            when '5' => Dot; Dot; Dot; Dot; Dot;
            when '6' => Dash; Dot; Dot; Dot; Dot;
            when '7' => Dash; Dash; Dot; Dot; Dot;
            when '8' => Dash; Dash; Dash; Dot; Dot;
            when '9' => Dash; Dash; Dash; Dash; Dot;
            when '0' => Dash; Dash; Dash; Dash; Dash;
            when others =>
               null;
         end case;

         if S (J) in 'a' .. 'z' or else S (J) in '0' .. '9' then
            delay until Clock + 2 * Dot_Time;
         elsif S (J) = ' ' then
            delay until Clock + 6 * Dot_Time;
         end if;
         Put (S (J));

      end loop;

      New_Line;
   end Morse;

begin
   GPIO_Init;

   GPIO_Config (DS3, Output_Mode);
   GPIO_Config (DS4, Output_Mode);

   loop
      Morse ("hello world");

      delay until Clock + Milliseconds (2000);
   end loop;
end Blinky;
