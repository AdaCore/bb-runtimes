------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

package Term is
   subtype Line_Range is Natural range 1 .. 64;
   Line : String (Line_Range);
   Line_Len : Natural range 0 .. Line_Range'Last;
   Pos, End_Pos : Natural range 0 .. Line_Range'Last;

   procedure Get_Line (Prompt : String);
   --  Display the prompt and get a line

   procedure Next_Word;
   --  Update Pos and End_Pos to the boundaries of the next word in the line

   subtype Hex_Digit_Type is Integer range -1 .. 255;
   Bad_Hex : constant Hex_Digit_Type := -1;

   function Read_Hex_Digit (Pos : Line_Range) return Hex_Digit_Type;
   --  Convert Line (Pos) to its hexadecimal value.  Return Bad_Hex if not
   --  an hexa character.
end Term;
