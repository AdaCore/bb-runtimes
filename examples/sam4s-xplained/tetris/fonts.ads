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
with Interfaces; use Interfaces;

package Fonts is
   type Glyph_Type is array (0 .. 7) of Unsigned_8;

   type Font_Type is array (0 .. 9) of Glyph_Type;
   Digit_Font : constant Font_Type :=
     (
      --  '0'
      (2#01111100#,
       2#11000110#,
       2#11001110#,
       2#11011110#,
       2#11110110#,
       2#11100110#,
       2#01111100#,
       2#00000000#),

      --  '1'
      (2#00110000#,
       2#01110000#,
       2#00110000#,
       2#00110000#,
       2#00110000#,
       2#00110000#,
       2#11111100#,
       2#00000000#),

      --  '2'
      (2#01111000#,
       2#11001100#,
       2#00001100#,
       2#00111000#,
       2#01100000#,
       2#11001100#,
       2#11111100#,
       2#00000000#),

      --  '3'
      (2#01111000#,
       2#11001100#,
       2#00001100#,
       2#00111000#,
       2#00001100#,
       2#11001100#,
       2#01111000#,
       2#00000000#),

      --  '4'
      (2#00011100#,
       2#00111100#,
       2#01101100#,
       2#11001100#,
       2#11111110#,
       2#00001100#,
       2#00011110#,
       2#00000000#),

      --  '5'
      (2#11111100#,
       2#11000000#,
       2#11111000#,
       2#00001100#,
       2#00001100#,
       2#11001100#,
       2#01111000#,
       2#00000000#),

      --  '6'
      (2#00111000#,
       2#01100000#,
       2#11000000#,
       2#11111000#,
       2#11001100#,
       2#11001100#,
       2#01111000#,
       2#00000000#),

      --  '7'
      (2#11111100#,
       2#11001100#,
       2#00001100#,
       2#00011000#,
       2#00110000#,
       2#00110000#,
       2#00110000#,
       2#00000000#),

      --  '8'
      (2#01111000#,
       2#11001100#,
       2#11001100#,
       2#01111000#,
       2#11001100#,
       2#11001100#,
       2#01111000#,
       2#00000000#),

      --  '9'
      (2#01111000#,
       2#11001100#,
       2#11001100#,
       2#01111100#,
       2#00001100#,
       2#00011000#,
       2#01110000#,
       2#00000000#)
     );
end Fonts;
