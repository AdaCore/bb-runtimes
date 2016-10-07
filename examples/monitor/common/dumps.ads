------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

with Interfaces; use Interfaces;

package Dumps is
   subtype String16 is String (1 .. 16);
   subtype String8 is String (1 .. 8);
   subtype String4 is String (1 .. 4);
   subtype String2 is String (1 .. 2);

   function Hex8 (V : Unsigned_64) return String16;
   function Hex4 (V : Unsigned_32) return String8;
   function Hex2 (V : Unsigned_32) return String4;
   function Hex1 (V : Unsigned_32) return String2;

   function Image8 (V : Unsigned_32) return String8 renames Hex4;
   function Image4 (V : Unsigned_32) return String4 renames Hex2;
   function Image2 (V : Unsigned_32) return String2 renames Hex1;
   function Image1 (V : Unsigned_32) return Character;
   --  Hexadecimal conversion

   procedure Put (V : Natural);
   --  Decimal output

   procedure Put_Bit (Set : Boolean; Name : Character);
   --  If Set is true, display Name otherwise display '-'

   procedure Put_Register_Desc (Val : Unsigned_32; Desc : String);
   --  Display bits of value Val. The parameter Desc is a string that
   --  describes the fields of the value. It is composed of a comma separated
   --  list. Each element of the list is either (without the quotes, length is
   --  a decimal number, name is a string without comma):
   --  * 'name': describes a bit, will be printed if the bit is set
   --  * 'length': describes a reserved fields, not printed
   --  * 'length:name': describes a field, will print name=value

   procedure Put_Register (Name : String;
                           Val : Unsigned_32;
                           Desc : String);
   --  Display name, its value and its bits using the above procedure.
end Dumps;
