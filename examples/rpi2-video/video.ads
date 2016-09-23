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

with Interfaces; use Interfaces;

package Video is
   type Video_Struct is record
      Phys_Width : Unsigned_32;
      Phys_Height : Unsigned_32;
      Virt_Width : Unsigned_32;
      Virt_Height : Unsigned_32;
      Pitch : Unsigned_32;
      Depth : Unsigned_32;
      X_Off : Unsigned_32;
      Y_Off : Unsigned_32;
      Fb_Addr : Unsigned_32;
      Fb_Size : Unsigned_32;
   end record;

   procedure Init_Video;

   type Frame_Buffer is array (Natural range 0 .. 479,
                               Natural range 0 .. 639) of Unsigned_32;
   type Frame_Buffer_Acc is access Frame_Buffer;

   Fb : Frame_Buffer_Acc;
end Video;
