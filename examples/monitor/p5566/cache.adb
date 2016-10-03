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

with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

package body Cache is
   procedure Cache_Flush_All is
      Val : Unsigned_32;
   begin
      --  Flush
      for Cset in 0 .. 127 loop
         for Cway in 0 .. 7 loop
            Val := Unsigned_32 (Cway * 16#1_000000# + Cset * 16#2_0# + 16#2#);
            Asm ("mtspr 1016, %0",
                 Inputs => Unsigned_32'Asm_Input ("r", Val),
                 Volatile => True);
         end loop;
      end loop;
   end Cache_Flush_All;

   procedure Cache_Disable is
   begin
      Cache_Flush_All;

      --  Disable
      Asm ("msync; isync; mtspr 1010, %0",
           Inputs => Unsigned_32'Asm_Input ("r", 0), Volatile => True);
   end Cache_Disable;

   procedure Cache_Flush_Line (Addr : Address) is
   begin
      Asm ("dcbf 0,%0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end Cache_Flush_Line;

   procedure Cache_Flush_Range (First, Last : Address) is
      Addr : Address := First;
   begin
      while Addr <= Last loop
         Cache_Flush_Line (Addr);
         Addr := Addr + Cache_Line;
      end loop;
   end Cache_Flush_Range;

end Cache;
