------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Hulls; use Hulls;
with Emu_PL011; use Emu_PL011;
with System; use System;
with IOEmu; use IOEmu;

package Hull_Qemu is

   type Hull_Qemu_Type is new Hull_Context with private;

   procedure Init (H : out Hull_Qemu_Type);

   procedure Find_IO
     (Hull : Hull_Qemu_Type;
      Addr : Address;
      Dev : out IOEmu_Dev_Acc;
      Off : out Off_T);
private
   type Hull_Qemu_Type is new Hull_Context with record
      Uart : aliased Emu_PL011.PL011_Uart_Dev;
      Iomap : aliased IOEmu_Map_Array (0 .. 0);
   end record;

end Hull_Qemu;
