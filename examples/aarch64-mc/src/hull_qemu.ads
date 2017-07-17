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
with Emu_PL011;
with Emu_GIC;
with System; use System;
with IOEmu; use IOEmu;

package Hull_Qemu is

   type Hull_Qemu_Type is new Hull_Context with private;

   type Hull_Qemu_Acc is access all Hull_Qemu_Type'Class;

   procedure Init (H : Hull_Qemu_Acc);

   procedure Find_IO
     (Hull : Hull_Qemu_Type;
      Addr : Address;
      Dev : out IOEmu_Dev_Acc;
      Off : out Off_T);
private

   type Qemu_Debug_Dev is new Debug_Dev with record
      Parent : Hull_Qemu_Acc;
   end record;

   procedure Debug (Dev : in out Qemu_Debug_Dev);

   type Hull_Qemu_Type is new Hull_Context with record
      Debug : aliased Qemu_Debug_Dev;
      Uart : aliased Emu_PL011.PL011_Uart_Dev;
      GIC  : aliased Emu_GIC.GIC_Dev;
      Iomap : aliased IOEmu_Map_Array (0 .. 1);
   end record;

end Hull_Qemu;
