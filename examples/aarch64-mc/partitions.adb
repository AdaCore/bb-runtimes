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

with Hull_Qemu; use Hull_Qemu;
with Hulls; use Hulls;
with Uart; use Uart;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package body Partitions is

   Part1_Desc : Hull_Desc;
   pragma Import (C, Part1_Desc, "__dir_part1");

   Part1 : aliased Hull_Qemu.Hull_Qemu_Type;

   Part2_Desc : Hull_Desc;
   pragma Import (C, Part2_Desc, "__dir_part2");

   Part2 : aliased Hull_Qemu.Hull_Qemu_Type;

   task body Part1_Task is
   begin
      Suspend_Until_True (Part1_Suspend);

      loop
         Init (Part1'Unchecked_Access);

         Hulls.Create_Hull (Part1_Desc, Part1'Unchecked_Access);
         Log ("??? return");
         Log_Line;
      end loop;
   end Part1_Task;

   task body Part2_Task is
   begin
      Suspend_Until_True (Part2_Suspend);

      loop
         Init (Part2'Unchecked_Access);

         Hulls.Create_Hull (Part2_Desc, Part2'Unchecked_Access);
         Log ("??? return");
         Log_Line;
      end loop;
   end Part2_Task;

end Partitions;
