------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

package Gdbstub.CPU is
   procedure Setup_Handlers;
   --  Install exception handlers to catch faults.

   procedure Breakpoint;
   --  Execute a breakpoint.  This is used to enter into the stub.

   procedure Set_Trace_Flag (Trace : Boolean);
   --  Set or reset trace flag.

   procedure Invalidate_Icache (Start : Address; Len : Storage_Offset);
   --  Synchronize the Icache with the Dcache.

   procedure Get_Register_Area (Reg : Natural;
                                Area : out Address;
                                Size : out Storage_Count);
   --  Get the area for register REG.
   --  Return 0,0 for invalid register.
end Gdbstub.CPU;
