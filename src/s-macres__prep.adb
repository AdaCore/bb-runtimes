------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2021, Free Software Foundation, Inc.       --
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

with System.IOPorts; use System.IOPorts;

package body System.Machine_Reset is
   procedure Os_Exit;
   pragma Export (Ada, Os_Exit, "_exit");
   pragma No_Return (Os_Exit);
   --  Reset the board or shut-down the simulator

   procedure Os_Abort;
   pragma Export (Ada, Os_Abort, "abort");
   pragma No_Return (Os_Abort);
   --  Same as Os_Exit (rename in body to allow multiple pragma Export)

   --------------
   -- Os_Abort --
   --------------

   procedure Os_Abort renames Os_Exit;

   -------------
   -- Os_Exit --
   -------------

   procedure Os_Exit is
   begin
      --  Trigger a reset

      Outb (16#92#, 0);
      Outb (16#92#, 1);

      --  Make sure we don't return before reset takes effect

      loop
         null;
      end loop;
   end Os_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop renames Os_Exit;
end System.Machine_Reset;
