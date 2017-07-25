------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2016, Free Software Foundation, Inc.       --
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

with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with Interfaces; use Interfaces;

package body System.Machine_Reset is
   procedure OS_Exit;
   pragma Export (Ada, OS_Exit, "_exit");
   pragma No_Return (OS_Exit);
   --  Reset the board or shut-down the simulator

   procedure OS_Abort;
   pragma Export (Ada, OS_Abort, "abort");
   pragma No_Return (OS_Abort);
   --  Same as OS_Exit (rename in body to allow multiple pragma Export)

   --------------
   -- OS_Abort --
   --------------

   procedure OS_Abort renames OS_Exit;

   -------------
   -- OS_Exit --
   -------------

   procedure OS_Exit is
      RPR : Unsigned_32;
      for RPR'Address use IMMRBAR + 16#0918#;
      pragma Volatile (RPR);
      pragma Import (Ada, RPR);

      RCR : Unsigned_32;
      for RCR'Address use IMMRBAR + 16#091C#;
      pragma Volatile (RCR);
      pragma Import (Ada, RCR);

   begin
      --  Enable access
      RPR := 16#52535445#;

      --  Trigger an hard reset

      RCR := 16#2#;
      --  Make sure we don't return before reset takes effect

      loop
         null;
      end loop;
   end OS_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop renames OS_Exit;
end System.Machine_Reset;
