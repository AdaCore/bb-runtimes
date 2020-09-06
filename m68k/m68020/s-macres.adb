------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2020, Free Software Foundation, Inc.       --
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

with MC68901; use MC68901;

package body System.Machine_Reset is
   procedure Os_Exit (Status : Integer);
   pragma No_Return (Os_Exit);
   pragma Export (Ada, Os_Exit, "_exit");
   --  Shutdown or restart the board

   procedure Os_Abort;
   pragma No_Return (Os_Abort);
   pragma Export (Ada, Os_Abort, "abort");
   --  Likewise

   --------------
   -- Os_Abort --
   --------------

   procedure Os_Abort is
   begin
      Os_Exit (1);
   end Os_Abort;

   -------------
   -- Os_Exit --
   -------------

   procedure Os_Exit (Status : Integer) is
      pragma Unreferenced (Status);
      --  The parameter is just for ISO-C compatibility

   begin
      --  TimerB output signal is bound to the M68020 reset input. To reset
      --  immediately the following sets the timer to the lowest prescaler
      --  and counter value.

      MC68901.TimerB_Data_Register.Data := 1;
      MC68901.TimerB_Control_Register.Prescaler := Prescaler_4;

      loop null; end loop;
   end Os_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Os_Exit (0);
   end Stop;
end System.Machine_Reset;
