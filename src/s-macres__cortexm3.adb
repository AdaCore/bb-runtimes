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

with Interfaces;
with System.Machine_Code; use System.Machine_Code;

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

      AIRCR : Interfaces.Unsigned_32 with
        Address => 16#E000_ED0C#,
        Import,
        Volatile;

   begin
      --  Apply a barrier prior to the reset request to ensure previous
      --  memory accesses complete before the reset occurs.

      Asm ("dsb 0xF", Volatile => True, Clobber => "memory");

      --  Depending on the implementation, the reset could take some time to
      --  occur, during which an interrupt could come in. In that case the
      --  reset could occur in the middle of the interrupt handler. Disable
      --  interrupts to prevent that.

      Asm ("cpsid i", Volatile => True);

      --  Request reset

      AIRCR := 16#05FA_0004#;

      --  Depending on the implementation, the processor could continue
      --  to execute a few instructions following the reset request before
      --  the reset actually takes place. Use a loop to ensure there is no
      --  application code to execute following the request.

      loop
         null;
      end loop;
   end Os_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Os_Exit (0);
   end Stop;

end System.Machine_Reset;
