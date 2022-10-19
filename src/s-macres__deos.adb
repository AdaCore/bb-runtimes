------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2011-2022, Free Software Foundation, Inc.        --
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

package body System.Machine_Reset is

   procedure Abort_Exit
     with No_Return, Export, Convention => C,
          External_Name => "abort";

   procedure Runtime_Exit (Status : Integer)
     with No_Return, Export, Convention => C,
          External_Name => "exit";

   procedure Abort_Exit is
   begin
      Runtime_Exit (-1);
   end Abort_Exit;

   procedure Runtime_Exit (Status : Integer) is
      procedure Call_Destructors
        with Import, Convention => C, External_Name => "_fini";

      procedure OS_Exit (Status : Integer)
        with No_Return, Import, Convention => C,
             External_Name => "_exit";
   begin
      --  Call the destructors so that we have the opportunity to run
      --  post-test cleanup.

      Call_Destructors;
      OS_Exit (Status);
   end Runtime_Exit;

   procedure Stop is
   begin
      Runtime_Exit (0);
   end Stop;

end System.Machine_Reset;
