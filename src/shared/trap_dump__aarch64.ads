------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Interfaces; use Interfaces;

package Trap_Dump is
   pragma No_Elaboration_Code_All;

   --  CPU registers saved in exception handler

   type X_Regs is array (0 .. 31) of Unsigned_64;
   pragma Suppress_Initialization (X_Regs);

   type Registers_List is record
      Xr : X_Regs;
   end record;
   pragma Convention (C, Registers_List);
   pragma Suppress_Initialization (Registers_List);

   type Registers_List_Acc is access Registers_List;

   procedure Dump (Regs : Registers_List_Acc; Id : Natural);
   pragma Export (C, Dump, "__trap_dump");
   --  Called from hardware exception
end Trap_Dump;
