------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    I N T E R F A C E S . X 8 6 _ 6 4                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2021, Free Software Foundation, Inc.            --
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

with System.BB.CPU_Specific;
with System.BB.Interrupts;

package Interfaces.X86_64.Exception_Handler is

   package SBC renames System.BB.CPU_Specific;
   package SBI renames System.BB.Interrupts;

   procedure Fatal_Exception (ID : SBI.Interrupt_ID; Code : SBC.Error_Code)
     with Export, External_Name => "__gnat_fatal_exception";

end Interfaces.X86_64.Exception_Handler;
