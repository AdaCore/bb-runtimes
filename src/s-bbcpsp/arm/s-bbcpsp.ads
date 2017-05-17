------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

--  This package contains the primitives which are dependent on the
--  underlying processor.

pragma Restrictions (No_Elaboration_Code);

with System.BB.CPU_Primitives;

package System.BB.CPU_Specific is
   pragma Preelaborate;

   subtype Vector_Id is System.BB.CPU_Primitives.Vector_Id;

   --  Define ARM vectors

   Reset_Vector                  : constant Vector_Id := 0; -- RESET
   Undefined_Instruction_Vector  : constant Vector_Id := 1; -- UNDEF
   Supervisor_Call_Vector        : constant Vector_Id := 2; -- SVC
   Prefetch_Abort_Vector         : constant Vector_Id := 3; -- PABT
   Data_Abort_Vector             : constant Vector_Id := 4; -- DABT
   Interrupt_Request_Vector      : constant Vector_Id := 5; -- IRQ
   Fast_Interrupt_Request_Vector : constant Vector_Id := 6; -- FIQ
end System.BB.CPU_Specific;
