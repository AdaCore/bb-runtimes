------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
------------------------------------------------------------------------------

--  This package implements PowerPC architecture specific support for the GNAT
--  Ravenscar run time.

package body System.BB.CPU_Specific is

   -------------------------------
   -- Finish_Initialize_Context --
   -------------------------------

   procedure Finish_Initialize_Context
     (Buffer : not null access Context_Buffer)
   is
   begin
      null;
   end Finish_Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
   begin
      null;
   end Initialize_CPU;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      --  To be implemented ???

      null;
   end Install_Error_Handlers;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Exception_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id)
   is
      procedure Copy_Handler
        (Service_Routine : System.Address;
         Vector          : Vector_Id;
         Id              : Integer);
      pragma Import (Asm, Copy_Handler, "copy_handler");
   begin
      Copy_Handler (Service_Routine, Vector, 0);
   end Install_Exception_Handler;

end System.BB.CPU_Specific;
