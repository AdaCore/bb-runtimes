------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           R I S C V _ H o s t _ T a r g e t _ I n t e r f a c e          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2017, Free Software Foundation, Inc.           --
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

--  This package provides access to some features of the RISC-V Host/Target
--  Interface (HTIF).

package RISCV_Host_Target_Interface is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   procedure Console_Put_Char (C : Character);
   --  Send a character to the HTIF

   procedure Console_Get_Char (C : out Character);
   --  Get the character in the buffer, if any, otherwise try to read a new
   --  character from the HTIF.

   function Console_Has_Char return Boolean;
   --  Return True if there's a character to read in the buffer

   procedure Power_Off
     with No_Return;
   --  Endlessly send a power-off syscall to HTIF

end RISCV_Host_Target_Interface;
