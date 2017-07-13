------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

with Interfaces;

package System.BB.CPU_Specific is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   type VFPU_Registers_Type is array (0 .. 15) of Interfaces.Unsigned_64;

   type VFPU_Context_Buffer is record
      --  Floating point context

      V_Init : Boolean;
      --  Set to true when the structure contains an actually saved context

      FPSCR   : Interfaces.Unsigned_32;
      --  Status and control register

      V      : VFPU_Registers_Type;
      --  General-purpose FPU registers
   end record;

   type VFPU_Context_Access is access all VFPU_Context_Buffer;

   type Context_Buffer is record
   --  Only callee-saved registers need to be saved, as the context switch
   --  is always synchronous.

      R0     : Interfaces.Unsigned_32;  --  Offset : 0
      R1     : Interfaces.Unsigned_32;
      PC     : Interfaces.Unsigned_32;  --  Offset : 8
      CPSR   : Interfaces.Unsigned_32;
      SP     : Interfaces.Unsigned_32;  --  Offset : 16
      LR     : Interfaces.Unsigned_32;

      VFPU    : aliased VFPU_Context_Buffer;
      Running : VFPU_Context_Access := null;
   end record;

   Stack_Alignment : constant := 8;
   --  Stack alignment defined by the ABI

end System.BB.CPU_Specific;
