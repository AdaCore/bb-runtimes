------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . C P U _ S P E C I F I C               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2022, AdaCore                     --
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
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ------------------------
   -- Context management --
   ------------------------

   --  The context buffer is a type that represents thread's state and is not
   --  otherwise stored in main memory. This typically includes all user-
   --  visible registers, and possibly some other status as well.

   --  In case different contexts have different amounts of state (for example,
   --  due to absence of a floating-point unit in a particular configuration,
   --  or just the FPU not being used), it is expected that these details are
   --  handled in the implementation.

   type SIMD_Vector_Type is record
      D0, D1 : Interfaces.Unsigned_64;
   end record;
   for SIMD_Vector_Type'Alignment use 16;

   type SIMD_Registers_Type is array (0 .. 31) of SIMD_Vector_Type;

   type FPU_Context_Buffer is record
      --  Floating point context: all registers need to be saved, as FPU
      --  context switch is asynchronous.

      FPSR   : Interfaces.Unsigned_32;  --  Offset: 112
      --  Status register

      FPCR   : Interfaces.Unsigned_32;  --  Offset: 116
      --  Control register

      V      : SIMD_Registers_Type;     --  Offset: 128
      --  General-purpose FPU registers

      V_Init : Boolean;                 --  Offset: 640
      --  Set to true when the structure contains an actually saved context
   end record;

   type FPU_Context_Access is access all FPU_Context_Buffer;

   type Context_Buffer is record
   --  Only callee-saved registers need to be saved, as the context switch
   --  is always synchronous.

      C19     : Address;  --  Offset : 0
      C20     : Address;
      C21     : Address;  --  Offset : 32
      C22     : Address;
      C23     : Address;  --  Offset : 64
      C24     : Address;
      C25     : Address;  --  Offset : 96
      C26     : Address;
      C27     : Address;  --  Offset : 128
      C28     : Address;
      C29     : Address;  --  FP, Offset : 160
      C30     : Address;  --  LR
      CSP     : Address;  --  Offset : 192

      FPU     : aliased FPU_Context_Buffer;
      Running : FPU_Context_Access := null;
   end record;

   Stack_Alignment : constant := 16;
   --  Stack alignment defined by the ABI

   ---------------------------------
   -- Interrupt and trap handling --
   ---------------------------------

   type Vector_Id is
     (Vector_Synchronous_Exception,
      Vector_Irq,
      Vector_Fiq,
      Vector_Serror);

end System.BB.CPU_Specific;
