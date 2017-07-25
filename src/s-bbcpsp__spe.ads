------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with System;
with Interfaces;

package System.BB.CPU_Specific is
   pragma Preelaborate;

   ------------------------
   -- Context management --
   ------------------------

   --  The context buffer is a type that represents thread's state and is not
   --  otherwise stored in main memory. This typically includes all user-
   --  visible registers, and possibly some other status as well.

   --  In case different contexts have different amounts of state (for example,
   --  due to absence of a floating-point unit in a particular configuration,
   --  or just the FPU not being used), it is expected that these details
   --  are handled in the implementation.

   type Context_Buffer is record

      --  Only callee-saved (ie dedicated and nonvolatile) registers need to
      --  be saved.

      R1h  : Interfaces.Unsigned_32;
      R1   : System.Address;
      R2   : Interfaces.Unsigned_64;
      R13  : Interfaces.Unsigned_64;
      R14h : Interfaces.Unsigned_32;
      R14  : System.Address;
      R15h : Interfaces.Unsigned_32;
      R15  : System.Address;
      R16 : Interfaces.Unsigned_64;
      R17 : Interfaces.Unsigned_64;
      R18 : Interfaces.Unsigned_64;
      R19 : Interfaces.Unsigned_64;
      R20 : Interfaces.Unsigned_64;
      R21 : Interfaces.Unsigned_64;
      R22 : Interfaces.Unsigned_64;
      R23 : Interfaces.Unsigned_64;
      R24 : Interfaces.Unsigned_64;
      R25 : Interfaces.Unsigned_64;
      R26 : Interfaces.Unsigned_64;
      R27 : Interfaces.Unsigned_64;
      R28 : Interfaces.Unsigned_64;
      R29 : Interfaces.Unsigned_64;
      R30 : Interfaces.Unsigned_64;
      R31 : Interfaces.Unsigned_64;

      CR  : System.Address;
      LR  : System.Address;

      SPEFPSCR : Interfaces.Unsigned_32;
   end record;

   Stack_Alignment : constant := 16;
   --  Stack alignment defined by the ABI

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize_CPU;
   --  Initialize the CPU

   procedure Finish_Initialize_Context
     (Buffer : not null access Context_Buffer);
   --  Complete context initialization

   ---------------------------------
   -- Interrupt and Trap Handling --
   ---------------------------------

   type Vector_Id is range 0 .. 63;
   --  This corresponds to the IVOR number

   External_Interrupt_Excp : constant Vector_Id := 4;
   Decrementer_Excp : constant Vector_Id := 10;
   Floatting_Point_Data_Excp : constant Vector_Id := 33;

   procedure Install_Exception_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id);
   --  Install a new handler in the exception table

   procedure Install_Error_Handlers;
   --  Called at system initialization time to install a CPU specific trap
   --  handler, GNAT_Error_Handler, that converts synchronous traps to
   --  appropriate exceptions.

   -------------
   -- Variant --
   -------------

   PowerPC_Book_E : constant Boolean := True;
   --  Does the CPU implement PowerPC Book-E standard

end System.BB.CPU_Specific;
