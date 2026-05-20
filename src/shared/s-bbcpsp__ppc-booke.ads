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
--                     Copyright (C) 2003-2021, AdaCore                     --
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

      --  Only callee-saved registers need to be saved

      R1  : System.Address;
      R2  : System.Address;
      R13 : System.Address;
      R14 : System.Address;
      R15 : System.Address;
      R16 : System.Address;
      R17 : System.Address;
      R18 : System.Address;
      R19 : System.Address;
      R20 : System.Address;
      R21 : System.Address;
      R22 : System.Address;
      R23 : System.Address;
      R24 : System.Address;
      R25 : System.Address;
      R26 : System.Address;
      R27 : System.Address;
      R28 : System.Address;
      R29 : System.Address;
      R30 : System.Address;
      R31 : System.Address;

      CR  : System.Address;
      LR  : System.Address;

      F14 : Long_Float;
      F15 : Long_Float;
      F16 : Long_Float;
      F17 : Long_Float;
      F18 : Long_Float;
      F19 : Long_Float;
      F20 : Long_Float;
      F21 : Long_Float;
      F22 : Long_Float;
      F23 : Long_Float;
      F24 : Long_Float;
      F25 : Long_Float;
      F26 : Long_Float;
      F27 : Long_Float;
      F28 : Long_Float;
      F29 : Long_Float;
      F30 : Long_Float;
      F31 : Long_Float;
      FPSCR : Long_Long_Integer;
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

   type Vector_Id is range 0 .. 15;
   --  This corresponds to the IVOR number

   External_Interrupt : constant Vector_Id := 4;
   FP_Unavailable     : constant Vector_Id := 7;
   Decrementer_Excp   : constant Vector_Id := 10;

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

   ------------------------------
   -- CPU Register Definitions --
   ------------------------------

   type Machine_State_Register is record
      Wait_State_Enable         : Boolean;
      Critical_Interrupt_Enable : Boolean;
      External_Interrupt_Enable : Boolean;
      Problem_State_Enable      : Boolean;
      Floating_Point_Available  : Boolean;
      Machine_Check_Enable      : Boolean;
      FP_Exception_Mode_0       : Boolean;
      Debug_Wait_Enable         : Boolean;
      Debug_Interrupt_Enable    : Boolean;
      FP_Exception_Mode_1       : Boolean;
      Instruction_Address_Space : Boolean;
      Data_Address_Space        : Boolean;
   end record with Size => 32;

   for Machine_State_Register use record
      Wait_State_Enable         at 0 range 13 .. 13;
      Critical_Interrupt_Enable at 0 range 14 .. 14;
      External_Interrupt_Enable at 0 range 16 .. 16;
      Problem_State_Enable      at 0 range 17 .. 17;
      Floating_Point_Available  at 0 range 18 .. 18;
      Machine_Check_Enable      at 0 range 19 .. 19;
      FP_Exception_Mode_0       at 0 range 20 .. 20;
      Debug_Wait_Enable         at 0 range 21 .. 21;
      Debug_Interrupt_Enable    at 0 range 22 .. 22;
      FP_Exception_Mode_1       at 0 range 23 .. 23;
      Instruction_Address_Space at 0 range 26 .. 26;
      Data_Address_Space        at 0 range 27 .. 27;
   end record;

   type Watchdog_Period is
     (Clocks_2_21, Clocks_2_25, Clocks_2_29, Clocks_2_33);
   type Watchdog_Reset_Type is
     (No_Reset, Core_Reset, Chip_Reset, System_Reset);
   type FIT_Period is (Clocks_2_13, Clocks_2_27, Clocks_2_21, Clocks_2_25);

   type Timer_Control_Register is record
      Watchdog_Timer_Period                 : Watchdog_Period;
      Watchdog_Timer_Reset_Control          : Watchdog_Reset_Type;
      Watchdog_Timer_Interrupt_Enable       : Boolean;
      Decrementer_Interrupt_Enable          : Boolean;
      Fixed_Interval_Timer_Period           : FIT_Period;
      Fixed_Interval_Timer_Interrupt_Enable : Boolean;
      Auto_Reload_Enable                    : Boolean;
   end record with Size => 32;

   for Timer_Control_Register use record
      Watchdog_Timer_Period                 at 0 range 0 .. 1;
      Watchdog_Timer_Reset_Control          at 0 range 2 .. 3;
      Watchdog_Timer_Interrupt_Enable       at 0 range 4 .. 4;
      Decrementer_Interrupt_Enable          at 0 range 5 .. 5;
      Fixed_Interval_Timer_Period           at 0 range 6 .. 7;
      Fixed_Interval_Timer_Interrupt_Enable at 0 range 8 .. 8;
      Auto_Reload_Enable                    at 0 range 9 .. 9;
   end record;

end System.BB.CPU_Specific;
