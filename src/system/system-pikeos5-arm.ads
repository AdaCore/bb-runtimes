------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                          (PikeOS 5 ARM Version)                          --
--                                                                          --
--          Copyright (C) 2016-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is a ZFP version of this package for ARM PikeOS 5 targets

pragma Restrictions (No_Exception_Propagation);
--  Only local exception handling is supported in this profile

pragma Restrictions (No_Exception_Registration);
--  Disable exception name registration. This capability is not used because
--  it is only required by exception stream attributes which are not supported
--  in this run time.

pragma Restrictions (No_Implicit_Dynamic_Code);
--  Pointers to nested subprograms are not allowed in this run time, in order
--  to prevent the compiler from building "trampolines".

pragma Restrictions (No_Finalization);
--  Controlled types are not supported in this run time

pragma Restrictions (No_Tasking);
--  Tasking is not supported in this run time

pragma Discard_Names;
--  Disable explicitly the generation of names associated with entities in
--  order to reduce the amount of storage used. These names are not used anyway
--  (attributes such as 'Image and 'Value are not supported in this run time).

package System is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable (see RM 13.7.1(15)). In Ada
   --  2005, this is Pure in any case (AI-362).

   pragma No_Elaboration_Code_All;
   --  Allow the use of that restriction in units that WITH this unit

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int             : constant := -2 ** (Standard'Max_Integer_Size - 1);
   Max_Int             : constant :=  2 ** (Standard'Max_Integer_Size - 1) - 1;

   Max_Binary_Modulus    : constant := 2 ** Standard'Max_Integer_Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := Standard'Max_Integer_Size - 1;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 0.0;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;
   Memory_Size  : constant := 2 ** 32;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := Low_Order_First;
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   --  For simplicity there is a 1-1 correspondence between Ada and PikeOS
   --  priorities. PikeOS priority 0 is reserved by the idle thread, so not
   --  available to Ada.

   --  PikeOS priorities are 0 .. 255

   --  Priorities greather than 245 are reserved to the system software (PSSW)

   --  This implementation reserves priorities 224-239 to interrupts

   --  Priorities 240-245 are reserved to HM and PikeOS exception handlers

   Max_Priority           : constant Positive := 223;
   Max_Interrupt_Priority : constant Positive := 239;

   subtype Any_Priority       is Integer range   1 .. Max_Interrupt_Priority;
   subtype Priority           is Any_Priority
     range Any_Priority'First .. Max_Priority;
   subtype Interrupt_Priority is Any_Priority
     range Priority'Last + 1 .. Any_Priority'Last;

   Default_Priority : constant Priority :=
                        (Priority'First + Priority'Last) / 2;

private

   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.

   Atomic_Sync_Default       : constant Boolean := False;
   Backend_Divide_Checks     : constant Boolean := False;
   Backend_Overflow_Checks   : constant Boolean := True;
   Command_Line_Args         : constant Boolean := False;
   Configurable_Run_Time     : constant Boolean := True;
   Denorm                    : constant Boolean := True;
   Duration_32_Bits          : constant Boolean := True;
   Exit_Status_Supported     : constant Boolean := True;
   Fractional_Fixed_Ops      : constant Boolean := False;
   Frontend_Layout           : constant Boolean := False;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := False;
   Stack_Check_Probes        : constant Boolean := False;
   Stack_Check_Limits        : constant Boolean := False;
   Support_Aggregates        : constant Boolean := True;
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := True;
   Suppress_Standard_Library : constant Boolean := True;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   Frontend_Exceptions       : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;

   --  The linker switches ordering comes from a project
   --  generated with Codeo or pikeos-cloneproject.

   pragma Linker_Options
      ("-u_p4_entry" & ASCII.NUL &
       "-u__cxx_local_dtors" & ASCII.NUL &
       "-nostdlib" & ASCII.NUL &
       "-T../ld/arm-pikeos5.ld" & ASCII.NUL &
       "-T../ld/memory.ld" & ASCII.NUL &
       "-lp4ext" & ASCII.NUL &
       "-lgnat" & ASCII.NUL &
       "-lvm" & ASCII.NUL &
       "-lstand" & ASCII.NUL &
       "-lp4" & ASCII.NUL &
       "-lgcc");

end System;
