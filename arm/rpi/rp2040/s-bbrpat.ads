------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            Copyright (C) AdaCore and other contributors, 2022            --
--      See https://github.com/AdaCore/bb-runtimes/graphs/contributors      --
--                           for more information                           --
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

--  This package implements some intrinsics not provided by GCC for the armv6m
--  architecture. An RP2040 hardware spinlock is used to ensure atomicity
--  across all processors.

with Interfaces.C;

package System.BB.RP2040_Atomic is

   ------------------------------
   -- __sync_lock_test_and_set --
   ------------------------------

   generic
      type T is mod <>;
   function Sync_Lock_Test_And_Set (Addr  : System.Address;
                                    Value : T)
                                   return T;

   function Sync_Lock_Test_And_Set_1 is
      new Sync_Lock_Test_And_Set (Interfaces.Unsigned_8);
   pragma Export (C, Sync_Lock_Test_And_Set_1,
                  "__sync_lock_test_and_set_1");

   ----------------------------------
   -- __sync_bool_compare_and_swap --
   ----------------------------------

   generic
      type T is mod <>;
   function Sync_Bool_Compare_And_Swap (Addr      : System.Address;
                                        Old_Value : T;
                                        New_Value : T)
                                       return Interfaces.C.char;

   function Sync_Bool_Compare_And_Swap_4 is
      new Sync_Bool_Compare_And_Swap (Interfaces.Unsigned_32);
   pragma Export (C, Sync_Bool_Compare_And_Swap_4,
                  "__sync_bool_compare_and_swap_4");

private

   function PRIMASK return Interfaces.Unsigned_32
     with Inline_Always;

   function Interrupt_Disabled return Boolean
     with Inline_Always;

   procedure Disable_Interrupts
     with Inline_Always;

   procedure Enable_Interrupts
     with Inline_Always;

   procedure Spinlock_Lock
     with Inline_Always,
     Pre => Interrupt_Disabled;
   --  Obtain the hardware spinlock.
   --
   --  This must be called with interrupts disabled to avoid deadlocks when
   --  an interrupt occurs and tries to do an atomic operation immediately
   --  after the spinlock was obtained by a lower priority task/interrupt.

   procedure Spinlock_Unlock
     with Inline_Always;

   generic
      with procedure Wrapped_Proc;
   procedure Atomic_Wrapper
     with Inline_Always;
   --  Calls Wrapped_Proc with interrupts disabled
   --  and the hardware spinlock obtained (locked).

end System.BB.RP2040_Atomic;