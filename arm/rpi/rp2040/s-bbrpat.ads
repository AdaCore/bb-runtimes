------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    Copyright (C) 2022, Daniel King                       --
--                      Copyright (C) 2022, AdaCore                         --
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

   -------------------
   -- Memory Orders --
   -------------------

   type Mem_Order is new Interfaces.C.int;

   Relaxed : constant Mem_Order := 0;
   --  Implies no inter-thread ordering constraints

   Consume : constant Mem_Order := 1;
   --  This is currently implemented using the stronger __ATOMIC_ACQUIRE
   --  memory order because of a deficiency in C++11's semantics for
   --  memory_order_consume.

   Acquire : constant Mem_Order := 2;
   --  Creates an inter-thread happens-before constraint from the release
   --  (or stronger) semantic store to this acquire load. Can prevent
   --  hoisting of code to before the operation.

   Release : constant Mem_Order := 3;
   --  Creates an inter-thread happens-before constraint to acquire (or
   --  stronger) semantic loads that read from this release store. Can
   --  prevent sinking of code to after the operation.

   Acq_Rel : constant Mem_Order := 4;
   --  Combines the effects of both Acquire and Release

   Seq_Cst : constant Mem_Order := 5;
   --  Enforces total ordering with all other Seq_Cst operations

   ---------------------
   -- __atomic_load_n --
   ---------------------

   generic
      type T is mod <>;
   function Atomic_Load (Addr  : System.Address;
                         Order : Mem_Order) return T;

   function Atomic_Load_1 is new Atomic_Load (Interfaces.Unsigned_8);
   pragma Export (C, Atomic_Load_1, "__atomic_load_1");
   function Atomic_Load_2 is new Atomic_Load (Interfaces.Unsigned_16);
   pragma Export (C, Atomic_Load_2, "__atomic_load_2");
   function Atomic_Load_4 is new Atomic_Load (Interfaces.Unsigned_32);
   pragma Export (C, Atomic_Load_4, "__atomic_load_4");
   function Atomic_Load_8 is new Atomic_Load (Interfaces.Unsigned_64);
   pragma Export (C, Atomic_Load_8, "__atomic_load_8");

   ----------------------
   -- __atomic_store_n --
   ----------------------

   generic
      type T is mod <>;
   procedure Atomic_Store (Addr  : System.Address;
                           Value : T;
                           Order : Mem_Order);

   procedure Atomic_Store_1 is new Atomic_Store (Interfaces.Unsigned_8);
   pragma Export (C, Atomic_Store_1, "__atomic_store_1");
   procedure Atomic_Store_2 is new Atomic_Store (Interfaces.Unsigned_16);
   pragma Export (C, Atomic_Store_2, "__atomic_store_2");
   procedure Atomic_Store_4 is new Atomic_Store (Interfaces.Unsigned_32);
   pragma Export (C, Atomic_Store_4, "__atomic_store_4");
   procedure Atomic_Store_8 is new Atomic_Store (Interfaces.Unsigned_64);
   pragma Export (C, Atomic_Store_8, "__atomic_store_8");

   -------------------------
   -- __atomic_exchange_n --
   -------------------------

   generic
      type T is mod <>;
   function Atomic_Exchange (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T;

   function Atomic_Exchange_1 is new Atomic_Exchange (Interfaces.Unsigned_8);
   pragma Export (C, Atomic_Exchange_1, "__atomic_exchange_1");
   function Atomic_Exchange_2 is new Atomic_Exchange (Interfaces.Unsigned_16);
   pragma Export (C, Atomic_Exchange_2, "__atomic_exchange_2");
   function Atomic_Exchange_4 is new Atomic_Exchange (Interfaces.Unsigned_32);
   pragma Export (C, Atomic_Exchange_4, "__atomic_exchange_4");
   function Atomic_Exchange_8 is new Atomic_Exchange (Interfaces.Unsigned_64);
   pragma Export (C, Atomic_Exchange_8, "__atomic_exchange_8");

   ---------------------------------
   -- __atomic_compare_exchange_n --
   ---------------------------------

   generic
      type T is mod <>;
   function Atomic_Compare_Exchange
      (Addr          : System.Address;
       Expected_Addr : System.Address;
       Desired       : T;
       Weak          : Interfaces.C.C_bool;
       Success_Order : Mem_Order;
       Failure_Order : Mem_Order) return Interfaces.C.C_bool;

   function Atomic_Compare_Exchange_1 is
      new Atomic_Compare_Exchange (Interfaces.Unsigned_8);
   pragma Export (C, Atomic_Compare_Exchange_1,
                  "__atomic_compare_exchange_1");

   function Atomic_Compare_Exchange_2 is
      new Atomic_Compare_Exchange (Interfaces.Unsigned_16);
   pragma Export (C, Atomic_Compare_Exchange_2,
                  "__atomic_compare_exchange_2");

   function Atomic_Compare_Exchange_4 is
      new Atomic_Compare_Exchange (Interfaces.Unsigned_32);
   pragma Export (C, Atomic_Compare_Exchange_4,
                  "__atomic_compare_exchange_4");

   function Atomic_Compare_Exchange_8 is
      new Atomic_Compare_Exchange (Interfaces.Unsigned_64);
   pragma Export (C, Atomic_Compare_Exchange_8,
                  "__atomic_compare_exchange_8");

   ------------------------
   -- __atomic_<op>_fetch --
   ------------------------

   generic
      type T is mod <>;
      with function Operation (Left, Right : T) return T;
   function Atomic_Op_Fetch (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T;

   --  __atomic_add_fetch

   function Atomic_Add_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Interfaces."+");
   pragma Export (C, Atomic_Add_Fetch_1, "__atomic_add_fetch_1");

   function Atomic_Add_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Interfaces."+");
   pragma Export (C, Atomic_Add_Fetch_2, "__atomic_add_fetch_2");

   function Atomic_Add_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Interfaces."+");
   pragma Export (C, Atomic_Add_Fetch_4, "__atomic_add_fetch_4");

   function Atomic_Add_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Interfaces."+");
   pragma Export (C, Atomic_Add_Fetch_8, "__atomic_add_fetch_8");

   --  __atomic_sub_fetch

   function Atomic_Sub_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Interfaces."-");
   pragma Export (C, Atomic_Sub_Fetch_1, "__atomic_sub_fetch_1");

   function Atomic_Sub_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Interfaces."-");
   pragma Export (C, Atomic_Sub_Fetch_2, "__atomic_sub_fetch_2");

   function Atomic_Sub_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Interfaces."-");
   pragma Export (C, Atomic_Sub_Fetch_4, "__atomic_sub_fetch_4");

   function Atomic_Sub_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Interfaces."-");
   pragma Export (C, Atomic_Sub_Fetch_8, "__atomic_sub_fetch_8");

   --  __atomic_and_fetch

   function Atomic_And_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Interfaces."and");
   pragma Export (C, Atomic_And_Fetch_1, "__atomic_and_fetch_1");

   function Atomic_And_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Interfaces."and");
   pragma Export (C, Atomic_And_Fetch_2, "__atomic_and_fetch_2");

   function Atomic_And_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Interfaces."and");
   pragma Export (C, Atomic_And_Fetch_4, "__atomic_and_fetch_4");

   function Atomic_And_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Interfaces."and");
   pragma Export (C, Atomic_And_Fetch_8, "__atomic_and_fetch_8");

   --  __atomic_xor_fetch

   function Atomic_Xor_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Interfaces."xor");
   pragma Export (C, Atomic_Xor_Fetch_1, "__atomic_xor_fetch_1");

   function Atomic_Xor_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Interfaces."xor");
   pragma Export (C, Atomic_Xor_Fetch_2, "__atomic_xor_fetch_2");

   function Atomic_Xor_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Interfaces."xor");
   pragma Export (C, Atomic_Xor_Fetch_4, "__atomic_xor_fetch_4");

   function Atomic_Xor_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Interfaces."xor");
   pragma Export (C, Atomic_Xor_Fetch_8, "__atomic_xor_fetch_8");

   --  __atomic_or_fetch

   function Atomic_Or_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Interfaces."or");
   pragma Export (C, Atomic_Or_Fetch_1, "__atomic_or_fetch_1");

   function Atomic_Or_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Interfaces."or");
   pragma Export (C, Atomic_Or_Fetch_2, "__atomic_or_fetch_2");

   function Atomic_Or_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Interfaces."or");
   pragma Export (C, Atomic_Or_Fetch_4, "__atomic_or_fetch_4");

   function Atomic_Or_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Interfaces."or");
   pragma Export (C, Atomic_Or_Fetch_8, "__atomic_or_fetch_8");

   --  __atomic_nand_fetch

   generic
      type T is mod <>;
   function Generic_Nand (Left, Right : T) return T;

   function Generic_Nand (Left, Right : T) return T is (not (Left and Right));

   function Nand_8  is new Generic_Nand (Interfaces.Unsigned_8);
   function Nand_16 is new Generic_Nand (Interfaces.Unsigned_16);
   function Nand_32 is new Generic_Nand (Interfaces.Unsigned_32);
   function Nand_64 is new Generic_Nand (Interfaces.Unsigned_64);

   function Atomic_Nand_Fetch_1 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_8, Nand_8);
   pragma Export (C, Atomic_Nand_Fetch_1, "__atomic_nand_fetch_1");

   function Atomic_Nand_Fetch_2 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_16, Nand_16);
   pragma Export (C, Atomic_Nand_Fetch_2, "__atomic_nand_fetch_2");

   function Atomic_Nand_Fetch_4 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_32, Nand_32);
   pragma Export (C, Atomic_Nand_Fetch_4, "__atomic_nand_fetch_4");

   function Atomic_Nand_Fetch_8 is new
      Atomic_Op_Fetch (Interfaces.Unsigned_64, Nand_64);
   pragma Export (C, Atomic_Nand_Fetch_8, "__atomic_nand_fetch_8");

   ------------------------
   -- __atomic_fetch_<op> --
   ------------------------

   generic
      type T is mod <>;
      with function Operation (Left, Right : T) return T;
   function Atomic_Fetch_Op (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T;

   --  __atomic_fetch_add

   function Atomic_Fetch_Add_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Interfaces."+");
   pragma Export (C, Atomic_Fetch_Add_1, "__atomic_fetch_add_1");

   function Atomic_Fetch_Add_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Interfaces."+");
   pragma Export (C, Atomic_Fetch_Add_2, "__atomic_fetch_add_2");

   function Atomic_Fetch_Add_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Interfaces."+");
   pragma Export (C, Atomic_Fetch_Add_4, "__atomic_fetch_add_4");

   function Atomic_Fetch_Add_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Interfaces."+");
   pragma Export (C, Atomic_Fetch_Add_8, "__atomic_fetch_add_8");

   --  __atomic_fetch_sub

   function Atomic_Fetch_Sub_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Interfaces."-");
   pragma Export (C, Atomic_Fetch_Sub_1, "__atomic_fetch_sub_1");

   function Atomic_Fetch_Sub_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Interfaces."-");
   pragma Export (C, Atomic_Fetch_Sub_2, "__atomic_fetch_sub_2");

   function Atomic_Fetch_Sub_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Interfaces."-");
   pragma Export (C, Atomic_Fetch_Sub_4, "__atomic_fetch_sub_4");

   function Atomic_Fetch_Sub_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Interfaces."-");
   pragma Export (C, Atomic_Fetch_Sub_8, "__atomic_fetch_sub_8");

   --  __atomic_fetch_and

   function Atomic_Fetch_And_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Interfaces."and");
   pragma Export (C, Atomic_Fetch_And_1, "__atomic_fetch_and_1");

   function Atomic_Fetch_And_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Interfaces."and");
   pragma Export (C, Atomic_Fetch_And_2, "__atomic_fetch_and_2");

   function Atomic_Fetch_And_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Interfaces."and");
   pragma Export (C, Atomic_Fetch_And_4, "__atomic_fetch_and_4");

   function Atomic_Fetch_And_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Interfaces."and");
   pragma Export (C, Atomic_Fetch_And_8, "__atomic_fetch_and_8");

   --  __atomic_fetch_xor

   function Atomic_Fetch_Xor_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Interfaces."xor");
   pragma Export (C, Atomic_Fetch_Xor_1, "__atomic_fetch_xor_1");

   function Atomic_Fetch_Xor_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Interfaces."xor");
   pragma Export (C, Atomic_Fetch_Xor_2, "__atomic_fetch_xor_2");

   function Atomic_Fetch_Xor_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Interfaces."xor");
   pragma Export (C, Atomic_Fetch_Xor_4, "__atomic_fetch_xor_4");

   function Atomic_Fetch_Xor_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Interfaces."xor");
   pragma Export (C, Atomic_Fetch_Xor_8, "__atomic_fetch_xor_8");

   --  __atomic_fetch_or

   function Atomic_Fetch_Or_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Interfaces."or");
   pragma Export (C, Atomic_Fetch_Or_1, "__atomic_fetch_or_1");

   function Atomic_Fetch_Or_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Interfaces."or");
   pragma Export (C, Atomic_Fetch_Or_2, "__atomic_fetch_or_2");

   function Atomic_Fetch_Or_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Interfaces."or");
   pragma Export (C, Atomic_Fetch_Or_4, "__atomic_fetch_or_4");

   function Atomic_Fetch_Or_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Interfaces."or");
   pragma Export (C, Atomic_Fetch_Or_8, "__atomic_fetch_or_8");

   --  __atomic_fetch_nand

   function Atomic_Fetch_Nand_1 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_8, Nand_8);
   pragma Export (C, Atomic_Fetch_Nand_1, "__atomic_fetch_nand_1");

   function Atomic_Fetch_Nand_2 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_16, Nand_16);
   pragma Export (C, Atomic_Fetch_Nand_2, "__atomic_fetch_nand_2");

   function Atomic_Fetch_Nand_4 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_32, Nand_32);
   pragma Export (C, Atomic_Fetch_Nand_4, "__atomic_fetch_nand_4");

   function Atomic_Fetch_Nand_8 is new
      Atomic_Fetch_Op (Interfaces.Unsigned_64, Nand_64);
   pragma Export (C, Atomic_Fetch_Nand_8, "__atomic_fetch_nand_8");

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