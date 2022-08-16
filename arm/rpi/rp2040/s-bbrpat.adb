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

with System.Machine_Code;   use System.Machine_Code;
with System.BB.Parameters;
with Interfaces;            use Interfaces;
with Interfaces.RP2040.SIO; use Interfaces.RP2040.SIO;

package body System.BB.RP2040_Atomic is

   -------------
   -- PRIMASK --
   -------------

   function PRIMASK return Unsigned_32 is
      R : Unsigned_32;
   begin
      Asm ("mrs %0, PRIMASK", Outputs => Unsigned_32'Asm_Output ("=r", R),
           Volatile => True);
      return R;
   end PRIMASK;

   ------------------------
   -- Interrupt_Disabled --
   ------------------------

   function Interrupt_Disabled return Boolean
   is ((PRIMASK and 1) /= 0);

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("cpsid i" & ASCII.CR & ASCII.LF
             & "dsb" & ASCII.CR & ASCII.LF
             & "isb",
           Clobber => "memory",
           Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      Asm ("cpsie i" & ASCII.CR & ASCII.LF
             & "dsb" & ASCII.CR & ASCII.LF
             & "isb",
           Clobber => "memory",
           Volatile => True);
   end Enable_Interrupts;

   -------------------
   -- Spinlock_Lock --
   -------------------

   procedure Spinlock_Lock is
      use type Interfaces.RP2040.UInt32;
   begin
      --  Reads attempt to claim the lock.
      --  Read value is nonzero if the lock was successfully claimed,
      --  or zero if the lock had already been claimed by a previous read.
      loop
         exit when SIO_Periph.SPINLOCK31 /= 0;
      end loop;
   end Spinlock_Lock;

   ---------------------
   -- Spinlock_Unlock --
   ---------------------

   procedure Spinlock_Unlock is
   begin
      --  Write any value to release the lock
      SIO_Periph.SPINLOCK31 := 0;
   end Spinlock_Unlock;

   --------------------
   -- Atomic_Wrapper --
   --------------------

   procedure Atomic_Wrapper is
      Already_Disabled : constant Boolean := Interrupt_Disabled;
      --  Make sure not to change the status of interrupt control by checking
      --  if they are enabled when entering the function.
   begin

      if not Already_Disabled then
         Disable_Interrupts;
      end if;

      if System.BB.Parameters.Multiprocessor then
         Spinlock_Lock;
      end if;

      Wrapped_Proc;

      if System.BB.Parameters.Multiprocessor then
         Spinlock_Unlock;
      end if;

      --  If the interrupts were disabled when entering this function, we do
      --  not want enable them.
      if not Already_Disabled then
         Enable_Interrupts;
      end if;
   end Atomic_Wrapper;

   ----------------------------
   -- Sync_Lock_Test_And_Set --
   ----------------------------

   function Sync_Lock_Test_And_Set (Addr  : System.Address;
                                    Value : T)
                                   return T
   is
      Data : T with Address => Addr;
      Ret  : T;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret  := Data;
         Data := Value;
      end Inner;

      procedure Atomic_Action is new Atomic_Wrapper (Inner);

   begin
      Atomic_Action;
      return Ret;
   end Sync_Lock_Test_And_Set;

   --------------------------------
   -- Sync_Bool_Compare_And_Swap --
   --------------------------------

   function Sync_Bool_Compare_And_Swap (Addr      : System.Address;
                                        Old_Value : T;
                                        New_Value : T)
                                       return Interfaces.C.char
   is
      Data : T with Address => Addr;
      Ret  : Interfaces.C.char;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         if Data = Old_Value then
            Data := New_Value;
            Ret := Interfaces.C.char'Succ (Interfaces.C.nul); -- True
         else
            Ret := Interfaces.C.nul; -- False
         end if;
      end Inner;

      procedure Atomic_Action is new Atomic_Wrapper (Inner);

   begin
      Atomic_Action;
      return Ret;
   end Sync_Bool_Compare_And_Swap;

   -----------------
   -- Atomic_Load --
   -----------------

   function Atomic_Load (Addr  : System.Address;
                         Order : Mem_Order) return T
   is
      Data : T with Address => Addr;
      Ret  : T;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret := Data;
      end Inner;

      procedure Fallback is new Atomic_Wrapper (Inner);

   begin

      --  Byte, halfword, and word accesses are always atomic on armv6m.
      --  doubleword accesses are not atomic, so we need to fallback to
      --  doing the load with interrupts disabled.

      if T'Size <= 32 then
         if Order = Relaxed then
            Ret := Data;
         else
            Fallback;
         end if;
      else
         Fallback;
      end if;

      return Ret;
   end Atomic_Load;

   ------------------
   -- Atomic_Store --
   ------------------

   procedure Atomic_Store (Addr  : System.Address;
                           Value : T;
                           Order : Mem_Order)
   is
      Data : T with Address => Addr;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Data := Value;
      end Inner;

      procedure Fallback is new Atomic_Wrapper (Inner);

   begin
      --  Byte, halfword, and word accesses are always atomic on armv6m.
      --  doubleword accesses are not atomic, so we need to fallback to
      --  doing the load with interrupts disabled.

      if T'Size <= 32 then
         if Order = Relaxed then
            Data := Value;
         else
            Fallback;
         end if;
      else
         Fallback;
      end if;
   end Atomic_Store;

   ---------------------
   -- Atomic_Exchange --
   ---------------------

   function Atomic_Exchange (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T
   is
      pragma Unreferenced (Order);

      Data : T with Address => Addr;
      Ret  : T;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret  := Data;
         Data := Value;
      end Inner;

      procedure Do_Atomic_Exchange is new Atomic_Wrapper (Inner);

   begin
      Do_Atomic_Exchange;
      return Ret;
   end Atomic_Exchange;

   -----------------------------
   -- Atomic_Compare_Exchange --
   -----------------------------

   function Atomic_Compare_Exchange
      (Addr          : System.Address;
       Expected_Addr : System.Address;
       Desired       : T;
       Weak          : Interfaces.C.C_bool;
       Success_Order : Mem_Order;
       Failure_Order : Mem_Order) return Interfaces.C.C_bool
   is
      pragma Unreferenced (Weak);
      pragma Unreferenced (Success_Order);
      pragma Unreferenced (Failure_Order);

      Data     : T with Address => Addr;
      Expected : T with Address => Expected_Addr;
      Ret      : Interfaces.C.C_bool;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret := Interfaces.C.C_bool (Data = Expected);
         if Ret then
            Data := Desired;
         end if;
      end Inner;

      procedure Do_Atomic_Compare_Exchange is new Atomic_Wrapper (Inner);

   begin
      Do_Atomic_Compare_Exchange;
      return Ret;
   end Atomic_Compare_Exchange;

   ---------------------
   -- Atomic_Op_Fetch --
   ---------------------

   function Atomic_Op_Fetch (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T
   is
      pragma Unreferenced (Order);

      Data : T with Address => Addr;
      Ret  : T;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret  := Operation (Data, Value);
         Data := Ret;
      end Inner;

      procedure Do_Atomic_Op_Fetch is new Atomic_Wrapper (Inner);

   begin
      Do_Atomic_Op_Fetch;
      return Ret;
   end Atomic_Op_Fetch;

   ---------------------
   -- Atomic_Fetch_Op --
   ---------------------

   function Atomic_Fetch_Op (Addr  : System.Address;
                             Value : T;
                             Order : Mem_Order) return T
   is
      pragma Unreferenced (Order);

      Data : T with Address => Addr;
      Ret  : T;

      procedure Inner
        with Inline_Always;

      procedure Inner
      is
      begin
         Ret  := Data;
         Data := Operation (Data, Value);
      end Inner;

      procedure Do_Atomic_Fetch_Op is new Atomic_Wrapper (Inner);

   begin
      Do_Atomic_Fetch_Op;
      return Ret;
   end Atomic_Fetch_Op;

end System.BB.RP2040_Atomic;
