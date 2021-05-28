--
--  Copyright 2021 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--

--  This package is used before elaboration, so it cannot have
--  any elaboration code.
package System.Bootrom
   with No_Elaboration_Code_All
is

   --  Cannot depend on package Interfaces due to No_Elaboration_Code_All,
   --  so define the equivalent types here.

   type Unsigned_32 is mod 2**32 with Size => 32;
   type Unsigned_16 is mod 2**16 with Size => 16;
   type Unsigned_8  is mod 2**8  with Size => 8;

   procedure Initialize
     with Export   => True,
     External_Name => "__gnat_initialize_bootrom";
   --  Lookup and cache the addresses of useful Bootrom functions.
   --  (e.g. memset, memcpy).
   --
   --  This must be called before using any of the following operations:
   --  memset, memset4, memcpy, memcpy4

   -----------------------
   -- Bootrom Functions --
   -----------------------
   --  2.8.3.1.

   function ROM_Table_Lookup (Table : System.Address;
                              Code  : Unsigned_32) return System.Address;

   function ROM_Table_Code (C1, C2 : Character) return Unsigned_32 is
     ((Unsigned_32 (Character'Pos (C2)) * 256) or
       Unsigned_32 (Character'Pos (C1)));

   function ROM_Func_Lookup (Code : Unsigned_32) return System.Address
   with Export        => True,
        Convention    => C,
        External_Name => "__gnat_rom_func_lookup";
   --  called from crt0.S

   function ROM_Data_Lookup
      (Code : Unsigned_32)
      return System.Address;

   --------------------------------------------
   -- Fast Bulk Memory Fill / Copy Functions --
   --------------------------------------------
   --  2.8.3.1.2.

   --  TODO

   ---------------------------------
   -- Fast Floating Point Library --
   ---------------------------------
   --  2.8.3.2.

   function FAdd (A, B : Float) return Float
     with Export    => True,
     Convention     => C,
     External_Name  => "__aeabi_fadd",
     Linker_Section => ".time_critical.aeabi_farithmetic";

   function FSub (A, B : Float) return Float
     with Export    => True,
     Convention     => C,
     External_Name  => "__aeabi_fsub",
     Linker_Section => ".time_critical.aeabi_farithmetic";

   function FMul (A, B : Float) return Float
     with Export    => True,
     Convention     => C,
     External_Name  => "__aeabi_fmul",
     Linker_Section => ".time_critical.aeabi_farithmetic";

   function FDiv (A, B : Float) return Float
     with Export    => True,
     Convention     => C,
     External_Name  => "__aeabi_fdiv",
     Linker_Section => ".time_critical.aeabi_farithmetic";

end System.Bootrom;
