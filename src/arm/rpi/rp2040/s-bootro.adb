------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  Copyright (C) 2021, Jeremy Grosser                      --
--                      Copyright (C) 2021, AdaCore                         --
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
pragma Suppress (All_Checks);

package body System.Bootrom is

   ---------------------------
   -- Bootrom Header Layout --
   ---------------------------
   --  2.8.3

   --  The first handful of words in the Bootrom are used to locate
   --  other useful content in the Bootrom.

   type ROM_ID_Array is array (0 .. 2) of Unsigned_8;

   type Bootrom_Header_Fields is record
      Initial_SP                    : System.Address;
      Reset_Handler                 : System.Address;
      NMI_Handler                   : System.Address;
      HardFault_Handler             : System.Address;
      ROM_ID                        : ROM_ID_Array;
      Version                       : Unsigned_8;
      Public_Func_Lookup_Table_Addr : Unsigned_16;
      Public_Data_Lookup_Table_Addr : Unsigned_16;
      ROM_Table_Lookup_Addr         : Unsigned_16;
   end record
      with Size => 26 * 8, Pack;

   Bootrom_Header : constant Bootrom_Header_Fields
     with Import,
     Address => System'To_Address (16#0000_0000#);

   ----------------------
   -- soft_float_table --
   ----------------------
   --  2.8.3.3

   type Soft_Float_Table_Fields is record
      FAdd_Address         : System.Address;
      FSub_Address         : System.Address;
      FMul_Address         : System.Address;
      FDiv_Address         : System.Address;
      Deprecated_1         : System.Address;
      Deprecated_2         : System.Address;
      FSqrt_Address        : System.Address;
      Float2Int_Address    : System.Address;
      Float2Fix_Address    : System.Address;
      Float2UInt_Address   : System.Address;
      Float2UFix_Address   : System.Address;
      Int2Float_Address    : System.Address;
      Fix2Float_Address    : System.Address;
      UInt2Float_Address   : System.Address;
      UFix2Float_Address   : System.Address;
      FCos_Address         : System.Address;
      FSin_Address         : System.Address;
      FTan_Address         : System.Address;
      Deprecated_3         : System.Address;
      FExp_Address         : System.Address;
      FLn_Address          : System.Address;

      --  Only present in V2 bootrom
      FCmp_Address         : System.Address;
      FATan2_Address       : System.Address;
      Int642Float_Address  : System.Address;
      Fix642Float_Address  : System.Address;
      UInt642Float_Address : System.Address;
      UFix642Float_Address : System.Address;
      Float2Int64_Address  : System.Address;
      Float2Fix64_Address  : System.Address;
      Float2UInt64_Address : System.Address;
      Float2UFix64_Address : System.Address;
      Float2Double_Address : System.Address;
   end record
     with Pack, Size => 16#80# * 8;

   ---------------------
   -- Cached pointers --
   ---------------------

   FAdd_Address : System.Address;
   FSub_Address : System.Address;
   FMul_Address : System.Address;
   FDiv_Address : System.Address;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Soft_Float_Table : constant Soft_Float_Table_Fields
        with Import => True,
        Address     => ROM_Data_Lookup (ROM_Table_Code ('S', 'F'));

   begin
      FAdd_Address := Soft_Float_Table.FAdd_Address;
      FSub_Address := Soft_Float_Table.FSub_Address;
      FMul_Address := Soft_Float_Table.FMul_Address;
      FDiv_Address := Soft_Float_Table.FDiv_Address;
   end Initialize;

   ----------------------
   -- ROM_Table_Lookup --
   ----------------------

   function ROM_Table_Lookup (Table : System.Address;
                              Code  : Unsigned_32) return System.Address
   is

      --  The address of the rom_table_lookup function in the Bootrom
      --  is stored in the Bootrom's header. This needs to be resolved
      --  dynamically after elaboration since No_Elaboration_Code_All
      --  is in effect.

      function Bootrom_ROM_Table_Lookup
        (Table : System.Address;
         Code  : Unsigned_32) return System.Address
      with Import => True,
      Convention  => C,
      Address     => System.Address (Bootrom_Header.ROM_Table_Lookup_Addr);

   begin
      return Bootrom_ROM_Table_Lookup (Table, Code);
   end ROM_Table_Lookup;

   ---------------------
   -- ROM_Func_Lookup --
   ---------------------

   function ROM_Func_Lookup (Code : Unsigned_32) return System.Address
   is
   begin
      return ROM_Table_Lookup
        (System.Address (Bootrom_Header.Public_Func_Lookup_Table_Addr), Code);
   end ROM_Func_Lookup;

   ---------------------
   -- ROM_Data_Lookup --
   ---------------------

   function ROM_Data_Lookup (Code : Unsigned_32) return System.Address
   is
   begin
      return ROM_Table_Lookup
        (System.Address (Bootrom_Header.Public_Data_Lookup_Table_Addr), Code);
   end ROM_Data_Lookup;

   ----------
   -- FAdd --
   ----------

   function FAdd (A, B : Float) return Float is

      function Bootrom_FAdd (A, B : Float) return Float
        with Import => True,
        Convention  => C,
        Address     => FAdd_Address;

   begin
      return Bootrom_FAdd (A, B);
   end FAdd;

   ----------
   -- FSub --
   ----------

   function FSub (A, B : Float) return Float is

      function Bootrom_FSub (A, B : Float) return Float
        with Import => True,
        Convention  => C,
        Address     => FSub_Address;

   begin
      return Bootrom_FSub (A, B);
   end FSub;

   ----------
   -- FMul --
   ----------

   function FMul (A, B : Float) return Float is

      function Bootrom_FMul (A, B : Float) return Float
        with Import => True,
        Convention  => C,
        Address     => FMul_Address;

   begin
      return Bootrom_FMul (A, B);
   end FMul;

   ----------
   -- FDiv --
   ----------

   function FDiv (A, B : Float) return Float is

      function Bootrom_FDiv (A, B : Float) return Float
        with Import => True,
        Convention  => C,
        Address     => FDiv_Address;

   begin
      return Bootrom_FDiv (A, B);
   end FDiv;

end System.Bootrom;
