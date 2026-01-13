------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M . M M U                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2021-2025, AdaCore                    --
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

package System.MMU with
   No_Elaboration_Code_All
is

   --  The types and tables are defined in the Arm Architecture Reference
   --  Manual for A-profile architecture (document number: ARM DDI 0487).
   --  This implementation assumes an output address size of 48 bits.

   type Level_1_Output_Address is mod 2 ** 18;

   type Level_2_Output_Address is mod 2 ** 27;

   type Level_1_Table_Address is mod 2 ** 36;

   type Level_3_Output_Address is mod 2 ** 36;

   type Attribute_Index is mod 2 ** 3;

   type Stage_1_Access_Permission is (Priv_RW,
                                      Priv_RW_Unpriv_RW,
                                      Priv_R,
                                      Priv_R_Unpriv_R) with
      Size => 2;

   for Stage_1_Access_Permission use (Priv_RW           => 0,
                                      Priv_RW_Unpriv_RW => 1,
                                      Priv_R            => 2,
                                      Priv_R_Unpriv_R   => 3);

   type Table_Access_Permission is (No_Effect,
                                    Remove_Unpriv_RW,
                                    Remove_Priv_W_Unpriv_W,
                                    Remove_Priv_W_Unpriv_RW) with
      Size => 2;

   for Table_Access_Permission use (No_Effect               => 0,
                                    Remove_Unpriv_RW        => 1,
                                    Remove_Priv_W_Unpriv_W  => 2,
                                    Remove_Priv_W_Unpriv_RW => 3);

   type Shareability is (Non_Shareable,
                         Unpredictable,
                         Outer_Shareable,
                         Inner_Shareable) with
      Size => 2;

   for Shareability use (Non_Shareable   => 0,
                         Unpredictable   => 1,
                         Outer_Shareable => 2,
                         Inner_Shareable => 3);

   type Descriptor_Type is (Block, Table) with
     Size => 1;

   for Descriptor_Type use (Block => 0, Table => 1);

   type Page_Bit is (Reserved, Page) with
     Size => 1;

   for Page_Bit use (Reserved => 0, Page => 1);

   type Level_1_Descriptor (Valid  : Boolean         := False;
                            D_Type : Descriptor_Type := Block) is record
      case Valid is
         when True =>
            case D_Type is
               when Block =>
                  Attr_Index    : Attribute_Index;
                  Non_Secure    : Boolean;
                  Access_Perm   : Stage_1_Access_Permission;
                  Share         : Shareability;
                  Access_Flag   : Boolean;
                  Non_Global    : Boolean;
                  Block_Address : Level_1_Output_Address;
                  Contiguous    : Boolean;
                  PXN_Block     : Boolean;
                  XN_Block      : Boolean;
               when Table =>
                  Table_Address     : Level_1_Table_Address;
                  PXN_Table         : Boolean;
                  XN_Table          : Boolean;
                  Data_Access_Table : Table_Access_Permission;
                  Non_Secure_Table  : Boolean;
            end case;
         when False =>
            null;
      end case;
   end record with
     Size => 64;

   for Level_1_Descriptor use record
      Valid             at 0 range  0 ..  0;
      D_Type            at 0 range  1 ..  1;
      Attr_Index        at 0 range  2 ..  4;
      Non_Secure        at 0 range  5 ..  5;
      Access_Perm       at 0 range  6 ..  7;
      Share             at 0 range  8 ..  9;
      Access_Flag       at 0 range 10 .. 10;
      Non_Global        at 0 range 11 .. 11;
      Block_Address     at 0 range 30 .. 47;
      Contiguous        at 0 range 52 .. 52;
      PXN_Block         at 0 range 53 .. 53;
      XN_Block          at 0 range 54 .. 54;
      Table_Address     at 0 range 12 .. 47;
      PXN_Table         at 0 range 59 .. 59;
      XN_Table          at 0 range 60 .. 60;
      Data_Access_Table at 0 range 61 .. 62;
      Non_Secure_Table  at 0 range 63 .. 63;
   end record;

   type Level_2_Descriptor (Valid  : Boolean         := False;
                            D_Type : Descriptor_Type := Block) is record
      case Valid is
         when True =>
            case D_Type is
               when Block =>
                  Attr_Index    : Attribute_Index;
                  Non_Secure    : Boolean;
                  Access_Perm   : Stage_1_Access_Permission;
                  Share         : Shareability;
                  Access_Flag   : Boolean;
                  Non_Global    : Boolean;
                  Block_Address : Level_2_Output_Address;
                  Contiguous    : Boolean;
                  PXN_Block     : Boolean;
                  XN_Block      : Boolean;
               when Table =>
                  Table_Address     : Level_1_Table_Address;
                  PXN_Table         : Boolean;
                  XN_Table          : Boolean;
                  Data_Access_Table : Table_Access_Permission;
                  Non_Secure_Table  : Boolean;
            end case;
         when False =>
            null;
      end case;
   end record with
     Size => 64;

   for Level_2_Descriptor use record
      Valid             at 0 range  0 ..  0;
      D_Type            at 0 range  1 ..  1;
      Attr_Index        at 0 range  2 ..  4;
      Non_Secure        at 0 range  5 ..  5;
      Access_Perm       at 0 range  6 ..  7;
      Share             at 0 range  8 ..  9;
      Access_Flag       at 0 range 10 .. 10;
      Non_Global        at 0 range 11 .. 11;
      Block_Address     at 0 range 21 .. 47;
      Contiguous        at 0 range 52 .. 52;
      PXN_Block         at 0 range 53 .. 53;
      XN_Block          at 0 range 54 .. 54;
      Table_Address     at 0 range 12 .. 47;
      PXN_Table         at 0 range 59 .. 59;
      XN_Table          at 0 range 60 .. 60;
      Data_Access_Table at 0 range 61 .. 62;
      Non_Secure_Table  at 0 range 63 .. 63;
   end record;

   type Level_3_Descriptor (Valid  : Boolean  := False;
                            D_Type : Page_Bit := Reserved) is record
      case Valid is
         when True =>
            case D_Type is
               when Page =>
                  Attr_Index    : Attribute_Index;
                  Non_Secure    : Boolean;
                  Access_Perm   : Stage_1_Access_Permission;
                  Share         : Shareability;
                  Access_Flag   : Boolean;
                  Non_Global    : Boolean;
                  Block_Address : Level_3_Output_Address;
                  Contiguous    : Boolean;
                  PXN_Block     : Boolean;
                  XN_Block      : Boolean;
               when Reserved =>
                  null;
            end case;
         when False =>
            null;
      end case;
   end record with
     Size => 64;

   for Level_3_Descriptor use record
      Valid         at 0 range  0 ..  0;
      D_Type        at 0 range  1 ..  1;
      Attr_Index    at 0 range  2 ..  4;
      Non_Secure    at 0 range  5 ..  5;
      Access_Perm   at 0 range  6 ..  7;
      Share         at 0 range  8 ..  9;
      Access_Flag   at 0 range 10 .. 10;
      Non_Global    at 0 range 11 .. 11;
      Block_Address at 0 range 12 .. 47;
      Contiguous    at 0 range 52 .. 52;
      PXN_Block     at 0 range 53 .. 53;
      XN_Block      at 0 range 54 .. 54;
   end record;

   type Level_1_Array is array (Natural range <>) of Level_1_Descriptor;

   type Level_2_Array is array (Natural range <>) of Level_2_Descriptor;

   type Level_3_Array is array (Natural range <>) of Level_3_Descriptor;

   type Level_1_Table is limited record
      Entries : Level_1_Array (0 .. 511);
   end record with
      Pack,
      Size => 32768;

   type Level_2_Table is limited record
      Entries : Level_2_Array (0 .. 511);
   end record with
      Pack,
      Size => 32768;

   type Level_3_Table is limited record
      Entries : Level_3_Array (0 .. 511);
   end record with
      Pack,
      Size => 32768;

   type Level_2_Tables is array (Natural range <>) of Level_2_Table;

   type Level_3_Tables is array (Natural range <>) of Level_3_Table;

   type Memory_Attribute_Indirection_Field  is mod 2 ** 8 with
      Size => 8;

   type Memory_Attribute_Indirection_Register is array
      (Attribute_Index'Range) of Memory_Attribute_Indirection_Field with
      Size => 64;

   Device_nGnRnE : constant Memory_Attribute_Indirection_Field := 16#00#;

   Normal_Memory : constant Memory_Attribute_Indirection_Field := 16#FF#;

   function Get_Level_1_Table return Address with
     Inline_Always;
   --  Retrieve the address of the level 1 translation table from the
   --  TTBR0_EL1 register.

   function Get_Address (Addr : Address) return Level_1_Output_Address with
      Inline_Always;

   function Get_Address (Addr : Address) return Level_1_Table_Address with
      Inline_Always;

   function Get_Address (Addr : Address) return Level_2_Output_Address with
      Inline_Always;

   function Get_Address (Addr : Address) return Level_3_Output_Address with
      Inline_Always;

   procedure Set_MAIR (Attrs : Memory_Attribute_Indirection_Register);

   procedure Initialize with
      Export,
      Convention     => C,
      External_Name  => "__initialize_mmu";

   procedure Initialize_Post (Level_1 : Level_1_Table) with
      Import,
      Convention    => C,
      External_Name => "__initialize_mmu_post";

end System.MMU;
