------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M . M M U                           --
--                                                                          --
--                                 B o d y                                  --
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

with System.Machine_Code;
with Interfaces;

package body System.MMU is

   ------------------------
   --  Get_Level_1_Table --
   ------------------------

   function Get_Level_1_Table return Address
   is
      Level_1_Table : Address;
   begin
      System.Machine_Code.Asm ("mrs %0, ttbr0_el1",
                               Outputs =>  Address'Asm_Output
                                              ("=r", Level_1_Table),
                               Volatile => True);
      return Level_1_Table;
   end Get_Level_1_Table;

   ------------------
   --  Get_Address --
   ------------------

   function Get_Address (Addr : Address)
      return Level_1_Output_Address is
     (Level_1_Output_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 30)));

   function Get_Address (Addr : Address)
      return Level_1_Table_Address is
     (Level_1_Table_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 12)));

   function Get_Address (Addr : Address)
      return Level_2_Output_Address is
     (Level_2_Output_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 21)));

   function Get_Address (Addr : Address)
      return Level_3_Output_Address is
     (Level_3_Output_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 12)));

   procedure Set_MAIR (Attrs : Memory_Attribute_Indirection_Register)
   is
   begin
      System.Machine_Code.Asm
         ("msr mair_el1, %0",
          Inputs =>
            Memory_Attribute_Indirection_Register'Asm_Input ("r", Attrs),
          Volatile => True);
   end Set_MAIR;

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize
   is
      Level_1           : Level_1_Table with
                            Import,
                            Address => Get_Level_1_Table;
      Start             : Address := Address'First;
      Memory_Attributes : constant Memory_Attribute_Indirection_Register :=
         (1 => Device_nGnRnE, others => Normal_Memory);
   begin
      --  0x00000000
      Start := Address'First;
      Level_1.Entries (0) := Level_1_Descriptor'
         (Valid         => True,
          D_Type        => Block,
          Attr_Index    => 1,
          Non_Secure    => True,
          Access_Perm   => Priv_RW_Unpriv_RW,
          Share         => Outer_Shareable,
          Access_Flag   => True,
          Non_Global    => False,
          Block_Address => Get_Address (Start),
          Contiguous    => True,
          PXN_Block     => True,
          XN_Block      => True);

      --  0x40000000
      Start := Start + 1024 ** 3;
      Level_1.Entries (1) := Level_1_Descriptor'
         (Valid         => True,
          D_Type        => Block,
          Attr_Index    => 1,
          Non_Secure    => True,
          Access_Perm   => Priv_RW_Unpriv_RW,
          Share         => Outer_Shareable,
          Access_Flag   => True,
          Non_Global    => False,
          Block_Address => Get_Address (Start),
          Contiguous    => True,
          PXN_Block     => True,
          XN_Block      => True);

      --  0x80000000
      Start := Start + 1024 ** 3;
      Level_1.Entries (2) := Level_1_Descriptor'
         (Valid         => True,
          D_Type        => Block,
          Attr_Index    => 0,
          Non_Secure    => True,
          Access_Perm   => Priv_RW,
          Share         => Inner_Shareable,
          Access_Flag   => True,
          Non_Global    => False,
          Block_Address => Get_Address (Start),
          Contiguous    => True,
          PXN_Block     => False,
          XN_Block      => False);

      --  0xc0000000
      Start := Start + 1024 ** 3;
      Level_1.Entries (3) := Level_1_Descriptor'
         (Valid         => True,
          D_Type        => Block,
          Attr_Index    => 0,
          Non_Secure    => True,
          Access_Perm   => Priv_RW,
          Share         => Inner_Shareable,
          Access_Flag   => True,
          Non_Global    => False,
          Block_Address => Get_Address (Start),
          Contiguous    => True,
          PXN_Block     => False,
          XN_Block      => False);

      Set_MAIR (Memory_Attributes);

      Initialize_Post (Level_1);
   end Initialize;

end System.MMU;
