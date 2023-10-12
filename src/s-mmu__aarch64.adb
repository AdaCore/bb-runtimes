------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M . M M U                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2021-2022, AdaCore                    --
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
with System.Machine_Reset;
with Interfaces;

package body System.MMU is

   Set_NX : constant Boolean := True;
   --  Select if the data region should be non executable.

   Set_RO : constant Boolean := False;
   --  Select if the code region should be read only.

   type Level_2_Output_Address is mod 2 ** 27;

   type Level_2_Table_Address is mod 2 ** 36;

   type Level_3_Output_Address is mod 2 ** 36;

   type Attribute_Index is mod 2 ** 3;

   type Access_Permission is (Read_Write_EL1,
                              Read_Write_EL0,
                              Read_Only_EL1,
                              Read_Only_EL0) with
     Size => 2;

   for Access_Permission use (Read_Write_EL1 => 0,
                              Read_Write_EL0 => 1,
                              Read_Only_EL1  => 2,
                              Read_Only_EL0  => 3);

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

   type Level_2_Descriptor (Valid  : Boolean         := False;
                            D_Type : Descriptor_Type := Block) is record
      case Valid is
         when True =>
            case D_Type is
               when Block =>
                  Attr_Index    : Attribute_Index;
                  Non_Secure    : Boolean;
                  Data_Access   : Access_Permission;
                  Share         : Shareability;
                  Access_Flag   : Boolean;
                  Non_Global    : Boolean;
                  Block_Address : Level_2_Output_Address;
                  Contiguous    : Boolean;
                  PXN_Block     : Boolean;
                  XN_Block      : Boolean;
               when Table =>
                  Table_Address     : Level_2_Table_Address;
                  PXN_Table         : Boolean;
                  XN_Table          : Boolean;
                  Data_Access_Table : Access_Permission;
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
      Data_Access       at 0 range  6 ..  7;
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
                  Data_Access   : Access_Permission;
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
      Data_Access   at 0 range  6 ..  7;
      Share         at 0 range  8 ..  9;
      Access_Flag   at 0 range 10 .. 10;
      Non_Global    at 0 range 11 .. 11;
      Block_Address at 0 range 12 .. 47;
      Contiguous    at 0 range 52 .. 52;
      PXN_Block     at 0 range 53 .. 53;
      XN_Block      at 0 range 54 .. 54;
   end record;

   type Level_2_Table is array (Natural range 0 .. 511) of Level_2_Descriptor;

   type Level_3_Table is array (Natural range 0 .. 511) of Level_3_Descriptor;

   type Level_2_Tables is array (Natural range <>) of Level_2_Table;

   type Level_3_Tables is array (Natural range <>) of Level_3_Table;

   function Get_Level_1_Table return Address with
     Inline_Always;
   --  Retrieve the address of the level 1 translation table from the
   --  TTBR0_EL1 register.

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
      return Level_2_Table_Address is
     (Level_2_Table_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 12)));

   ------------------
   --  Get_Address --
   ------------------

   function Get_Address (Addr : Address)
      return Level_3_Output_Address is
     (Level_3_Output_Address
        (Interfaces.Shift_Right (Interfaces.Unsigned_64 (Addr), 12)));

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize
   is
      type Symbol is null record;
      Level_1                   : Level_2_Table with
                                    Import,
                                    Address => Get_Level_1_Table;
      Level_2                   : Level_2_Tables (0 .. 1) with
                                    Import,
                                    External_Name => "__mmu_ram_table_2_start";
      Level_3                   : Level_3_Tables (0 .. 1023) with
                                    Import,
                                    External_Name => "__mmu_ram_table_3_start";
      Start                     : Address := Address'First;
      Region_Data_Start         : Symbol with
                                    Import,
                                    External_Name => "__region_data_start";
      Region_Data_Start_Address : constant Address :=
                                    Region_Data_Start'Address;
      Region_Data_End           : Symbol with
                                    Import,
                                    External_Name => "__region_data_end";
      Region_Data_End_Address   : constant Address :=
                                    Region_Data_End'Address;
      Level_2_End               : Symbol with
                                    Import,
                                    External_Name => "__mmu_ram_table_2_end";
      Level_3_End               : Symbol with
                                    Import,
                                    External_Name => "__mmu_ram_table_3_end";
      CPU0_Stack_Start          : Symbol with
                                    Import,
                                    External_name => "__cpu0_stack_start";
      CPU0_Stack_End            : Symbol with
                                    Import,
                                    External_Name => "__cpu0_stack_end";
      CPU1_Stack_End            : Symbol with
                                    Import,
                                    External_Name => "__cpu1_stack_end";
      CPU2_Stack_End            : Symbol with
                                    Import,
                                    External_Name => "__cpu2_stack_end";
      CPU3_Stack_End            : Symbol with
                                    Import,
                                    External_Name => "__cpu3_stack_end";
      function Data_Access return Access_Permission is
        (if Set_RO and then Start < Region_Data_Start_Address
         then Read_Only_EL1
         else Read_Write_EL1);
      type Guard_Pages_Type is array (Natural range <>)
         of Level_3_Output_Address;
      Guard_Pages : constant Guard_Pages_Type :=
         (Get_Address (CPU0_Stack_Start'Address) - 1,
          Get_Address (CPU0_Stack_End'Address),
          Get_Address (CPU1_Stack_End'Address),
          Get_Address (CPU2_Stack_End'Address),
          Get_Address (CPU3_Stack_End'Address));
      function Is_Guard_Page (Addr : Level_3_Output_Address) return Boolean is
         (for some P of Guard_Pages => P = Addr);
   begin
      if
         Level_2_End'Address - Level_2'Address /= Level_2'Size / 8
         or else Level_3_End'Address - Level_3'Address /= Level_3'Size / 8
         or else Region_Data_Start_Address mod 4096 /= 0
         or else Region_Data_End_Address mod 4096 /= 0
      then
         --  Abort if the memory regions assigned to the translation tables
         --  do not fit their sizes or if the start of the data region is not
         --  page aligned.
         --  The abort is done by stopping through System.Machine_Reset.
         --  Raising an exception is not possible since the last chance
         --  handler on Embedded requires the secondary stack which
         --  is not set up yet.
         System.Machine_Reset.Stop;
      end if;

      for T of Level_3 loop
         for J in T'Range loop
            declare
               Current_Address : constant Level_3_Output_Address
                  := Get_Address (Start);
               NX : constant Boolean :=
                  Set_NX and Start >= Region_Data_Start_Address;
            begin
               if Is_Guard_Page (Current_Address)
                  or Start >= Region_Data_End_Address
               then
                  T (J) := Level_3_Descriptor'(Valid  => False,
                                               D_Type => Reserved);
               else
                  T (J) := Level_3_Descriptor'
                             (Valid         => True,
                              D_Type        => Page,
                              Attr_Index    => 0,
                              Non_Secure    => True,
                              Data_Access   => Data_Access,
                              Share         => Outer_Shareable,
                              Access_Flag   => True,
                              Non_Global    => False,
                              Block_Address => Current_Address,
                              Contiguous    => False,
                              PXN_Block     => NX,
                              XN_Block      => NX);
               end if;
            end;
            Start := Start + 4096;
         end loop;
      end loop;

      Start := Address'First;
      for J in Level_2'Range loop
         for K in Level_2 (J)'Range loop
            if Start < Region_Data_End_Address then
               Level_2 (J) (K) := Level_2_Descriptor'
                                    (Valid             => True,
                                     D_Type            => Table,
                                     Table_Address     =>
                                       Get_Address (Level_3
                                                      (J * 512 + K)'Address),
                                     PXN_Table         => False,
                                     XN_Table          => False,
                                     Data_Access_Table => Read_Write_EL1,
                                     Non_Secure_Table  => True);
            else
               Level_2 (J) (K) := Level_2_Descriptor'(Valid  => False,
                                                      D_Type => Block);
            end if;
            Start := Start + 512 * 4096;
         end loop;
      end loop;

      Start := Address'First;
      for J in 0 .. 1 loop
         if Start < Region_Data_End_Address then
            Level_1 (J) := Level_2_Descriptor'
                             (Valid             => True,
                              D_Type            => Table,
                              Table_Address     =>
                                Get_Address (Level_2 (J)'Address),
                              PXN_Table         => False,
                              XN_Table          => False,
                              Data_Access_Table => Read_Write_EL1,
                              Non_Secure_Table  => True);
         else
            Level_1 (J) := Level_2_Descriptor'(Valid  => False,
                                               D_Type => Block);
         end if;
         Start := Start + 4096 * 512 ** 2;
      end loop;
   end Initialize;

end System.MMU;
