------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      Copyright (C) 2016-2021, AdaCore                    --
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
with Ada.Unchecked_Conversion;
with Interfaces;

--  This package defines the structures used to configure the MPU on ARM
--  Cortex-R or Cortex-M devices.

package System.MPU_Definitions is
   pragma No_Elaboration_Code_All;

   package I renames Interfaces;

   ---------------------------
   -- MPU table definitions --
   ---------------------------

   --  Region Size:

   type Region_Size is
     (Size_Null,
      Size_32B,
      Size_64B,
      Size_126B,
      Size_256B,
      Size_512B,
      Size_1KiB,
      Size_2KiB,
      Size_4KiB,
      Size_8KiB,
      Size_16KiB,
      Size_32KiB,
      Size_64KiB,
      Size_128KiB,
      Size_256KiB,
      Size_512KiB,
      Size_1MiB,
      Size_2MiB,
      Size_4MiB,
      Size_8MiB,
      Size_16MiB,
      Size_32MiB,
      Size_64MiB,
      Size_128MiB,
      Size_256MiB,
      Size_512MiB,
      Size_1GiB,
      Size_2GiB,
      Size_4GiB) with Size => 5;

   for Region_Size use
     (Size_Null   => 2#00000#,
      Size_32B    => 2#00100#,
      Size_64B    => 2#00101#,
      Size_126B   => 2#00110#,
      Size_256B   => 2#00111#,
      Size_512B   => 2#01000#,
      Size_1KiB   => 2#01001#,
      Size_2KiB   => 2#01010#,
      Size_4KiB   => 2#01011#,
      Size_8KiB   => 2#01100#,
      Size_16KiB  => 2#01101#,
      Size_32KiB  => 2#01110#,
      Size_64KiB  => 2#01111#,
      Size_128KiB => 2#10000#,
      Size_256KiB => 2#10001#,
      Size_512KiB => 2#10010#,
      Size_1MiB   => 2#10011#,
      Size_2MiB   => 2#10100#,
      Size_4MiB   => 2#10101#,
      Size_8MiB   => 2#10110#,
      Size_16MiB  => 2#10111#,
      Size_32MiB  => 2#11000#,
      Size_64MiB  => 2#11001#,
      Size_128MiB => 2#11010#,
      Size_256MiB => 2#11011#,
      Size_512MiB => 2#11100#,
      Size_1GiB   => 2#11101#,
      Size_2GiB   => 2#11110#,
      Size_4GiB   => 2#11111#);

   --  Executable bit (XN):

   type Access_Control_XN is
     (Execute_All,
      Execute_Never) with Size => 1;

   for Access_Control_XN use
     (Execute_All   => 0,
      Execute_Never => 1);

   --  Access Privilege bits (AP):

   type Access_Control_AP is
     (Privileged_NA_User_NA,
      Privileged_RW_User_NA,
      Privileged_RW_User_RO,
      Privileged_RW_User_RW,
      Privileged_RO_User_NA,
      Privileged_RO_User_RO) with Size => 3;

   for Access_Control_AP use
     (Privileged_NA_User_NA => 2#000#,
      Privileged_RW_User_NA => 2#001#,
      Privileged_RW_User_RO => 2#010#,
      Privileged_RW_User_RW => 2#011#,
      Privileged_RO_User_NA => 2#101#,
      Privileged_RO_User_RO => 2#110#);

   --  TEX S C B bits:

   type Access_Control_TEX_S_C_B is
     (Strongly_Ordered,
      Shareable_Device,
      Outer_Inner_WT_NWA_NS,
      Outer_Inner_WB_NWA_NS,
      Outer_Inner_WT_NWA_S,
      Outer_Inner_WB_NWA_S,
      Outer_Inner_NC_NS,
      Outer_Inner_WB_WA_NS,
      Outer_Inner_NC_S,
      Outer_Inner_WB_WA_S,
      Non_Shareable_Device) with Size => 6;

   for Access_Control_TEX_S_C_B use
     (Strongly_Ordered      => 2#000000#,
      Shareable_Device      => 2#000001#,
      Outer_Inner_WT_NWA_NS => 2#000010#,
      Outer_Inner_WB_NWA_NS => 2#000011#,
      Outer_Inner_WT_NWA_S  => 2#000110#,
      Outer_Inner_WB_NWA_S  => 2#000111#,
      Outer_Inner_NC_NS     => 2#001000#,
      Outer_Inner_WB_WA_NS  => 2#001011#,
      Outer_Inner_NC_S      => 2#001100#,
      Outer_Inner_WB_WA_S   => 2#001111#,
      Non_Shareable_Device  => 2#010000#);

   type Unsigned_1 is mod 2 ** 1 with Size => 1;
   type Unsigned_2 is mod 2 ** 2 with Size => 2;
   type Unsigned_19 is mod 2 ** 19 with Size => 19;

   type Size_And_Enable_Reg is record
      Sub_Region_Disable : I.Unsigned_8  := 0;
      Size               : Region_Size;
      Enable             : Boolean;
   end record with Size => 32;

   for Size_And_Enable_Reg use record
      Sub_Region_Disable at 0 range 8 .. 15;
      Size               at 0 range 1 .. 5;
      Enable             at 0 range 0 .. 0;
   end record;

   type Access_Control_Reg is record
      XN         : Access_Control_XN;
      AP         : Access_Control_AP;
      TEX_S_C_B  : Access_Control_TEX_S_C_B;
   end record with Size => 32;

   for Access_Control_Reg use record
      XN         at 0 range 12 .. 12;
      AP         at 0 range 8 .. 10;
      TEX_S_C_B  at 0 range 0 .. 5;
   end record;

   type MPU_Region_Configuration is record
      Base_Address    : I.Unsigned_32;
      Size_And_Enable : Size_And_Enable_Reg;
      Access_Control  : Access_Control_Reg;
   end record;

   function As_W32 is new Ada.Unchecked_Conversion
     (Size_And_Enable_Reg, I.Unsigned_32);
   function As_W32 is new Ada.Unchecked_Conversion
     (Access_Control_Reg, I.Unsigned_32);

   subtype MPU_Region_Index is I.Unsigned_32 range 0 .. 15;

   type MPU_Region_Config_Table is
     array (MPU_Definitions.MPU_Region_Index range <>) of
     MPU_Definitions.MPU_Region_Configuration;

end System.MPU_Definitions;
