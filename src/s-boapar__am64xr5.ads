------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B O A R D _ P A R A M E T E R S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2023, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines basic parameters used by the non tasking part of
--  the runtime. It contains MPU configuration.

--  This is the TI AM64x/AM243x Cortex-R5 version of this package using the
--  following MPU layout:
--    * ATCM (first 32KB at 16#0000_0000#)
--    * BTCM (first 32KB at 16#4101_0000#)
--    * MSRAM (full 2MB non shared access)
--    * DDR (full 2GB non shared access)
--    * Background region read/write accessable

with System.MPU_Definitions;

package System.Board_Parameters is
   pragma No_Elaboration_Code_All;

   package MD renames System.MPU_Definitions;

   MPU_Config : constant System.MPU_Definitions.MPU_Region_Config_Table :=
     (
      --  Background region (access to most MAIN peripherals via memory bus)

      0 => (Base_Address    => 16#0000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_2GiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RO,
               TEX_S_C_B          => MD.Strongly_Ordered)),

      --  ATCM (configured for dual core mode)

      1 => (Base_Address => 16#0000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_32KiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RW_User_RO,
               TEX_S_C_B          => MD.Outer_Inner_NC_NS)),

      --  MAIN peripherals (via VBUSP peripheral port)

      2 => (Base_Address    => 16#2000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_64MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RO,
               TEX_S_C_B          => MD.Strongly_Ordered)),

      --  BTCM (configured for dual core mode)

      3 => (Base_Address => 16#4101_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_32KiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RW_User_RO,
               TEX_S_C_B          => MD.Outer_Inner_NC_NS)),

      --  OSPI (FLASH) Memory. For production change the address and size to
      --  match the memory region the application should have access to.

      4 => (Base_Address => 16#6000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_256MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RO_User_RO,
               TEX_S_C_B          => MD.Outer_Inner_WB_WA_NS)),

      --  MSRAM

      5 => (Base_Address => 16#7000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_2MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RW_User_RO,
               TEX_S_C_B          => MD.Outer_Inner_WB_WA_NS)),

      --  DDR

      6 => (Base_Address => 16#8000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_2GiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device))
     );
end System.Board_Parameters;
