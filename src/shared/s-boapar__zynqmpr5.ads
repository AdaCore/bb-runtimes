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
--                     Copyright (C) 2003-2021, AdaCore                     --
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
--  the runtime.

--  This is the ZynqMP Cortex-R5 (ARMv7-R) version of this package

with System.MPU_Definitions;

package System.Board_Parameters is
   pragma No_Elaboration_Code_All;

   package MD renames System.MPU_Definitions;

   MPU_Config : constant System.MPU_Definitions.MPU_Region_Config_Table :=
     (
      --  Background region
      0 => (Base_Address    => 16#0000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 2#1111_1111#,
               Size               => MD.Size_4GiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_NA_User_NA,
               TEX_S_C_B          => MD.Strongly_Ordered)),
      --  DDR
      1 => (Base_Address => 16#0000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_1GiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Outer_Inner_WB_WA_NS)),
      --  Strongly ordered memory for PL interface
      2 => (Base_Address => 16#8000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_1GiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Strongly_Ordered)),
      --  QSPI
      3 => (Base_Address => 16#C000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_512MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_All,
               AP                 => MD.Privileged_RO_User_RO,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  PCIe Low
      4 => (Base_Address => 16#E000_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_256MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  STM_CORESIGHT
      5 => (Base_Address => 16#F800_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_16MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  RPU_A53_GIC
      6 => (Base_Address => 16#F900_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_1MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  FPS slaves
      7 => (Base_Address => 16#FD00_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_16MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  Upper LPS slaves
      8 => (Base_Address => 16#FE00_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_16MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  Lower LPS slaves
      9 => (Base_Address => 16#FF00_0000#,
            Size_And_Enable =>
              (Sub_Region_Disable => 0,
               Size               => MD.Size_16MiB,
               Enable             => True),
            Access_Control  =>
              (XN                 => MD.Execute_Never,
               AP                 => MD.Privileged_RW_User_RW,
               TEX_S_C_B          => MD.Non_Shareable_Device)),
      --  OCM Ram
      10 => (Base_Address => 16#FFFC_0000#,
             Size_And_Enable =>
               (Sub_Region_Disable => 0,
                Size               => MD.Size_256KiB,
                Enable             => True),
             Access_Control  =>
               (XN                 => MD.Execute_Never,
                AP                 => MD.Privileged_RW_User_RW,
                TEX_S_C_B          => MD.Outer_Inner_WB_WA_NS))
     );

end System.Board_Parameters;
