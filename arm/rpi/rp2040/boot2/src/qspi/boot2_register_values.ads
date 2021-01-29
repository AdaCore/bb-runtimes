------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
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
with Interfaces;   use Interfaces;
with Flash_Config;

--  This package defines register values used to configure the SSI and
--  GPIO_QSPI pads to operate in QSPI mode.
package Boot2_Register_Values with
  No_Elaboration_Code_All
is

   --  SLEWFAST[0:0] = 1 (no slew limiting)
   --  SCHMITT[1:1]  = 0 (disable Schmitt trigger)
   --  DRIVE[5:4]    = 2 (8 mA drive strength)
   GPIO_QSPI_SCLK_Value : constant Unsigned_32 := 16#0000_0021#;

   --  OD[7:7]       = 0 (don't disable output)
   --  IE[6:6]       = 1 (enable input)
   --  DRIVE[5:4]    = 1 (4 mA drive strength)
   --  PUE[3:3]      = 0 (no pull up)
   --  PDE[2:2]      = 0 (no pull down)
   --  SCHMITT[1:1]  = 0 (disable Schmitt trigger)
   --  SLEWFAST[0:0] = 0 (slow)
   GPIO_QSPI_SD_Value : constant Unsigned_32 := 2#0101_0000#;

   --  SCKDIV[15:0] = 2 (/2 clock divider)
   BAUDR_Value : constant Unsigned_32 := Flash_Config.SSI_Clock_Divider;

   --  Configuration of CTRLR0 to configure the W25Q080
   --  DFS[20:16]     = 7 (32-bit data frame size)
   --  SPI_FRF[22:21] = 0  (Standard SPI frame format)
   --  TMOD[9:8]      = 0  (Tx/Rx read mode)
   CTRLR0_Value_For_Config : constant Unsigned_32 := 16#0007_0000#;

   --  Configuration of CTRLR0 to operate in XIP mode
   --  DFS[20:16]     = 31 (32-bit data frame size)
   --  SPI_FRF[22:21] = 2  (Quad SPI frame format)
   --  TMOD[9:8]      = 3  (EEPROM read mode)
   CTRLR0_Value_For_XIP : constant Unsigned_32 := 16#005F_0300#;

   --  Configuration of SPI_CTRLR0 to perform a dummy read.
   --  TRANS_TYPE[1:0]    = 1 (cmd byte uses std. SPI, addr uses FRF format)
   --  ADDR_L[5:2]        = 8
   --  INST_L[9:8]        = 2 (8-bit instruction)
   --  WAIT_CYCLES[15:11] = Flash_Config.Dummy_Cycles
   --  XIP_CMD[31:24]     = 0 (not used)
   SPI_CTRLR0_Value_For_Dummy_Read : constant Unsigned_32 :=
     16#0000_0221# + (Flash_Config.Fast_Read_Quad_IO_Dummy_Cycles * 2**11);

   --  Configuration of SPI_CTRLR0 to operate in XIP mode
   --  using the "continuous read" mode of the QSPI flash.
   --  TRANS_TYPE[1:0]    = 2 (command & address both use FRF format)
   --  ADDR_L[5:2]        = 8
   --  INST_L[9:8]        = 0 (no instruction)
   --  WAIT_CYCLES[15:11] = Flash_Config.Dummy_Cycles
   --  XIP_CMD[31:24]     = Flash_Config.Mode_Continuous_Read
   SPI_CTRLR0_Value_For_XIP : constant Unsigned_32 :=
     16#0000_0022#
     + (Flash_Config.Fast_Read_Quad_IO_Dummy_Cycles * 2**11)
     + (Flash_Config.Mode_Continuous_Read           * 2**24);

end Boot2_Register_Values;
