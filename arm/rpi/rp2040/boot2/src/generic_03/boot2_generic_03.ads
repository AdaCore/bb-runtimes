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
with Interfaces; use Interfaces;

package Boot2_Generic_03 with
  No_Elaboration_Code_All
is

   --  Be conservative with the divider to ensure that the max. SCLK
   --  frequency does not exceed the limit for the 03h read command.
   --  For example, the W25Q080 has a max. speed of 50 MHz.
   --  SCKDIV[15:0] = 4 (/4 clock divider)
   XIP_SSI_BAUDR_Value : constant Unsigned_32 := 16#0000_0004#;

   --  DFS[20:16]     = 31 (32-bit data frame size)
   --  SPI_FRF[22:21] = 0  (standard 1-bit SPI frame format)
   --  TMOD[9:8]      = 3  (EEPROM read mode)
   XIP_SSI_CTRLR0_Value : constant Unsigned_32 := 16#001F_0300#;

   --  TRANS_TYPE[1:0]    = 0 (1C1A mode)
   --  ADDR_L[5:2]        = 6 (address bits divided by 4)
   --  INST_L[9:8]        = 2 (8-bit instruction)
   --  WAIT_CYCLES[15:11] = 0
   --  XIP_CMD[31:24]     = 16#03# (SPI read command)
   XIP_SSI_SPI_CTRLR0_Value : constant Unsigned_32 := 16#0300_0218#;

end Boot2_Generic_03;
