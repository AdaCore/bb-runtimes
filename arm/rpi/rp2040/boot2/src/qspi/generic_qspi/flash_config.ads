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

--  This configuration is compatible with most flash devices, except for
--  legacy Winbond devices (e.g. W25Q80DV). The following devices are
--  supported:
--    AT25SF128A
--    GD25Q64C
--    IS25LP128F
--    W25Q16JV
--    W25Q32JV
--    W25Q64JV
--    W25Q128JV
package Flash_Config with
  No_Elaboration_Code_All
is

   --  Use the generic procedure to check and set the QE bit.
   Use_Winbond_QE_Write_Procedure : constant Boolean := False;

   --  Configure max. SCLK speed of 62.5 MHz (133 MHz / 2).
   SSI_Clock_Divider : constant Unsigned_32 := 2;

   --  Number of dummy cycles for the Fast Read Quad I/O instruction.
   Fast_Read_Quad_IO_Dummy_Cycles : constant := 4;

   --  Value for the M[7:0] bits in the "Fast Read Quad I/O" command
   --  to put the device into "continuous read" mode
   Mode_Continuous_Read : constant Unsigned_32 := 16#A0#;

end Flash_Config;
