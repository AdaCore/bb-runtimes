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

--  The SSI cannot be configured while enabled, so the boot ROM copies the
--  first 256 bytes of flash (referred to by the linker as .boot2) to SRAM and
--  executes it. This procedure should disable SSI, configure it for the flash
--  chip in use, re-enable SSI, then jump to the reset vector in XIP memory.
--
--  This is the generic version for all SPI flash chips that respond to the
--  0x03 read command.
with Boot2_Generic_03; use Boot2_Generic_03;
with Registers;        use Registers;

procedure Boot2 is
begin
   --  Disable SSI
   XIP_SSI.Periph.SSIENR := 0;

   --  Configure SSI
   XIP_SSI.Periph.BAUDR      := XIP_SSI_BAUDR_Value;
   XIP_SSI.Periph.CTRLR0     := XIP_SSI_CTRLR0_Value;
   XIP_SSI.Periph.SPI_CTRLR0 := XIP_SSI_SPI_CTRLR0_Value;
   XIP_SSI.Periph.CTRLR1     := 0;

   --  Enable SSI
   XIP_SSI.Periph.SSIENR := 1;
end Boot2;
