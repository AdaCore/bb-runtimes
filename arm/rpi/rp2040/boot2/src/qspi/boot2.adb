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
--  executes it. This procedure disables SSI, configures it for the flash
--  chip in use, re-enables SSI, then jump to the reset vector in XIP memory.
--
--  This is the generic version for QSPI flashes. It reads the flash chip's
--  status register to check if the "Quad Enable" (QE) bit is set, then
--  performs the status register programming cycle if necessary.
--  It then configures the SSI to perform the EBh "Fast Read Quad I/O" command,
--  with the "continuous read" mode bits set so that subsequent reads will not
--  need the EBh command prefix.
with Interfaces;            use Interfaces;
with Boot2_Register_Values; use Boot2_Register_Values;
with Flash_Config;
with Registers;             use Registers;

procedure Boot2 is

   --  Flash instructions common to all supported devices.
   CMD_Write_Status_Register_1 : constant Unsigned_32 := 16#01#;
   CMD_Write_Status_Register_2 : constant Unsigned_32 := 16#31#;
   CMD_Read_Status_Register_1  : constant Unsigned_32 := 16#05#;
   CMD_Read_Status_Register_2  : constant Unsigned_32 := 16#35#;
   CMD_Write_Enable            : constant Unsigned_32 := 16#06#;
   CMD_Fast_Read_Quad_IO       : constant Unsigned_32 := 16#EB#;
   CMD_Read_JEDEC_ID           : constant Unsigned_32 := 16#9F#;

   --  BUSY flag mask in Status Register 1
   --  Some devices call this Write In Progress (WIP), but it's always located
   --  as bit 0 in Status Register 1 on all supported devices.
   SR1_BUSY_Mask : constant Unsigned_32 := 2#0000_0001#;

   --  Depending on the manufacturer, the "Quad Enable" (QE) bit can
   --  be in Status Register 1 or 2.
   SR1_QE_Mask : constant Unsigned_32 := 2#0100_0000#;
   SR2_QE_Mask : constant Unsigned_32 := 2#0000_0010#;

   --  Manufacturer IDs
   Manufacturer_ISSI : constant Unsigned_32 := 16#9D#;

   procedure Finish_SSI_Transaction (Rx_Byte_2 : out Unsigned_32);
   procedure Check_And_Set_QE_Bit_Generic with Inline_Always;
   procedure Check_And_Set_QE_Bit_Winbond with Inline_Always;
   function Read_Register (Command : Unsigned_32) return Unsigned_32;

   ----------------------------
   -- Finish_SSI_Transaction --
   ----------------------------

   --  This procedure waits for the SSI transaction to complete
   --  (SR.TFE = 1 and SR.BUSY = 0), then empties the FIFO and
   --  outputs the 2nd byte from the FIFO (this is usually the
   --  first received byte after the instruction byte).
   --
   --  Note that this assumes that the entire transaction was no more
   --  than 3 bytes, otherwise the Rx FIFO will not be completely emptied.

   procedure Finish_SSI_Transaction (Rx_Byte_2 : out Unsigned_32) is
      Mask : constant Unsigned_32 := XIP_SSI.SR_TFE_Mask or
                                     XIP_SSI.SR_BUSY_Mask;
      Dummy : Unsigned_32;
   begin
      loop
         --  Exit when BUSY = 0 and TFE = 1
         exit when (XIP_SSI.Periph.SR and Mask) = XIP_SSI.SR_TFE_Mask;
      end loop;

      --  Empty the FIFO
      Dummy     := XIP_SSI.Periph.DR0; --  Discard first Rx byte
      Rx_Byte_2 := XIP_SSI.Periph.DR0; --  Save second Rx byte
      Dummy     := XIP_SSI.Periph.DR0; --  Discard third Rx byte
   end Finish_SSI_Transaction;

   ----------------------------------
   -- Check_And_Set_QE_Bit_Generic --
   ----------------------------------

   --  This is the generic procedure to check and write the QE bit.
   --
   --  This is compatible with any of the supported flash devices, except the
   --  older Winbond devices (e.g. W25Q80DV). Note that newer Winbond
   --  devices (e.g. W25Q16JV) are compatible with this implementation.

   procedure Check_And_Set_QE_Bit_Generic
   is
      Dummy           : Unsigned_32;
      Manufacturer_ID : Unsigned_32;

      Read_SR_Cmd  : Unsigned_32;
      Write_SR_Cmd : Unsigned_32;
      SR_QE_Mask   : Unsigned_32;

   begin

      --  Read the JEDEC ID to determine which register the "Quad Enable" (QE)
      --  bit is in.
      --
      --  ISSI devices have the QE bit as bit 2 of Status Register 1.
      --  All other supported devices have the QE bit as bit 6 of
      --  Status Register 2.

      Manufacturer_ID := Read_Register (CMD_Read_JEDEC_ID);
      if Manufacturer_ID = Manufacturer_ISSI then
         Read_SR_Cmd  := CMD_Read_Status_Register_1;
         Write_SR_Cmd := CMD_Write_Status_Register_1;
         SR_QE_Mask   := SR1_QE_Mask;
      else
         Read_SR_Cmd  := CMD_Read_Status_Register_2;
         Write_SR_Cmd := CMD_Write_Status_Register_2;
         SR_QE_Mask   := SR2_QE_Mask;
      end if;

      --  Read the QE bit and check if it is already set.
      if (Read_Register (Read_SR_Cmd) and SR_QE_Mask) = 0 then

         --  Send Write Enable command (WREN)
         XIP_SSI.Periph.DR0 := CMD_Write_Enable;
         Finish_SSI_Transaction (Dummy);

         --  Send the "Write Status Register" command to set the QE bit.
         XIP_SSI.Periph.DR0 := Write_SR_Cmd;
         XIP_SSI.Periph.DR0 := SR_QE_Mask;
         Finish_SSI_Transaction (Dummy);

         --  Poll Status Register 1 until the BUSY flag is cleared.
         loop
            exit when (Read_Register (CMD_Read_Status_Register_1)
                       and SR1_BUSY_Mask) = 0;
         end loop;
      end if;
   end Check_And_Set_QE_Bit_Generic;

   ----------------------------------
   -- Check_And_Set_QE_Bit_Winbond --
   ----------------------------------

   --  This is the procedure to check and set the QE bit for Winbond devices.
   --
   --  Older Winbond devices have the QE bit in Status Register 2, but don't
   --  support the "write status register 2" (31h) instruction.
   --  So be compatible with these parts we write to SR2 via a continuous write
   --  to SR1 which requires a slightly longer command sequence.
   --
   --  Note that newer Winbond parts (e.g. the W25Q16JV) support this method,
   --  but non-Winbond devices (e.g. AT25S128A) do not.

   procedure Check_And_Set_QE_Bit_Winbond
   is
      Dummy : Unsigned_32;

   begin
      --  Read the QE bit and check if it is already set.
      if (Read_Register (CMD_Read_Status_Register_2) and SR2_QE_Mask) = 0 then

         --  Send Write Enable command (WREN)
         XIP_SSI.Periph.DR0 := CMD_Write_Enable;
         Finish_SSI_Transaction (Dummy);

         --  Send the "Write Status Register" command.
         --  Note that we can turn the "Read Status Register" command into
         --  its write equivalent by subtracting 4.
         XIP_SSI.Periph.DR0 := CMD_Write_Status_Register_1;
         XIP_SSI.Periph.DR0 := 0;           -- SR1 value
         XIP_SSI.Periph.DR0 := SR2_QE_Mask; -- SR2 value
         Finish_SSI_Transaction (Dummy);

         --  Poll the Status Register until the BUSY flag is cleared.
         loop
            exit when (Read_Register (CMD_Read_Status_Register_1)
                       and SR1_BUSY_Mask) = 0;
         end loop;
      end if;
   end Check_And_Set_QE_Bit_Winbond;

   -------------------
   -- Read_Register --
   -------------------

   --  This sends a SPI command sequence to read a single byte register.

   function Read_Register (Command : Unsigned_32) return Unsigned_32 is
      Result : Unsigned_32;
   begin
      XIP_SSI.Periph.DR0 := Command;
      XIP_SSI.Periph.DR0 := 0; --  Dummy byte
      Finish_SSI_Transaction (Result);
      return Result;
   end Read_Register;

   Temp : Unsigned_32;

begin
   --  Configure QSPI pads
   PADS_QSPI.Periph.GPIO_QSPI_SCLK := GPIO_QSPI_SCLK_Value;
   PADS_QSPI.Periph.GPIO_QSPI_SD0  := GPIO_QSPI_SD_Value;
   PADS_QSPI.Periph.GPIO_QSPI_SD1  := GPIO_QSPI_SD_Value;
   PADS_QSPI.Periph.GPIO_QSPI_SD2  := GPIO_QSPI_SD_Value;
   PADS_QSPI.Periph.GPIO_QSPI_SD3  := GPIO_QSPI_SD_Value;

   --  Disable SSI
   XIP_SSI.Periph.SSIENR := 0;

   --  Basic SSI configuration
   XIP_SSI.Periph.BAUDR         := BAUDR_Value;
   XIP_SSI.Periph.RX_SAMPLE_DLY := 1;

   --  Configure the SSI to send 8-bit commands to read & write the
   --  flash chip's registers in standard SPI mode.
   XIP_SSI.Periph.CTRLR0 := CTRLR0_Value_For_Config;
   XIP_SSI.Periph.SSIENR := 1;

   --  Check if the the flash is already in QSPI mode.
   --  If not, then send the commands needed to put it into QSPI mode.
   if Flash_Config.Use_Winbond_QE_Write_Procedure then
      Check_And_Set_QE_Bit_Winbond;
   else
      Check_And_Set_QE_Bit_Generic;
   end if;

   --  Disable again so we can reconfigure SSI
   XIP_SSI.Periph.SSIENR := 0;

   --  Configure SSI to do a dummy read.
   XIP_SSI.Periph.CTRLR0     := CTRLR0_Value_For_XIP;
   XIP_SSI.Periph.CTRLR1     := 0;
   XIP_SSI.Periph.SPI_CTRLR0 := SPI_CTRLR0_Value_For_Dummy_Read;

   --  Send command to do a dummy read.
   --  This dummy read puts the flash into "continous read" mode and means
   --  that the read command does not need to be sent for each subsequent read.
   XIP_SSI.Periph.SSIENR := 1;
   XIP_SSI.Periph.DR0    := CMD_Fast_Read_Quad_IO;
   XIP_SSI.Periph.DR0    := Flash_Config.Mode_Continuous_Read;
   Finish_SSI_Transaction (Temp);
   XIP_SSI.Periph.SSIENR := 0;

   pragma Unreferenced (Temp);

   --  Configure SSI for XIP mode
   XIP_SSI.Periph.SPI_CTRLR0 := SPI_CTRLR0_Value_For_XIP;
   XIP_SSI.Periph.SSIENR     := 1;
end Boot2;
