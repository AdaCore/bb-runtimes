--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAM3X8E.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  High Speed MultiMedia Card Interface
package Interfaces.SAM3x8e.HSMCI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype HSMCI_CR_MCIEN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CR_MCIDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CR_PWSEN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CR_PWSDIS_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CR_SWRST_Field is Interfaces.SAM3x8e.Bit;

   --  Control Register
   type HSMCI_CR_Register is record
      --  Write-only. Multi-Media Interface Enable
      MCIEN         : HSMCI_CR_MCIEN_Field := 16#0#;
      --  Write-only. Multi-Media Interface Disable
      MCIDIS        : HSMCI_CR_MCIDIS_Field := 16#0#;
      --  Write-only. Power Save Mode Enable
      PWSEN         : HSMCI_CR_PWSEN_Field := 16#0#;
      --  Write-only. Power Save Mode Disable
      PWSDIS        : HSMCI_CR_PWSDIS_Field := 16#0#;
      --  unspecified
      Reserved_4_6  : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Write-only. Software Reset
      SWRST         : HSMCI_CR_SWRST_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM3x8e.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_CR_Register use record
      MCIEN         at 0 range 0 .. 0;
      MCIDIS        at 0 range 1 .. 1;
      PWSEN         at 0 range 2 .. 2;
      PWSDIS        at 0 range 3 .. 3;
      Reserved_4_6  at 0 range 4 .. 6;
      SWRST         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype HSMCI_MR_CLKDIV_Field is Interfaces.SAM3x8e.Byte;
   subtype HSMCI_MR_PWSDIV_Field is Interfaces.SAM3x8e.UInt3;
   subtype HSMCI_MR_RDPROOF_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_MR_WRPROOF_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_MR_FBYTE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_MR_PADV_Field is Interfaces.SAM3x8e.Bit;

   --  Mode Register
   type HSMCI_MR_Register is record
      --  Clock Divider
      CLKDIV         : HSMCI_MR_CLKDIV_Field := 16#0#;
      --  Power Saving Divider
      PWSDIV         : HSMCI_MR_PWSDIV_Field := 16#0#;
      RDPROOF        : HSMCI_MR_RDPROOF_Field := 16#0#;
      WRPROOF        : HSMCI_MR_WRPROOF_Field := 16#0#;
      --  Force Byte Transfer
      FBYTE          : HSMCI_MR_FBYTE_Field := 16#0#;
      --  Padding Value
      PADV           : HSMCI_MR_PADV_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : Interfaces.SAM3x8e.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_MR_Register use record
      CLKDIV         at 0 range 0 .. 7;
      PWSDIV         at 0 range 8 .. 10;
      RDPROOF        at 0 range 11 .. 11;
      WRPROOF        at 0 range 12 .. 12;
      FBYTE          at 0 range 13 .. 13;
      PADV           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype HSMCI_DTOR_DTOCYC_Field is Interfaces.SAM3x8e.UInt4;

   --  Data Timeout Multiplier
   type DTOR_DTOMUL_Field is
     (--  DTOCYC
      Val_1,
      --  DTOCYC x 16
      Val_16,
      --  DTOCYC x 128
      Val_128,
      --  DTOCYC x 256
      Val_256,
      --  DTOCYC x 1024
      Val_1024,
      --  DTOCYC x 4096
      Val_4096,
      --  DTOCYC x 65536
      Val_65536,
      --  DTOCYC x 1048576
      Val_1048576)
     with Size => 3;
   for DTOR_DTOMUL_Field use
     (Val_1 => 0,
      Val_16 => 1,
      Val_128 => 2,
      Val_256 => 3,
      Val_1024 => 4,
      Val_4096 => 5,
      Val_65536 => 6,
      Val_1048576 => 7);

   --  Data Timeout Register
   type HSMCI_DTOR_Register is record
      --  Data Timeout Cycle Number
      DTOCYC        : HSMCI_DTOR_DTOCYC_Field := 16#0#;
      --  Data Timeout Multiplier
      DTOMUL        : DTOR_DTOMUL_Field := Interfaces.SAM3x8e.HSMCI.Val_1;
      --  unspecified
      Reserved_7_31 : Interfaces.SAM3x8e.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_DTOR_Register use record
      DTOCYC        at 0 range 0 .. 3;
      DTOMUL        at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  SDCard/SDIO Slot
   type SDCR_SDCSEL_Field is
     (--  Slot A is selected.
      Slota,
      --  SDCARD/SDIO Slot B selected
      Slotb,
      --  -
      Slotc,
      --  -
      Slotd)
     with Size => 2;
   for SDCR_SDCSEL_Field use
     (Slota => 0,
      Slotb => 1,
      Slotc => 2,
      Slotd => 3);

   --  SDCard/SDIO Bus Width
   type SDCR_SDCBUS_Field is
     (--  1 bit
      Val_1,
      --  4 bit
      Val_4,
      --  8 bit
      Val_8)
     with Size => 2;
   for SDCR_SDCBUS_Field use
     (Val_1 => 0,
      Val_4 => 2,
      Val_8 => 3);

   --  SD/SDIO Card Register
   type HSMCI_SDCR_Register is record
      --  SDCard/SDIO Slot
      SDCSEL        : SDCR_SDCSEL_Field := Interfaces.SAM3x8e.HSMCI.Slota;
      --  unspecified
      Reserved_2_5  : Interfaces.SAM3x8e.UInt4 := 16#0#;
      --  SDCard/SDIO Bus Width
      SDCBUS        : SDCR_SDCBUS_Field := Interfaces.SAM3x8e.HSMCI.Val_1;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM3x8e.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_SDCR_Register use record
      SDCSEL        at 0 range 0 .. 1;
      Reserved_2_5  at 0 range 2 .. 5;
      SDCBUS        at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype HSMCI_CMDR_CMDNB_Field is Interfaces.SAM3x8e.UInt6;

   --  Response Type
   type CMDR_RSPTYP_Field is
     (--  No response.
      Noresp,
      --  48-bit response.
      Val_48_Bit,
      --  136-bit response.
      Val_136_Bit,
      --  R1b response type
      R1B)
     with Size => 2;
   for CMDR_RSPTYP_Field use
     (Noresp => 0,
      Val_48_Bit => 1,
      Val_136_Bit => 2,
      R1B => 3);

   --  Special Command
   type CMDR_SPCMD_Field is
     (--  Not a special CMD.
      Std,
      --  Initialization CMD: 74 clock cycles for initialization sequence.
      Init,
      --  Synchronized CMD: Wait for the end of the current data block transfer
--  before sending the pending command.
      Sync,
      --  CE-ATA Completion Signal disable Command. The host cancels the ability for
--  the device to return a command completion signal on the command line.
      Ce_Ata,
      --  Interrupt command: Corresponds to the Interrupt Mode (CMD40).
      It_Cmd,
      --  Interrupt response: Corresponds to the Interrupt Mode (CMD40).
      It_Resp,
      --  Boot Operation Request. Start a boot operation mode, the host processor can
--  read boot data from the MMC device directly.
      Bor,
      --  End Boot Operation. This command allows the host processor to terminate the
--  boot operation mode.
      Ebo)
     with Size => 3;
   for CMDR_SPCMD_Field use
     (Std => 0,
      Init => 1,
      Sync => 2,
      Ce_Ata => 3,
      It_Cmd => 4,
      It_Resp => 5,
      Bor => 6,
      Ebo => 7);

   --  Open Drain Command
   type CMDR_OPDCMD_Field is
     (--  Push pull command.
      Pushpull,
      --  Open drain command.
      Opendrain)
     with Size => 1;
   for CMDR_OPDCMD_Field use
     (Pushpull => 0,
      Opendrain => 1);

   --  Max Latency for Command to Response
   type CMDR_MAXLAT_Field is
     (--  5-cycle max latency.
      Val_5,
      --  64-cycle max latency.
      Val_64)
     with Size => 1;
   for CMDR_MAXLAT_Field use
     (Val_5 => 0,
      Val_64 => 1);

   --  Transfer Command
   type CMDR_TRCMD_Field is
     (--  No data transfer
      No_Data,
      --  Start data transfer
      Start_Data,
      --  Stop data transfer
      Stop_Data)
     with Size => 2;
   for CMDR_TRCMD_Field use
     (No_Data => 0,
      Start_Data => 1,
      Stop_Data => 2);

   --  Transfer Direction
   type CMDR_TRDIR_Field is
     (--  Write.
      Write,
      --  Read.
      Read)
     with Size => 1;
   for CMDR_TRDIR_Field use
     (Write => 0,
      Read => 1);

   --  Transfer Type
   type CMDR_TRTYP_Field is
     (--  MMC/SDCard Single Block
      Single,
      --  MMC/SDCard Multiple Block
      Multiple,
      --  MMC Stream
      Stream,
      --  SDIO Byte
      Byte,
      --  SDIO Block
      Block)
     with Size => 3;
   for CMDR_TRTYP_Field use
     (Single => 0,
      Multiple => 1,
      Stream => 2,
      Byte => 4,
      Block => 5);

   --  SDIO Special Command
   type CMDR_IOSPCMD_Field is
     (--  Not an SDIO Special Command
      Std,
      --  SDIO Suspend Command
      Suspend,
      --  SDIO Resume Command
      Resume)
     with Size => 2;
   for CMDR_IOSPCMD_Field use
     (Std => 0,
      Suspend => 1,
      Resume => 2);

   --  ATA with Command Completion Signal
   type CMDR_ATACS_Field is
     (--  Normal operation mode.
      Normal,
      --  This bit indicates that a completion signal is expected within a programmed
--  amount of time (HSMCI_CSTOR).
      Completion)
     with Size => 1;
   for CMDR_ATACS_Field use
     (Normal => 0,
      Completion => 1);

   subtype HSMCI_CMDR_BOOT_ACK_Field is Interfaces.SAM3x8e.Bit;

   --  Command Register
   type HSMCI_CMDR_Register is record
      --  Write-only. Command Number
      CMDNB          : HSMCI_CMDR_CMDNB_Field := 16#0#;
      --  Write-only. Response Type
      RSPTYP         : CMDR_RSPTYP_Field := Interfaces.SAM3x8e.HSMCI.Noresp;
      --  Write-only. Special Command
      SPCMD          : CMDR_SPCMD_Field := Interfaces.SAM3x8e.HSMCI.Std;
      --  Write-only. Open Drain Command
      OPDCMD         : CMDR_OPDCMD_Field := Interfaces.SAM3x8e.HSMCI.Pushpull;
      --  Write-only. Max Latency for Command to Response
      MAXLAT         : CMDR_MAXLAT_Field := Interfaces.SAM3x8e.HSMCI.Val_5;
      --  unspecified
      Reserved_13_15 : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Write-only. Transfer Command
      TRCMD          : CMDR_TRCMD_Field := Interfaces.SAM3x8e.HSMCI.No_Data;
      --  Write-only. Transfer Direction
      TRDIR          : CMDR_TRDIR_Field := Interfaces.SAM3x8e.HSMCI.Write;
      --  Write-only. Transfer Type
      TRTYP          : CMDR_TRTYP_Field := Interfaces.SAM3x8e.HSMCI.Single;
      --  unspecified
      Reserved_22_23 : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only. SDIO Special Command
      IOSPCMD        : CMDR_IOSPCMD_Field := Interfaces.SAM3x8e.HSMCI.Std;
      --  Write-only. ATA with Command Completion Signal
      ATACS          : CMDR_ATACS_Field := Interfaces.SAM3x8e.HSMCI.Normal;
      --  Write-only. Boot Operation Acknowledge.
      BOOT_ACK       : HSMCI_CMDR_BOOT_ACK_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : Interfaces.SAM3x8e.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_CMDR_Register use record
      CMDNB          at 0 range 0 .. 5;
      RSPTYP         at 0 range 6 .. 7;
      SPCMD          at 0 range 8 .. 10;
      OPDCMD         at 0 range 11 .. 11;
      MAXLAT         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TRCMD          at 0 range 16 .. 17;
      TRDIR          at 0 range 18 .. 18;
      TRTYP          at 0 range 19 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      IOSPCMD        at 0 range 24 .. 25;
      ATACS          at 0 range 26 .. 26;
      BOOT_ACK       at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MMC/SDIO Block Count - SDIO Byte Count
   type BLKR_BCNT_Field is
     (--  MMC/SDCARD Multiple BlockFrom 1 to 65635: Value 0 corresponds to an
--  infinite block transfer.
      Multiple,
      --  SDIO ByteFrom 1 to 512 bytes: Value 0 corresponds to a 512-byte
--  transfer.Values from 0x200 to 0xFFFF are forbidden.
      Byte,
      --  SDIO BlockFrom 1 to 511 blocks: Value 0 corresponds to an infinite block
--  transfer.Values from 0x200 to 0xFFFF are forbidden.
      Block)
     with Size => 16;
   for BLKR_BCNT_Field use
     (Multiple => 0,
      Byte => 4,
      Block => 5);

   subtype HSMCI_BLKR_BLKLEN_Field is Interfaces.SAM3x8e.UInt16;

   --  Block Register
   type HSMCI_BLKR_Register is record
      --  MMC/SDIO Block Count - SDIO Byte Count
      BCNT   : BLKR_BCNT_Field := Interfaces.SAM3x8e.HSMCI.Multiple;
      --  Data Block Length
      BLKLEN : HSMCI_BLKR_BLKLEN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_BLKR_Register use record
      BCNT   at 0 range 0 .. 15;
      BLKLEN at 0 range 16 .. 31;
   end record;

   subtype HSMCI_CSTOR_CSTOCYC_Field is Interfaces.SAM3x8e.UInt4;

   --  Completion Signal Timeout Multiplier
   type CSTOR_CSTOMUL_Field is
     (--  CSTOCYC x 1
      Val_1,
      --  CSTOCYC x 16
      Val_16,
      --  CSTOCYC x 128
      Val_128,
      --  CSTOCYC x 256
      Val_256,
      --  CSTOCYC x 1024
      Val_1024,
      --  CSTOCYC x 4096
      Val_4096,
      --  CSTOCYC x 65536
      Val_65536,
      --  CSTOCYC x 1048576
      Val_1048576)
     with Size => 3;
   for CSTOR_CSTOMUL_Field use
     (Val_1 => 0,
      Val_16 => 1,
      Val_128 => 2,
      Val_256 => 3,
      Val_1024 => 4,
      Val_4096 => 5,
      Val_65536 => 6,
      Val_1048576 => 7);

   --  Completion Signal Timeout Register
   type HSMCI_CSTOR_Register is record
      --  Completion Signal Timeout Cycle Number
      CSTOCYC       : HSMCI_CSTOR_CSTOCYC_Field := 16#0#;
      --  Completion Signal Timeout Multiplier
      CSTOMUL       : CSTOR_CSTOMUL_Field := Interfaces.SAM3x8e.HSMCI.Val_1;
      --  unspecified
      Reserved_7_31 : Interfaces.SAM3x8e.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_CSTOR_Register use record
      CSTOCYC       at 0 range 0 .. 3;
      CSTOMUL       at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Response Register

   --  Response Register
   type HSMCI_RSPR_Registers is array (0 .. 3) of Interfaces.SAM3x8e.UInt32;

   subtype HSMCI_SR_CMDRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_TXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_BLKE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_DTIP_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_NOTBUSY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_SDIOIRQforSlotA_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_SDIOIRQforSlotB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_SDIOWAIT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_CSRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RINDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RDIRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RENDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_RTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_DCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_DTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_CSTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_BLKOVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_DMADONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_FIFOEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_XFRDONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_ACKRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_ACKRCVE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_OVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_SR_UNRE_Field is Interfaces.SAM3x8e.Bit;

   --  Status Register
   type HSMCI_SR_Register is record
      --  Read-only. Command Ready
      CMDRDY          : HSMCI_SR_CMDRDY_Field;
      --  Read-only. Receiver Ready
      RXRDY           : HSMCI_SR_RXRDY_Field;
      --  Read-only. Transmit Ready
      TXRDY           : HSMCI_SR_TXRDY_Field;
      --  Read-only. Data Block Ended
      BLKE            : HSMCI_SR_BLKE_Field;
      --  Read-only. Data Transfer in Progress
      DTIP            : HSMCI_SR_DTIP_Field;
      --  Read-only. HSMCI Not Busy
      NOTBUSY         : HSMCI_SR_NOTBUSY_Field;
      --  unspecified
      Reserved_6_7    : Interfaces.SAM3x8e.UInt2;
      --  Read-only.
      SDIOIRQforSlotA : HSMCI_SR_SDIOIRQforSlotA_Field;
      --  Read-only.
      SDIOIRQforSlotB : HSMCI_SR_SDIOIRQforSlotB_Field;
      --  unspecified
      Reserved_10_11  : Interfaces.SAM3x8e.UInt2;
      --  Read-only. SDIO Read Wait Operation Status
      SDIOWAIT        : HSMCI_SR_SDIOWAIT_Field;
      --  Read-only. CE-ATA Completion Signal Received
      CSRCV           : HSMCI_SR_CSRCV_Field;
      --  unspecified
      Reserved_14_15  : Interfaces.SAM3x8e.UInt2;
      --  Read-only. Response Index Error
      RINDE           : HSMCI_SR_RINDE_Field;
      --  Read-only. Response Direction Error
      RDIRE           : HSMCI_SR_RDIRE_Field;
      --  Read-only. Response CRC Error
      RCRCE           : HSMCI_SR_RCRCE_Field;
      --  Read-only. Response End Bit Error
      RENDE           : HSMCI_SR_RENDE_Field;
      --  Read-only. Response Time-out Error
      RTOE            : HSMCI_SR_RTOE_Field;
      --  Read-only. Data CRC Error
      DCRCE           : HSMCI_SR_DCRCE_Field;
      --  Read-only. Data Time-out Error
      DTOE            : HSMCI_SR_DTOE_Field;
      --  Read-only. Completion Signal Time-out Error
      CSTOE           : HSMCI_SR_CSTOE_Field;
      --  Read-only. DMA Block Overrun Error
      BLKOVRE         : HSMCI_SR_BLKOVRE_Field;
      --  Read-only. DMA Transfer done
      DMADONE         : HSMCI_SR_DMADONE_Field;
      --  Read-only. FIFO empty flag
      FIFOEMPTY       : HSMCI_SR_FIFOEMPTY_Field;
      --  Read-only. Transfer Done flag
      XFRDONE         : HSMCI_SR_XFRDONE_Field;
      --  Read-only. Boot Operation Acknowledge Received
      ACKRCV          : HSMCI_SR_ACKRCV_Field;
      --  Read-only. Boot Operation Acknowledge Error
      ACKRCVE         : HSMCI_SR_ACKRCVE_Field;
      --  Read-only. Overrun
      OVRE            : HSMCI_SR_OVRE_Field;
      --  Read-only. Underrun
      UNRE            : HSMCI_SR_UNRE_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_SR_Register use record
      CMDRDY          at 0 range 0 .. 0;
      RXRDY           at 0 range 1 .. 1;
      TXRDY           at 0 range 2 .. 2;
      BLKE            at 0 range 3 .. 3;
      DTIP            at 0 range 4 .. 4;
      NOTBUSY         at 0 range 5 .. 5;
      Reserved_6_7    at 0 range 6 .. 7;
      SDIOIRQforSlotA at 0 range 8 .. 8;
      SDIOIRQforSlotB at 0 range 9 .. 9;
      Reserved_10_11  at 0 range 10 .. 11;
      SDIOWAIT        at 0 range 12 .. 12;
      CSRCV           at 0 range 13 .. 13;
      Reserved_14_15  at 0 range 14 .. 15;
      RINDE           at 0 range 16 .. 16;
      RDIRE           at 0 range 17 .. 17;
      RCRCE           at 0 range 18 .. 18;
      RENDE           at 0 range 19 .. 19;
      RTOE            at 0 range 20 .. 20;
      DCRCE           at 0 range 21 .. 21;
      DTOE            at 0 range 22 .. 22;
      CSTOE           at 0 range 23 .. 23;
      BLKOVRE         at 0 range 24 .. 24;
      DMADONE         at 0 range 25 .. 25;
      FIFOEMPTY       at 0 range 26 .. 26;
      XFRDONE         at 0 range 27 .. 27;
      ACKRCV          at 0 range 28 .. 28;
      ACKRCVE         at 0 range 29 .. 29;
      OVRE            at 0 range 30 .. 30;
      UNRE            at 0 range 31 .. 31;
   end record;

   subtype HSMCI_IER_CMDRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_TXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_BLKE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_DTIP_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_NOTBUSY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_SDIOIRQforSlotA_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_SDIOIRQforSlotB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_SDIOWAIT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_CSRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RINDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RDIRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RENDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_RTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_DCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_DTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_CSTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_BLKOVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_DMADONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_FIFOEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_XFRDONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_ACKRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_ACKRCVE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_OVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IER_UNRE_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Enable Register
   type HSMCI_IER_Register is record
      --  Write-only. Command Ready Interrupt Enable
      CMDRDY          : HSMCI_IER_CMDRDY_Field := 16#0#;
      --  Write-only. Receiver Ready Interrupt Enable
      RXRDY           : HSMCI_IER_RXRDY_Field := 16#0#;
      --  Write-only. Transmit Ready Interrupt Enable
      TXRDY           : HSMCI_IER_TXRDY_Field := 16#0#;
      --  Write-only. Data Block Ended Interrupt Enable
      BLKE            : HSMCI_IER_BLKE_Field := 16#0#;
      --  Write-only. Data Transfer in Progress Interrupt Enable
      DTIP            : HSMCI_IER_DTIP_Field := 16#0#;
      --  Write-only. Data Not Busy Interrupt Enable
      NOTBUSY         : HSMCI_IER_NOTBUSY_Field := 16#0#;
      --  unspecified
      Reserved_6_7    : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only.
      SDIOIRQforSlotA : HSMCI_IER_SDIOIRQforSlotA_Field := 16#0#;
      --  Write-only.
      SDIOIRQforSlotB : HSMCI_IER_SDIOIRQforSlotB_Field := 16#0#;
      --  unspecified
      Reserved_10_11  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only. SDIO Read Wait Operation Status Interrupt Enable
      SDIOWAIT        : HSMCI_IER_SDIOWAIT_Field := 16#0#;
      --  Write-only. Completion Signal Received Interrupt Enable
      CSRCV           : HSMCI_IER_CSRCV_Field := 16#0#;
      --  unspecified
      Reserved_14_15  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only. Response Index Error Interrupt Enable
      RINDE           : HSMCI_IER_RINDE_Field := 16#0#;
      --  Write-only. Response Direction Error Interrupt Enable
      RDIRE           : HSMCI_IER_RDIRE_Field := 16#0#;
      --  Write-only. Response CRC Error Interrupt Enable
      RCRCE           : HSMCI_IER_RCRCE_Field := 16#0#;
      --  Write-only. Response End Bit Error Interrupt Enable
      RENDE           : HSMCI_IER_RENDE_Field := 16#0#;
      --  Write-only. Response Time-out Error Interrupt Enable
      RTOE            : HSMCI_IER_RTOE_Field := 16#0#;
      --  Write-only. Data CRC Error Interrupt Enable
      DCRCE           : HSMCI_IER_DCRCE_Field := 16#0#;
      --  Write-only. Data Time-out Error Interrupt Enable
      DTOE            : HSMCI_IER_DTOE_Field := 16#0#;
      --  Write-only. Completion Signal Timeout Error Interrupt Enable
      CSTOE           : HSMCI_IER_CSTOE_Field := 16#0#;
      --  Write-only. DMA Block Overrun Error Interrupt Enable
      BLKOVRE         : HSMCI_IER_BLKOVRE_Field := 16#0#;
      --  Write-only. DMA Transfer completed Interrupt Enable
      DMADONE         : HSMCI_IER_DMADONE_Field := 16#0#;
      --  Write-only. FIFO empty Interrupt enable
      FIFOEMPTY       : HSMCI_IER_FIFOEMPTY_Field := 16#0#;
      --  Write-only. Transfer Done Interrupt enable
      XFRDONE         : HSMCI_IER_XFRDONE_Field := 16#0#;
      --  Write-only. Boot Acknowledge Interrupt Enable
      ACKRCV          : HSMCI_IER_ACKRCV_Field := 16#0#;
      --  Write-only. Boot Acknowledge Error Interrupt Enable
      ACKRCVE         : HSMCI_IER_ACKRCVE_Field := 16#0#;
      --  Write-only. Overrun Interrupt Enable
      OVRE            : HSMCI_IER_OVRE_Field := 16#0#;
      --  Write-only. Underrun Interrupt Enable
      UNRE            : HSMCI_IER_UNRE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_IER_Register use record
      CMDRDY          at 0 range 0 .. 0;
      RXRDY           at 0 range 1 .. 1;
      TXRDY           at 0 range 2 .. 2;
      BLKE            at 0 range 3 .. 3;
      DTIP            at 0 range 4 .. 4;
      NOTBUSY         at 0 range 5 .. 5;
      Reserved_6_7    at 0 range 6 .. 7;
      SDIOIRQforSlotA at 0 range 8 .. 8;
      SDIOIRQforSlotB at 0 range 9 .. 9;
      Reserved_10_11  at 0 range 10 .. 11;
      SDIOWAIT        at 0 range 12 .. 12;
      CSRCV           at 0 range 13 .. 13;
      Reserved_14_15  at 0 range 14 .. 15;
      RINDE           at 0 range 16 .. 16;
      RDIRE           at 0 range 17 .. 17;
      RCRCE           at 0 range 18 .. 18;
      RENDE           at 0 range 19 .. 19;
      RTOE            at 0 range 20 .. 20;
      DCRCE           at 0 range 21 .. 21;
      DTOE            at 0 range 22 .. 22;
      CSTOE           at 0 range 23 .. 23;
      BLKOVRE         at 0 range 24 .. 24;
      DMADONE         at 0 range 25 .. 25;
      FIFOEMPTY       at 0 range 26 .. 26;
      XFRDONE         at 0 range 27 .. 27;
      ACKRCV          at 0 range 28 .. 28;
      ACKRCVE         at 0 range 29 .. 29;
      OVRE            at 0 range 30 .. 30;
      UNRE            at 0 range 31 .. 31;
   end record;

   subtype HSMCI_IDR_CMDRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_TXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_BLKE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_DTIP_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_NOTBUSY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_SDIOIRQforSlotA_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_SDIOIRQforSlotB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_SDIOWAIT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_CSRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RINDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RDIRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RENDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_RTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_DCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_DTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_CSTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_BLKOVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_DMADONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_FIFOEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_XFRDONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_ACKRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_ACKRCVE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_OVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IDR_UNRE_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Disable Register
   type HSMCI_IDR_Register is record
      --  Write-only. Command Ready Interrupt Disable
      CMDRDY          : HSMCI_IDR_CMDRDY_Field := 16#0#;
      --  Write-only. Receiver Ready Interrupt Disable
      RXRDY           : HSMCI_IDR_RXRDY_Field := 16#0#;
      --  Write-only. Transmit Ready Interrupt Disable
      TXRDY           : HSMCI_IDR_TXRDY_Field := 16#0#;
      --  Write-only. Data Block Ended Interrupt Disable
      BLKE            : HSMCI_IDR_BLKE_Field := 16#0#;
      --  Write-only. Data Transfer in Progress Interrupt Disable
      DTIP            : HSMCI_IDR_DTIP_Field := 16#0#;
      --  Write-only. Data Not Busy Interrupt Disable
      NOTBUSY         : HSMCI_IDR_NOTBUSY_Field := 16#0#;
      --  unspecified
      Reserved_6_7    : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only.
      SDIOIRQforSlotA : HSMCI_IDR_SDIOIRQforSlotA_Field := 16#0#;
      --  Write-only.
      SDIOIRQforSlotB : HSMCI_IDR_SDIOIRQforSlotB_Field := 16#0#;
      --  unspecified
      Reserved_10_11  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only. SDIO Read Wait Operation Status Interrupt Disable
      SDIOWAIT        : HSMCI_IDR_SDIOWAIT_Field := 16#0#;
      --  Write-only. Completion Signal received interrupt Disable
      CSRCV           : HSMCI_IDR_CSRCV_Field := 16#0#;
      --  unspecified
      Reserved_14_15  : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  Write-only. Response Index Error Interrupt Disable
      RINDE           : HSMCI_IDR_RINDE_Field := 16#0#;
      --  Write-only. Response Direction Error Interrupt Disable
      RDIRE           : HSMCI_IDR_RDIRE_Field := 16#0#;
      --  Write-only. Response CRC Error Interrupt Disable
      RCRCE           : HSMCI_IDR_RCRCE_Field := 16#0#;
      --  Write-only. Response End Bit Error Interrupt Disable
      RENDE           : HSMCI_IDR_RENDE_Field := 16#0#;
      --  Write-only. Response Time-out Error Interrupt Disable
      RTOE            : HSMCI_IDR_RTOE_Field := 16#0#;
      --  Write-only. Data CRC Error Interrupt Disable
      DCRCE           : HSMCI_IDR_DCRCE_Field := 16#0#;
      --  Write-only. Data Time-out Error Interrupt Disable
      DTOE            : HSMCI_IDR_DTOE_Field := 16#0#;
      --  Write-only. Completion Signal Time out Error Interrupt Disable
      CSTOE           : HSMCI_IDR_CSTOE_Field := 16#0#;
      --  Write-only. DMA Block Overrun Error Interrupt Disable
      BLKOVRE         : HSMCI_IDR_BLKOVRE_Field := 16#0#;
      --  Write-only. DMA Transfer completed Interrupt Disable
      DMADONE         : HSMCI_IDR_DMADONE_Field := 16#0#;
      --  Write-only. FIFO empty Interrupt Disable
      FIFOEMPTY       : HSMCI_IDR_FIFOEMPTY_Field := 16#0#;
      --  Write-only. Transfer Done Interrupt Disable
      XFRDONE         : HSMCI_IDR_XFRDONE_Field := 16#0#;
      --  Write-only. Boot Acknowledge Interrupt Disable
      ACKRCV          : HSMCI_IDR_ACKRCV_Field := 16#0#;
      --  Write-only. Boot Acknowledge Error Interrupt Disable
      ACKRCVE         : HSMCI_IDR_ACKRCVE_Field := 16#0#;
      --  Write-only. Overrun Interrupt Disable
      OVRE            : HSMCI_IDR_OVRE_Field := 16#0#;
      --  Write-only. Underrun Interrupt Disable
      UNRE            : HSMCI_IDR_UNRE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_IDR_Register use record
      CMDRDY          at 0 range 0 .. 0;
      RXRDY           at 0 range 1 .. 1;
      TXRDY           at 0 range 2 .. 2;
      BLKE            at 0 range 3 .. 3;
      DTIP            at 0 range 4 .. 4;
      NOTBUSY         at 0 range 5 .. 5;
      Reserved_6_7    at 0 range 6 .. 7;
      SDIOIRQforSlotA at 0 range 8 .. 8;
      SDIOIRQforSlotB at 0 range 9 .. 9;
      Reserved_10_11  at 0 range 10 .. 11;
      SDIOWAIT        at 0 range 12 .. 12;
      CSRCV           at 0 range 13 .. 13;
      Reserved_14_15  at 0 range 14 .. 15;
      RINDE           at 0 range 16 .. 16;
      RDIRE           at 0 range 17 .. 17;
      RCRCE           at 0 range 18 .. 18;
      RENDE           at 0 range 19 .. 19;
      RTOE            at 0 range 20 .. 20;
      DCRCE           at 0 range 21 .. 21;
      DTOE            at 0 range 22 .. 22;
      CSTOE           at 0 range 23 .. 23;
      BLKOVRE         at 0 range 24 .. 24;
      DMADONE         at 0 range 25 .. 25;
      FIFOEMPTY       at 0 range 26 .. 26;
      XFRDONE         at 0 range 27 .. 27;
      ACKRCV          at 0 range 28 .. 28;
      ACKRCVE         at 0 range 29 .. 29;
      OVRE            at 0 range 30 .. 30;
      UNRE            at 0 range 31 .. 31;
   end record;

   subtype HSMCI_IMR_CMDRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_TXRDY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_BLKE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_DTIP_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_NOTBUSY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_SDIOIRQforSlotA_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_SDIOIRQforSlotB_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_SDIOWAIT_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_CSRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RINDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RDIRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RENDE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_RTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_DCRCE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_DTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_CSTOE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_BLKOVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_DMADONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_FIFOEMPTY_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_XFRDONE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_ACKRCV_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_ACKRCVE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_OVRE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_IMR_UNRE_Field is Interfaces.SAM3x8e.Bit;

   --  Interrupt Mask Register
   type HSMCI_IMR_Register is record
      --  Read-only. Command Ready Interrupt Mask
      CMDRDY          : HSMCI_IMR_CMDRDY_Field;
      --  Read-only. Receiver Ready Interrupt Mask
      RXRDY           : HSMCI_IMR_RXRDY_Field;
      --  Read-only. Transmit Ready Interrupt Mask
      TXRDY           : HSMCI_IMR_TXRDY_Field;
      --  Read-only. Data Block Ended Interrupt Mask
      BLKE            : HSMCI_IMR_BLKE_Field;
      --  Read-only. Data Transfer in Progress Interrupt Mask
      DTIP            : HSMCI_IMR_DTIP_Field;
      --  Read-only. Data Not Busy Interrupt Mask
      NOTBUSY         : HSMCI_IMR_NOTBUSY_Field;
      --  unspecified
      Reserved_6_7    : Interfaces.SAM3x8e.UInt2;
      --  Read-only.
      SDIOIRQforSlotA : HSMCI_IMR_SDIOIRQforSlotA_Field;
      --  Read-only.
      SDIOIRQforSlotB : HSMCI_IMR_SDIOIRQforSlotB_Field;
      --  unspecified
      Reserved_10_11  : Interfaces.SAM3x8e.UInt2;
      --  Read-only. SDIO Read Wait Operation Status Interrupt Mask
      SDIOWAIT        : HSMCI_IMR_SDIOWAIT_Field;
      --  Read-only. Completion Signal Received Interrupt Mask
      CSRCV           : HSMCI_IMR_CSRCV_Field;
      --  unspecified
      Reserved_14_15  : Interfaces.SAM3x8e.UInt2;
      --  Read-only. Response Index Error Interrupt Mask
      RINDE           : HSMCI_IMR_RINDE_Field;
      --  Read-only. Response Direction Error Interrupt Mask
      RDIRE           : HSMCI_IMR_RDIRE_Field;
      --  Read-only. Response CRC Error Interrupt Mask
      RCRCE           : HSMCI_IMR_RCRCE_Field;
      --  Read-only. Response End Bit Error Interrupt Mask
      RENDE           : HSMCI_IMR_RENDE_Field;
      --  Read-only. Response Time-out Error Interrupt Mask
      RTOE            : HSMCI_IMR_RTOE_Field;
      --  Read-only. Data CRC Error Interrupt Mask
      DCRCE           : HSMCI_IMR_DCRCE_Field;
      --  Read-only. Data Time-out Error Interrupt Mask
      DTOE            : HSMCI_IMR_DTOE_Field;
      --  Read-only. Completion Signal Time-out Error Interrupt Mask
      CSTOE           : HSMCI_IMR_CSTOE_Field;
      --  Read-only. DMA Block Overrun Error Interrupt Mask
      BLKOVRE         : HSMCI_IMR_BLKOVRE_Field;
      --  Read-only. DMA Transfer Completed Interrupt Mask
      DMADONE         : HSMCI_IMR_DMADONE_Field;
      --  Read-only. FIFO Empty Interrupt Mask
      FIFOEMPTY       : HSMCI_IMR_FIFOEMPTY_Field;
      --  Read-only. Transfer Done Interrupt Mask
      XFRDONE         : HSMCI_IMR_XFRDONE_Field;
      --  Read-only. Boot Operation Acknowledge Received Interrupt Mask
      ACKRCV          : HSMCI_IMR_ACKRCV_Field;
      --  Read-only. Boot Operation Acknowledge Error Interrupt Mask
      ACKRCVE         : HSMCI_IMR_ACKRCVE_Field;
      --  Read-only. Overrun Interrupt Mask
      OVRE            : HSMCI_IMR_OVRE_Field;
      --  Read-only. Underrun Interrupt Mask
      UNRE            : HSMCI_IMR_UNRE_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_IMR_Register use record
      CMDRDY          at 0 range 0 .. 0;
      RXRDY           at 0 range 1 .. 1;
      TXRDY           at 0 range 2 .. 2;
      BLKE            at 0 range 3 .. 3;
      DTIP            at 0 range 4 .. 4;
      NOTBUSY         at 0 range 5 .. 5;
      Reserved_6_7    at 0 range 6 .. 7;
      SDIOIRQforSlotA at 0 range 8 .. 8;
      SDIOIRQforSlotB at 0 range 9 .. 9;
      Reserved_10_11  at 0 range 10 .. 11;
      SDIOWAIT        at 0 range 12 .. 12;
      CSRCV           at 0 range 13 .. 13;
      Reserved_14_15  at 0 range 14 .. 15;
      RINDE           at 0 range 16 .. 16;
      RDIRE           at 0 range 17 .. 17;
      RCRCE           at 0 range 18 .. 18;
      RENDE           at 0 range 19 .. 19;
      RTOE            at 0 range 20 .. 20;
      DCRCE           at 0 range 21 .. 21;
      DTOE            at 0 range 22 .. 22;
      CSTOE           at 0 range 23 .. 23;
      BLKOVRE         at 0 range 24 .. 24;
      DMADONE         at 0 range 25 .. 25;
      FIFOEMPTY       at 0 range 26 .. 26;
      XFRDONE         at 0 range 27 .. 27;
      ACKRCV          at 0 range 28 .. 28;
      ACKRCVE         at 0 range 29 .. 29;
      OVRE            at 0 range 30 .. 30;
      UNRE            at 0 range 31 .. 31;
   end record;

   subtype HSMCI_DMA_OFFSET_Field is Interfaces.SAM3x8e.UInt2;

   --  DMA Channel Read and Write Chunk Size
   type DMA_CHKSIZE_Field is
     (--  1 data available
      Val_1,
      --  4 data available
      Val_4)
     with Size => 1;
   for DMA_CHKSIZE_Field use
     (Val_1 => 0,
      Val_4 => 1);

   subtype HSMCI_DMA_DMAEN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_DMA_ROPT_Field is Interfaces.SAM3x8e.Bit;

   --  DMA Configuration Register
   type HSMCI_DMA_Register is record
      --  DMA Write Buffer Offset
      OFFSET         : HSMCI_DMA_OFFSET_Field := 16#0#;
      --  unspecified
      Reserved_2_3   : Interfaces.SAM3x8e.UInt2 := 16#0#;
      --  DMA Channel Read and Write Chunk Size
      CHKSIZE        : DMA_CHKSIZE_Field := Interfaces.SAM3x8e.HSMCI.Val_1;
      --  unspecified
      Reserved_5_7   : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  DMA Hardware Handshaking Enable
      DMAEN          : HSMCI_DMA_DMAEN_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Read Optimization with padding
      ROPT           : HSMCI_DMA_ROPT_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.SAM3x8e.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_DMA_Register use record
      OFFSET         at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      CHKSIZE        at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DMAEN          at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      ROPT           at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype HSMCI_CFG_FIFOMODE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CFG_FERRCTRL_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CFG_HSMODE_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_CFG_LSYNC_Field is Interfaces.SAM3x8e.Bit;

   --  Configuration Register
   type HSMCI_CFG_Register is record
      --  HSMCI Internal FIFO control mode
      FIFOMODE       : HSMCI_CFG_FIFOMODE_Field := 16#0#;
      --  unspecified
      Reserved_1_3   : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Flow Error flag reset control mode
      FERRCTRL       : HSMCI_CFG_FERRCTRL_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  High Speed Mode
      HSMODE         : HSMCI_CFG_HSMODE_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : Interfaces.SAM3x8e.UInt3 := 16#0#;
      --  Synchronize on the last block
      LSYNC          : HSMCI_CFG_LSYNC_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : Interfaces.SAM3x8e.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_CFG_Register use record
      FIFOMODE       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      FERRCTRL       at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      HSMODE         at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      LSYNC          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype HSMCI_WPMR_WP_EN_Field is Interfaces.SAM3x8e.Bit;
   subtype HSMCI_WPMR_WP_KEY_Field is Interfaces.SAM3x8e.UInt24;

   --  Write Protection Mode Register
   type HSMCI_WPMR_Register is record
      --  Write Protection Enable
      WP_EN        : HSMCI_WPMR_WP_EN_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM3x8e.UInt7 := 16#0#;
      --  Write Protection Key password
      WP_KEY       : HSMCI_WPMR_WP_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_WPMR_Register use record
      WP_EN        at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WP_KEY       at 0 range 8 .. 31;
   end record;

   --  Write Protection Violation Status
   type WPSR_WP_VS_Field is
     (--  No Write Protection Violation occurred since the last read of this register
--  (WP_SR)
      None,
      --  Write Protection detected unauthorized attempt to write a control register
--  had occurred (since the last read.)
      Write,
      --  Software reset had been performed while Write Protection was enabled (since
--  the last read).
      Reset,
      --  Both Write Protection violation and software reset with Write Protection
--  enabled have occurred since the last read.
      Both)
     with Size => 4;
   for WPSR_WP_VS_Field use
     (None => 0,
      Write => 1,
      Reset => 2,
      Both => 3);

   subtype HSMCI_WPSR_WP_VSRC_Field is Interfaces.SAM3x8e.UInt16;

   --  Write Protection Status Register
   type HSMCI_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WP_VS          : WPSR_WP_VS_Field;
      --  unspecified
      Reserved_4_7   : Interfaces.SAM3x8e.UInt4;
      --  Read-only. Write Protection Violation SouRCe
      WP_VSRC        : HSMCI_WPSR_WP_VSRC_Field;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM3x8e.Byte;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_WPSR_Register use record
      WP_VS          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      WP_VSRC        at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  FIFO Memory Aperture0

   --  FIFO Memory Aperture0
   type HSMCI_FIFO_Registers is array (0 .. 255) of Interfaces.SAM3x8e.UInt32;

   -----------------
   -- Peripherals --
   -----------------

   --  High Speed MultiMedia Card Interface
   type HSMCI_Peripheral is record
      --  Control Register
      CR    : aliased HSMCI_CR_Register;
      --  Mode Register
      MR    : aliased HSMCI_MR_Register;
      --  Data Timeout Register
      DTOR  : aliased HSMCI_DTOR_Register;
      --  SD/SDIO Card Register
      SDCR  : aliased HSMCI_SDCR_Register;
      --  Argument Register
      ARGR  : aliased Interfaces.SAM3x8e.UInt32;
      --  Command Register
      CMDR  : aliased HSMCI_CMDR_Register;
      --  Block Register
      BLKR  : aliased HSMCI_BLKR_Register;
      --  Completion Signal Timeout Register
      CSTOR : aliased HSMCI_CSTOR_Register;
      --  Response Register
      RSPR  : aliased HSMCI_RSPR_Registers;
      --  Receive Data Register
      RDR   : aliased Interfaces.SAM3x8e.UInt32;
      --  Transmit Data Register
      TDR   : aliased Interfaces.SAM3x8e.UInt32;
      --  Status Register
      SR    : aliased HSMCI_SR_Register;
      --  Interrupt Enable Register
      IER   : aliased HSMCI_IER_Register;
      --  Interrupt Disable Register
      IDR   : aliased HSMCI_IDR_Register;
      --  Interrupt Mask Register
      IMR   : aliased HSMCI_IMR_Register;
      --  DMA Configuration Register
      DMA   : aliased HSMCI_DMA_Register;
      --  Configuration Register
      CFG   : aliased HSMCI_CFG_Register;
      --  Write Protection Mode Register
      WPMR  : aliased HSMCI_WPMR_Register;
      --  Write Protection Status Register
      WPSR  : aliased HSMCI_WPSR_Register;
      --  FIFO Memory Aperture0
      FIFO  : aliased HSMCI_FIFO_Registers;
   end record
     with Volatile;

   for HSMCI_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      MR    at 16#4# range 0 .. 31;
      DTOR  at 16#8# range 0 .. 31;
      SDCR  at 16#C# range 0 .. 31;
      ARGR  at 16#10# range 0 .. 31;
      CMDR  at 16#14# range 0 .. 31;
      BLKR  at 16#18# range 0 .. 31;
      CSTOR at 16#1C# range 0 .. 31;
      RSPR  at 16#20# range 0 .. 127;
      RDR   at 16#30# range 0 .. 31;
      TDR   at 16#34# range 0 .. 31;
      SR    at 16#40# range 0 .. 31;
      IER   at 16#44# range 0 .. 31;
      IDR   at 16#48# range 0 .. 31;
      IMR   at 16#4C# range 0 .. 31;
      DMA   at 16#50# range 0 .. 31;
      CFG   at 16#54# range 0 .. 31;
      WPMR  at 16#E4# range 0 .. 31;
      WPSR  at 16#E8# range 0 .. 31;
      FIFO  at 16#200# range 0 .. 8191;
   end record;

   --  High Speed MultiMedia Card Interface
   HSMCI_Periph : aliased HSMCI_Peripheral
     with Import, Address => HSMCI_Base;

end Interfaces.SAM3x8e.HSMCI;
