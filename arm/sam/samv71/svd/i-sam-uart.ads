--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.SAM.UART is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype UART_UART_CR_RSTRX_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_RSTTX_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_RXEN_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_RXDIS_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_TXEN_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_TXDIS_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_RSTSTA_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_REQCLR_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CR_DBGE_Field is Interfaces.SAM.Bit;

   --  Control Register
   type UART_UART_CR_Register is record
      --  unspecified
      Reserved_0_1   : Interfaces.SAM.UInt2 := 16#0#;
      --  Write-only. Reset Receiver
      RSTRX          : UART_UART_CR_RSTRX_Field := 16#0#;
      --  Write-only. Reset Transmitter
      RSTTX          : UART_UART_CR_RSTTX_Field := 16#0#;
      --  Write-only. Receiver Enable
      RXEN           : UART_UART_CR_RXEN_Field := 16#0#;
      --  Write-only. Receiver Disable
      RXDIS          : UART_UART_CR_RXDIS_Field := 16#0#;
      --  Write-only. Transmitter Enable
      TXEN           : UART_UART_CR_TXEN_Field := 16#0#;
      --  Write-only. Transmitter Disable
      TXDIS          : UART_UART_CR_TXDIS_Field := 16#0#;
      --  Write-only. Reset Status
      RSTSTA         : UART_UART_CR_RSTSTA_Field := 16#0#;
      --  unspecified
      Reserved_9_11  : Interfaces.SAM.UInt3 := 16#0#;
      --  Write-only. Request Clear
      REQCLR         : UART_UART_CR_REQCLR_Field := 16#0#;
      --  unspecified
      Reserved_13_14 : Interfaces.SAM.UInt2 := 16#0#;
      --  Write-only. Debug Enable
      DBGE           : UART_UART_CR_DBGE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      RSTRX          at 0 range 2 .. 2;
      RSTTX          at 0 range 3 .. 3;
      RXEN           at 0 range 4 .. 4;
      RXDIS          at 0 range 5 .. 5;
      TXEN           at 0 range 6 .. 6;
      TXDIS          at 0 range 7 .. 7;
      RSTSTA         at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      REQCLR         at 0 range 12 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      DBGE           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Receiver Digital Filter
   type UART_MR_FILTER_Field is
     (
      --  UART does not filter the receive line.
      Disabled,
      --  UART filters the receive line using a three-sample filter (16x-bit
      --  clock) (2 over 3 majority).
      Enabled)
     with Size => 1;
   for UART_MR_FILTER_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Parity Type
   type UART_MR_PAR_Field is
     (
      --  Even Parity
      Even,
      --  Odd Parity
      Odd,
      --  Space: parity forced to 0
      Space,
      --  Mark: parity forced to 1
      Mark,
      --  No parity
      No)
     with Size => 3;
   for UART_MR_PAR_Field use
     (Even => 0,
      Odd => 1,
      Space => 2,
      Mark => 3,
      No => 4);

   --  Baud Rate Source Clock
   type UART_MR_BRSRCCK_Field is
     (
      --  The baud rate is driven by the peripheral clock
      Periph_Clk,
      --  The baud rate is driven by a PMC-programmable clock PCK (see section
      --  Power Management Controller (PMC)).
      Pmc_Pck)
     with Size => 1;
   for UART_MR_BRSRCCK_Field use
     (Periph_Clk => 0,
      Pmc_Pck => 1);

   --  Channel Mode
   type UART_MR_CHMODE_Field is
     (
      --  Normal mode
      Normal,
      --  Automatic echo
      Automatic,
      --  Local loopback
      Local_Loopback,
      --  Remote loopback
      Remote_Loopback)
     with Size => 2;
   for UART_MR_CHMODE_Field use
     (Normal => 0,
      Automatic => 1,
      Local_Loopback => 2,
      Remote_Loopback => 3);

   --  Mode Register
   type UART_UART_MR_Register is record
      --  unspecified
      Reserved_0_3   : Interfaces.SAM.UInt4 := 16#0#;
      --  Receiver Digital Filter
      FILTER         : UART_MR_FILTER_Field := Interfaces.SAM.UART.Disabled;
      --  unspecified
      Reserved_5_8   : Interfaces.SAM.UInt4 := 16#0#;
      --  Parity Type
      PAR            : UART_MR_PAR_Field := Interfaces.SAM.UART.Even;
      --  Baud Rate Source Clock
      BRSRCCK        : UART_MR_BRSRCCK_Field :=
                        Interfaces.SAM.UART.Periph_Clk;
      --  unspecified
      Reserved_13_13 : Interfaces.SAM.Bit := 16#0#;
      --  Channel Mode
      CHMODE         : UART_MR_CHMODE_Field := Interfaces.SAM.UART.Normal;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_MR_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      FILTER         at 0 range 4 .. 4;
      Reserved_5_8   at 0 range 5 .. 8;
      PAR            at 0 range 9 .. 11;
      BRSRCCK        at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      CHMODE         at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UART_UART_IER_RXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_TXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_OVRE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_FRAME_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_PARE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_TXEMPTY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IER_CMP_Field is Interfaces.SAM.Bit;

   --  Interrupt Enable Register
   type UART_UART_IER_Register is record
      --  Write-only. Enable RXRDY Interrupt
      RXRDY          : UART_UART_IER_RXRDY_Field := 16#0#;
      --  Write-only. Enable TXRDY Interrupt
      TXRDY          : UART_UART_IER_TXRDY_Field := 16#0#;
      --  unspecified
      Reserved_2_4   : Interfaces.SAM.UInt3 := 16#0#;
      --  Write-only. Enable Overrun Error Interrupt
      OVRE           : UART_UART_IER_OVRE_Field := 16#0#;
      --  Write-only. Enable Framing Error Interrupt
      FRAME          : UART_UART_IER_FRAME_Field := 16#0#;
      --  Write-only. Enable Parity Error Interrupt
      PARE           : UART_UART_IER_PARE_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM.Bit := 16#0#;
      --  Write-only. Enable TXEMPTY Interrupt
      TXEMPTY        : UART_UART_IER_TXEMPTY_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : Interfaces.SAM.UInt5 := 16#0#;
      --  Write-only. Enable Comparison Interrupt
      CMP            : UART_UART_IER_CMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_IER_Register use record
      RXRDY          at 0 range 0 .. 0;
      TXRDY          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVRE           at 0 range 5 .. 5;
      FRAME          at 0 range 6 .. 6;
      PARE           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      CMP            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UART_UART_IDR_RXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_TXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_OVRE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_FRAME_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_PARE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_TXEMPTY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IDR_CMP_Field is Interfaces.SAM.Bit;

   --  Interrupt Disable Register
   type UART_UART_IDR_Register is record
      --  Write-only. Disable RXRDY Interrupt
      RXRDY          : UART_UART_IDR_RXRDY_Field := 16#0#;
      --  Write-only. Disable TXRDY Interrupt
      TXRDY          : UART_UART_IDR_TXRDY_Field := 16#0#;
      --  unspecified
      Reserved_2_4   : Interfaces.SAM.UInt3 := 16#0#;
      --  Write-only. Disable Overrun Error Interrupt
      OVRE           : UART_UART_IDR_OVRE_Field := 16#0#;
      --  Write-only. Disable Framing Error Interrupt
      FRAME          : UART_UART_IDR_FRAME_Field := 16#0#;
      --  Write-only. Disable Parity Error Interrupt
      PARE           : UART_UART_IDR_PARE_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM.Bit := 16#0#;
      --  Write-only. Disable TXEMPTY Interrupt
      TXEMPTY        : UART_UART_IDR_TXEMPTY_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : Interfaces.SAM.UInt5 := 16#0#;
      --  Write-only. Disable Comparison Interrupt
      CMP            : UART_UART_IDR_CMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_IDR_Register use record
      RXRDY          at 0 range 0 .. 0;
      TXRDY          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVRE           at 0 range 5 .. 5;
      FRAME          at 0 range 6 .. 6;
      PARE           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      CMP            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UART_UART_IMR_RXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_TXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_OVRE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_FRAME_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_PARE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_TXEMPTY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_IMR_CMP_Field is Interfaces.SAM.Bit;

   --  Interrupt Mask Register
   type UART_UART_IMR_Register is record
      --  Read-only. Mask RXRDY Interrupt
      RXRDY          : UART_UART_IMR_RXRDY_Field;
      --  Read-only. Disable TXRDY Interrupt
      TXRDY          : UART_UART_IMR_TXRDY_Field;
      --  unspecified
      Reserved_2_4   : Interfaces.SAM.UInt3;
      --  Read-only. Mask Overrun Error Interrupt
      OVRE           : UART_UART_IMR_OVRE_Field;
      --  Read-only. Mask Framing Error Interrupt
      FRAME          : UART_UART_IMR_FRAME_Field;
      --  Read-only. Mask Parity Error Interrupt
      PARE           : UART_UART_IMR_PARE_Field;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM.Bit;
      --  Read-only. Mask TXEMPTY Interrupt
      TXEMPTY        : UART_UART_IMR_TXEMPTY_Field;
      --  unspecified
      Reserved_10_14 : Interfaces.SAM.UInt5;
      --  Read-only. Mask Comparison Interrupt
      CMP            : UART_UART_IMR_CMP_Field;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_IMR_Register use record
      RXRDY          at 0 range 0 .. 0;
      TXRDY          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVRE           at 0 range 5 .. 5;
      FRAME          at 0 range 6 .. 6;
      PARE           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      CMP            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UART_UART_SR_RXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_TXRDY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_OVRE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_FRAME_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_PARE_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_TXEMPTY_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_CMP_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_SWES_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_CLKREQ_Field is Interfaces.SAM.Bit;
   subtype UART_UART_SR_WKUPREQ_Field is Interfaces.SAM.Bit;

   --  Status Register
   type UART_UART_SR_Register is record
      --  Read-only. Receiver Ready
      RXRDY          : UART_UART_SR_RXRDY_Field;
      --  Read-only. Transmitter Ready
      TXRDY          : UART_UART_SR_TXRDY_Field;
      --  unspecified
      Reserved_2_4   : Interfaces.SAM.UInt3;
      --  Read-only. Overrun Error
      OVRE           : UART_UART_SR_OVRE_Field;
      --  Read-only. Framing Error
      FRAME          : UART_UART_SR_FRAME_Field;
      --  Read-only. Parity Error
      PARE           : UART_UART_SR_PARE_Field;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM.Bit;
      --  Read-only. Transmitter Empty
      TXEMPTY        : UART_UART_SR_TXEMPTY_Field;
      --  unspecified
      Reserved_10_14 : Interfaces.SAM.UInt5;
      --  Read-only. Comparison Match
      CMP            : UART_UART_SR_CMP_Field;
      --  unspecified
      Reserved_16_20 : Interfaces.SAM.UInt5;
      --  Read-only. SleepWalking Enable Status
      SWES           : UART_UART_SR_SWES_Field;
      --  Read-only. Clock Request
      CLKREQ         : UART_UART_SR_CLKREQ_Field;
      --  Read-only. Wake-Up Request
      WKUPREQ        : UART_UART_SR_WKUPREQ_Field;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM.Byte;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_SR_Register use record
      RXRDY          at 0 range 0 .. 0;
      TXRDY          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVRE           at 0 range 5 .. 5;
      FRAME          at 0 range 6 .. 6;
      PARE           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      TXEMPTY        at 0 range 9 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      CMP            at 0 range 15 .. 15;
      Reserved_16_20 at 0 range 16 .. 20;
      SWES           at 0 range 21 .. 21;
      CLKREQ         at 0 range 22 .. 22;
      WKUPREQ        at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype UART_UART_RHR_RXCHR_Field is Interfaces.SAM.Byte;

   --  Receive Holding Register
   type UART_UART_RHR_Register is record
      --  Read-only. Received Character
      RXCHR         : UART_UART_RHR_RXCHR_Field;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_RHR_Register use record
      RXCHR         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype UART_UART_THR_TXCHR_Field is Interfaces.SAM.Byte;

   --  Transmit Holding Register
   type UART_UART_THR_Register is record
      --  Write-only. Character to be Transmitted
      TXCHR         : UART_UART_THR_TXCHR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.SAM.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_THR_Register use record
      TXCHR         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype UART_UART_BRGR_CD_Field is Interfaces.SAM.UInt16;

   --  Baud Rate Generator Register
   type UART_UART_BRGR_Register is record
      --  Clock Divisor
      CD             : UART_UART_BRGR_CD_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.SAM.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_BRGR_Register use record
      CD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype UART_UART_CMPR_VAL1_Field is Interfaces.SAM.Byte;

   --  Comparison Mode
   type UART_CMPR_CMPMODE_Field is
     (
      --  Any character is received and comparison function drives CMP flag.
      Flag_Only,
      --  Comparison condition must be met to start reception.
      Start_Condition)
     with Size => 1;
   for UART_CMPR_CMPMODE_Field use
     (Flag_Only => 0,
      Start_Condition => 1);

   subtype UART_UART_CMPR_CMPPAR_Field is Interfaces.SAM.Bit;
   subtype UART_UART_CMPR_VAL2_Field is Interfaces.SAM.Byte;

   --  Comparison Register
   type UART_UART_CMPR_Register is record
      --  First Comparison Value for Received Character
      VAL1           : UART_UART_CMPR_VAL1_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : Interfaces.SAM.UInt4 := 16#0#;
      --  Comparison Mode
      CMPMODE        : UART_CMPR_CMPMODE_Field :=
                        Interfaces.SAM.UART.Flag_Only;
      --  unspecified
      Reserved_13_13 : Interfaces.SAM.Bit := 16#0#;
      --  Compare Parity
      CMPPAR         : UART_UART_CMPR_CMPPAR_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.SAM.Bit := 16#0#;
      --  Second Comparison Value for Received Character
      VAL2           : UART_UART_CMPR_VAL2_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_CMPR_Register use record
      VAL1           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      CMPMODE        at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      CMPPAR         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      VAL2           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype UART_UART_WPMR_WPEN_Field is Interfaces.SAM.Bit;

   --  Write Protection Key
   type UART_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Uart_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 24;
   for UART_WPMR_WPKEY_Field use
     (Uart_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 5587282);

   --  Write Protection Mode Register
   type UART_UART_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : UART_UART_WPMR_WPEN_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : UART_WPMR_WPKEY_Field := Uart_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype UART_UART_VERSION_VERSION_Field is Interfaces.SAM.UInt12;
   subtype UART_UART_VERSION_MFN_Field is Interfaces.SAM.UInt3;

   --  Version Register
   type UART_UART_VERSION_Register is record
      --  Read-only. Hardware Module Version
      VERSION        : UART_UART_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : Interfaces.SAM.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : UART_UART_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for UART_UART_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal Asynchronous Receiver Transmitter
   type UART_Peripheral is record
      --  Control Register
      UART_CR      : aliased UART_UART_CR_Register;
      --  Mode Register
      UART_MR      : aliased UART_UART_MR_Register;
      --  Interrupt Enable Register
      UART_IER     : aliased UART_UART_IER_Register;
      --  Interrupt Disable Register
      UART_IDR     : aliased UART_UART_IDR_Register;
      --  Interrupt Mask Register
      UART_IMR     : aliased UART_UART_IMR_Register;
      --  Status Register
      UART_SR      : aliased UART_UART_SR_Register;
      --  Receive Holding Register
      UART_RHR     : aliased UART_UART_RHR_Register;
      --  Transmit Holding Register
      UART_THR     : aliased UART_UART_THR_Register;
      --  Baud Rate Generator Register
      UART_BRGR    : aliased UART_UART_BRGR_Register;
      --  Comparison Register
      UART_CMPR    : aliased UART_UART_CMPR_Register;
      --  Write Protection Mode Register
      UART_WPMR    : aliased UART_UART_WPMR_Register;
      --  Version Register
      UART_VERSION : aliased UART_UART_VERSION_Register;
   end record
     with Volatile;

   for UART_Peripheral use record
      UART_CR      at 16#0# range 0 .. 31;
      UART_MR      at 16#4# range 0 .. 31;
      UART_IER     at 16#8# range 0 .. 31;
      UART_IDR     at 16#C# range 0 .. 31;
      UART_IMR     at 16#10# range 0 .. 31;
      UART_SR      at 16#14# range 0 .. 31;
      UART_RHR     at 16#18# range 0 .. 31;
      UART_THR     at 16#1C# range 0 .. 31;
      UART_BRGR    at 16#20# range 0 .. 31;
      UART_CMPR    at 16#24# range 0 .. 31;
      UART_WPMR    at 16#E4# range 0 .. 31;
      UART_VERSION at 16#FC# range 0 .. 31;
   end record;

   --  Universal Asynchronous Receiver Transmitter
   UART0_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#400E0800#);

   --  Universal Asynchronous Receiver Transmitter
   UART1_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#400E0A00#);

   --  Universal Asynchronous Receiver Transmitter
   UART2_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#400E1A00#);

   --  Universal Asynchronous Receiver Transmitter
   UART3_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#400E1C00#);

   --  Universal Asynchronous Receiver Transmitter
   UART4_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#400E1E00#);

end Interfaces.SAM.UART;
