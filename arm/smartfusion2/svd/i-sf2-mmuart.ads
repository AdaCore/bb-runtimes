--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from M2Sxxx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.SF2.MMUART is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Interupt Enable Register
   type IER_Register is record
      --  Enables received data available interrupt
      ERBFI        : Boolean := False;
      --  Transmitter holding register empty interrupt enable
      ETBEI        : Boolean := False;
      --  Receiver line status interrupt enable
      ELSI         : Boolean := False;
      --  Modem status interrupt enable
      EDSSI        : Boolean := False;
      --  unspecified
      Reserved_4_7 : Interfaces.SF2.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for IER_Register use record
      ERBFI        at 0 range 0 .. 0;
      ETBEI        at 0 range 1 .. 1;
      ELSI         at 0 range 2 .. 2;
      EDSSI        at 0 range 3 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
   end record;

   --  These bits are used to set the trigger level for the Rx FIFO interrupt.
   --  Rx FIFO trigger level (bytes) are: 0b00: 1 byte 0b01: 4 bytes 0b10: 8
   --  bytes 0b11: 14 bytes
   type FCR_RX_TRIG_Field is
     (
      Fifo_1_Byte,
      Fifo_4_Bytes,
      Fifo_8_Bytes,
      Fifo_14_Bytes)
     with Size => 2;
   for FCR_RX_TRIG_Field use
     (Fifo_1_Byte => 0,
      Fifo_4_Bytes => 1,
      Fifo_8_Bytes => 2,
      Fifo_14_Bytes => 3);

   --  FIFO control register
   type FCR_Register is record
      --  Write-only. It enables both the Tx and Rx FIFOs and is hardwired to
      --  1, which means it is always enabled and cannot be changed.
      ENABLE_TX_RX_FIFO  : Boolean := False;
      --  Write-only. Clears all bytes in Rx FIFO and resets counter logic.
      --  This shift register is not cleared. 0: Disabled (default) 1: Enabled
      CLEAR_RX_FIFO      : Boolean := False;
      --  Write-only. Clears all bytes in the Tx FIFO and resets its counter
      --  logic. The shift register is not cleared. 0: Disabled (default) 1:
      --  Enabled
      CLEAR_TX_FIFO      : Boolean := False;
      --  Write-only. Software must always set this bit to 1 for efficient data
      --  transfer from transmit FIFO to PDMA.
      ENABLE_TXRDY_RXRDY : Boolean := False;
      --  unspecified
      Reserved_4_5       : Interfaces.SF2.UInt2 := 16#0#;
      --  Write-only. These bits are used to set the trigger level for the Rx
      --  FIFO interrupt. Rx FIFO trigger level (bytes) are: 0b00: 1 byte 0b01:
      --  4 bytes 0b10: 8 bytes 0b11: 14 bytes
      RX_TRIG            : FCR_RX_TRIG_Field :=
                            Interfaces.SF2.MMUART.Fifo_1_Byte;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for FCR_Register use record
      ENABLE_TX_RX_FIFO  at 0 range 0 .. 0;
      CLEAR_RX_FIFO      at 0 range 1 .. 1;
      CLEAR_TX_FIFO      at 0 range 2 .. 2;
      ENABLE_TXRDY_RXRDY at 0 range 3 .. 3;
      Reserved_4_5       at 0 range 4 .. 5;
      RX_TRIG            at 0 range 6 .. 7;
   end record;

   --  Interrupt identification bits.
   type IIR_IIR_Field is
     (
      --  Fourth priority. Modem status interrupt due to clear to send, data
      --  set ready, ring indicator, or data carrier detect being asserted.
      --  Reading the modem status register resets this interrupt.
      Modem_Status,
      --  Third priority. Transmit holding register empty interrupt. Reading
      --  the IIR or writing to the transmit holding register (THR) resets the
      --  interrupt.
      Transmitter_Holding_Register_Empty,
      --  Fifth priority. Multi-mode interrupts can occur due to any of the
      --  interrupts mentioned in IIM. For more details refer to Table 12-15.
      Multi_Mode_Interrupt,
      --  Second priority. Receive data available interrupt modem status
      --  interrupt. Reading the receiver buffer register (RBR) or the FIFO
      --  drops below the trigger level resets this interrupt.
      Receiver_Data_Available,
      --  Highest priority. Receiver line status interrupts due to overrun
      --  error, parity error, framing error, or break interrupt. Reading the
      --  line status register resets this interrupt.
      Receiver_Line_Status,
      --  Second priority. Character timeout indication interrupt occurs when
      --  no characters have been read from the Rx FIFO during the last four
      --  character times and there was at least one character in it during
      --  this time. Reading the RBR resets this interrupt.
      Character_Timeout_Indication)
     with Size => 4;
   for IIR_IIR_Field use
     (Modem_Status => 0,
      Transmitter_Holding_Register_Empty => 2,
      Multi_Mode_Interrupt => 3,
      Receiver_Data_Available => 4,
      Receiver_Line_Status => 6,
      Character_Timeout_Indication => 12);

   subtype IIR_Mode_Field is Interfaces.SF2.UInt2;

   --  Interrupt identification register
   type IIR_Register is record
      --  Read-only. Interrupt identification bits.
      IIR          : IIR_IIR_Field;
      --  unspecified
      Reserved_4_5 : Interfaces.SF2.UInt2;
      --  Read-only. Always 0b11. Enables FIFO mode.
      Mode         : IIR_Mode_Field;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for IIR_Register use record
      IIR          at 0 range 0 .. 3;
      Reserved_4_5 at 0 range 4 .. 5;
      Mode         at 0 range 6 .. 7;
   end record;

   --  Word length select
   type LCR_WLS_Field is
     (
      Length_5_Bits,
      Length_6_Bits,
      Length_7_Bits,
      Length_8_Bits)
     with Size => 2;
   for LCR_WLS_Field use
     (Length_5_Bits => 0,
      Length_6_Bits => 1,
      Length_7_Bits => 2,
      Length_8_Bits => 3);

   --  Number of stop bits (STB)
   type LCR_STB_Field is
     (
      --  1 stop bit
      Stop_Bit_1,
      --  1 1/2 stop bits when WLS=00. The number of stop bits is 2 for all
      --  other cases not described above (STB=1 and WLS=01, 10, or 11).
      Stop_Bit_1_AND_HALF)
     with Size => 1;
   for LCR_STB_Field use
     (Stop_Bit_1 => 0,
      Stop_Bit_1_AND_HALF => 1);

   --  Even parity select
   type LCR_EPS_Field is
     (
      --  Odd parity
      Odd,
      --  Even parity
      Even)
     with Size => 1;
   for LCR_EPS_Field use
     (Odd => 0,
      Even => 1);

   --  Line Control register
   type LCR_Register is record
      --  Word length select
      WLS  : LCR_WLS_Field := Interfaces.SF2.MMUART.Length_5_Bits;
      --  Number of stop bits (STB)
      STB  : LCR_STB_Field := Interfaces.SF2.MMUART.Stop_Bit_1;
      --  Parity enable
      PEN  : Boolean := False;
      --  Even parity select
      EPS  : LCR_EPS_Field := Interfaces.SF2.MMUART.Odd;
      --  Stick parity. When stick parity is enabled, the parity is set
      --  according to bits [4:3] as follows: 11: 0 will be sent as a parity
      --  bit and checked when receiving. 01: 1 will be sent as a parity bit
      --  and checked when receiving.
      SP   : Boolean := False;
      --  Set break. Enabling this bit sets MMUART_x_TXD to 0. This does not
      --  have any effect on transmitter logic.
      SB   : Boolean := False;
      --  Divisor latch access bit. Enables access to the divisor latch
      --  registers during read or write operation to address 0 and 1.
      DLAB : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for LCR_Register use record
      WLS  at 0 range 0 .. 1;
      STB  at 0 range 2 .. 2;
      PEN  at 0 range 3 .. 3;
      EPS  at 0 range 4 .. 4;
      SP   at 0 range 5 .. 5;
      SB   at 0 range 6 .. 6;
      DLAB at 0 range 7 .. 7;
   end record;

   --  Data terminal ready (MMUART_x_DTR) output. Active Low
   type MCR_DTR_Field is
     (
      Set_1,
      Set_0)
     with Size => 1;
   for MCR_DTR_Field use
     (Set_1 => 0,
      Set_0 => 1);

   --  Controls the request to send (MMUART_x_RTS) signal. Active Low
   type MCR_RTS_Field is
     (
      Set_1,
      Set_0)
     with Size => 1;
   for MCR_RTS_Field use
     (Set_1 => 0,
      Set_0 => 1);

   --  Controls the output1 (OUT1) signal. Active Low
   type MCR_OUT_1_Field is
     (
      Set_1,
      Set_0)
     with Size => 1;
   for MCR_OUT_1_Field use
     (Set_1 => 0,
      Set_0 => 1);

   --  Controls the output2 (OUT2) signal. Active Low
   type MCR_OUT_2_Field is
     (
      Set_1,
      Set_0)
     with Size => 1;
   for MCR_OUT_2_Field use
     (Set_1 => 0,
      Set_0 => 1);

   --  Remote loopback enable bits. In the Remote loopback mode, when a bit is
   --  received, it is sent directly out the transmit line, bypassing the
   --  transmitter block, and disabling the receiver. In the Automatic echo
   --  mode, when a bit is received, it is sent directly out the transmit line,
   --  bypassing the transmitter block, while the receiver is still enabled.
   type MCR_RLoop_Field is
     (
      Disabled,
      --  Remote loopback enabled
      Enabled,
      --  Automatic echo enabled
      Echo)
     with Size => 2;
   for MCR_RLoop_Field use
     (Disabled => 0,
      Enabled => 1,
      Echo => 2);

   --  Modem Control register
   type MCR_Register is record
      --  Data terminal ready (MMUART_x_DTR) output. Active Low
      DTR          : MCR_DTR_Field := Interfaces.SF2.MMUART.Set_1;
      --  Controls the request to send (MMUART_x_RTS) signal. Active Low
      RTS          : MCR_RTS_Field := Interfaces.SF2.MMUART.Set_1;
      --  Controls the output1 (OUT1) signal. Active Low
      OUT_1        : MCR_OUT_1_Field := Interfaces.SF2.MMUART.Set_1;
      --  Controls the output2 (OUT2) signal. Active Low
      OUT_2        : MCR_OUT_2_Field := Interfaces.SF2.MMUART.Set_1;
      --  In the Loopback mode, MMUART_x_TXD is set to 1. The MMUART_x_RXD,
      --  MMUART_x_DSR, MMUART_x_CTS, MMUART_x_RI, and MMUART_x_DCD inputs are
      --  disconnected. The output of the transmitter shift register is looped
      --  back into the receiver shift register. The modem control outputs
      --  (MMUART_x_DTR, MMUART_x_RTS, MMUART_x_OUT1, and MMUART_x_OUT2) are
      --  connected internally to the modem control inputs, and the modem
      --  control output pins are set as 1. The transmitted data is immediately
      --  received, allowing Cortex-M3 processor to check the operation of the
      --  MMUART_x. The interrupts are operating in the Loopback mode.
      Loopback     : Boolean := False;
      --  Remote loopback enable bits. In the Remote loopback mode, when a bit
      --  is received, it is sent directly out the transmit line, bypassing the
      --  transmitter block, and disabling the receiver. In the Automatic echo
      --  mode, when a bit is received, it is sent directly out the transmit
      --  line, bypassing the transmitter block, while the receiver is still
      --  enabled.
      RLoop        : MCR_RLoop_Field := Interfaces.SF2.MMUART.Disabled;
      --  unspecified
      Reserved_7_7 : Interfaces.SF2.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MCR_Register use record
      DTR          at 0 range 0 .. 0;
      RTS          at 0 range 1 .. 1;
      OUT_1        at 0 range 2 .. 2;
      OUT_2        at 0 range 3 .. 3;
      Loopback     at 0 range 4 .. 4;
      RLoop        at 0 range 5 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
   end record;

   --  Line Status register
   type LSR_Register is record
      --  Read-only. Data ready (DR). Indicates when a data byte is received
      --  and stored in the receive buffer or the FIFO. DR is cleared to 0 when
      --  the Cortex-M3 processor reads the data from the receive buffer or the
      --  FIFO.
      DR   : Boolean;
      --  Read-only. Overrun error (OE). Indicates that the new byte was
      --  received before the Cortex-M3 processor reads the byte from the
      --  receive buffer, and that the earlier data byte was destroyed. OE is
      --  cleared when the Cortex-M3 processor reads the LSR. If the data
      --  continues to fill the FIFO beyond the trigger level, an overrun error
      --  occurs once the FIFO is full and the next character has been
      --  completely received in the shift register. The character in the shift
      --  register is overwritten, but it is not transferred to the FIFO.
      OE   : Boolean;
      --  Read-only. Parity error (PE). Indicates that the receive byte had a
      --  parity error. PE is cleared when the Cortex-M3 processor reads the
      --  LSR. This error is revealed to the Cortex-M3 processor when it is
      --  associated character is at the top of the FIFO.
      PE   : Boolean;
      --  Read-only. Framing error (FE). Indicates that the receive byte did
      --  not have a valid stop bit. FE is cleared when Cortex-M3 processor
      --  reads the LSR. The MMUART_x tries to resynchronize after a framing
      --  error. To do this, it assumes that the framing error was due to the
      --  next start bit, so it samples this start bit twice, and then starts
      --  receiving the data. This error is revealed to Cortex-M3 processor
      --  when it is associated character is at the top of the FIFO.
      FE   : Boolean;
      --  Read-only. Break interrupt (BI). Indicates that the receive data is
      --  at 0 longer than a full word transmission time (start bit + data bits
      --  + parity + stop bits). BI is cleared when Cortex-M3 processor reads
      --  the line status register (LSR). This error is revealed to the
      --  Cortex-M3 processor when it is associated character is at the top of
      --  the FIFO. When break occurs, only one zero character is loaded into
      --  the FIFO.
      BI   : Boolean;
      --  Read-only. Transmitter holding register empty (THRE). Indicates that
      --  the MMUART_x is ready to transmit a new data byte. THRE causes an
      --  interrupt to the Cortex-M3 processor when bit 1 (ETBEI) in the
      --  interrupt enable register is 1. This bit is set when the Tx FIFO is
      --  empty. It is cleared when at least one byte is written to the Tx
      --  FIFO.
      THRE : Boolean;
      --  Read-only. Transmit empty (TEMT). This bit is set to 1 when both the
      --  transmitter FIFO and shift registers are empty.
      TEMT : Boolean;
      --  Read-only. This bit is set when there is at least one parity error,
      --  framing error, or break indication in the FIFO. FIER is cleared when
      --  Cortex-M3 processor reads the LSR, if there are no subsequent errors
      --  in the FIFO.
      FIER : Boolean;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for LSR_Register use record
      DR   at 0 range 0 .. 0;
      OE   at 0 range 1 .. 1;
      PE   at 0 range 2 .. 2;
      FE   at 0 range 3 .. 3;
      BI   at 0 range 4 .. 4;
      THRE at 0 range 5 .. 5;
      TEMT at 0 range 6 .. 6;
      FIER at 0 range 7 .. 7;
   end record;

   --  Modem Status register
   type MSR_Register is record
      --  Read-only. Delta clear to send (DCTS) indicator. Indicates that the
      --  CTSn input has changed state since the last time it was read by the
      --  Cortex-M3 processor. Whenever bit 0, 1, 2, or 3 is set to 1, a modem
      --  status interrupt is generated.
      DCTS : Boolean;
      --  Read-only. Delta data set ready (DDSR) indicator. Indicates that the
      --  DSRn input has changed state since the last time it was read by the
      --  Cortex-M3 processor. Whenever bit 0, 1, 2, or 3 is set to 1, a modem
      --  status interrupt is generated.
      DDSR : Boolean;
      --  Read-only. Trailing edge of ring indicator (TERI) detector. Indicates
      --  that RI input has changed from 0 to 1. Whenever bit 0, 1, 2, or 3 is
      --  set to 1, a modem status interrupt is generated.
      TERI : Boolean;
      --  Read-only. Delta data carrier detect (DDCD) indicator. Indicates that
      --  DCD input has changed state. Whenever bit 0, 1, 2, or 3 is set to 1,
      --  a modem status interrupt is generated.
      DDCD : Boolean;
      --  Read-only. Clear to send (CTS) (MMUART_x_CTS). The complement of the
      --  CTS input. When bit 4 of the MCR is set to 1 (loop), this bit is
      --  equivalent to DTR in the MCR.
      CTS  : Boolean;
      --  Read-only. Data set ready (DSR) (MMUART_x_DSR). The complement of the
      --  DSR input. When bit 4 of the MCR is set to 1 (loop), this bit is
      --  equivalent to RTS in the MCR.
      DSR  : Boolean;
      --  Read-only. Ring indicator (RI) (MMUART_x_RI). The complement of the
      --  RI input. When bit 4 of the MCR is set to 1 (loop), this bit is
      --  equivalent to OUT1 in the MCR.
      RI   : Boolean;
      --  Read-only. Data carrier detect (DCD) (MMUART_x_DCD).The complement of
      --  DCD input. When bit 4 of the MCR is set to 1 (loop), this bit is
      --  equivalent to OUT2 in the MCR.
      DCD  : Boolean;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MSR_Register use record
      DCTS at 0 range 0 .. 0;
      DDSR at 0 range 1 .. 1;
      TERI at 0 range 2 .. 2;
      DDCD at 0 range 3 .. 3;
      CTS  at 0 range 4 .. 4;
      DSR  at 0 range 5 .. 5;
      RI   at 0 range 6 .. 6;
      DCD  at 0 range 7 .. 7;
   end record;

   --  Multi-mode interrupt enable register
   type IEM_Register is record
      --  Enables receiver timeout interrupt
      ERTOI        : Boolean := False;
      --  Enables NACK interrupt
      ENACKI       : Boolean := False;
      --  Enables PID parity error interrupt
      EPID_PEI     : Boolean := False;
      --  Enables LIN break interrupt
      ELINBI       : Boolean := False;
      --  Enables the LIN sync detection interrupt
      ELINSI       : Boolean := False;
      --  unspecified
      Reserved_5_7 : Interfaces.SF2.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for IEM_Register use record
      ERTOI        at 0 range 0 .. 0;
      ENACKI       at 0 range 1 .. 1;
      EPID_PEI     at 0 range 2 .. 2;
      ELINBI       at 0 range 3 .. 3;
      ELINSI       at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
   end record;

   --  Multi-mode Interrupt identification register
   type IIM_Register is record
      --  Receiver time-out (RTO) interrupt ID. RTO interrupt is asserted when
      --  RTO value is reached by the counter. It gets cleared when writing to
      --  the RTO register.
      RTOII        : Boolean := False;
      --  NACK interrupt is asserted when EERR bit is set in MM2. Reading the
      --  MM2 clears the interrupt.
      NACKI        : Boolean := False;
      --  Protected identifier field (PID) parity error interrupt is generated
      --  when there is a mismatch in PID in LIN header, that is, when either
      --  the P0 or P1 bits in the incoming PID byte do not match the
      --  calculated P0 and P1 error.
      PID_PEI      : Boolean := False;
      --  LIN break interrupt, set automatically when break length of 11.5
      --  Tbits is detected. Reading the IIM register clears this interrupt.
      LINBI        : Boolean := True;
      --  LIN sync detection interrupt ID. This bit set when 5th falling edge
      --  is detected by the sync timer. It resets the FIFO address pointers so
      --  that the PID will be in the first location. Reading the IIM register
      --  clears this interrupt.
      LINSI        : Boolean := False;
      --  unspecified
      Reserved_5_7 : Interfaces.SF2.UInt3 := 16#1#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for IIM_Register use record
      RTOII        at 0 range 0 .. 0;
      NACKI        at 0 range 1 .. 1;
      PID_PEI      at 0 range 2 .. 2;
      LINBI        at 0 range 3 .. 3;
      LINSI        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
   end record;

   --  Enable synchronous operation. There are four types of Synchronous
   --  Operation modes that can be enabled.
   type MM0_ESYN_Field is
     (
      --  Disabled, that is, Asynchronous mode
      Disabled,
      --  Synchronous slave enabled, positive-edge clock
      Slave_Positive_Edge,
      --  Synchronous slave enabled, negative-edge clock
      Slave_Negative_Edge,
      --  Synchronous master enabled, positive-edge clock
      Master_Positive_Edge,
      --  Synchronous master enabled, negative-edge clock
      Master_Negative_Edge)
     with Size => 3;
   for MM0_ESYN_Field use
     (Disabled => 0,
      Slave_Positive_Edge => 1,
      Slave_Negative_Edge => 2,
      Master_Positive_Edge => 3,
      Master_Negative_Edge => 4);

   --  Multi-mode control register 0
   type MM0_Register is record
      --  Enable synchronous operation. There are four types of Synchronous
      --  Operation modes that can be enabled.
      ESYN         : MM0_ESYN_Field := Interfaces.SF2.MMUART.Disabled;
      --  Enable LIN header detection and automatic baud rate calculation.
      ELIN         : Boolean := False;
      --  unspecified
      Reserved_4_4 : Interfaces.SF2.Bit := 16#0#;
      --  Enable transmitter time guard (TTG). The time guard value is
      --  determined by the TTG Register.
      ETTG         : Boolean := False;
      --  Enable receiver timeout (RTO). Writing this bit enables the timeout
      --  and restarts the counter value. The timeout value is determined by
      --  the RTO register.
      ERTO         : Boolean := False;
      --  Enable fractional baud rate (FBR) mode.
      EFBR         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MM0_Register use record
      ESYN         at 0 range 0 .. 2;
      ELIN         at 0 range 3 .. 3;
      Reserved_4_4 at 0 range 4 .. 4;
      ETTG         at 0 range 5 .. 5;
      ERTO         at 0 range 6 .. 6;
      EFBR         at 0 range 7 .. 7;
   end record;

   --  You can configure input polarity for RZI demodulation.
   type MM1_EIRX_Field is
     (
      --  RZI input pulses are active Low, signifying a low NRZ value
      Active_Low,
      --  RZI input pulses are active High, signifying a high NRZ value
      Active_High)
     with Size => 1;
   for MM1_EIRX_Field use
     (Active_Low => 0,
      Active_High => 1);

   --  You can configure output polarity for RZI modulation.
   type MM1_EITX_Field is
     (
      --  RZI output pulses are active Low, signifying a low NRZ value
      Active_Low,
      --  RZI output pulses are active High, signifying a high NRZ value
      Active_High)
     with Size => 1;
   for MM1_EITX_Field use
     (Active_Low => 0,
      Active_High => 1);

   --  Output pulse width for RZI mod can be modified using this bit.
   type MM1_EITP_Field is
     (
      --  3/16th Tbit pulse width
      Width_3_16,
      --  1/4th Tbit pulse width.
      Width_1_4)
     with Size => 1;
   for MM1_EITP_Field use
     (Width_3_16 => 0,
      Width_1_4 => 1);

   --  Multi-mode control register 1
   type MM1_Register is record
      --  LSB or MSB can be received first by configuring this bit. By default,
      --  the receiver buffer register's (RBR) bit 0 is the LSB, and is the
      --  first received bit. Bit 0 of the RBR may be configured as the last
      --  received bit, MSB.
      E_MSB_RX     : Boolean := False;
      --  LSB or MSB can be sent first by configuring this bit. By default, the
      --  "THR" bit 0 is the LSB, and is the first transmitted bit. Bit 0 of
      --  the THR may be configured as the last transmitted bit, MSB.
      E_MSB_TX     : Boolean := False;
      --  Enables RZI modulation/demodulation.
      EIRD         : Boolean := False;
      --  You can configure input polarity for RZI demodulation.
      EIRX         : MM1_EIRX_Field := Interfaces.SF2.MMUART.Active_Low;
      --  You can configure output polarity for RZI modulation.
      EITX         : MM1_EITX_Field := Interfaces.SF2.MMUART.Active_Low;
      --  Output pulse width for RZI mod can be modified using this bit.
      EITP         : MM1_EITP_Field := Interfaces.SF2.MMUART.Width_3_16;
      --  unspecified
      Reserved_6_7 : Interfaces.SF2.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MM1_Register use record
      E_MSB_RX     at 0 range 0 .. 0;
      E_MSB_TX     at 0 range 1 .. 1;
      EIRD         at 0 range 2 .. 2;
      EIRX         at 0 range 3 .. 3;
      EITX         at 0 range 4 .. 4;
      EITP         at 0 range 5 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
   end record;

   --  Multi-mode control register 2
   type MM2_Register is record
      --  When the EERR bit is set, the receiver forces an error signal
      --  transmit out, if an incoming parity error is detected. Error signal
      --  (ACK/NACK) is sent during stop time enable.
      EERR         : Boolean := False;
      --  Enable automatic 9-bit address flag mode (EAFM). It should be noted
      --  that for enabling this bit it requires, the LCR should be in an 8-bit
      --  and stick parity (SP) bit configured to 0. If EAFM bit is disabled,
      --  the Rx FIFO is enabled by receiving all the bytes. When EAFM bit is
      --  enabled, the Rx FIFO is disabled until an address flag with matching
      --  address is received.
      EAFM         : Boolean := False;
      --  Enable a flag clear (EAFC). When EAFM is enabled the Rx FIFO is
      --  disabled until another address flag with matching address is
      --  received. The bit gets cleared on write in multi-mode control
      --  registers 2.
      EAFC         : Boolean := False;
      --  Enable single-wire, half-duplex mode.
      ESWM         : Boolean := False;
      --  unspecified
      Reserved_4_7 : Interfaces.SF2.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MM2_Register use record
      EERR         at 0 range 0 .. 0;
      EAFM         at 0 range 1 .. 1;
      EAFC         at 0 range 2 .. 2;
      ESWM         at 0 range 3 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
   end record;

   subtype DFR_DFR_Field is Interfaces.SF2.UInt6;

   --  Divisor Fractional Register
   type DFR_Register is record
      --  The fractional divisor register (DFR) is used to store the fractional
      --  divisor used to calculate the fractional baud rate value in 1/64th
      --  using EQ 5. 0x0: 0/64 0x1: 1/64 etc.
      DFR          : DFR_DFR_Field := 16#0#;
      --  unspecified
      Reserved_6_7 : Interfaces.SF2.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for DFR_Register use record
      DFR          at 0 range 0 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
   end record;

   type GFR_GLR_Field is
     (
      --  Two resynchronize flip-flops are used but there is no spike
      --  suppression.
      Two_Flip_Flops_No_Spike,
      --  Three resynchronize flip-flops are used but there is no spike
      --  suppression.
      Three_Flip_Flops_No_Spike,
      --  Three resynchronize flip-flops are used and it also causes 1 APB
      --  clock cycle suppression.
      Three_Flip_Flops_1_Cycle_Suppression,
      --  Three resynchronize flip-flops are used and it also causes 2 APB
      --  clock cycle suppression.
      Three_Flip_Flops_2_Cycle_Suppression,
      --  Three resynchronize flip-flops are used and it also causes 3 APB
      --  clock cycle suppression.
      Three_Flip_Flops_3_Cycle_Suppression,
      --  Three resynchronize flip-flops are used and it also causes 4 APB
      --  clock cycle suppression.
      Three_Flip_Flops_4_Cycle_Suppression,
      --  Three resynchronize flip-flops are used and it also causes 5 APB
      --  clock cycle suppression.
      Three_Flip_Flops_5_Cycle_Suppression,
      --  Three resynchronize flip-flops are used and it also causes 6 APB
      --  clock cycle suppression.
      Three_Flip_Flops_6_Cycle_Suppression)
     with Size => 3;
   for GFR_GLR_Field use
     (Two_Flip_Flops_No_Spike => 0,
      Three_Flip_Flops_No_Spike => 1,
      Three_Flip_Flops_1_Cycle_Suppression => 2,
      Three_Flip_Flops_2_Cycle_Suppression => 3,
      Three_Flip_Flops_3_Cycle_Suppression => 4,
      Three_Flip_Flops_4_Cycle_Suppression => 5,
      Three_Flip_Flops_5_Cycle_Suppression => 6,
      Three_Flip_Flops_6_Cycle_Suppression => 7);

   --  Glitch filter register
   type GFR_Register is record
      GLR          : GFR_GLR_Field :=
                      Interfaces.SF2.MMUART.Two_Flip_Flops_No_Spike;
      --  unspecified
      Reserved_3_7 : Interfaces.SF2.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for GFR_Register use record
      GLR          at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type MMUART_0_Disc is
     (
      Mode_1,
      Mode_2,
      Mode_3);

   --  Multi-mode universal asynchronous/synchronous receiver/transmitter
   --  peripheral
   type MMUART_Peripheral
     (Discriminent : MMUART_0_Disc := Mode_1)
   is record
      --  Line Control register
      LCR : aliased LCR_Register;
      --  Modem Control register
      MCR : aliased MCR_Register;
      --  Line Status register
      LSR : aliased LSR_Register;
      --  Modem Status register
      MSR : aliased MSR_Register;
      --  Scratch register. This register has no effect on MMUART_x operation.
      SR  : aliased Interfaces.SF2.Byte;
      --  Multi-mode interrupt enable register
      IEM : aliased IEM_Register;
      --  Multi-mode Interrupt identification register
      IIM : aliased IIM_Register;
      --  Multi-mode control register 0
      MM0 : aliased MM0_Register;
      --  Multi-mode control register 1
      MM1 : aliased MM1_Register;
      --  Multi-mode control register 2
      MM2 : aliased MM2_Register;
      --  Divisor Fractional Register
      DFR : aliased DFR_Register;
      --  Glitch filter register
      GFR : aliased GFR_Register;
      --  Transmitter time guard register. If the transmitter time guard is
      --  enabled from the multi-mode control register 0 (MM0), the transmitter
      --  time guard value determines the amount of system clock cycles to wait
      --  between transmissions. The time guard equation is based on the baud
      --  rate bit time (Tbit) value as follows: Tx Time Guard Value = TTG x
      --  Bit Time (Tbit)
      TTG : aliased Interfaces.SF2.Byte;
      --  Receiver time-tou receiver. Writing to the RTO register sets the
      --  counter value and enables, if the ERTO bit in the MM0 is enabled. You
      --  can configure the timeout value by writing into this register. The
      --  RTO counts when the Rx block input state is idle; is reset when a
      --  start condition occurs, and restarts counting upon returning to the
      --  idle state. When the RTO value is reached, the RTOII interrupt is
      --  set. Re-writing the RTO register clears the interrupt and sets the
      --  counter. The receiver timeout value equation is based on the baud
      --  rate bit time (Tbit) as follows: Rx Timeout Value = 4 x RTO x Bit
      --  Time (Tbit)
      RTO : aliased Interfaces.SF2.Byte;
      --  Address register. The address register is used in 9-bit Address Flag
      --  mode. When an address flag is received on the 9th bit, and EAFM is
      --  set in MM2, the incoming data is checked against the address
      --  register. If a match occurs, the Rx FIFO is enabled.
      ADR : aliased Interfaces.SF2.Byte;
      case Discriminent is
         when Mode_1 =>
            --  Receiver buffer register. This register holds the receive data
            --  bits for MMUART_x. The default value is unknown since the
            --  register is loaded with data in the receive FIFO. Bit 0 is the
            --  LSB and it is the first bit received. It may be configured as
            --  the MSB by configuring the E_MSB_RX bit in the MM1. The divisor
            --  latch access bit (DLAB), bit 7 of LCR, must be 0 to read this
            --  register. This register is read only. Writing to this register
            --  with the DLAB 0 changes the transmit holding register (THR)
            --  register value.
            RBR : aliased Interfaces.SF2.Byte;
            --  Divisor latch (MSB)
            DMR : aliased Interfaces.SF2.Byte;
            --  FIFO control register
            FCR : aliased FCR_Register;
         when Mode_2 =>
            --  Transmit holding register. This register holds the data bits to
            --  be transmitted. Bit 0 is the LSB and is transmitted first. The
            --  MSB may be transmitted first, if it is configured with the
            --  E_MSB_TX bit in the MM1. The reset value is unknown since the
            --  register is loaded with data in the transmit FIFO. The DLAB,
            --  bit 7 of LCR, must be 0 to write to this register. This
            --  register is write only. Reading from this register with the
            --  DLAB 0 reads the RBR register value.
            THR : aliased Interfaces.SF2.Byte;
            --  Interupt Enable Register
            IER : aliased IER_Register;
            --  Interrupt identification register
            IIR : aliased IIR_Register;
         when Mode_3 =>
            --  Divisor latch (LSB)
            DLR : aliased Interfaces.SF2.Byte;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for MMUART_Peripheral use record
      LCR at 16#C# range 0 .. 7;
      MCR at 16#10# range 0 .. 7;
      LSR at 16#14# range 0 .. 7;
      MSR at 16#18# range 0 .. 7;
      SR  at 16#1C# range 0 .. 7;
      IEM at 16#24# range 0 .. 7;
      IIM at 16#28# range 0 .. 7;
      MM0 at 16#30# range 0 .. 7;
      MM1 at 16#34# range 0 .. 7;
      MM2 at 16#38# range 0 .. 7;
      DFR at 16#3C# range 0 .. 7;
      GFR at 16#44# range 0 .. 7;
      TTG at 16#48# range 0 .. 7;
      RTO at 16#4C# range 0 .. 7;
      ADR at 16#50# range 0 .. 7;
      RBR at 16#0# range 0 .. 7;
      DMR at 16#4# range 0 .. 7;
      FCR at 16#8# range 0 .. 7;
      THR at 16#0# range 0 .. 7;
      IER at 16#4# range 0 .. 7;
      IIR at 16#8# range 0 .. 7;
      DLR at 16#0# range 0 .. 7;
   end record;

   --  Multi-mode universal asynchronous/synchronous receiver/transmitter
   --  peripheral
   MMUART_0_Periph : aliased MMUART_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

   --  Multi-mode universal asynchronous/synchronous receiver/transmitter
   --  peripheral
   MMUART_1_Periph : aliased MMUART_Peripheral
     with Import, Address => System'To_Address (16#40010000#);

end Interfaces.SF2.MMUART;
