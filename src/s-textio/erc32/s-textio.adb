------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
-- The port of GNARL to bare board  targets was initially developed by the  --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

package body System.Text_IO is

   -----------
   -- Local --
   -----------

   Control_Register_Address :
   constant System.Address := 16#1F80000#;

   UART_Channel_A_Rx_Tx_Register_Address :
   constant System.Address := 16#01F800E0#;

   UART_Status_Register_Address :
   constant System.Address := 16#01F800E8#;

   type Scaler_8 is mod 2 ** 8;
   for Scaler_8'Size use  8;
   --  8-bit scaler

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Control_Register is
      record
         PRD : Boolean;
         --  Power-down 1  : enabled (allowed) 0 : disabled r/w

         SWR : Boolean;
         --  Software reset 1  : enabled (allowed) 0 : disabled r/w

         BTO : Boolean;
         --  Bus timeout 1  : enabled 0 : disabled r/w

         BP : Boolean;
         --  Block protection instead of normal access protection
         --  1  : enabled 0 : disabled r/w

         WDCS : Boolean;
         --  Watchdog clock supply
         --  1  : external clock with prescaler (divide by 16)
         --  0  : external clock, no prescaler r/w

         IUEMMSK : Boolean;
         --  IU Error Mode Mask 1  : Error masked (= disabled)
         --  0  : Error not masked r/w

         RHIUEM : Boolean;
         --  Reset or Halt when IU error mode (ERROR*)
         --  1  : Reset 0 : Halt r/w

         IUHEMSK : Boolean;
         --  IU Hardware Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHIUHE : Boolean;
         --  Reset or Halt when IU Hardware Error (HWERR*)
         --  1  : Reset 0 : Halt r/w

         IUCMPMSK : Boolean;
         --  IU Comparison Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHIUCMP : Boolean;
         --  Reset or Halt when IU comparison error 1  : Reset 0 : Halt r/w

         FPUCMPMSK : Boolean;
         --  FPU Comparison Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHFPUCMP : Boolean;
         --  Reset or Halt when FPU comparison error
         --  1  : Reset 0 : Halt r/w

         MECHEMSK : Boolean;
         --  MEC HW Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHMECHE : Boolean;
         --  Reset or Halt when MEC HW Error (MECHWERR)
         --  1  : Reset 0 : Halt r/w

         RESERVED : Boolean;
         --  Not used r

         DMAE : Boolean;
         --  1 DMA 1  : enabled 0 : disabled r/w

         DPE : Boolean;
         --  DMA Parity Enabled 1  : enabled 0 : disabled r/w

         DST : Boolean;
         --  DMA session timeout 1  : enabled 0 : disabled r/w

         UBR : Boolean;
         --  UART baud rate(1)
         --  1  : No change of UART scaler baudrate
         --  0  : Divide UART scaler baudrate by two r/w

         UPE : Boolean;
         --  UART parity enable
         --  1  : parity enabled 0 : no parity r/w

         UP : Boolean;
         --  UART parity 1  : odd parity 0 : even parity r/w

         USB : Boolean;
         --   UART stop bits 1  : two stop bits 0 : one stop bit r/w

         UCS : Boolean;
         --  UART clock supply 1  : system clock 0 : external clock r/w

         UART_Scaler : Scaler_8;
         --  1 - 255 : Divide factor (1) 0: stops the UART clock r/w
      end record;

   for Control_Register use
      record
         PRD at 0 range 31 .. 31;
         SWR at 0 range 30 .. 30;
         BTO at 0 range 29 .. 29;
         BP at 0 range 28 .. 28;
         WDCS at 0 range 27 .. 27;
         IUEMMSK at 0 range 26 .. 26;
         RHIUEM at 0 range 25 .. 25;
         IUHEMSK at 0 range 24 .. 24;
         RHIUHE at 0 range 23 .. 23;
         IUCMPMSK at 0 range 22 .. 22;
         RHIUCMP at 0 range 21 .. 21;
         FPUCMPMSK at 0 range 20 .. 20;
         RHFPUCMP at 0 range 19 .. 19;
         MECHEMSK at 0 range 18 .. 18;
         RHMECHE at 0 range 17 .. 17;
         RESERVED at 0 range 16 .. 16;
         DMAE at 0 range 15 .. 15;
         DPE at 0 range 14 .. 14;
         DST at 0 range 13 .. 13;
         UBR at 0 range 12 .. 12;
         UPE at 0 range 11 .. 11;
         UP at 0 range 10 .. 10;
         USB at 0 range 9 .. 9;
         UCS at 0 range 8 .. 8;
         UART_scaler at 0 range 0 .. 7;
      end record;

   for Control_Register'Size use 32;

   pragma Suppress_Initialization (Control_Register);

   Control : Control_Register;
   pragma Atomic (Control);
   for Control'Address use Control_Register_Address;

   type UART_Channel_Rx_Tx_Register is
      record
         RTD : Character;
         --  Rx/Tx Data r/w

         Reserved24 : Reserved_24;
         --  Not used r
      end record;

   for UART_Channel_Rx_Tx_Register use
      record
         RTD at 0 range 24 .. 31;
         Reserved24 at 0 range 0 .. 23;
      end record;

   for UART_Channel_Rx_Tx_Register'Size use 32;

   pragma Suppress_Initialization (UART_Channel_Rx_Tx_Register);

   UART_Channel_A : UART_Channel_Rx_Tx_Register;
   pragma Atomic (UART_Channel_A);
   for UART_Channel_A'Address use UART_Channel_A_Rx_Tx_Register_Address;

      type UART_Status_Register is
      record
         DRA : Boolean;
         --  Data Ready in channel A r

         TSEA : Boolean;
         --  Transmitter A Send register Empty (no data to send) r

         THEA : Boolean;
         --  Transmitter A Holding register Empty (ready to load data) r

         Reserved1A : Boolean;
         --  Not used r

         FEA : Boolean;
         --  Framing Error in receiver A r

         PEA : Boolean;
         --  Parity Error in receiver A r

         OEA : Boolean;
         --  Overrun Error in receiver A r

         CUA : Boolean;
         --  Clear UART A (bit read as zero) r/w

         Reserved8A : Reserved_8;
         --  Not used r

         DRB : Boolean;
         --  Data Ready in channel B r

         TSEB : Boolean;
         --  Transmitter B Send register Empty (no data to send) r

         THEB : Boolean;
         --  Transmitter B Holding register Empty (ready to load data) r

         Reserved1B : Boolean;
         --  Not used r

         FEB : Boolean;
         --  Framing Error in receiver B r

         PEB : Boolean;
         --  Parity Error in receiver B r

         OEB : Boolean;
         --  Overrun Error in receiver B r

         CUB : Boolean;
         --  Clear UART B (bit read as zero) r/w

         Reserved8B : Reserved_8;
         --  Not used r
      end record;

   for UART_Status_Register use
      record
         DRA at 0 range 31 .. 31;
         TSEA at 0 range 30 .. 30;
         THEA at 0 range 29 .. 29;
         Reserved1A at 0 range 28 .. 28;
         FEA at 0 range 27 .. 27;
         PEA at 0 range 26 .. 26;
         OEA at 0 range 25 .. 25;
         CUA at 0 range 24 .. 24;
         Reserved8A at 0 range 16 .. 23;
         DRB at 0 range 15 .. 15;
         TSEB at 0 range 14 .. 14;
         THEB at 0 range 13 .. 13;
         Reserved1B at 0 range 12 .. 12;
         FEB at 0 range 11 .. 11;
         PEB at 0 range 10 .. 10;
         OEB at 0 range 9 .. 9;
         CUB at 0 range 8 .. 8;
         Reserved8B at 0 range 0 .. 7;
      end record;

   for UART_Status_Register'Size use 32;

   pragma Suppress_Initialization (UART_Status_Register);

   UART_Status : UART_Status_Register;
   pragma Atomic (UART_Status);
   for UART_Status'Address use UART_Status_Register_Address;

   Clock_Frequency : constant Natural;  --  Hertz
   pragma Import (Asm, Clock_Frequency, "clock_frequency");
   --  Frequency of the system clock

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Will never be called

      raise Program_Error;
      return ASCII.NUL;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Control_Aux : Control_Register;
      Scaler_Aux  : Scaler_8;

   begin
      --  Initialize the UART1 as output console

      --  Read the Control Register

      Control_Aux := Control;

      --  Set the UART scaler according to the baudrate given

      Scaler_Aux := Scaler_8 (Clock_Frequency / (32 * 115200 * 2));

      Control_Aux.UART_Scaler := Scaler_8'Max (Scaler_Aux - 1, 1);

      Control_Aux.UBR := False;

      Control_Aux.UPE := False;
      Control_Aux.USB := False;

      Control_Aux.UCS := True;

      --  Write to the Control Register in the MEC

      Control := Control_Aux;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return False;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      UART_Status_Aux : constant UART_Status_Register := UART_Status;
   begin
      return UART_Status_Aux.THEA;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
      UART_Tx : constant UART_Channel_Rx_Tx_Register :=
                (RTD => C, Reserved24 => (others => False));
   begin
      UART_Channel_A := UART_Tx;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
