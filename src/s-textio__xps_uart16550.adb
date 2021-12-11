------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This is the Xilinx XPS UART16550 version of this package

--  The UART interface is configured for 115200 baud rate, 8-bits, no parity
--  and 1 stop bit. The baud rate is set by the constant below. This package
--  uses a simple polling interface.

with Interfaces;
with System.BB.Board_Parameters;

package body System.Text_IO is
   use Interfaces;

   Baud_Rate : constant := 115_200;

   --  XPS UART16550 Registers

   pragma Warnings (Off, "*is not referenced*");
   --  Disable warnings on unused interface entities

   UART16550_Base_Address          : constant := 16#83E0_0000#;
   Receiver_Buffer_Offset          : constant := 16#1000#;
   Transmitter_Holding_Offset      : constant := 16#1000#;
   Interrupt_Enable_Offset         : constant := 16#1004#;
   Interrupt_Identification_Offset : constant := 16#1008#;
   FIFO_Control_Offset             : constant := 16#1008#;
   Line_Control_Offset             : constant := 16#100C#;
   Modem_Control_Offset            : constant := 16#1010#;
   Line_Status_Offset              : constant := 16#1014#;
   Modem_Status_Offset             : constant := 16#1018#;
   Scratch_Offset                  : constant := 16#101C#;
   Divisor_Latch_Low_Offset        : constant := 16#1000#;
   Divisor_Latch_High_Offset       : constant := 16#1004#;

   type Interrupt_Enable is record
      Enable_Modem_Status_Interrupt                       : Boolean;
      Enable_Receiver_Line_Status_Interrupt               : Boolean;
      Enable_Transmitter_Holding_Register_Empty_Interrupt : Boolean;
      Enable_Received_Data_Available_Interrupt            : Boolean;
   end record with Size => 32;

   for Interrupt_Enable use record
      Enable_Modem_Status_Interrupt                       at 0 range 28 .. 28;
      Enable_Receiver_Line_Status_Interrupt               at 0 range 29 .. 29;
      Enable_Transmitter_Holding_Register_Empty_Interrupt at 0 range 30 .. 30;
      Enable_Received_Data_Available_Interrupt            at 0 range 31 .. 31;
   end record;

   type UART16550_Interrupt_ID is
     (Modem_Status, Transmitter_Holding_Register_Empty,
      Received_Data_Available, Receiver_Line_Status, Character_Timeout);

   type Interrupt_Identification is record
      FIFOs_Enabled         : Boolean;
      Interrupt_ID          : UART16550_Interrupt_ID;
      Interrupt_Not_Pending : Boolean;
   end record with Size => 32;

   for Interrupt_Identification use record
      FIFOs_Enabled         at 0 range 24 .. 25;
      Interrupt_ID          at 0 range 28 .. 30;
      Interrupt_Not_Pending at 0 range 31 .. 31;
   end record;

   type Stop_Bits_Type is (One, Two);
   type Word_Length_Type is (Five, Six, Seven, Eight);

   type Line_Control is record
      Divisor_Latch_Access : Boolean;
      Set_Break            : Boolean;
      Stick_Parity         : Boolean;
      Even_Parity          : Boolean;
      Parity_Enable        : Boolean;
      Stop_Bits            : Stop_Bits_Type;
      Word_Length          : Word_Length_Type;
   end record with Size => 32;

   for Line_Control use record
      Divisor_Latch_Access at 0 range 24 .. 24;
      Set_Break            at 0 range 25 .. 25;
      Stick_Parity         at 0 range 26 .. 26;
      Even_Parity          at 0 range 27 .. 27;
      Parity_Enable        at 0 range 28 .. 28;
      Stop_Bits            at 0 range 29 .. 29;
      Word_Length          at 0 range 30 .. 31;
   end record;

   type Line_Status is record
      Error_in_RCVR_FIFO                 : Boolean;
      Transmitter_Empty                  : Boolean;
      Transmitter_Holding_Register_Empty : Boolean;
      Break_Interrupt                    : Boolean;
      Framing_Error                      : Boolean;
      Parity_Error                       : Boolean;
      Overrun_Error                      : Boolean;
      Data_Ready                         : Boolean;
   end record with Size => 32;

   for Line_Status use record
      Error_in_RCVR_FIFO                 at 0 range 24 .. 24;
      Transmitter_Empty                  at 0 range 25 .. 25;
      Transmitter_Holding_Register_Empty at 0 range 26 .. 26;
      Break_Interrupt                    at 0 range 27 .. 27;
      Framing_Error                      at 0 range 28 .. 28;
      Parity_Error                       at 0 range 29 .. 29;
      Overrun_Error                      at 0 range 30 .. 30;
      Data_Ready                         at 0 range 31 .. 31;
   end record;

   Receiver_Buffer_Register : Character
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Receiver_Buffer_Offset + 3);

   Transmitter_Holding_Register : Character
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Transmitter_Holding_Offset + 3);

   Interrupt_Enable_Register : Interrupt_Enable
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Interrupt_Enable_Offset);

   Interrupt_Identification_Register : Interrupt_Identification
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Interrupt_Identification_Offset);

   Line_Control_Register : Line_Control
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Line_Control_Offset);

   Line_Status_Register : Line_Status
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Line_Status_Offset);

   Divisor_Latch_Low : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Divisor_Latch_Low_Offset);

   Divisor_Latch_High : Unsigned_32
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (UART16550_Base_Address + Divisor_Latch_High_Offset);

   pragma Warnings (On, "*is not referenced*");
   --  Restore warnings

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Retrive character from receiver buffer
      return Receiver_Buffer_Register;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Divisor : constant Unsigned_32 :=
         System.BB.Board_Parameters.PLB_Clock_Frequency / (Baud_Rate * 16);
   begin
      --  Configured the UART for 115200 baud rate, 8-bits, no parity
      --  and 1 stop bit.

      --  Set divisor latch

      Line_Control_Register.Divisor_Latch_Access := True;
      Divisor_Latch_Low  := Divisor;
      Divisor_Latch_High := Shift_Right (Divisor, 8);

      --  Setup Line Control Register

      Line_Control_Register :=
        (Divisor_Latch_Access => False,
         Set_Break            => False,
         Stick_Parity         => False,
         Even_Parity          => True,
         Parity_Enable        => False,
         Stop_Bits            => One,
         Word_Length          => Eight);

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return Line_Status_Register.Data_Ready;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return Line_Status_Register.Transmitter_Holding_Register_Empty;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character
      Transmitter_Holding_Register := C;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
