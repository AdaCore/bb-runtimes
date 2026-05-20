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

--  Implementation for the MPC5200B.
--  This package uses PSC1 and configures it for UART operation with a baud
--  speed of 115,200, 8 data bits, no parity and 1 stop bit.

with Interfaces;
with System.BB.Board_Parameters;

package body System.Text_IO is
   use Interfaces;
   use System.BB.Board_Parameters;

   Baud_Rate : constant := 115_200;
   --  UART baud rate

   --  GPS Port Configuration Register
   --  See MPC5200B User's Manual, Section 7.3.2

   type Other_GPS_Data is mod 2 ** 29;
   type PSC1_Pins_Configuration is (GPIO, UART);
   for PSC1_Pins_Configuration use (GPIO => 2#0#, UART => 2#100#);

   type UART_GPS_Port_Configuration is record
      Other_Data : Other_GPS_Data;
      PSC1       : PSC1_Pins_Configuration;
   end record with Size => 32;

   for UART_GPS_Port_Configuration use record
      Other_Data at 0 range 0  .. 28;
      PSC1       at 0 range 29 .. 31;
   end record;

   GPS_Port_Configuration_Register : UART_GPS_Port_Configuration
     with Volatile_Full_Access,
       Address =>
         System'To_Address (MBAR + 16#0B00#);

   --  PSC1 Registers
   --  See MPC5200B User's Manual, Chapter 15

   pragma Warnings (Off, "*is not referenced*");
   --  Disable warnings on unused interface entities

   PSC1_Base_Address               : constant := MBAR + 16#2000#;
   Mode_Offset                     : constant := 16#00#;
   Status_Offset                   : constant := 16#04#;
   Clock_Select_Offset             : constant := 16#04#;
   Command_Offset                  : constant := 16#08#;
   Buffer_Offset                   : constant := 16#0C#;
   Interrupt_Mask_Offset           : constant := 16#14#;
   Counter_Timer_Upper_Offfset     : constant := 16#18#;
   Counter_Timer_Lower_Offfset     : constant := 16#1C#;
   Serial_Interface_Control_Offset : constant := 16#40#;
   Tx_FIFO_Control_Offset          : constant := 16#88#;
   Transmitter_FIFO_Alarm_Offset   : constant := 16#8E#;

   type Receiver_Interrupt_Source is (RxRDY, FFULL);
   type Parity_Mode is (With_Parity, Force_Parity, No_Parity, Multidrop_Mode);
   type Parity_Option is (Even_Low_Data_Char, Odd_High_Addr_Char);
   type Bits_Option is (Five, Six, Seven, Eight);

   type Mode_1 is record
      Receiver_Request_To_Send : Boolean;
      Receiver_Interrupt       : Receiver_Interrupt_Source;
      Parity                   : Parity_Mode;
      Parity_Type              : Parity_Option;
      Bits_Per_Character       : Bits_Option;
   end record;

   for Mode_1 use record
      Receiver_Request_To_Send at 0 range 0 .. 0;
      Receiver_Interrupt       at 0 range 1 .. 1;
      Parity                   at 0 range 3 .. 4;
      Parity_Type              at 0 range 5 .. 5;
      Bits_Per_Character       at 0 range 6 .. 7;
   end record;

   type Channel_Mode_Type is
     (Normal, Automatic_Echo, Local_Loop_Back, Remote_Loop_Back);
   type Four_Bits is mod 2 ** 4;

   type Mode_2 is record
      Channel_Mode              : Channel_Mode_Type;
      Transmitter_Ready_To_Send : Boolean;
      Transmitter_Clear_To_Send : Boolean;
      Stop_Bit_Length           : Four_Bits;
   end record;

   for Mode_2 use record
      Channel_Mode              at 0 range 0 .. 1;
      Transmitter_Ready_To_Send at 0 range 2 .. 2;
      Transmitter_Clear_To_Send at 0 range 3 .. 3;
      Stop_Bit_Length           at 0 range 4 .. 7;
   end record;

   type Status is record
      Received_Break      : Boolean;
      Framing_Error       : Boolean;
      Parity_Error        : Boolean;
      Overrun_Error       : Boolean;
      Transmitter_Empty   : Boolean;
      Transmitter_Ready   : Boolean;
      Receiver_FIFO_Full  : Boolean;
      Receiver_Ready      : Boolean;
      DCD_Error           : Boolean;
      Error_Status_Detect : Boolean;
   end record with Size => 16;

   for Status use record
      Received_Break      at 0 range 0 .. 0;
      Framing_Error       at 0 range 1 .. 1;
      Parity_Error        at 0 range 2 .. 2;
      Overrun_Error       at 0 range 3 .. 3;
      Transmitter_Empty   at 0 range 4 .. 4;
      Transmitter_Ready   at 0 range 5 .. 5;
      Receiver_FIFO_Full  at 0 range 6 .. 6;
      Receiver_Ready      at 0 range 7 .. 7;
      DCD_Error           at 0 range 8 .. 8;
      Error_Status_Detect at 0 range 9 .. 9;
   end record;

   type Clock_Source is
     (IPB_Divided_By_32, Disable_Clock, IPB_Divided_By_4);

   for Clock_Source use
     (IPB_Divided_By_32 => 2#0#,
      Disable_Clock     => 2#1110#,
      IPB_Divided_By_4  => 2#1111#);

   type Clock_Select is record
      Receiver_Clock_Select    : Clock_Source;
      Transmitter_Clock_Select : Clock_Source;
   end record;

   for Clock_Select use record
      Receiver_Clock_Select    at 0 range 0 .. 3;
      Transmitter_Clock_Select at 0 range 4 .. 7;
   end record;

   type Misc_Commands is
     (No_Command, Reset_Mode_Register_Pointer, Reset_Receiver,
      Reset_Transmitter, Reset_Error_Status, Reset_Break_Change_Interrupt,
      Start_Break, Stop_Break);

   type Tx_Rx_Commands is (No_Action, Enable, Disable);

   type Command is record
      Misc        : Misc_Commands;
      Transmitter : Tx_Rx_Commands;
      Receiver    : Tx_Rx_Commands;
   end record;

   for Command use record
      Misc        at 0 range 1 .. 3;
      Transmitter at 0 range 4 .. 5;
      Receiver    at 0 range 6 .. 7;
   end record;

   type PSC_Modes is (UART, UART_DCD_Effective);
   for PSC_Modes use (UART => 2#0000#, UART_DCD_Effective => 2#1000#);

   type Serial_Interface_Control is record
      Operation_Mode : PSC_Modes;
   end record;

   for Serial_Interface_Control use record
      Operation_Mode at 0 range 4 .. 7;
   end record;

   type Alarm_Type is mod 2 ** 11 with Size => 16;

   Mode_1_Register : Mode_1
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Mode_Offset);

   Mode_2_Register : Mode_2
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Mode_Offset);

   Status_Register : Status
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Status_Offset);

   Clock_Select_Register : Clock_Select
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Clock_Select_Offset);

   Command_Register : Command
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Command_Offset);

   Transmitter_Buffer : Character
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Buffer_Offset);

   Receiver_Buffer : Character
     with Volatile_Full_Access,
       Address => System'To_Address (PSC1_Base_Address + Buffer_Offset);

   Interrupt_Mask_Register : Unsigned_16
     with Volatile_Full_Access,
       Address =>
         System'To_Address (PSC1_Base_Address + Interrupt_Mask_Offset);

   Counter_Timer_Upper_Register : Unsigned_8
     with Volatile_Full_Access,
       Address =>
         System'To_Address (PSC1_Base_Address + Counter_Timer_Upper_Offfset);

   Counter_Timer_Lower_Register : Unsigned_8
     with Volatile_Full_Access,
       Address =>
         System'To_Address (PSC1_Base_Address + Counter_Timer_Lower_Offfset);

   Serial_Interface_Control_Register : Serial_Interface_Control
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (PSC1_Base_Address + Serial_Interface_Control_Offset);

   Tx_FIFO_Control_Register : Unsigned_8
     with Volatile_Full_Access, Size => 8,
       Address =>
         System'To_Address (PSC1_Base_Address + Tx_FIFO_Control_Offset);

   Transmitter_FIFO_Alarm : Alarm_Type
     with Volatile_Full_Access, Size => 16,
       Address =>
         System'To_Address (PSC1_Base_Address + Transmitter_FIFO_Alarm_Offset);

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Receiver_Buffer;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Counter_Timer : constant Unsigned_16 :=
        Unsigned_16 (((IPB_Frequency * 10) / (32 * Baud_Rate) + 5) / 10);
      --  Perform the Counter_Timer calculation so that it rounds to the
      --  nearest integer. This helps produce an effective Baud Rate that is as
      --  close to the requested Baud Rate as possible.

   begin
      --  Initialize PSC1 following the guide in MPC5200B User's Manual,
      --  Section 15.3.1.

      --  Explicitly set Mode Register Pointer to Mode_1_Register in case the
      --  bootloader had set up PSC1 for its own use. If we did not do this
      --  then the next instructions will end up writing to the wrong address.

      Command_Register :=
        (Misc        => Reset_Mode_Register_Pointer,
         Transmitter => No_Action,
         Receiver    => No_Action);

      --  Reset the Transmitter and receiver to a known state before
      --  configuring.

      Command_Register :=
        (Misc        => Reset_Receiver,
         Transmitter => No_Action,
         Receiver    => No_Action);
      Command_Register :=
        (Misc        => Reset_Transmitter,
         Transmitter => No_Action,
         Receiver    => No_Action);

      --  PSC1 is configured for eight data bits, no parity and 1 stop bit

      Clock_Select_Register :=
        (Receiver_Clock_Select    => IPB_Divided_By_32,
         Transmitter_Clock_Select => IPB_Divided_By_32);
      Serial_Interface_Control_Register :=
        (Operation_Mode => UART);
      Mode_1_Register :=
        (Receiver_Request_To_Send => False,
         Receiver_Interrupt       => RxRDY,
         Parity                   => No_Parity,
         Parity_Type              => Even_Low_Data_Char,
         Bits_Per_Character       => Eight);
      Mode_2_Register :=
        (Channel_Mode              => Normal,
         Transmitter_Ready_To_Send => False,
         Transmitter_Clear_To_Send => False,
         Stop_Bit_Length           => 2#0111#);

      --  Split Counter_Timer into its upper and lower halves

      Counter_Timer_Lower_Register := Unsigned_8 (Counter_Timer and 16#FF#);
      Counter_Timer_Upper_Register :=
        Unsigned_8 (Shift_Right (Counter_Timer, 8));

      Tx_FIFO_Control_Register := 1;
      Transmitter_FIFO_Alarm := 428;
      GPS_Port_Configuration_Register.PSC1 := UART;

      --  Mask all PSC1 interrupts

      Interrupt_Mask_Register := 0;

      --  Enable PSC1 transmitter and receiver units

      Command_Register :=
        (Misc        => No_Command,
         Transmitter => Enable,
         Receiver    => Enable);
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return Status_Register.Receiver_Ready;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return Status_Register.Transmitter_Ready;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Transmitter_Buffer := C;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
