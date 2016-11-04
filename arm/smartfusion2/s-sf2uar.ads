------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with Interfaces;
private with Interfaces.SF2.MMUART;

package System.SF2.UART is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   type MSS_UART_Baud_Rate is
     (Baud_110,
      Baud_300,
      Baud_1200,
      Baud_2400,
      Baud_4800,
      Baud_9600,
      Baud_19200,
      Baud_38400,
      Baud_57600,
      Baud_115200,
      Baud_230400,
      Baud_460800,
      Baud_921600);

   type MSS_UART_Data_Bits is
     (Data_5_Bits,
      Data_6_Bits,
      Data_7_Bits,
      Data_8_Bits)
     with Size => 2;

   type MSS_UART_Parity is
     (No_Parity,
      Odd_Parity,
      Even_Parity,
      Stick_Parity_0,
      Stick_Parity_1);

   type MSS_UART_Word_Length is
     (
      Length_5_Bits,
      Length_6_Bits,
      Length_7_Bits,
      Length_8_Bits);

   --  Number of stop bits (STB)
   type MSS_UART_Stop_Bits is
     (
      --  1 stop bit
      Stop_Bit_1,
      --  1 1/2 stop bits when WLS=00. The number of stop bits is 2 for all
      --  other cases not described above (STB=1 and WLS=01, 10, or 11).
      Stop_Bit_1_AND_HALF);

   --  Even parity select
   type MSS_UART_Polarity is
     (Odd,
      Even);

   type MSS_UART_Line_Configuration is record
      --  Word length select
      Word_Length              : MSS_UART_Word_Length := Length_5_Bits;
      --  Number of stop bits (STB)
      Stop_Bits                : MSS_UART_Stop_Bits := Stop_Bit_1;
      --  Parity enable
      Parity_Enable            : Boolean := False;
      --  Even parity select
      Even_Parity_Enable       : MSS_UART_Polarity := Odd;
      --  Stick parity. When stick parity is enabled, the parity is set
      --  according to bits [4:3] as follows: 11: 0 will be sent as a parity
      --  bit and checked when receiving. 01: 1 will be sent as a parity bit
      --  and checked when receiving.
      Stick_Parity             : Boolean := False;
      --  Set break. Enabling this bit sets MMUART_x_TXD to 0. This does not
      --  have any effect on transmitter logic.
      Set_Break                : Boolean := False;
      --  Divisor latch access bit. Enables access to the divisor latch
      --  registers during read or write operation to address 0 and 1.
      Divisor_Latch_Access_Bit : Boolean := False;
   end record;

   type MSS_UART is limited private;

   procedure Configure
     (This        : in out MSS_UART;
      Baud_Rate   : MSS_UART_Baud_Rate;
      Line_Config : MSS_UART_Line_Configuration;
      Status      : out Boolean);

   type UART_Data is array (Natural range <>) of Interfaces.Unsigned_8;

   procedure Send
     (This : in out MSS_UART;
      Data : UART_Data);

   procedure Send
     (This : in out MSS_UART;
      Data : String);

private

   for MSS_UART_Baud_Rate use
     (Baud_110    =>     110,
      Baud_300    =>     300,
      Baud_1200   =>   1_200,
      Baud_2400   =>   2_400,
      Baud_4800   =>   4_800,
      Baud_9600   =>   9_600,
      Baud_19200  =>  19_200,
      Baud_38400  =>  38_400,
      Baud_57600  =>  57_600,
      Baud_115200 => 115_200,
      Baud_230400 => 230_400,
      Baud_460800 => 460_800,
      Baud_921600 => 921_600);

   type MSS_UART is limited record
      Regs : Interfaces.SF2.MMUART.MMUART_Peripheral;
   end record;

end System.SF2.UART;
