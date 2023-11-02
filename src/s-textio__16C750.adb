------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  Minimal version of Text_IO body for use on with 16C750-compatible UARTs

--  This package is in charge of sending characters to the remote host
--  machine. The application output is sent through the UART, from which the
--  host machine extracts the application output.
--
--  It's configured for 115200 baud rate, one stop bit, no parity using the
--  48 MHz clock source

with Interfaces;           use Interfaces;
with System.BB.Parameters; use System.BB.Parameters;

package body System.Text_IO is

   use System.BB.Parameters;

   ----------------
   -- UART Types --
   ----------------

   --  Based on TI AM64x/AM243x Technical Reference Manual, 12.1.5 Universal
   --  Asynchronous Receiver/Transmitter. Limited implementation to serve only
   --  the needs to this package.

   pragma Warnings (Off, "*not referenced");
   --  Suppress warning for unreferenced enumeration values

   type UART_Config_Mode is
     (Configuration_Mode_A, Configuration_Mode_B, Operational_Mode);

   type Enable_Bit is (Disable, Enable);
   for Enable_Bit use (Disable => 0, Enable => 1);

   type Holding_Register is record
      Data : Character;
   end record with Size => 32, Volatile_Full_Access;

   for Holding_Register use record
      Data at 0 range 0 .. 7;
   end record;

   subtype Baud_Clock_Divisor_Low is Unsigned_32 range 0 .. 2 ** 8 - 1;
   subtype Baud_Clock_Divisor_High is Unsigned_32 range 0 .. 2 ** 5 - 1;

   type Baud_Clock_Divisor is record
      High : Baud_Clock_Divisor_High;
      Low  : Baud_Clock_Divisor_Low;
   end record with Size => 64;

   for Baud_Clock_Divisor use record
      Low  at 0 range 0 .. 7;
      High at 4 range 0 .. 5;
   end record;

   type Interrupt_Enable is record
      CTS_Interrupt          : Enable_Bit;
      RTS_Interrupt          : Enable_Bit;
      XOFF_Interrupt         : Enable_Bit;
      Sleep_Mode             : Enable_Bit;
      Modem_Status_Interrupt : Enable_Bit;
      Line_Status_Interrupt  : Enable_Bit;
      THR_Interrupt          : Enable_Bit;
      RHR_Interrupt          : Enable_Bit;
   end record with Size => 32;

   for Interrupt_Enable use record
      CTS_Interrupt          at 0 range 7 .. 7;
      RTS_Interrupt          at 0 range 6 .. 6;
      XOFF_Interrupt         at 0 range 5 .. 5;
      Sleep_Mode             at 0 range 4 .. 4;
      Modem_Status_Interrupt at 0 range 3 .. 3;
      Line_Status_Interrupt  at 0 range 2 .. 2;
      THR_Interrupt          at 0 range 1 .. 1;
      RHR_Interrupt          at 0 range 0 .. 0;
   end record;

   type Software_Flow_Control_Bits is mod 2 ** 3;

   type Enhanced_Feature is record
      Auto_CTS              : Enable_Bit;
      Auto_RTS              : Enable_Bit;
      Special_Char_Dectect  : Enable_Bit;
      Enhanced_Functions    : Enable_Bit;
      Software_Flow_Control : Software_Flow_Control_Bits;
   end record with Size => 32;

   for Enhanced_Feature use record
      Auto_CTS              at 0 range 7 .. 7;
      Auto_RTS              at 0 range 6 .. 6;
      Special_Char_Dectect  at 0 range 5 .. 5;
      Enhanced_Functions    at 0 range 4 .. 4;
      Software_Flow_Control at 0 range 0 .. 3;
   end record;

   type Force_Parity_Format is (Not_Forced, Forced);
   for Force_Parity_Format use (Not_Forced => 0, Forced => 1);

   type Parity_Type is (Odd, Even);
   for Parity_Type use (Odd => 0, Even => 1);

   type Stop_Bits is (One_Bit, Two_Bits);
   for Stop_Bits use (One_Bit => 0, Two_Bits => 1);

   type Word_Lengths is (Five_Bits, Six_Bits, Seven_Bits, Eight_Bits);
   for Word_Lengths use
     (Five_Bits => 0, Six_Bits => 1, Seven_Bits => 2, Eight_Bits => 3);

   type Line_Control is record
      Divisor_Latch       : Enable_Bit;
      Break_Control_Bit   : Enable_Bit;
      Parity_Type_2       : Force_Parity_Format;
      Parity_Type_1       : Parity_Type;
      Parity              : Enable_Bit;
      Number_Of_Stop_Bits : Stop_Bits;
      Word_Length         : Word_Lengths;
   end record with Size => 32, Volatile_Full_Access;

   for Line_Control use record
      Divisor_Latch       at 0 range 7 .. 7;
      Break_Control_Bit   at 0 range 6 .. 6;
      Parity_Type_2       at 0 range 5 .. 5;
      Parity_Type_1       at 0 range 4 .. 4;
      Parity              at 0 range 3 .. 3;
      Number_Of_Stop_Bits at 0 range 2 .. 2;
      Word_Length         at 0 range 0 .. 1;
   end record;

   type Line_Status_Type is record
      TX_Shift_Reg_Empty : Boolean;
      TX_FIFO_Empty      : Boolean;
      RX_FIFO_Has_Data   : Boolean;
   end record with Size => 32, Volatile_Full_Access;

   for Line_Status_Type use record
      TX_Shift_Reg_Empty at 0 range 6 .. 6;
      TX_FIFO_Empty      at 0 range 5 .. 5;
      RX_FIFO_Has_Data   at 0 range 0 .. 0;
   end record;

   type Module_Mode is
     (UART_16x, SIR, UART_16x_Auto_Baud,
      UART_13x, MIR, FIR, CIR, Disabled);

   for Module_Mode use
     (UART_16x => 0, SIR => 1, UART_16x_Auto_Baud => 2,
      UART_13x => 3, MIR => 4, FIR => 5, CIR => 6, Disabled => 7);

   type Mode_Definition_1 is record
      Mode_Select : Module_Mode;
   end record with Size => 32;

   for Mode_Definition_1 use record
      Mode_Select at 0 range 0 .. 2;
   end record;

   type Idle_Mode_Kind is
     (Force_Idle, No_Idle, Smart_Idle, Smart_Idle_Wake_Up);

   type System_Config_Type is record
      Idle_Mode      : Idle_Mode_Kind;
      Wakeup_Enabled : Boolean;
      Software_Reset : Boolean;
      Auto_Idle      : Boolean;
   end record with Size => 32;

   for System_Config_Type use record
      Idle_Mode      at 0 range 3 .. 4;
      Wakeup_Enabled at 0 range 2 .. 2;
      Software_Reset at 0 range 1 .. 1;
      Auto_Idle      at 0 range 0 .. 0;
   end record;

   type System_Status_Type is record
      Reset_Done : Boolean;
   end record with Size => 32;

   for System_Status_Type use record
      Reset_Done at 0 range 0 .. 0;
   end record;

   pragma Warnings (Off, "*bits of*");
   --  Suppress warning of unused bits in UART_Modules. The unused bits is
   --  expected for the UART module as each module is placed at 16#1_0000#
   --  offsets.

   type UART_Modules (Mode : UART_Config_Mode := Operational_Mode) is record
      Line_Control_Register      : Line_Control;
      Mode_Definition_1_Register : Mode_Definition_1;
      System_Config_Register     : System_Config_Type;
      System_Status_Register     : System_Status_Type;

      case Mode is
         when Operational_Mode =>
            TX_RX_Holding_Register     : Holding_Register;
            Interrupt_Enable_Register  : Interrupt_Enable;
            Line_Status_Regsiter       : Line_Status_Type;

         when Configuration_Mode_A =>
            null;

         when Configuration_Mode_B =>
            Baud_Clock_Divisor_Register : Baud_Clock_Divisor;
            Enhanced_Feature_Register   : Enhanced_Feature;
      end case;
   end record with Unchecked_Union, Size => 16#1_0000# * Storage_Unit;

   pragma Warnings (On, "*bits of*");

   for UART_Modules use record
      TX_RX_Holding_Register      at 16#0#  range 0 .. 31;
      Interrupt_Enable_Register   at 16#4#  range 0 .. 31;
      Baud_Clock_Divisor_Register at 16#0#  range 0 .. 63;
      Enhanced_Feature_Register   at 16#8#  range 0 .. 31;
      Line_Control_Register       at 16#C#  range 0 .. 31;
      Line_Status_Regsiter        at 16#14# range 0 .. 31;
      Mode_Definition_1_Register  at 16#20# range 0 .. 31;
      System_Config_Register      at 16#54# range 0 .. 31;
      System_Status_Register      at 16#58# range 0 .. 31;
   end record;

   pragma Warnings (On, "*not referenced");
   --  Reenable warning for unreferenced enumeration values

   --------------------
   -- UART Registers --
   --------------------

   UART : array (UART_ID) of UART_Modules
      with Address => UART_Base_Address, Volatile_Components;
   --  Access to the UART peripherals

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return UART (IO_Module).TX_RX_Holding_Register.Data;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      procedure Enter_UART_Mode (Mode : UART_Config_Mode);

      procedure Enter_UART_Mode
        (Mode         : UART_Config_Mode;
         Previous_LCR : out Line_Control);

      ---------------------
      -- Enter_UART_Mode --
      ---------------------

      procedure Enter_UART_Mode (Mode : UART_Config_Mode) is
         Old_LCR : Line_Control;
      begin
         Enter_UART_Mode (Mode, Old_LCR);
      end Enter_UART_Mode;

      procedure Enter_UART_Mode
        (Mode         : UART_Config_Mode;
         Previous_LCR : out Line_Control) is
      begin
         Previous_LCR := UART (IO_Module).Line_Control_Register;
         case Mode is
            when Configuration_Mode_A =>
               --  Configuration Mode A entered when LCR /= 16#BF#

               UART (IO_Module).Line_Control_Register :=
                 (Divisor_Latch       => Enable,
                  Break_Control_Bit   => Disable,
                  Parity_Type_2       => Not_Forced,
                  Parity_Type_1       => Odd,
                  Parity              => Disable,
                  Number_Of_Stop_Bits => One_Bit,
                  Word_Length         => Five_Bits);

            when Configuration_Mode_B =>
               --  Configuration Mode B entered when LCR == 16#BF#

               UART (IO_Module).Line_Control_Register :=
                 (Divisor_Latch       => Enable,
                  Break_Control_Bit   => Disable,
                  Parity_Type_2       => Not_Forced,
                  Parity_Type_1       => Odd,
                  Parity              => Disable,
                  Number_Of_Stop_Bits => One_Bit,
                  Word_Length         => Five_Bits);

            when Operational_Mode =>
               UART (IO_Module).Line_Control_Register.Divisor_Latch := Disable;
         end case;
      end Enter_UART_Mode;

   begin
      --  Initialize UART module per the UART Programming Guide section in
      --  the TI AM64x Technical Reference Manual

      --  Ensure the last character is sent before reseting the UART module
      --  so we don't send garbage down the link.

      while not UART (IO_Module).Line_Status_Regsiter.TX_Shift_Reg_Empty loop
         null;
      end loop;

      --  Disable the module if it is enabled

      UART (IO_Module).Mode_Definition_1_Register := (Mode_Select => Disabled);

      --  Reset UART module

      UART (IO_Module).System_Config_Register.Software_Reset := True;

      loop
         exit when UART (IO_Module).System_Status_Register.Reset_Done;
      end loop;

      --  Disable module interrupts and sleep mode

      Enter_UART_Mode (Configuration_Mode_B);
      UART (IO_Module).Enhanced_Feature_Register.Enhanced_Functions := Enable;

      Enter_UART_Mode (Operational_Mode);
      UART (IO_Module).Interrupt_Enable_Register := (others => Disable);

      Enter_UART_Mode (Configuration_Mode_B);
      UART (IO_Module).Enhanced_Feature_Register.Enhanced_Functions := Disable;

      --  Set baud rate to 115.2 kbps and enable UART module. For the standard
      --  48 MHz source clock we use the 16x baud multiple and a divider value
      --  of 26. The clock divider is set while in Configuration_Mode_B.

      UART (IO_Module).Baud_Clock_Divisor_Register :=
        (High => 16#00#, Low => 16#1A#);
      UART (IO_Module).Mode_Definition_1_Register := (Mode_Select => UART_16x);

      --  Configure Line Control Register with 8 data bits, no parity and 1
      --  stop bit.

      UART (IO_Module).Line_Control_Register :=
        (Divisor_Latch       => Enable,
         Break_Control_Bit   => Enable,
         Parity_Type_2       => Not_Forced,
         Parity_Type_1       => Odd,
         Parity              => Disable,
         Number_Of_Stop_Bits => One_Bit,
         Word_Length         => Eight_Bits);

      Enter_UART_Mode (Operational_Mode);
      UART (IO_Module).Line_Control_Register.Break_Control_Bit := Disable;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   --  We use TX_FIFO_Empty rather than TX_Shift_Reg_Empty to indicate the
   --  transmitter is ready as it allows the CPU to load the next character
   --  straight after the previous character has moved to the transmit shift
   --  register.

   function Is_Tx_Ready return Boolean is
      (UART (IO_Module).Line_Status_Regsiter.TX_FIFO_Empty);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      (UART (IO_Module).Line_Status_Regsiter.RX_FIFO_Has_Data);

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      UART (IO_Module).TX_RX_Holding_Register := (Data => C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
