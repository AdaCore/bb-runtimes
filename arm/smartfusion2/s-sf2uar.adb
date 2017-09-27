------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

with System;
with Interfaces;                      use Interfaces;
with Ada.Unchecked_Conversion;

with Interfaces.SF2;                  use Interfaces.SF2;
with Interfaces.SF2.System_Registers; use Interfaces.SF2.System_Registers;

package body System.SF2.UART is

   TX_FIFO_SIZE : constant := 16;

   procedure Configure_Baud_Rate
     (This        : in out MSS_UART;
      Baud_Rate   : MSS_UART_Baud_Rate;
      Status      : out Boolean);

   -------------------------
   -- Configure_Baud_Rate --
   -------------------------

   procedure Configure_Baud_Rate
     (This        : in out MSS_UART;
      Baud_Rate   : MSS_UART_Baud_Rate;
      Status      : out Boolean)
   is
      BR_Value     : constant Unsigned_32 :=
                       MSS_UART_Baud_Rate'Enum_Rep (Baud_Rate);
      Clocks       : constant System_Clocks := Get_System_Clocks;
      Pclk_Freq    : Unsigned_32;
      Value_By_128 : Unsigned_32;
      Value_By_64  : Unsigned_32;
      Baud_Value   : Unsigned_32;
      Fractional_Value : Unsigned_32;

   begin
      --  Reset the peripheral
      if This'Address = MMUART_0_Base then
         Pclk_Freq := Unsigned_32 (Clocks.PCLK0);
      else
         Pclk_Freq := Unsigned_32 (Clocks.PCLK1);
      end if;

      Status       := True; --  Assume configuration is fine
      Value_By_128 := (8 * Pclk_Freq) / BR_Value;
      Value_By_64  := Value_By_128 / 2;
      Baud_Value   := Value_By_64 / 64;

      Fractional_Value := Value_By_128 - (128 * Baud_Value) -
        Value_By_64 + (64 * Baud_Value);

      if Baud_Value > Unsigned_32 (UInt16'Last) then
         Status := False;

         return;
      end if;

      --  Set divisor latch
      This.Regs.LCR.DLAB := True;

      This.Regs.DMR := Byte (Baud_Value / 256);
      This.Regs.DLR := Byte (Baud_Value and 16#FF#);

      --  Reset divisor latch
      This.Regs.LCR.DLAB := False;

      if Fractional_Value > 1 then
         --  Enable fractional baud rate
         This.Regs.MM0.EFBR := True;

         This.Regs.DFR.DFR := UInt6 (Fractional_Value);
      else
         --  Do not use Fractional Baud Rate divisors.
         This.Regs.MM0.EFBR := False;
      end if;
   end Configure_Baud_Rate;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This        : in out MSS_UART;
      Baud_Rate   : MSS_UART_Baud_Rate;
      Line_Config : MSS_UART_Line_Configuration;
      Status      : out Boolean)
   is
   begin
      --  Reset the peripheral
      if This'Address = MMUART_0_Base then
         System_Registers_Periph.SOFT_RESET_CR.MMUART0_SOFTRESET := True;
         System_Registers_Periph.SOFT_RESET_CR.MMUART0_SOFTRESET := False;
      elsif This'Address = MMUART_0_Base then
         System_Registers_Periph.SOFT_RESET_CR.MMUART1_SOFTRESET := True;
         System_Registers_Periph.SOFT_RESET_CR.MMUART1_SOFTRESET := False;
      else
         Status := False;

         return;
      end if;

      --  Disable interrupts.
      This.Regs.IER := (others => <>);

      --  FIFO configuration
      This.Regs.FCR := (others => <>);
      --  Clear the receiver and transmitter FIFO
      This.Regs.FCR.CLEAR_RX_FIFO := True;
      This.Regs.FCR.CLEAR_TX_FIFO := True;

      --  Set default READY mode
      This.Regs.FCR.ENABLE_TXRDY_RXRDY := True;

      --  Disable loopback
      This.Regs.MCR.Loopback := False;
      This.Regs.MCR.RLoop    := MMUART.Disabled;

      --  Set default TX/RX endian
      This.Regs.MM1.E_MSB_TX := False;
      This.Regs.MM1.E_MSB_RX := False;

      --  Default AFM: disabled
      This.Regs.MM2.EAFM := False;

      --  Disable TX time guard
      This.Regs.MM0.ETTG := False;

      --  Set default RX timeout
      This.Regs.MM0.EFBR := False;

      --  Disable fractional baud-rate
      This.Regs.MM0.EFBR := False;

      --  Disable single-wire mode
      This.Regs.MM2.ESWM := False;

      --  Set filter to minimum value
      This.Regs.GFR.GLR := MMUART.Two_Flip_Flops_No_Spike;
      --  Default TX time guard
      This.Regs.TTG := 0;
      --  Default RX timeout
      This.Regs.RTO := 0;

      --  Configure baud rate divisors
      Configure_Baud_Rate (This, Baud_Rate, Status);

      if not Status then
         return;
      end if;

      --  Set the line configuration
      declare
         LCR : Interfaces.SF2.MMUART.LCR_Register;
      begin
         case Line_Config.Word_Length is
            when Length_5_Bits =>
               LCR.WLS := MMUART.Length_5_Bits;
            when Length_6_Bits =>
               LCR.WLS := MMUART.Length_6_Bits;
            when Length_7_Bits =>
               LCR.WLS := MMUART.Length_7_Bits;
            when Length_8_Bits =>
               LCR.WLS := MMUART.Length_8_Bits;
         end case;

         case Line_Config.Stop_Bits is
            when Stop_Bit_1 =>
               LCR.STB := MMUART.Stop_Bit_1;
            when Stop_Bit_1_AND_HALF =>
               LCR.STB := MMUART.Stop_Bit_1_AND_HALF;
         end case;

         LCR.PEN := Line_Config.Parity_Enable;

         case Line_Config.Even_Parity_Enable is
            when Odd =>
               LCR.EPS := MMUART.Odd;
            when Even =>
               LCR.EPS := MMUART.Even;
         end case;

         LCR.SP   := Line_Config.Stick_Parity;
         LCR.SB   := Line_Config.Set_Break;
         LCR.DLAB := Line_Config.Divisor_Latch_Access_Bit;

         This.Regs.LCR := LCR;
      end;

      --  Disable LIN mode
      This.Regs.MM0.ELIN := False;

      --  Disable IrDA mode
      This.Regs.MM1.EIRD := False;

      --  Disable SmartCard Mode
      This.Regs.MM2.EERR := False;
   end Configure;

   ----------
   -- Send --
   ----------

   procedure Send
     (This : in out MSS_UART;
      Data : UART_Data)
   is
      Index    : Natural := Data'First;
      Transmit : Natural;

   begin
      loop
         --  Check if TX FIFO is empty.
         if This.Regs.LSR.THRE then
            if Index + TX_FIFO_SIZE - 1 <= Data'Last then
               Transmit := TX_FIFO_SIZE;
            else
               Transmit := Data'Last - Index + 1;
            end if;

            for J in 1 .. Transmit loop
               This.Regs.THR := Interfaces.SF2.Byte (Data (Index));
               Index := Index + 1;
            end loop;

            exit when Index > Data'Last;
         end if;
      end loop;
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (This : in out MSS_UART;
      Data : String)
   is
      subtype My_String is String (Data'Range);
      subtype My_Data is UART_Data (Data'Range);
      function To_Data is new Ada.Unchecked_Conversion (My_String, My_Data);
   begin
      Send (This, To_Data (My_String (Data)));
   end Send;

end System.SF2.UART;
