------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this library; see the file COPYING3. If not, see:                   --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Minimal version of Text_IO body for use on STM32F4xxx, using USART1

with Interfaces; use Interfaces;

with Interfaces.Bit_Types;   use Interfaces.Bit_Types;
with Interfaces.STM32.RCC;   use Interfaces.STM32.RCC;
with Interfaces.STM32.GPIO;  use Interfaces.STM32.GPIO;
with Interfaces.STM32.USART; use Interfaces.STM32.USART;
with System.STM32;           use System.STM32;
with System.BB.Parameters;
with Ada.Unchecked_Conversion;

package body System.Text_IO is

   type GPIO_Cnf_Input is
     (Cnf_Analogue, Cnf_Floating, Cnf_Input, Cnf_Reserved);

   for GPIO_Cnf_Input use
     (Cnf_Analogue => 2#0000#,
      Cnf_Floating => 2#0100#,
      Cnf_Input    => 2#1000#,
      Cnf_Reserved => 2#1100#);
   --  when Mode = 0

   type GPIO_Cnf_Output is
     (Cnf_Push_Pull,
      Cnf_Open_Drain,
      Cnf_AF_Push_Pull,
      Cnf_AF_Open_Drain);

   for GPIO_Cnf_Output use
     (Cnf_Push_Pull      => 2#0000#,
      Cnf_Open_Drain     => 2#0100#,
      Cnf_AF_Push_Pull   => 2#1000#,
      Cnf_AF_Open_Drain  => 2#1100#);
   --  when Mode /= 0

   type GPIO_Mode is
     (Mode_Input,
      Mode_Output_10MHz,
      Mode_Output_2MHz,
      Mode_Output_50MHz)
     with Size => 4;

   pragma Unreferenced (Mode_Input);
   pragma Unreferenced (Mode_Output_10MHz);
   pragma Unreferenced (Mode_Output_2MHz);

   function To_UInt4 is new Ada.Unchecked_Conversion (GPIO_Mode, UInt4);
   function To_UInt4 is new Ada.Unchecked_Conversion (GPIO_Cnf_Output, UInt4);

   Baudrate : constant := 115_200;
   --  Bitrate to use

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      APB_Clock    : constant Positive := Positive (STM32.System_Clocks.PCLK2);
      Int_Divider  : constant Positive := (APB_Clock / (16 * Baudrate)) * 100;
      Frac_Divider : constant Natural := Int_Divider rem 100;
      Compose      : UInt4;
      Mode_Tmp     : GPIO_Mode;
      Output       : GPIO_Cnf_Output;
   begin
      Initialized := True;

      RCC_Periph.APB2ENR.USART1EN := 1;
      RCC_Periph.APB2ENR.IOPAEN := 1;

      Mode_Tmp := Mode_Output_50MHz;
      Output := Cnf_AF_Push_Pull;
      Compose := To_UInt4 (Output) or To_UInt4 (Mode_Tmp);

      GPIOA_Periph.CRH.Arr (9 - 8) := Compose;
      GPIOA_Periph.CRH.Arr (10 - 8) := Compose;

      USART1_Periph.BRR :=
        (DIV_Fraction => UInt4  (((Frac_Divider * 16 + 50) / 100) mod 16),
         DIV_Mantissa => UInt12 (Int_Divider / 100),
         others => <>);
      USART1_Periph.CR1 :=
        (UE => 1,
         RE => 1,
         TE => 1,
         others => <>);
      USART1_Periph.CR2 := (others => <>);
      USART1_Periph.CR3 := (others => <>);

   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      (USART1_Periph.SR.TC = 1);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      (USART1_Periph.SR.RXNE = 1);

   ---------
   -- Get --
   ---------

   function Get return Character is (Character'Val (USART1_Periph.DR.DR));

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      USART1_Periph.DR.DR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is (True);

end System.Text_IO;
