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

--  Minimal version of Text_IO body for use on STM32F4xxx, using USART1

with Interfaces; use Interfaces;

with Interfaces.STM32;       use Interfaces.STM32;
with Interfaces.STM32.RCC;   use Interfaces.STM32.RCC;
with Interfaces.STM32.GPIO;  use Interfaces.STM32.GPIO;
with Interfaces.STM32.USART; use Interfaces.STM32.USART;
with System.STM32;           use System.STM32;
with System.BB.Parameters;

package body System.Text_IO is

   Baudrate : constant := 115_200;
   --  Bitrate to use

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use System.BB.Parameters;

      APB_Clock    : constant Positive := Positive (STM32.System_Clocks.PCLK2);
      Int_Divider  : constant Positive := (25 * APB_Clock) / (4 * Baudrate);
      Frac_Divider : constant Natural := Int_Divider rem 100;

   begin
      Initialized := True;

      RCC_Periph.APB2ENR.USART1EN := 1;
      RCC_Periph.AHB1ENR.GPIOBEN  := 1;

      GPIOB_Periph.MODER.Arr     (6 .. 7) := (Mode_AF,     Mode_AF);
      GPIOB_Periph.OSPEEDR.Arr   (6 .. 7) := (Speed_50MHz, Speed_50MHz);
      GPIOB_Periph.OTYPER.OT.Arr (6 .. 7) := (Push_Pull,   Push_Pull);
      GPIOB_Periph.PUPDR.Arr     (6 .. 7) := (Pull_Up,     Pull_Up);
      GPIOB_Periph.AFRL.Arr      (6 .. 7) := (AF_USART1,   AF_USART1);

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
