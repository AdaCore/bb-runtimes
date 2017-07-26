------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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
with Interfaces;

package body System.Text_IO is
   use Interfaces;

   UARTDR : Unsigned_32;
   for UARTDR'Address use 16#4000_C000#;
   pragma Import (Ada, UARTDR);
   pragma Volatile (UARTDR);

   UARTFR : Unsigned_32;
   for UARTFR'Address use 16#4000_C018#;
   pragma Import (Ada, UARTFR);
   pragma Volatile (UARTFR);

   --  TXFE : constant := 2#1000_0000#;
   --  RXFF : constant := 2#0100_0000#;
   --  Why are the above present commented out???

   TXFF : constant := 2#0010_0000#;
   RXFE : constant := 2#0001_0000#;
   --  UARTFR bits

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (UARTDR and 16#FF#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      RCGC1 : Unsigned_32;
      for RCGC1'Address use 16#400F_E000# + 16#104#;
      pragma Import (Ada, RCGC1);
      pragma Volatile (RCGC1);

      RCGC2 : Unsigned_32;
      for RCGC2'Address use 16#400F_E000# + 16#108#;
      pragma Import (Ada, RCGC2);
      pragma Volatile (RCGC2);

      GPIOA_AFSEL : Unsigned_32;
      for GPIOA_AFSEL'Address use 16#4000_4000# + 16#420#;
      pragma Import (Ada, GPIOA_AFSEL);
      pragma Volatile (GPIOA_AFSEL);

      GPIOA_DEN : Unsigned_32;
      for GPIOA_DEN'Address use 16#4000_4000# + 16#51C#;
      pragma Import (Ada, GPIOA_DEN);
      pragma Volatile (GPIOA_DEN);

      UART0_IBRD : Unsigned_32;
      for UART0_IBRD'Address use 16#4000_C000# + 16#24#;
      pragma Import (Ada, UART0_IBRD);
      pragma Volatile (UART0_IBRD);

      UART0_FBRD : Unsigned_32;
      for UART0_FBRD'Address use 16#4000_C000# + 16#28#;
      pragma Import (Ada, UART0_FBRD);
      pragma Volatile (UART0_FBRD);

      UART0_LCRH : Unsigned_32;
      for UART0_LCRH'Address use 16#4000_C000# + 16#2C#;
      pragma Import (Ada, UART0_LCRH);
      pragma Volatile (UART0_LCRH);

      UART0_CTL : Unsigned_32;
      for UART0_CTL'Address use 16#4000_C000# + 16#30#;
      pragma Import (Ada, UART0_CTL);
      pragma Volatile (UART0_CTL);

      Freq : constant := 50_000_000;
      Baud : constant := 115_200;
      Brate : constant := (64 * Freq / 16 + Baud / 2) / Baud;

   begin
      --  Enable the clocks to the UART and GPIO modules

      RCGC2 := RCGC2 or 16#1#;
      RCGC1 := RCGC1 or 16#1#;

      --  Wait a little bit so that the modules are clocked

      for I in 1 .. 1024 loop
         null;
      end loop;

      --  Set GPIO A0 and A1 as UART pins

      GPIOA_AFSEL := GPIOA_AFSEL or 3;

      --  Set the pin type

      GPIOA_DEN := GPIOA_DEN or 3;

      --  Set the bit rate

      UART0_IBRD := Brate / 64;
      UART0_FBRD := Brate mod 64;

      --  8N1, FIFO enabled

      UART0_LCRH := 16#70#;

      --  Enable RX, TX, and the UART

      UART0_CTL := 16#301#;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (UARTFR and RXFE) = 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (UARTFR and TXFF) = 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character

      UARTDR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
