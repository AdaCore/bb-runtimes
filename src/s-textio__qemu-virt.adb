------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  Uart I/O for PL011

with System;
with Interfaces; use Interfaces;

package body System.Text_IO is

   PL011_Base : constant := 16#900_0000#;

   type Pl011_Registers_Type is record
      DR     : Unsigned_32;
      RSRECR : Unsigned_32;
      Pad08  : Unsigned_32;
      Pad0c  : Unsigned_32;

      Pad10  : Unsigned_32;
      Pad14  : Unsigned_32;
      FR     : Unsigned_32;
      Pad1c  : Unsigned_32;

      ILPR   : Unsigned_32;
      IBRD   : Unsigned_32;
      FBRD   : Unsigned_32;
      LCRH   : Unsigned_32;

      CR     : Unsigned_32;
      IFLS   : Unsigned_32;
      IMSC   : Unsigned_32;
      RIS    : Unsigned_32;

      MIS    : Unsigned_32;
      ICR    : Unsigned_32;
      DMACR  : Unsigned_32;
      Pad4c  : Unsigned_32;
   end record;

   PL011_Registers : Pl011_Registers_Type
     with Address => System'To_Address (PL011_Base), Volatile, Import;

   FR_TXFF : constant := 2**5;
   FR_RXFE : constant := 2**4;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (PL011_Registers.DR and 16#ff#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialized := True;

      --  Disable UART
      PL011_Registers.CR := 0;

      --  Freq = 46_000_000, baud = 115_200
      --  Freq / baud ~= 26 + 3/64
      PL011_Registers.IBRD := 26;
      PL011_Registers.FBRD := 3;

      --  8n1, FIFO en
      PL011_Registers.LCRH := 16#70#;

      --  Clear interrupts
      PL011_Registers.ICR := 16#3ff#;

      --  Enable
      PL011_Registers.CR := 16#301#;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (PL011_Registers.FR and FR_RXFE) = 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (PL011_Registers.FR and FR_TXFF) = 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character

      PL011_Registers.DR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
