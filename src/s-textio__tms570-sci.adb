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

--  Minimal version of Text_IO body for use on TMS570 family of MCUs, using
--  SCI1/LIN1

--  @design
--  This package is in charge of sending characters to the remote host
--  machine. The application output is sent through the UART, from which the
--  host machine extracts the application output.
--
--  The TMS570 runtime uses the SCI1/LIN1 module, configured for 115200 baud
--  rate, one stop bit, no parity.

with Interfaces;              use Interfaces;
--  @design used for the 32-bit integers definition
with System.Board_Parameters; use System.Board_Parameters;
--  @design used to retrieve the system clock, used to calculate the prescaler
--  values to achieve the desired baud rate.

package body System.Text_IO is

   SCI_BASE : constant := 16#FFF7_E400#;
   --  SCI base address

   SCIGCR0 : Unsigned_32;
   for SCIGCR0'Address use SCI_BASE + 16#00#;
   pragma Volatile (SCIGCR0);
   pragma Import (Ada, SCIGCR0);

   SCIGCR1 : Unsigned_32;
   for SCIGCR1'Address use SCI_BASE + 16#04#;
   pragma Volatile (SCIGCR1);
   pragma Import (Ada, SCIGCR1);

   SCIFLR : Unsigned_32;
   for SCIFLR'Address use SCI_BASE + 16#1C#;
   pragma Volatile (SCIFLR);
   pragma Import (Ada, SCIFLR);

   TX_READY : constant := 16#100#;
   RX_READY : constant := 16#200#;

   BRS : Unsigned_32;
   for BRS'Address use SCI_BASE + 16#2C#;
   pragma Volatile (BRS);
   pragma Import (Ada, BRS);

   SCIFORMAT : Unsigned_32;
   for SCIFORMAT'Address use SCI_BASE + 16#28#;
   pragma Volatile (SCIFORMAT);
   pragma Import (Ada, SCIFORMAT);

   SCIRD : Unsigned_32;
   for SCIRD'Address use SCI_BASE + 16#34#;
   pragma Volatile (SCIRD);
   pragma Import (Ada, SCIRD);

   SCITD : Unsigned_32;
   for SCITD'Address use SCI_BASE + 16#38#;
   pragma Volatile (SCITD);
   pragma Import (Ada, SCITD);

   SCIPIO0 : Unsigned_32;
   for SCIPIO0'Address use SCI_BASE + 16#3C#;
   pragma Volatile (SCIPIO0);
   pragma Import (Ada, SCIPIO0);

   SCIPIO8 : Unsigned_32;
   for SCIPIO8'Address use SCI_BASE + 16#5C#;
   pragma Volatile (SCIPIO8);
   pragma Import (Ada, SCIPIO8);

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (SCIRD and 16#FF#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Do not reinitialize the SCI, if it is already initialized
      if (SCIGCR0 and 1) = 0 then

         --  Bring out of reset
         SCIGCR0 := 1;

         --  8n1, enable RX and TX, async, idle-line mode, SWnRST, internal clk
         --  NOTE: SPNU499A (Nov 2012) is incorrect on COMM MODE: Idle line
         --  mode value is 0.
         SCIGCR1 := 16#03_00_00_22#;

         --  Baud rate. VCLK = RTI1CLK
         declare
            Baud : constant := 115200;
            P : constant := VCLK_Frequency / (16 * Baud) - 1;
            M : constant := (VCLK_Frequency / Baud) rem 16;
         begin
            BRS := P + M * 2**24;
         end;

         --  8 bits
         SCIFORMAT := 7;

         --  Enable Tx and Rx pins, pull-up
         SCIPIO0 := 2#110#;
         SCIPIO8 := 2#110#;

         --  Enable SCI
         SCIGCR1 := SCIGCR1 or 16#80#;
      end if;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      ((SCIFLR and TX_READY) /= 0);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      ((SCIFLR and RX_READY) /= 0);

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      SCITD := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
