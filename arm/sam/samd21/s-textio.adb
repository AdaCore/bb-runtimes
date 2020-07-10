------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . T E X T _ I O                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

--  Really simple implementation of System.Text_IO for systems without
--  console.

with Interfaces.SAM; use Interfaces.SAM;
with Interfaces.SAM.PM; use Interfaces.SAM.PM;
with Interfaces.SAM.GCLK; use Interfaces.SAM.GCLK;
with Interfaces.SAM.SERCOM; use Interfaces.SAM.SERCOM;
with Interfaces.SAM.PORT; use Interfaces.SAM.PORT;

package body System.Text_IO is

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  if SERCOM5_Periph.SERCOM_USART.INTFLAG.RXC = 1 then
      return Character'Val (SERCOM5_Periph.SERCOM_USART.DATA.DATA);
      --  end if;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

   begin
      --  Already have OSC8M as a source to GCLK Gen 3 running at 8MHz
      --  Now just need to source GCLKGen3 to SERCOM5, which is conn to EDBG
      GCLK_Periph.CLKCTRL.ID := Sercom5_Core;
      GCLK_Periph.CLKCTRL.GEN := Gclk3;
      GCLK_Periph.CLKCTRL.CLKEN := Bit (1);

      PM_Periph.APBCMASK.SERCOM5 := Bit (1);

      --  port_init
      PORT_Periph.PINCFG1 (22).PMUXEN := Bit (1);
      PORT_Periph.PINCFG1 (23).PMUXEN := Bit (1);

      PORT_Periph.PMUX1 (11).PMUXE := D;
      PORT_Periph.PMUX1 (11).PMUXO := D;

      --  see datasheet SERCOM USART Basic Operation 26.6.2.1
      SERCOM5_Periph.SERCOM_USART.CTRLA.MODE := Usart_Int_Clk;
      SERCOM5_Periph.SERCOM_USART.CTRLA.CMODE := Bit (0);
      SERCOM5_Periph.SERCOM_USART.CTRLA.RXPO := UInt2 (3);
      SERCOM5_Periph.SERCOM_USART.CTRLA.TXPO := UInt2 (1);
      SERCOM5_Periph.SERCOM_USART.CTRLA.DORD := Bit (1);
      SERCOM5_Periph.SERCOM_USART.CTRLB.CHSIZE := UInt3 (0);
      SERCOM5_Periph.SERCOM_USART.CTRLA.FORM := UInt4 (0);
      SERCOM5_Periph.SERCOM_USART.CTRLB.SBMODE := Bit (0);
      SERCOM5_Periph.SERCOM_USART.BAUD := 50437; --  115200 baud
      SERCOM5_Periph.SERCOM_USART.CTRLB.RXEN := Bit (1);
      SERCOM5_Periph.SERCOM_USART.CTRLB.TXEN := Bit (1);
      SERCOM5_Periph.SERCOM_USART.CTRLA.ENABLE := Bit (1);

   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return SERCOM5_Periph.SERCOM_USART.INTFLAG.RXC = 1;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return SERCOM5_Periph.SERCOM_USART.INTFLAG.DRE = 1;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is

      --  SERCOM_UART_PERIPH : SERCOM_Peripheral := SERCOM0_Periph;
   begin
      SERCOM5_Periph.SERCOM_USART.DATA.DATA := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return False;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;

--  Notes:
--  SERCOM0 connected to Arduino Zero pins D0/D1, SERCOM5 to EDBG