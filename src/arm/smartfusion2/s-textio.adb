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

--  Minimal version of Text_IO body for use on SmartFusion2, uses MUART_0
with System.SF2.UART; use System.SF2.UART;
with Interfaces.SF2;  use Interfaces.SF2;

package body System.Text_IO is

   MSS_UART0 : aliased MSS_UART with Import, Address => MMUART_0_Base;
   --  UART to use

   Baudrate : constant MSS_UART_Baud_Rate := Baud_115200;
   --  Bitrate to use

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Status : Boolean;
   begin
      Initialized := True;
      Configure (MSS_UART0,
                 Baudrate,
                 (Word_Length  => Length_8_Bits,
                  others       => <>),
                 Status);
      --  pragma Assert (Status, "Cannot initialize the UART interface");
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
     (True);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
     (False);

   ---------
   -- Get --
   ---------

   function Get return Character is ('?');

   ---------
   -- Put --
   ---------

   procedure Put (C : Character)
   is
      S : constant String := (1 => C);
   begin
      Send (MSS_UART0, S);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is (True);

end System.Text_IO;
