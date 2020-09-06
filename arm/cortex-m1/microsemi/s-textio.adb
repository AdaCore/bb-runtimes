------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Interfaces.Microsemi;             use Interfaces.Microsemi;
with Interfaces.Microsemi.CoreUARTapb; use Interfaces.Microsemi.CoreUARTapb;
with System.BB.Board_Parameters;

package body System.Text_IO is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Baud_Rate : constant := 115200;
   begin
      Initialized := True;

      --  Divider for Baudrate
      CoreUARTapb_Periph.Control_1.Baud_Value :=
         Byte (System.BB.Board_Parameters.Clock_Frequency /
                (16 * Baud_Rate) - 1);

      CoreUARTapb_Periph.Control_2.Bit8 := True;
      CoreUARTapb_Periph.Control_2.Parity_En := False;
      CoreUARTapb_Periph.Control_2.Odd_N_Even := Even;
      CoreUARTapb_Periph.Control_2.Baud_Value := 0;
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
     (CoreUARTapb_Periph.Status.TX_Rdy);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
     (CoreUARTapb_Periph.Status.RX_Rdy);

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (CoreUARTapb_Periph.Rx_Data.Value);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character)
   is
   begin
      CoreUARTapb_Periph.Tx_Data.Value := Byte (Character'Pos (C));
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is (True);

end System.Text_IO;
