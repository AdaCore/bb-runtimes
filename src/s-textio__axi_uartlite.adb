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

with Interfaces; use Interfaces;

--  I/O for the AMD AXI UARTLITE IP

package body System.Text_IO is

   Base_Address : constant := 16#8006_0000#;

   Rx_FIFO : Unsigned_32
   with Import, Volatile, Address => System'To_Address (Base_Address);

   Tx_FIFO : Unsigned_32
   with Import, Volatile, Address => System'To_Address (Base_Address + 4);

   Status_Reg : Unsigned_32
   with Import, Volatile, Address => System'To_Address (Base_Address + 8);

   Ctrl_Reg : Unsigned_32
   with Import, Volatile, Address => System'To_Address (Base_Address + 12);

   Status_Rx_Data_Present : constant Unsigned_32 := 16#01#;
   Status_Tx_Buffer_Full  : constant Unsigned_32 := 16#08#;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Character'Val (Rx_FIFO and 16#FF#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (Status_Reg and Status_Rx_Data_Present) /= 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (Status_Reg and Status_Tx_Buffer_Full) = 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Tx_FIFO := Unsigned_32 (Character'Pos (C));
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return False;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
