------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Uart is
   --  A Char_Emu is a client for a char device
   type Char_Emu_Type is interface;
   procedure Rx_Cb (Emu : in out Char_Emu_Type; C : Unsigned_32) is abstract;

   --  Implementation (singleton) of the console (char device)
   type Uart_Emu_Type is abstract new Char_Emu_Type with private;
   procedure Rx_Cb (Uart : in out Uart_Emu_Type; C : Unsigned_32) is abstract;
   procedure Tx (Uart : in out Uart_Emu_Type'Class; C : Unsigned_32);

   type Uart_Emu_Acc is access all Uart_Emu_Type'Class;

   Break : constant Unsigned_32 := 16#100#;

   procedure Init;
   procedure Register_Client (Uart : Uart_Emu_Acc);

   procedure Dump_Status;

   --  Log
   procedure Log (C : Character);
   procedure Log (Item : String);
   procedure Log_Line;
   procedure Log_Hex8 (V : Unsigned_64);
   procedure Log_Hex4 (V : Unsigned_32);
   procedure Log_Dec (N : Natural);

private
   type Uart_Emu_Type is abstract new Char_Emu_Type with record
      Client_Id : Natural;
   end record;

end Uart;
