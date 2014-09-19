------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2010-2011, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with System.IOPorts; use System.IOPorts;
with System.Machine_Reset;
pragma Warnings (On);
with Interfaces; use Interfaces;

package body Gdbstub_Io is
   Debug_Port : constant Port_Id := 16#3f8#;
   Serial_Port : constant Port_Id := 16#2f8#;
   --  IO ports.

   RBR : constant Port_Id := 0;
   THR : constant Port_Id := 0;

   DLL : constant Port_Id := 0;
   DLM : constant Port_Id := 1;

   FCR : constant Port_Id := 2;
   FCR_FE : constant Unsigned_8 := 16#01#;
   FCR_ITL_14 : constant Unsigned_8 := 16#c0#;

   LCR : constant Port_Id := 3;
   LCR_5bits : constant Unsigned_8 := 16#00#;
   LCR_6bits : constant Unsigned_8 := 16#01#;
   LCR_7bits : constant Unsigned_8 := 16#02#;
   LCR_8bits : constant Unsigned_8 := 16#03#;
   LCR_2stop : constant Unsigned_8 := 16#04#;
   LCR_No_Parity : constant Unsigned_8 := 16#08#;
   LCR_Break : constant Unsigned_8 := 16#40#;
   LCR_DLAB : constant Unsigned_8 := 16#80#;

   LSR : constant Port_Id := 5;
   LSR_Data : constant Unsigned_8 := 16#01#;

   procedure Initialize_Uart (Port : Port_Id);
   --  Initialize an UART.

   function Can_Read return Boolean;
   --  True if can read on the debug port.

   procedure Initialize_Uart (Port : Port_Id) is
   begin
      --  115200 8n1
      Outb (Port + LCR, LCR_DLAB or LCR_No_Parity or LCR_8bits);
      Outb (Port + DLL, 1);
      Outb (Port + DLM, 0);
      Outb (Port + LCR, LCR_No_Parity or LCR_8bits);
      --  Enable FIFO.
      Outb (Port + FCR, FCR_FE or FCR_ITL_14);
   end Initialize_Uart;

   procedure Initialize is
   begin
      Initialize_Uart (Serial_Port);
      Initialize_Uart (Debug_Port);
   end Initialize;

   function Can_Read return Boolean is
   begin
      return (Inb (Serial_Port + LSR) and LSR_Data) /= 0;
   end Can_Read;

   function Read_Byte return Character is
   begin
      while not Can_Read loop
         null;
      end loop;
      return Character'Val (Inb (Serial_Port + RBR));
   end Read_Byte;

   procedure Write_Byte (C : Character) is
   begin
      Outb (Serial_Port + THR, Character'Pos (C));
   end Write_Byte;

   procedure Write_Debug (C : Character) is
   begin
      Outb (Debug_Port + THR, Character'Pos (C));
   end Write_Debug;

   procedure Kill is
   begin
      System.Machine_Reset.Stop;
   end Kill;
end Gdbstub_Io;
