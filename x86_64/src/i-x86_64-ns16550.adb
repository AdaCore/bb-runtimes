------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              N S 1 6 5 5 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

with Ada.Unchecked_Conversion;

package body Interfaces.X86_64.NS16550 is
   Data_Register_Register : constant := 0;
   Line_Control_Register  : constant := 3;
   Line_Status_Register   : constant := 5;

   ----------------------
   -- Set_Line_Control --
   ----------------------

   procedure Set_Line_Control (Data : Line_Control; Port : IO_Port) is
      function To_Byte is new
        Ada.Unchecked_Conversion (Line_Control, Unsigned_8);
   begin
      Write_IO_Byte (To_Byte (Data), Port + Line_Control_Register);
   end Set_Line_Control;

   ---------------------
   -- Get_Line_Status --
   ---------------------

   function Get_Line_Status (Port : IO_Port) return Line_Status is
      function To_Line_Status is new
        Ada.Unchecked_Conversion (Unsigned_8, Line_Status);
   begin
      return To_Line_Status (Read_IO_Byte (Port + Line_Status_Register));
   end Get_Line_Status;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Port : IO_Port) return Character is
      function To_Character is new
        Ada.Unchecked_Conversion (Unsigned_8, Character);
   begin
      return To_Character (Read_IO_Byte (Port + Data_Register_Register));
   end Read_Data;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (Data : Character; Port : IO_Port) is
      function To_Byte is new
        Ada.Unchecked_Conversion (Character, Unsigned_8);
   begin
      Write_IO_Byte (To_Byte (Data), Port + Data_Register_Register);
   end Write_Data;
end Interfaces.X86_64.NS16550;
