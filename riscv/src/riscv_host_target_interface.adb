------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           R I S C V _ H o s t _ T a r g e t _ I n t e r f a c e          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2017, Free Software Foundation, Inc.           --
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
with Interfaces; use Interfaces;

package body RISCV_Host_Target_Interface is

   -- Payload --

   type HTIF_Payload is mod 2**48 with Size => 48;

   -- Device --

   type HTIF_Device is new Unsigned_8;

   Syscall_Device : constant HTIF_Device := 0;
   Console_Device : constant HTIF_Device := 1;

   -- Command --

   type HTIF_Command is new Unsigned_8;

   Console_Command_Get : constant HTIF_Command := 0;
   Console_Command_Put : constant HTIF_Command := 1;

   -- Host Target Interface register --

   type HTIF_Register is record
      Payload : HTIF_Payload;
      Device  : HTIF_Device;
      Command : HTIF_Command;
   end record with Volatile_Full_Access, Size => 64;

   for HTIF_Register use record
      Payload at 0 range 0  .. 47;
      Command at 0 range 48 .. 55;
      Device  at 0 range 56 .. 63;
   end record;

   To_Host   : HTIF_Register
     with Import, Size => 64, Address => System'To_Address (0);

   From_Host : HTIF_Register
     with Import, Size => 64, Address => System'To_Address (8);

   Console_Input_Buffer : HTIF_Payload := HTIF_Payload'Last;
   --  Store one character from the host.
   --  If the value is HTIF_Payload'Last, it means the buffer is empty.

   procedure Send_To_Host (Device  : HTIF_Device;
                           Command : HTIF_Command;
                           Payload : HTIF_Payload);
   --  Send a command to the host

   procedure Check_From_Host;
   --  Check if there's data from the host, and process it

   function Console_Input_Buffer_Empty return Boolean
   is (Console_Input_Buffer = HTIF_Payload'Last);

   procedure Syscall (Arg : Unsigned_8);
   --  Send a syscall to the HTIF

   ------------------
   -- Send_To_Host --
   ------------------

   procedure Send_To_Host (Device  : HTIF_Device;
                           Command : HTIF_Command;
                           Payload : HTIF_Payload)
   is
      Data : HTIF_Register;
   begin

      Data.Device  := Device;
      Data.Command := Command;
      Data.Payload := Payload;

      --  Send the command
      To_Host := Data;

      Check_From_Host;
   end Send_To_Host;

   ---------------------
   -- Check_From_Host --
   ---------------------

   procedure Check_From_Host is
      Data : HTIF_Register;
   begin

      --  Read data
      Data := From_Host;

      if Data = (Payload => 0, Command => 0, Device  => 0) then
         return;
      end if;

      From_Host := (Payload => 0, Command => 0, Device  => 0);

      --  We only handle the console get char
      if Data.Device = Console_Device
        and then
         Data.Command = Console_Command_Get
      then
         Console_Input_Buffer := Data.Payload;
      end if;
   end Check_From_Host;

   ----------------------
   -- Console_Put_Char --
   ----------------------

   procedure Console_Put_Char (C : Character) is
   begin
      Send_To_Host (Device  => Console_Device,
                    Command => Console_Command_Put,
                    Payload => HTIF_Payload (Character'Pos (C)));
   end Console_Put_Char;

   ----------------------
   -- Console_Get_Char --
   ----------------------

   procedure Console_Get_Char (C : out Character) is
   begin
      if Console_Input_Buffer_Empty then
         --  No character in the buffer, let's try to get one
         Check_From_Host;
      end if;

      if not Console_Input_Buffer_Empty then
         C := Character'Val (Console_Input_Buffer and 16#FF#);

         --  Clear the input buffer
         Console_Input_Buffer := HTIF_Payload'Last;

         --  Ack the data
         Send_To_Host (Device  => Console_Device,
                       Command => Console_Command_Get,
                       Payload => 0);
      else
         C := ASCII.NUL;
      end if;

   end Console_Get_Char;

   ----------------------
   -- Console_Has_Char --
   ----------------------

   function Console_Has_Char return Boolean is
   begin
      --  Check if we already have a character
      if not Console_Input_Buffer_Empty then
         return True;
      end if;

      --  Otherwise, try to get one
      Check_From_Host;

      return not Console_Input_Buffer_Empty;
   end Console_Has_Char;

   -------------
   -- Syscall --
   -------------

   procedure Syscall (Arg : Unsigned_8) is
   begin
      Send_To_Host (Device  => Syscall_Device,
                    Command => 0,
                    Payload => HTIF_Payload (Arg));
   end Syscall;

   ---------------
   -- Power_Off --
   ---------------

   procedure Power_Off is
   begin
      loop
         Check_From_Host;
         Syscall (1);
      end loop;
   end Power_Off;

end RISCV_Host_Target_Interface;
