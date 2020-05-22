------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M A C H I N E _ R E S E T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2020, Free Software Foundation, Inc.       --
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

with Interfaces;        use Interfaces;
with Interfaces.X86_64; use Interfaces.X86_64;

with Ada.Unchecked_Conversion;

package body System.Machine_Reset is

   --  Reset through the Reset Control Register in the I/O Controller Hub

   procedure Os_Exit (Status : Integer);
   pragma No_Return (Os_Exit);
   pragma Export (Ada, Os_Exit, "_exit");
   --  Shutdown or restart the board

   procedure Os_Abort;
   pragma No_Return (Os_Abort);
   pragma Export (Ada, Os_Abort, "abort");
   --  Likewise

   --  Reset Control Register
   type Reset_Control is record
      System_Reset : Boolean;
      CPU_Reset    : Boolean;
      Full_Reset   : Boolean;
   end record with Size => 8;

   for Reset_Control use record
      System_Reset at 0 range 1 .. 1;
      CPU_Reset    at 0 range 2 .. 2;
      Full_Reset   at 0 range 3 .. 3;
   end record;

   function To_Data is new
     Ada.Unchecked_Conversion (Reset_Control, Unsigned_8);

   Reset_Control_Register_Port : constant := 16#cf9#;

   --------------
   -- Os_Abort --
   --------------

   procedure Os_Abort is
   begin
      Os_Exit (1);
   end Os_Abort;

   -------------
   -- Os_Exit --
   -------------

   procedure Os_Exit (Status : Integer) is
      pragma Unreferenced (Status);
      --  The parameter is just for ISO-C compatibility
      Reset_Command : constant Reset_Control :=
        (System_Reset => True, CPU_Reset => True, Full_Reset => True);
   begin
      Write_IO_Byte (To_Data (Reset_Command), Reset_Control_Register_Port);
      loop null; end loop;
   end Os_Exit;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Os_Exit (0);
   end Stop;
end System.Machine_Reset;
