------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

package Commands is
   type String_Acc is access constant String;
   --  The command name and help is described by a string. It must start with
   --  a printable character and all characters until the first space (not
   --  included) or the end of the string are part of the command name.
   --  Examples:
   --  "help - Print this help"
   --  "dump address [length] - Dump memory"
   --  "reset"

   type Command_Proc is access procedure;

   type Command_Type is record
      Name : String_Acc;
      --  Name of the command

      Proc : Command_Proc;
      --  Procedure to execute for this command
   end record;

   type Command_Array is array (Natural range <>) of Command_Type;

   type Command_List;
   type Command_List_Acc is access all Command_List;

   type Command_List (Num : Natural) is record
      Commands : Command_Array (1 .. Num);
      Next : Command_List_Acc;
   end record;

   procedure Register_Commands (Cmd : Command_List_Acc);
   --  Register a list of commands

   procedure Parse_Unsigned32 (Res : out Unsigned_32; Ok : out Boolean);
   --  Helper to parse an unsigned number

   procedure Handle_Command;
end Commands;
