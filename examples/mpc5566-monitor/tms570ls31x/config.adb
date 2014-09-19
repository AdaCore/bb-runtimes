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

with Loader;
with Srec;
with Hdk;
with Commands; use Commands;
with Console; use Console;
with Term;

package body Config is
   procedure Disp_Start is
   begin
      null;
   end Disp_Start;

   procedure Proc_Load is
   begin
      Put_Line ("Waiting for srec.");
      Srec.Read_Srec;
   end Proc_Load;

   procedure Proc_Echo is
      use Term;
      use Srec;
   begin
      loop
         Read_Srec_Line;
         Put ("Got: ");
         Put_Line (Line (1 .. Line_Len));
         exit when Line_Len = 0;
      end loop;
   end Proc_Echo;

   Commands : aliased Command_List :=
     (2,
      (1 => (new String'("load - S-Record loader"),
             Proc_Load'Access),
       2 => (new String'("echo - test serial line"),
             Proc_Echo'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Config;
