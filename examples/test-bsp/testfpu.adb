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
pragma Warnings (Off);
with System.BB.CPU_Primitives; use System.BB.CPU_Primitives;
pragma Warnings (On);

with Text_IO; use Text_IO;
with Testdata; use Testdata;

procedure Testfpu (Arg : access Float) is
begin
   pragma Debug (Put_Line ("  . in Testfpu context"));
   Arg.all := Arg.all + 1.0;
   Disable_Interrupts;
   First_Thread := Test_Context (1)'Access;
   Context_Switch;
   Enable_Interrupts (0);
   pragma Debug (Put_Line
     ("  . Testfpu got control back after context switch"));

   --  Now quietly update the var and switch back.
   for J in 1 .. 1002 loop
      Arg.all := Arg.all + 1.0;
      Disable_Interrupts;
      First_Thread := Test_Context (1)'Access;
      Context_Switch;
      Enable_Interrupts (0);
   end loop;

   --  Now stop using floating point for 1000 iterations
   for J in 1 .. 1000 loop
      Disable_Interrupts;
      First_Thread := Test_Context (1)'Access;
      Context_Switch;
      Enable_Interrupts (0);
   end loop;

   --  Update Arg reflecting last 1000 switches, and return to using the FPU
   Arg.all := Arg.all + 1000.0;
   loop
      Arg.all := Arg.all + 1.0;
      Disable_Interrupts;
      First_Thread := Test_Context (1)'Access;
      Context_Switch;
      Enable_Interrupts (0);
   end loop;
end Testfpu;
