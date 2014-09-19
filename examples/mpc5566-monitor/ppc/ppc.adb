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

with System.Machine_Code; use System.Machine_Code;

package body Ppc is
   function Get_Spr return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mfspr %0,%1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Inputs => Natural'Asm_Input ("K", Spr),
           Volatile => True);
      return Res;
   end Get_Spr;

   procedure Set_Spr (V : Unsigned_32) is
   begin
      Asm ("mtspr %0,%1",
           Inputs => (Natural'Asm_Input ("K", Spr),
                      Unsigned_32'Asm_Input ("r", V)),
           Volatile => True);
   end Set_Spr;

   function Get_Msr return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mfmsr %0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Msr;

   function Get_Tbu return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbu %0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Tbu;

   function Get_Tbl return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbl %0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Tbl;
end Ppc;
