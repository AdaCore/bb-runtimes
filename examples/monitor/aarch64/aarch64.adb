------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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
with Interfaces; use Interfaces;
with Console; use Console;
with Commands; use Commands;
with Dumps; use Dumps;

package body Aarch64 is
   function Get_Current_EL return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, currentel",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Current_EL;

   function Get_ELR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL3;

   function Get_SPSR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, spsr_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR_EL3;

   function Get_VBAR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, vbar_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_VBAR_EL3;

   function Get_SCR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, scr_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SCR_EL3;

   function Get_SP_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL2;

   function Get_HCR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, hcr_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HCR_EL2;

   function Get_SP_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL1;

   procedure Proc_El1 is
   begin
      Asm ("adr x0, 1f" & ASCII.LF & ASCII.HT &
           "msr elr_el3, x0" & ASCII.LF & ASCII.HT &
           "mov x0, #0x1c5"  & ASCII.LF & ASCII.HT &
           "msr spsr_el3, x0"  & ASCII.LF & ASCII.HT &
           "mov x0, sp"  & ASCII.LF & ASCII.HT &
           "msr sp_el1, x0"   & ASCII.LF & ASCII.HT &
           "isb"  & ASCII.LF & ASCII.HT &
           "eret"  & ASCII.LF & ASCII.HT &
           "1:",
           Volatile => True,
           Clobber => "x0");
   end Proc_El1;

   procedure Proc_Cr
   is
      L : Natural;
   begin
      declare
         EL : constant Unsigned_32 := Get_Current_EL;
      begin
         Put ("Current_EL: ");
         Put (Hex4 (EL));
         Put (" (");
         Put (Hex1 ((EL / 4) and 3));
         Put_Line (")");
         L := Natural ((EL / 4) and 3);
      end;

      if L >= 3 then
         Put ("EL3 ELR: ");
         Put (Hex8 (Get_ELR_EL3));
         Put (" SPSR: ");
         Put (Hex8 (Get_SPSR_EL3));
         Put (" VBAR: ");
         Put (Hex8 (Get_VBAR_EL3));
         New_Line;
         Put ("EL3 SCR: ");
         Put (Hex8 (Get_SCR_EL3));
         New_Line;
      end if;

      if L >= 2 then
         Put ("EL2 SP: ");
         Put (Hex8 (Get_SP_EL2));
         Put (", HCR: ");
         Put (Hex8 (Get_HCR_EL2));
         New_Line;
      end if;

      if L > 1 then
         Put ("EL1 SP: ");
         Put (Hex8 (Get_SP_EL1));
         New_Line;
      end if;
   end Proc_Cr;

   Commands : aliased Command_List :=
     (2,
      (1 => (new String'("cr - Display some config registers"),
             Proc_Cr'Access),
       2 => (new String'("el1 - Switch to el1"),
             Proc_El1'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Aarch64;
