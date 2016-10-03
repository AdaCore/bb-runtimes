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

with Interfaces.ARM_V7AR;
with System.Machine_Code; use System.Machine_Code;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Commands; use Commands;
with Dumps; use Dumps;

package body Armv7a is
   procedure Proc_Cr is
      Vbar : Unsigned_32;
   begin
      Asm ("mrc p15, #0, %0, c12, c0, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Vbar),
           Volatile => True);
      Put ("VBAR: " & Image8 (Vbar));
      New_Line;
   end Proc_Cr;

   procedure Disp_Cache_Size (L : Natural)
   is
      use Interfaces.ARM_V7AR;
      Ccsidr : Unsigned_32;
      Lines, Assoc, Sets : Natural;
   begin
      CP15.Set_CSSELR (Unsigned_32 (L));
      Barriers.ISB;
      Ccsidr := CP15.Get_CCSIDR;
      Lines := 2**(2 + Natural (Ccsidr and 3));
      Assoc := Natural (Shift_Right (Ccsidr, 3) and 16#3ff#) + 1;
      Sets := Natural (Shift_Right (Ccsidr, 13) and 16#7fff#) + 1;
      Put ("Line: ");
      Put (Lines);
      Put ("B, assoc: ");
      Put (Assoc);
      Put (", sets: ");
      Put (Sets);
      Put (" (size: ");
      Put (Lines * Assoc * Sets / 1024);
      Put ("KB)");
   end Disp_Cache_Size;

   procedure Proc_Cache
   is
      use Interfaces.ARM_V7AR;
      Sctlr : constant Unsigned_32 := CP15.Get_SCTLR;
      Clidr : constant Unsigned_32 := CP15.Get_CLIDR;
   begin
      Put ("SCTLR: ");
      Put (Image8 (Sctlr));
      Put (", CLIDR: ");
      Put (Image8 (Clidr));
      New_Line;

      --  Disp cache levels
      for I in 0 .. 6 loop
         declare
            L : constant Unsigned_32 :=
              Shift_Right (Clidr, I * 3) and 7;
         begin
            if L /= 0 then
               Put (" L");
               Put (I + 1);
               Put (": ");
               case L is
                  when 1 =>
                     Put ("  I-cache: ");
                     Disp_Cache_Size (I * 2 + 1);
                  when 2 =>
                     Put ("  D-cache");
                     Disp_Cache_Size (I * 2);
                  when 3 =>
                     Put ("  I-cache: ");
                     Disp_Cache_Size (I * 2 + 1);
                     New_Line;
                     Put ("       D-cache: ");
                     Disp_Cache_Size (I * 2);
                  when 4 =>
                     Put ("I+D-cache: ");
                     Disp_Cache_Size (I * 2);
                  when others =>
                     Put ("??");
                     Put (Natural (L));
               end case;

               New_Line;
            end if;
         end;
      end loop;
      Put ("LoUU: ");
      Put (Natural (Shift_Right (Clidr, 27) and 7));
      New_Line;
      Put ("LoC : ");
      Put (Natural (Shift_Right (Clidr, 27) and 7));
      New_Line;
   end Proc_Cache;

   Commands : aliased Command_List :=
     (2,
      (1 => (new String'("cache - disp cache"),
             Proc_Cache'Access),
       2 => (new String'("cr - Display some config registers"),
             Proc_Cr'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Armv7a;
