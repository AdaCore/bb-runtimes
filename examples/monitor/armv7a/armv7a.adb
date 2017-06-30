------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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
with Trap_Handler;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Commands; use Commands;
with Dumps; use Dumps;

package body Armv7a is
   function Get_VBAR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #0, %0, c12, c0, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_VBAR;

   function Get_CPSR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0,cpsr",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CPSR;

   function Get_SPSR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0,spsr",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR;

   pragma Unreferenced (Get_SPSR);

   function Get_SCR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15,#0,%0,c1,c1,#0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SCR;

   function Get_CPUECTLR return Unsigned_32
   is
      Lo, Hi : Unsigned_32;
   begin
      Asm ("mrrc p15, 1, %0, %1, c15",
           Outputs => (Unsigned_32'Asm_Output ("=r", Lo),
                       Unsigned_32'Asm_Output ("=r", Hi)),
           Volatile => True);
      return Lo;
   end Get_CPUECTLR;

   function Get_MIDR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #0, %0, c0, c0, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_MIDR;

   function Get_HSCTLR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #4, %0, c1, c0, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HSCTLR;

   function Get_HCPTR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #4, %0, c1, c1, #2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HCPTR;

   function Get_HMAIR0 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #4, %0, c10, c2, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HMAIR0;

   function Get_HMAIR1 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #4, %0, c10, c2, #1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HMAIR1;

   function Get_HTCR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #4, %0, c2, c0, #2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HTCR;

   function Get_HTTBR return Unsigned_64
   is
      Lo, Hi : Unsigned_32;
   begin
      Asm ("mrrc p15, 4, %0, %1, c2",
           Outputs => (Unsigned_32'Asm_Output ("=r", Lo),
                       Unsigned_32'Asm_Output ("=r", Hi)),
           Volatile => True);
      return Shift_Left (Unsigned_64 (Hi), 32) or Unsigned_64 (Lo);
   end Get_HTTBR;

   procedure Proc_Cr is
      CPSR : constant Unsigned_32 := Get_CPSR;
      MIDR : constant Unsigned_32 := Get_MIDR;
   begin
      Put ("VBAR: " & Image8 (Get_VBAR));
      Put (", CPSR: " & Image8 (CPSR));
      Put (' ');
      case CPSR and 16#1f# is
         when 2#10000# => Put ("usr");
         when 2#10001# => Put ("fiq");
         when 2#10010# => Put ("irq");
         when 2#10011# => Put ("svc");
         when 2#10110# => Put ("mon");
         when 2#10111# => Put ("abt");
         when 2#11010# => Put ("hyp");
         when 2#11011# => Put ("und");
         when 2#11111# => Put ("sys");
         when others => Put ("???");
      end case;
      New_Line;

      Put ("MIDR: " & Image8 (Get_MIDR));
      New_Line;

      --  Only for A53
      if (MIDR and 16#ff_0_f_fff_0#) = 16#41_0_f_d03_0# then
         Put ("CPUECTLR: ");
         Put (Image8 (Get_CPUECTLR));
         New_Line;
      end if;

      if (CPSR and 16#1f#) = 2#11010# then
         --  For hypervisor
         Put ("HSCTLR: ");
         Put (Hex4 (Get_HSCTLR));
         Put (", HCPTR: ");
         Put (Hex4 (Get_HCPTR));
         New_Line;
         Put ("HMAIR0: ");
         Put (Hex4 (Get_HMAIR0));
         Put (", HMAIR1: ");
         Put (Hex4 (Get_HMAIR1));
         Put (", HTCR: ");
         Put (Hex4 (Get_HTCR));
         Put (", HTTBR: ");
         Put (Hex8 (Get_HTTBR));
         New_Line;
      end if;

      if (CPSR and 16#1f#) = 2#10110# then
         --  Only for monitor.
         Put ("SCR: ");
         Put (Image8 (Get_SCR));
         New_Line;
      end if;
   end Proc_Cr;

   procedure Proc_Cpsid is
   begin
      Asm ("cpsid if", Volatile => True);
   end Proc_Cpsid;

   procedure Proc_Cpsie is
   begin
      Asm ("cpsie if", Volatile => True);
   end Proc_Cpsie;

   procedure Proc_Svc is
   begin
      Asm ("svc #0", Volatile => True);
   end Proc_Svc;

   procedure Proc_Smc is
   begin
      Asm ("mov r10, #0x27; mov r11, #0x15; smc #0", Volatile => True);
   end Proc_Smc;

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
     (6,
      (1 => (new String'("cache - disp cache"),
             Proc_Cache'Access),
       2 => (new String'("cr - Display some config registers"),
             Proc_Cr'Access),
       3 => (new String'("cpsid - Disable interrupts"),
             Proc_Cpsid'Access),
       4 => (new String'("cpsie - Enable interrupts"),
             Proc_Cpsie'Access),
       5 => (new String'("svc - Supervisor call"),
             Proc_Svc'Access),
       6 => (new String'("smc - Monitor call"),
             Proc_Smc'Access)),
      null);
begin
   Register_Commands (Commands'Access);
   if False then
      Trap_Handler.Install_Monitor_Handlers;
      Put_Line ("Handlers installed");
   end if;
end Armv7a;
