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

with System.Machine_Code; use System.Machine_Code;
with Interfaces; use Interfaces;
with Console; use Console;
with Commands; use Commands;
with Dumps; use Dumps;
with Term; use Term;

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

   function Get_DAIF return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, daif",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DAIF;

   function Get_SPSel return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, spsel",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSel;

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

   function Get_TCR_EL2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, tcr_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_TCR_EL2;

   function Get_SCTLR_EL2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, sctlr_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SCTLR_EL2;

   function Get_TTBR0_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, ttbr0_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_TTBR0_EL2;

   function Get_VBAR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, vbar_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_VBAR_EL2;

   --  EL1

   function Get_SP_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL1;

   function Get_SCTLR_EL1 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, sctlr_el1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SCTLR_EL1;

   function Get_TCR_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, tcr_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_TCR_EL1;

   function Get_VBAR_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, vbar_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_VBAR_EL1;

   function Get_TTBR0_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, ttbr0_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_TTBR0_EL1;

   function Get_TTBR1_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, ttbr1_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_TTBR1_EL1;

   function Get_ID_AA64MMFR0_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, id_aa64mmfr0_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ID_AA64MMFR0_EL1;

   --  Timers

   function Get_CNTP_CTL_EL0 return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, cntp_ctl_el0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CNTP_CTL_EL0;

   function Get_CNTV_CTL_EL0 return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, cntv_ctl_el0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CNTV_CTL_EL0;

   function Get_CNTHP_CTL_EL2 return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, cnthp_ctl_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CNTHP_CTL_EL2;

   function Get_CNTPCT_EL0 return Unsigned_64 is
      Res : Unsigned_64;
   begin
      --  Read CNTPCT
      Asm ("mrs %0, cntpct_el0",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CNTPCT_EL0;

   procedure Proc_El1 is
   begin
      if Get_Current_EL = 3 * 4 then
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
      elsif Get_Current_EL = 2 * 4 then
         Asm ("adr x0, 1f" & ASCII.LF & ASCII.HT &
              "msr elr_el2, x0" & ASCII.LF & ASCII.HT &
              "mov x0, #0x1c5"  & ASCII.LF & ASCII.HT &
              "msr spsr_el2, x0"  & ASCII.LF & ASCII.HT &
              "mov x0, sp"  & ASCII.LF & ASCII.HT &
              "msr sp_el1, x0"   & ASCII.LF & ASCII.HT &
              "isb"  & ASCII.LF & ASCII.HT &
              "eret"  & ASCII.LF & ASCII.HT &
              "1:",
              Volatile => True,
              Clobber => "x0");
      else
         Put_Line ("Not in EL3/EL2!");
      end if;
   end Proc_El1;

   procedure Proc_El2 is
   begin
      if Get_Current_EL /= 3 * 4 then
         Put_Line ("Not in EL3!");
         return;
      end if;

      Asm ("adr x0, 1f" & ASCII.LF & ASCII.HT &
           "msr elr_el3, x0" & ASCII.LF & ASCII.HT &
           "mov x0, #0x1c9"  & ASCII.LF & ASCII.HT &
           "msr spsr_el3, x0"  & ASCII.LF & ASCII.HT &
           "mov x0, sp"  & ASCII.LF & ASCII.HT &
           "msr sp_el2, x0"   & ASCII.LF & ASCII.HT &
           "isb"  & ASCII.LF & ASCII.HT &
           "eret"  & ASCII.LF & ASCII.HT &
           "1:",
           Volatile => True,
           Clobber => "x0");
   end Proc_El2;

   procedure Proc_Svc is
   begin
      Asm ("svc #0",
           Volatile => True);
   end Proc_Svc;

   procedure Proc_Hvc is
   begin
      Asm ("hvc #0",
           Volatile => True);
   end Proc_Hvc;

   procedure Proc_Smc is
   begin
      Asm ("smc #0",
           Volatile => True);
   end Proc_Smc;

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
         Put (")");
         L := Natural ((EL / 4) and 3);

         Put (", DAIF: ");
         Put (Hex4 (Get_DAIF));

         Put (", SPSEL: ");
         Put (Hex4 (Get_SPSel));
         New_Line;
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
         Put ("EL2 SP: ");
         Put (Hex8 (Get_SP_EL2));
         New_Line;
      end if;

      if L >= 2 then
         Put ("EL2 HCR: ");
         Put (Hex8 (Get_HCR_EL2));
         Put (" TCR: ");
         Put (Hex4 (Get_TCR_EL2));
         Put (" VBAR: ");
         Put (Hex8 (Get_VBAR_EL2));
         New_Line;
         Put ("EL2 SCTLR: ");
         Put (Hex4 (Get_SCTLR_EL2));
         Put (" TTBR0: ");
         Put (Hex8 (Get_TTBR0_EL2));
         New_Line;
         Put ("EL1 SP: ");
         Put (Hex8 (Get_SP_EL1));
         New_Line;
      end if;

      if L >= 1 then
         Put ("EL1 SCTLR: ");
         Put (Hex4 (Get_SCTLR_EL1));
         Put (" VBAR: ");
         Put (Hex8 (Get_VBAR_EL1));
         Put (" TCR: ");
         Put (Hex8 (Get_TCR_EL1));
         New_Line;
         Put ("EL1 TTBR0: ");
         Put (Hex8 (Get_TTBR0_EL1));
         Put (" TTBR1: ");
         Put (Hex8 (Get_TTBR1_EL1));
         New_Line;
      end if;
   end Proc_Cr;

   procedure Proc_At
   is
      type At_Kind is (S1e1r, S1e1w, S1e0r, S1e0w);
      Kind : At_Kind;
      Addr : Unsigned_32;
      Addr64 : Unsigned_64;
      Ok : Boolean;
      Res : Unsigned_64;
   begin
      Next_Word;
      Parse_Unsigned32 (Addr, Ok);
      if not Ok then
         return;
      end if;

      Next_Word;
      if Pos > Line_Len then
         Kind := S1e1r;
      else
         if Line (Pos .. End_Pos) = "s1e1r" then
            Kind := S1e1r;
         elsif Line (Pos .. End_Pos) = "s1e1w" then
            Kind := S1e1w;
         elsif Line (Pos .. End_Pos) = "s1e0r" then
            Kind := S1e0r;
         elsif Line (Pos .. End_Pos) = "s1e0w" then
            Kind := S1e0w;
         else
            Put_Line ("at command: unknown kind");
            return;
         end if;
      end if;

      Addr64 := Unsigned_64 (Addr);
      Put ("addr: ");
      Put (Hex8 (Addr64));

      case Kind is
         when S1e1r =>
            Asm ("at s1e1r,%0",
                 Inputs => Unsigned_64'Asm_Input ("r", Addr64),
                 Volatile => True);
         when S1e1w =>
            Asm ("at s1e1w,%0",
                 Inputs => Unsigned_64'Asm_Input ("r", Addr64),
                 Volatile => True);
         when S1e0r =>
            Asm ("at s1e0r,%0",
                 Inputs => Unsigned_64'Asm_Input ("r", Addr64),
                 Volatile => True);
         when S1e0w =>
            Asm ("at s1e0w,%0",
                 Inputs => Unsigned_64'Asm_Input ("r", Addr64),
                 Volatile => True);
      end case;

      Asm ("isb", Volatile => True);
      Asm ("mrs %0,par_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      Put ("  PAR_EL1: ");
      Put (Hex8 (Res));
      New_Line;
   end Proc_At;

   procedure Proc_Features is
   begin
      Put ("ID_AA64MMFR0: ");
      Put (Hex8 (Get_ID_AA64MMFR0_EL1));
      New_Line;
   end Proc_Features;

   procedure Proc_Mmu_On is
      EL : constant Unsigned_32 := Get_Current_EL;
      Sctlr : Unsigned_32;
   begin
      if EL = 2 * 4 then
         Sctlr := Get_SCTLR_EL2;
         Sctlr := Sctlr or 1;
         Asm ("msr sctlr_el2, %0"  & ASCII.LF & ASCII.HT &
              "isb",
              Inputs => Unsigned_32'Asm_Input ("r", Sctlr),
              Volatile => True);
      else
         Put_Line ("Not implemented at this level");
      end if;
   end Proc_Mmu_On;

   procedure Proc_Timers is
      EL : constant Unsigned_32 := Get_Current_EL;
   begin
      if EL >= 2 * 4 then
         Put ("EL2: CNTHP_CTL: ");
         Put (Hex4 (Get_CNTHP_CTL_EL2));
         New_Line;
      end if;

      Put ("EL0: CNTP_CTL: ");
      Put (Hex4 (Get_CNTP_CTL_EL0));
      Put (" CNTV_CTL: ");
      Put (Hex4 (Get_CNTV_CTL_EL0));
      Put (" CNTPCT: ");
      Put (Hex8 (Get_CNTPCT_EL0));
      New_Line;
   end Proc_Timers;

   Commands : aliased Command_List :=
     (10,
      (1 => (new String'("cr - Display some config registers"),
             Proc_Cr'Access),
       2 => (new String'("el1 - Switch to el1"),
             Proc_El1'Access),
       3 => (new String'("el2 - Switch to el2"),
             Proc_El2'Access),
       4 => (new String'("svc - sys call"),
             Proc_Svc'Access),
       5 => (new String'("hvc - hyper call"),
             Proc_Hvc'Access),
       6 => (new String'("smc - monitor call"),
             Proc_Smc'Access),
       7 => (new String'("at - address translate"),
             Proc_At'Access),
       8 => (new String'("mmu_on - Enable mmu"),
             Proc_Mmu_On'Access),
       9 => (new String'("timers - Display timers regs"),
             Proc_Timers'Access),
       10 => (new String'("features - disp processor features"),
             Proc_Features'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Aarch64;
