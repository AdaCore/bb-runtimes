------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;
with Uart; use Uart;
with IOEmu; use IOEmu;
with Emu_PL011;
with Interfaces; use Interfaces;
with Interfaces.AArch64; use Interfaces.AArch64;

package body Hulls is
   type Hull_Context_AArch64_Acc is access all Hull_Context_AArch64;
   pragma Convention (C, Hull_Context_AArch64_Acc);

   procedure Execute_Hull (Ctxt : Hull_Context_AArch64_Acc);
   pragma Import (C, Execute_Hull);

   procedure Handler_Syn_A64 (Ctxt : Hull_Context_Acc);

   function To_unsigned_64 is new Ada.Unchecked_Conversion
     (Source => Address, Target => Unsigned_64);

   procedure Cache_Sync_By_Range
     (Start : System.Address;
      Len   : System.Storage_Elements.Storage_Count)
   is
      Line_Size : constant := 16;
      Line_Off : Storage_Count;
      Off : Storage_Count;
      Addr : Address;
   begin
      Line_Off := Start mod Line_Size;
      Addr := Start - Line_Off;
      Off := 0;
      loop
         DC_CVAU (Addr);
         IC_IVAU (Addr);
         Off := Off + Line_Size;
         exit when Off > Len + Line_Off;
         Addr := Addr + Line_Size;
      end loop;

      DSB_ISH;
      ISB;
   end Cache_Sync_By_Range;

   procedure Create_Hull (Desc : Hull_Desc; Ctxt : Hull_Context_Acc) is
   begin
      --  Copy file
      if Desc.File_Base /= Null_Address then
         Put ("Copying Hull......");
         New_Line;

         if Desc.File_Size > Desc.Ram_Size then
            raise Constraint_Error;
         end if;

         declare
            Src : Storage_Array (1 .. Desc.File_Size);
            pragma Import (Ada, Src);
            for Src'Address use Desc.File_Base;

            Dest : Storage_Array (1 .. Desc.File_Size);
            pragma Import (Ada, Dest);
            for Dest'Address use Desc.Ram_Paddr;
         begin
            Dest := Src;

            Cache_Sync_By_Range (Dest'Address, Desc.File_Size);
         end;
      end if;

      Put ("Starting Hull......");
      New_Line;

      Ctxt.Cpu.PC := To_unsigned_64 (Desc.Ram_Vaddr);

      Ctxt.Cpu.Vttbr := To_unsigned_64 (Desc.Mmu_Base);
      Ctxt.Cpu.Vtcr := TCR_PS_4GB or TCR_TG0_4KB or TCR_SH0_OS
        or TCR_ORGN0_WBWAC or TCR_IRGN0_WBWAC or TCR_SL0_00 or (38 * TCR_T0SZ);
      Ctxt.Cpu.Hcr := HCR_RW or HCR_DC or HCR_IMO or HCR_FMO or HCR_VM;
      Ctxt.Cpu.Vbar := 16#ffffffff_fffff000#;
      Ctxt.Cpu.Sp := 16#100#;
      Ctxt.Cpu.Pstate := 16#1c5#;  -- el1h
      Ctxt.Cpu.Xregs := (others => 0);
      Ctxt.Cpu.Xregs (0) := 16#ff_00#;
      Ctxt.Cpu.Xregs (1) := 16#01_00#;
      Ctxt.Cpu.Xregs (12) := 16#12_00#;
      Ctxt.Cpu.Xregs (30) := 16#30_00#;

      Ctxt.Machine_Reset := False;

      loop
         Execute_Hull (Ctxt.Cpu'Access);
         Handler_Syn_A64 (Ctxt);
         exit when Ctxt.Machine_Reset;
      end loop;
   end Create_Hull;

   --  Exceptions

   procedure Dump (Regs : Hull_Context_Acc; Id : Natural);

   procedure Handler_Syn_A64 (Ctxt : Hull_Context_Acc)
   is
      ESR : constant Unsigned_32 := Ctxt.Cpu.Esr;
      EC : constant Unsigned_8 := Unsigned_8 (Shift_Right (ESR, 26));
   begin
      if False then
         Put ("SYN, EC=");
         Put_Hex4 (ESR);
         New_Line;
      end if;

      if EC = 16#24# then
         --  Exception from a Data abort.

         --  Check ISV, if no -> fails
         if (ESR and 16#100_0000#) = 0 then
            Dump (Ctxt, 16 + 1);
            return;
         end if;

         --  Check AR, fails if set.
         if (ESR and 16#4000#) /= 0 then
            Dump (Ctxt, 16 + 4);
            return;
         end if;

         --  Check DFSC
         case ESR and 16#3f# is
            when 2#100# | 2#101# | 2#110# | 2#111# =>
               null;
            when others =>
               Dump (Ctxt, 16 + 5);
         end case;

         --  Check EA
         if (ESR and 16#200#) /= 0 then
            Dump (Ctxt, 16 + 6);
            return;
         end if;

         --  Check FAR/HPFAR and find emu in table.  If no -> fails
         declare
            HPFAR : constant Unsigned_64 := Ctxt.Cpu.Hpfar;
            FAR : constant Unsigned_64 := Ctxt.Cpu.Far;
            Addr : constant Address :=
              System'To_Address (HPFAR * 16#100# or (FAR and 16#fff#));
            SAS : constant Natural range 0 .. 3 :=
              Natural (Shift_Right (ESR, 22) and 3);
            SRT : constant Natural :=
              Natural (Shift_Right (ESR, 16) and 31);
            Dev : IOEmu_Dev_Acc;
            Off : Off_T;
         begin
            Find_IO (Ctxt.all, Addr, Dev, Off);
            if Dev = null then
               Dump (Ctxt, 16 + 2);
               return;
            end if;

            if (ESR and 16#40#) = 0 then
               --  If read: dispatch and compute result
               declare
                  SSE : constant Boolean :=
                    Boolean'Val (Shift_Right (ESR, 21) and 1);
                  SF : constant Boolean :=
                    Boolean'Val (Shift_Right (ESR, 15) and 1);

                  R : Unsigned_64;
                  V8 : Unsigned_8;
                  V16 : Unsigned_16;
                  V32 : Unsigned_32;
               begin
                  case SAS is
                     when 0 =>
                        V8 := Dev.Read8 (Off);
                        if SSE and (V8 and 16#80#) /= 0 then
                           R := 16#ffffffff_ffffff00# or Unsigned_64 (V8);
                        else
                           R := Unsigned_64 (V8);
                        end if;
                     when 1 =>
                        V16 := Dev.Read16 (Off);
                        if SSE and (V16 and 16#8000#) /= 0 then
                           R := 16#ffffffff_ffff0000# or Unsigned_64 (V16);
                        else
                           R := Unsigned_64 (V16);
                        end if;
                     when 2 =>
                        V32 := Dev.Read32 (Off);
                        if SSE and (V32 and 16#80000000#) /= 0 then
                           R := 16#ffffffff_00000000# or Unsigned_64 (V32);
                        else
                           R := Unsigned_64 (V32);
                        end if;
                     when 3 =>
                        R := Dev.Read64 (Off);
                  end case;
                  if SRT >= 31 then
                     Dump (Ctxt, 16 + 3);
                     return;
                  end if;

                  if not SF then
                     R := R and 16#00000000_ffffffff#;
                  end if;

                  Ctxt.Cpu.Xregs (SRT) := R;
               end;
            else
               --  If write: compute data and dispatch
               declare
                  R : Unsigned_64;
               begin
                  if SRT = 31 then
                     R := 0;
                  else
                     R := Ctxt.Cpu.Xregs (SRT);
                  end if;

                  case SAS is
                     when 0 =>
                        Dev.Write8 (Off, Unsigned_8 (R and 16#ff#));
                     when 1 =>
                        Dev.Write16 (Off, Unsigned_16 (R and 16#ffff#));
                     when 2 =>
                        Dev.Write32 (Off, Unsigned_32 (R and 16#ffff_ffff#));
                     when 3 =>
                        Dev.Write64 (Off, R);
                  end case;
               end;
            end if;
         end;

         --  PC = PC + 4
         Ctxt.Cpu.PC := Ctxt.Cpu.PC + 4;
      else
         Dump (Ctxt, 32);
      end if;
   end Handler_Syn_A64;

   procedure Dump (Regs : Hull_Context_Acc; Id : Natural)
   is
      procedure Trap_dump (Regs : Hull_Context_AArch64_Acc; Id : Natural);
      pragma Import (C, Trap_dump, "__trap_dump");
   begin
      Asm ("msr DAIFset, #3", Volatile => True);
      Trap_dump (Regs.Cpu'Unrestricted_Access, Id);
      Asm ("msr DAIFclr, #3", Volatile => True);
   end Dump;
end Hulls;
