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

with System.Machine_Code; use System.Machine_Code;
with Ada.Text_IO; use Ada.Text_IO;
with IOEmu; use IOEmu;
with Emu_Uart;
with Interfaces; use Interfaces;

package body Hulls is
   procedure Start_Hull (Mmu_Map : Address; Start : Address);
   pragma Import (Asm, Start_Hull);

   procedure DC_CVAU (Addr : Address) is
   begin
      Asm ("dc cvau, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end DC_CVAU;

   procedure IC_IVAU (Addr : Address) is
   begin
      Asm ("ic ivau, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end IC_IVAU;

   procedure DSB is
   begin
      Asm ("dsb ish", Volatile => True);
   end DSB;

   procedure ISB is
   begin
      Asm ("isb", Volatile => True);
   end ISB;

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

      DSB;
      ISB;
   end Cache_Sync_By_Range;

   procedure Create_Hull (Desc : Hull_Desc) is
   begin
      --  Copy file
      if Desc.File_Base /= Null_Address then
         Put_Line ("Copying Hull......");

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

      Put_Line ("Starting Hull......");

      Start_Hull (Desc.Mmu_Base, Desc.Ram_Vaddr);
   end Create_Hull;

   --  IO Emulation

   type IOEmu_Dev_Acc is access all IOEmu_Dev'Class;

   type IOEmu_Map_Entry is record
      Base : Address;
      Len  : Storage_Count;
      Dev : IOEmu_Dev_Acc;
   end record;

   type IOEmu_Map_Array is array (Natural range <>) of IOEmu_Map_Entry;

   Uart : aliased Emu_Uart.Uart_Dev;

   IOEmu_Map : constant IOEmu_Map_Array :=
     (0 => (System'To_Address (16#e000_1000#), 16#100#, Uart'Access));

   procedure Find_IO
     (Addr : Address; Dev : out IOEmu_Dev_Acc; Off : out Off_T)
   is
   begin
      for I in IOEmu_Map'Range loop
         declare
            E : IOEmu_Map_Entry renames IOEmu_Map (I);
         begin
            if Addr >= E.Base and then Addr < E.Base + E.Len then
               Dev := E.Dev;
               Off := Addr - E.Base;
               return;
            end if;
         end;
      end loop;

      Dev := null;
      Off := 0;
   end Find_IO;

   --  Exceptions

   type X_Regs is array (0 .. 31) of Unsigned_64;
   pragma Suppress_Initialization (X_Regs);

   type Registers_List is record
      Xr : X_Regs;
   end record;
   pragma Convention (C, Registers_List);
   pragma Suppress_Initialization (Registers_List);

   type Registers_List_Acc is access Registers_List;

   procedure Handler_Syn_A64 (Xregs : Registers_List_Acc; Id : Natural);
   pragma Export (C, Handler_Syn_A64, "__handler_trap_syn_a64");

   function Get_ESR_EL2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, esr_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ESR_EL2;

   function Get_HPFAR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, hpfar_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_HPFAR_EL2;

   function Get_FAR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, far_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_FAR_EL2;

   function Get_ELR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL2;

   procedure Set_ELR_EL2 (V : Unsigned_64) is
   begin
      Asm ("msr elr_el2, %0",
           Inputs => Unsigned_64'Asm_Input ("r", V),
           Volatile => True);
   end Set_ELR_EL2;

   procedure Dump (Regs : Registers_List_Acc; Id : Natural);
   pragma Import (C, Dump, "__trap_dump");

   procedure Handler_Syn_A64 (Xregs : Registers_List_Acc; Id : Natural)
   is
      ESR : constant Unsigned_32 := Get_ESR_EL2;
      EC : constant Unsigned_8 := Unsigned_8 (Shift_Right (ESR, 26));
   begin
      if EC = 16#24# then
         --  Exception from a Data abort.

         --  Check ISV, if no -> fails
         if (ESR and 16#100_0000#) = 0 then
            Dump (Xregs, 16 + 1);
            return;
         end if;

         --  Check AR, fails if set.
         if (ESR and 16#4000#) /= 0 then
            Dump (Xregs, 16 + 4);
            return;
         end if;

         --  Check DFSC
         case ESR and 16#3f# is
            when 2#100# | 2#101# | 2#110# | 2#111# =>
               null;
            when others =>
               Dump (Xregs, 16 + 5);
         end case;

         --  Check EA
         if (ESR and 16#200#) /= 0 then
            Dump (Xregs, 16 + 6);
            return;
         end if;

         --  Check FAR/HPFAR and find emu in table.  If no -> fails
         declare
            HPFAR : constant Unsigned_64 := Get_HPFAR_EL2;
            FAR : constant Unsigned_64 := Get_FAR_EL2;
            Addr : constant Address :=
              System'To_Address (HPFAR * 16#100# or (FAR and 16#fff#));
            SAS : constant Natural range 0 .. 3 :=
              Natural (Shift_Right (ESR, 22) and 3);
            SRT : constant Natural :=
              Natural (Shift_Right (ESR, 16) and 31);
            Dev : IOEmu_Dev_Acc;
            Off : Off_T;
         begin
            Find_IO (Addr, Dev, Off);
            if Dev = null then
               Dump (Xregs, 16 + 2);
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
                     Dump (Xregs, 16 + 3);
                     return;
                  end if;

                  if not SF then
                     R := R and 16#00000000_ffffffff#;
                  end if;

                  Xregs.Xr (SRT) := R;
               end;
            else
               --  If write: compute data and dispatch
               declare
                  R : Unsigned_64;
               begin
                  if SRT >= 31 then
                     Dump (Xregs, 16 + 4);
                     return;
                  end if;

                  R := Xregs.Xr (SRT);

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
         Set_ELR_EL2 (Get_ELR_EL2 + 4);
      else
         Dump (Xregs, 32);
      end if;
   end Handler_Syn_A64;
end Hulls;
