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

with Uart; use Uart;
with System.Storage_Elements; use System.Storage_Elements;

package body Emu_GIC is
   --  Emit debug traces
   Flag_Debug : constant Boolean := False;

   --  Registers
   Reg_GICD_CTLR        : constant := 16#000#;
   Reg_GICD_TYPER       : constant := 16#004#;
   Reg_GICD_IIDR        : constant := 16#008#;
   Reg_GICD_IGROUPRn    : constant := 16#080#;
   Reg_GICD_ISENABLERn  : constant := 16#100#;
   Reg_GICD_ISENABLER0  : constant := 16#100#;
   Reg_GICD_ISENABLER1  : constant := 16#104#;
   Reg_GICD_ICENABLERn  : constant := 16#180#;
   Reg_GICD_ICENABLER0  : constant := 16#180#;
   Reg_GICD_ICENABLER1  : constant := 16#184#;
   Reg_GICD_ISPENDRn    : constant := 16#200#;
   Reg_GICD_ICPENDRn    : constant := 16#280#;
   Reg_GICD_ISACTIVERn  : constant := 16#300#;
   Reg_GICD_ISACTIVER0  : constant := 16#300#;
   Reg_GICD_ISACTIVER1  : constant := 16#304#;
   Reg_GICD_ICACTIVERn  : constant := 16#380#;
   Reg_GICD_ICACTIVER0  : constant := 16#380#;
   Reg_GICD_ICACTIVER1  : constant := 16#384#;
   Reg_GICD_IPRIORITYRn : constant := 16#400#;
   Reg_GICD_IPRIORITYR0 : constant := 16#400#;
   Reg_GICD_IPRIORITYR1 : constant := 16#404#;
   Reg_GICD_IPRIORITYR2 : constant := 16#408#;
   Reg_GICD_IPRIORITYR3 : constant := 16#40c#;
   Reg_GICD_IPRIORITYR4 : constant := 16#410#;
   Reg_GICD_IPRIORITYR5 : constant := 16#414#;
   Reg_GICD_IPRIORITYR6 : constant := 16#418#;
   Reg_GICD_IPRIORITYR7 : constant := 16#41c#;
   Reg_GICD_IPRIORITYR8 : constant := 16#420#;
   Reg_GICD_IPRIORITYR9 : constant := 16#424#;
   Reg_GICD_IPRIORITYR10 : constant := 16#428#;
   Reg_GICD_IPRIORITYR11 : constant := 16#42c#;
   Reg_GICD_IPRIORITYR12 : constant := 16#430#;
   Reg_GICD_IPRIORITYR13 : constant := 16#434#;
   Reg_GICD_IPRIORITYR14 : constant := 16#438#;
   Reg_GICD_IPRIORITYR15 : constant := 16#43c#;
   Reg_GICD_ITARGETSR0  : constant := 16#800#;
   Reg_GICD_ITARGETSR1  : constant := 16#804#;
   Reg_GICD_ITARGETSR2  : constant := 16#808#;
   Reg_GICD_ITARGETSR3  : constant := 16#80c#;
   Reg_GICD_ITARGETSR4  : constant := 16#810#;
   Reg_GICD_ITARGETSR5  : constant := 16#814#;
   Reg_GICD_ITARGETSR6  : constant := 16#818#;
   Reg_GICD_ITARGETSR7  : constant := 16#81c#;
   Reg_GICD_ITARGETSR8  : constant := 16#820#;
   Reg_GICD_ITARGETSR9  : constant := 16#824#;
   Reg_GICD_ITARGETSR10 : constant := 16#828#;
   Reg_GICD_ITARGETSR11 : constant := 16#82c#;
   Reg_GICD_ITARGETSR12 : constant := 16#830#;
   Reg_GICD_ITARGETSR13 : constant := 16#834#;
   Reg_GICD_ITARGETSR14 : constant := 16#838#;
   Reg_GICD_ITARGETSR15 : constant := 16#83c#;
   Reg_GICD_ICFGRn      : constant := 16#C00#;
   Reg_GICD_ICFGR0      : constant := 16#C00#;
   Reg_GICD_ICFGR1      : constant := 16#C04#;
   Reg_GICD_ICFGR2      : constant := 16#C08#;
   Reg_GICD_ICFGR3      : constant := 16#C0c#;
   Reg_GICD_NSACRn      : constant := 16#E00#;
   Reg_GICD_SGIR        : constant := 16#F00#;
   Reg_GICD_CPENDSGIRn  : constant := 16#F10#;
   Reg_GICD_SPENDSGIRn  : constant := 16#F20#;

   Reg_GICC_CTLR    : constant := 16#1_0000#;
   Reg_GICC_PMR     : constant := 16#1_0004#;
   Reg_GICC_BPR     : constant := 16#1_0008#;
   Reg_GICC_IAR     : constant := 16#1_000c#;
   Reg_GICC_EOIR    : constant := 16#1_0010#;
   Reg_GICC_RPR     : constant := 16#1_0014#;
   Reg_GICC_HPPIR   : constant := 16#1_0018#;
   Reg_GICC_ABPR    : constant := 16#1_001c#;
   Reg_GICC_AIAR    : constant := 16#1_0020#;
   Reg_GICC_AEOIR   : constant := 16#1_0024#;
   Reg_GICC_AHPPIR  : constant := 16#1_0028#;
   Reg_GICC_APRn    : constant := 16#1_00d0#;
   Reg_GICC_NSAPRn  : constant := 16#1_00e0#;
   Reg_GICC_IIDR    : constant := 16#1_00fc#;
   Reg_GICC_DIR     : constant := 16#1_1000#;

   procedure Generate_Exceptions (Dev : in out GIC_Dev);

   procedure Compute_Highest_Priority_Pending_Interrupt (Dev : in out GIC_Dev);
   function Compute_Highest_Priority_Active_Interrupt
     (Dev : GIC_Dev) return Unsigned_32;

   procedure Disp_Reg_Name (Off : Off_T) is
   begin
      case Off is
         when Reg_GICD_CTLR =>
            Put ("GICD_CTLR");
         when Reg_GICD_TYPER =>
            Put ("GICD_TYPER");
         when Reg_GICD_IIDR =>
            Put ("GICD_IIDR");
         when Reg_GICD_IGROUPRn =>
            Put ("GICD_IGROUPRn");
         when Reg_GICD_ISENABLER0 .. Reg_GICD_ISENABLER1 =>
            Put ("GICD_ISENABLER");
            Put_Dec (Natural (Off - Reg_GICD_ISENABLER0) / 4);
         when Reg_GICD_ICENABLER0 .. Reg_GICD_ICENABLER1 =>
            Put ("GICD_ICENABLER");
            Put_Dec (Natural (Off - Reg_GICD_ICENABLER0) / 4);
         when Reg_GICD_ISPENDRn =>
            Put ("GICD_ISPENDRn");
         when Reg_GICD_ICPENDRn =>
            Put ("GICD_ICPENDRn");
         when Reg_GICD_ISACTIVER0 .. Reg_GICD_ISACTIVER1 =>
            Put ("GICD_ISACTIVER");
            Put_Dec (Natural (Off - Reg_GICD_ISACTIVER0) / 4);
         when Reg_GICD_ICACTIVER0 .. Reg_GICD_ICACTIVER1 =>
            Put ("GICD_ICACTIVER");
            Put_Dec (Natural (Off - Reg_GICD_ICACTIVER0) / 4);
         when Reg_GICD_IPRIORITYR0 .. Reg_GICD_IPRIORITYR15 =>
            Put ("GICD_IPRIORITYR");
            Put_Dec (Natural (Off - Reg_GICD_IPRIORITYR0) / 4);
         when Reg_GICD_ITARGETSR0 .. Reg_GICD_ITARGETSR15 =>
            Put ("GICD_ITARGETSR");
            Put_Dec (Natural (Off - Reg_GICD_ITARGETSR0) / 4);
         when Reg_GICD_ICFGR0 .. Reg_GICD_ICFGR3 =>
            Put ("GICD_ICFGR");
            Put_Dec (Natural (Off - Reg_GICD_ICFGR0) / 4);
         when Reg_GICD_NSACRn =>
            Put ("GICD_NSACRn");
         when Reg_GICD_SGIR =>
            Put ("GICD_SGIR");
         when Reg_GICD_CPENDSGIRn =>
            Put ("GICD_CPENDSGIRn");
         when Reg_GICD_SPENDSGIRn =>
            Put ("GICD_SPENDSGIRn");

         when Reg_GICC_CTLR =>
            Put ("GICC_CTLR");
         when Reg_GICC_PMR =>
            Put ("GICC_PMR");
         when Reg_GICC_BPR =>
            Put ("GICC_BPR");
         when Reg_GICC_IAR =>
            Put ("GICC_IAR");
         when Reg_GICC_EOIR =>
            Put ("GICC_EOIR");
         when Reg_GICC_RPR =>
            Put ("GICC_RPR");
         when Reg_GICC_HPPIR =>
            Put ("GICC_HPPIR");
         when Reg_GICC_ABPR =>
            Put ("GICC_ABPR");
         when Reg_GICC_AIAR =>
            Put ("GICC_AIAR");
         when Reg_GICC_AEOIR =>
            Put ("GICC_AEOIR");
         when Reg_GICC_AHPPIR =>
            Put ("GICC_AHPPIR");
         when Reg_GICC_APRn =>
            Put ("GICC_APRn");
         when Reg_GICC_NSAPRn =>
            Put ("GICC_NSAPRn");
         when Reg_GICC_IIDR =>
            Put ("GICC_IIDR");
         when Reg_GICC_DIR =>
            Put ("GICC_DIR");

         when others =>
            Put_Hex4 (Unsigned_32 (Off));
      end case;
   end Disp_Reg_Name;

   --  Get configuration (level or edge triggered) of interrupt ID.
   function Get_Icfg (Dev : GIC_Dev; Id : Natural) return Unsigned_32 is
   begin
      return Shift_Right (Dev.ICFGR (Id / 16), 2 * (Id mod 16)) and 3;
   end Get_Icfg;

   function Read32 (Dev : in out GIC_Dev; Off : Off_T)
                    return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      case Off is
         when Reg_GICD_CTLR =>
            Res := Dev.DCTLR;
         when Reg_GICD_TYPER =>
            --  64 interrupts, 1 cpu, no security extension.
            Res := 2#00000_0_00_000_00000# + Unsigned_32 ((Nbr_Int / 32) - 1);
         when Reg_GICD_ITARGETSR0
            | Reg_GICD_ITARGETSR1
            | Reg_GICD_ITARGETSR2
            | Reg_GICD_ITARGETSR3
            | Reg_GICD_ITARGETSR4
            | Reg_GICD_ITARGETSR5
            | Reg_GICD_ITARGETSR6
            | Reg_GICD_ITARGETSR7 =>
            Res := 16#01_01_01_01#;
         when Reg_GICD_ITARGETSR8
            | Reg_GICD_ITARGETSR9
            | Reg_GICD_ITARGETSR10
            | Reg_GICD_ITARGETSR11
            | Reg_GICD_ITARGETSR12
            | Reg_GICD_ITARGETSR13
            | Reg_GICD_ITARGETSR14
            | Reg_GICD_ITARGETSR15 =>
            --  Fixed in hardware.
            Res := 16#01_01_01_01#;
         when Reg_GICD_ICFGR0 =>
            --  SGIR: level-sensitive
            Res := 16#00000000#;
         when Reg_GICD_ICFGR1 =>
            --  Interrupt 14 (timer ns el1) is level-hi.
            Res := 16#00000000#;
         when Reg_GICD_ICFGR2
            | Reg_GICD_ICFGR3 =>
            Res := Dev.ICFGR (Natural (Off - Reg_GICD_ICFGRn) / 4);
         when Reg_GICD_IPRIORITYR0
            | Reg_GICD_IPRIORITYR1
            | Reg_GICD_IPRIORITYR2
            | Reg_GICD_IPRIORITYR3
            | Reg_GICD_IPRIORITYR4
            | Reg_GICD_IPRIORITYR5
            | Reg_GICD_IPRIORITYR6
            | Reg_GICD_IPRIORITYR7
            | Reg_GICD_IPRIORITYR8
            | Reg_GICD_IPRIORITYR9
            | Reg_GICD_IPRIORITYR10
            | Reg_GICD_IPRIORITYR11
            | Reg_GICD_IPRIORITYR12
            | Reg_GICD_IPRIORITYR13
            | Reg_GICD_IPRIORITYR14
            | Reg_GICD_IPRIORITYR15 =>
            Res := Dev.PRIORITY (Natural (Off - Reg_GICD_IPRIORITYRn) / 4);
         when Reg_GICD_ISACTIVER0
            | Reg_GICD_ISACTIVER1 =>
            Res := Dev.ACTIVE (Natural (Off - Reg_GICD_ISACTIVERn) / 4);
         when Reg_GICD_ICACTIVER0
            | Reg_GICD_ICACTIVER1 =>
            Res := Dev.ACTIVE (Natural (Off - Reg_GICD_ICACTIVERn) / 4);
         when Reg_GICD_ISENABLER0
            | Reg_GICD_ISENABLER1 =>
            Res := Dev.ENABLE (Natural (Off - Reg_GICD_ISENABLERn) / 4);
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            Res := Dev.ENABLE (Natural (Off - Reg_GICD_ICENABLERn) / 4);

         when Reg_GICC_CTLR =>
            Res := Dev.CCTLR;
         when Reg_GICC_PMR =>
            Res := Dev.PMR;

         when Reg_GICC_IAR =>
            Res := Dev.HPPIR;
            declare
               Id : constant Natural := Natural (Res and 16#3ff#);
            begin
               if Res /= 1023 then
                  --  Acknowledge interrupt.
                  --  interrupt is active.
                  Set_Enable (Dev.ACTIVE (Id / 32), Shift_Left (1, Id mod 32));
                  --  If it is edge-triggered, disable pending bit.
                  if (Get_Icfg (Dev, Id) and 2) = 2 then
                     Set_Disable
                       (Dev.PEND (Id / 32), Shift_Left (1, Id mod 32));
                  end if;
                  --  Change running priority
                  Dev.CRPR := Dev.HPPIR_Prio;
                  --  Recompute highest pending interrupt.
                  Compute_Highest_Priority_Pending_Interrupt (Dev);
               end if;
            end;

         when others =>
            Put ("gic.read ");
            Disp_Reg_Name (Off);
            New_Line;
            raise Program_Error;
      end case;

      if Flag_Debug then
         Put ("RGIC: ");
         Disp_Reg_Name (Off);
         Put (" -> ");
         Put_Hex4 (Res);
         New_Line;
      end if;

      return Res;
   end Read32;

   procedure Write32_Mask
     (Dev : in out GIC_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32)
   is
      Idx : Natural;
   begin
      if Flag_Debug then
         Put ("WGIC: ");
         Disp_Reg_Name (Off);
         Put (" <- ");
         Put_Hex4 (Val);
         New_Line;
      end if;

      case Off is
         when Reg_GICD_CTLR =>
            Update (Dev.DCTLR, Val and 16#01#, Mask);
            --  FIXME: update output.
         when Reg_GICD_ITARGETSR0
            | Reg_GICD_ITARGETSR1
            | Reg_GICD_ITARGETSR2
            | Reg_GICD_ITARGETSR3
            | Reg_GICD_ITARGETSR4
            | Reg_GICD_ITARGETSR5
            | Reg_GICD_ITARGETSR6
            | Reg_GICD_ITARGETSR7 =>
            --  Write ignored.
            return;
         when Reg_GICD_ITARGETSR8
            | Reg_GICD_ITARGETSR9
            | Reg_GICD_ITARGETSR10
            | Reg_GICD_ITARGETSR11
            | Reg_GICD_ITARGETSR12
            | Reg_GICD_ITARGETSR13
            | Reg_GICD_ITARGETSR14
            | Reg_GICD_ITARGETSR15 =>
            --  Write ignored.
            return;
         when Reg_GICD_ICFGR0
            | Reg_GICD_ICFGR1 =>
            return;
         when Reg_GICD_ICFGR2
            | Reg_GICD_ICFGR3 =>
            Idx := Natural (Off - Reg_GICD_ICFGRn) / 4;
            Update (Dev.ICFGR (Idx), Val, Mask);
            --  Update ?
         when Reg_GICD_IPRIORITYR0
            | Reg_GICD_IPRIORITYR1
            | Reg_GICD_IPRIORITYR2
            | Reg_GICD_IPRIORITYR3
            | Reg_GICD_IPRIORITYR4
            | Reg_GICD_IPRIORITYR5
            | Reg_GICD_IPRIORITYR6
            | Reg_GICD_IPRIORITYR7
            | Reg_GICD_IPRIORITYR8
            | Reg_GICD_IPRIORITYR9
            | Reg_GICD_IPRIORITYR10
            | Reg_GICD_IPRIORITYR11
            | Reg_GICD_IPRIORITYR12
            | Reg_GICD_IPRIORITYR13
            | Reg_GICD_IPRIORITYR14
            | Reg_GICD_IPRIORITYR15 =>
            Idx := Natural (Off - Reg_GICD_IPRIORITYRn) / 4;
            Update (Dev.PRIORITY (Idx), Val, Mask);
            Generate_Exceptions (Dev);
         when Reg_GICD_ISACTIVER0
            | Reg_GICD_ISACTIVER1 =>
            Idx := Natural (Off - Reg_GICD_ISACTIVERn) / 4;
            Set_Enable (Dev.ACTIVE (Idx), Val);
            --  Update
         when Reg_GICD_ICACTIVER0
            | Reg_GICD_ICACTIVER1 =>
            Idx := Natural (Off - Reg_GICD_ICACTIVERn) / 4;
            Set_Disable (Dev.ACTIVE (Idx), Val);
            --  Update.
         when Reg_GICD_ISENABLER0
            | Reg_GICD_ISENABLER1 =>
            Idx := Natural (Off - Reg_GICD_ISENABLERn) / 4;
            Set_Enable (Dev.ENABLE (Idx), Val);
            Generate_Exceptions (Dev);
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            Idx := Natural (Off - Reg_GICD_ICENABLERn) / 4;
            Set_Disable (Dev.ENABLE (Idx), Val);
            Generate_Exceptions (Dev);

         when Reg_GICC_CTLR =>
            Update (Dev.CCTLR, Val and 1, Mask);
            Generate_Exceptions (Dev);
         when Reg_GICC_PMR =>
            Update (Dev.PMR, Val and 16#ff#, Mask);
            Generate_Exceptions (Dev);

         when Reg_GICC_EOIR =>
            declare
               Id : constant Natural := Natural (Val and 16#3ff#);
               Nprio : Unsigned_32;
            begin
               --  Ignore write of spurious interrupt
               if Id >= 1020 then
                  return;
               end if;

               --  Deactivate interrupt
               Set_Disable (Dev.ACTIVE (Id / 32), Shift_Left (1, Id mod 32));

               --  Set running priority to the highest active interrupt
               Nprio := Compute_Highest_Priority_Active_Interrupt (Dev);

               Dev.CRPR := Nprio;

               Generate_Exceptions (Dev);
            end;

         when others =>
            Put ("gic.write ");
            Disp_Reg_Name (Off);
            New_Line;
            raise Program_Error;
      end case;
   end Write32_Mask;

   procedure Init (Dev : access GIC_Dev; Cpu : Interrupt_Dev_Acc) is
   begin
      Dev.all := (Cpu => Cpu,
                  IT => (Parent => GIC_Dev_Acc (Dev)),
                  Lines => (others => False),
                  DCTLR => 0,
                  ICFGR => (others => 0),
                  PRIORITY => (others => 0),
                  ACTIVE => (others => 0),
                  ENABLE => (others => 0),
                  PEND => (others => 0),

                  CCTLR => 0,
                  PMR => 0,
                  BPR => 0,
                  CRPR => 16#ff#,
                  HPPIR => 16#3ff#,
                  HPPIR_Prio => 16#ff#,

                  Irq => False,
                  Fiq => False);
   end Init;

   function Get_Interrupt_Dev (Dev : access GIC_Dev) return Interrupt_Dev_Acc
   is
   begin
      return Dev.IT'Access;
   end Get_Interrupt_Dev;

   function Get_Priority (Dev : GIC_Dev; Id : Natural) return Unsigned_32 is
   begin
      return Shift_Left (Dev.PRIORITY (Id / 4), 8 * (Id mod 4)) and 16#ff#;
   end Get_Priority;

   procedure Compute_Highest_Priority_Pending_Interrupt (Dev : in out GIC_Dev)
   is
      Pend : Unsigned_32;
      Cur_Mask : Unsigned_32;
      Id, Cur : Natural;
      Prio, Cur_Prio : Unsigned_32;
   begin
      Id := 1023;
      Prio := 16#ff#;

      for I in 0 .. (Nbr_Int / 32) - 1 loop
         Pend := Dev.PEND (I) and not Dev.ACTIVE (I);
         for J in 0 .. 31 loop
            Cur := I * 32 + J;
            Cur_Mask := Shift_Left (1, J);
            if (Pend and Cur_Mask) /= 0 then
               --  Interrupt is pending (and not active).
               Cur_Prio := Get_Priority (Dev, Cur);
               if Cur_Prio < Prio then
                  Id := Cur;
                  Prio := Cur_Prio;
               end if;
               Pend := Pend and not Cur_Mask;
               exit when Pend = 0;
            end if;
         end loop;
      end loop;

      Dev.HPPIR := Unsigned_32 (Id);
      Dev.HPPIR_Prio := Prio;
   end Compute_Highest_Priority_Pending_Interrupt;

   function Compute_Highest_Priority_Active_Interrupt
     (Dev : GIC_Dev) return Unsigned_32
   is
      Active : Unsigned_32;
      Cur_Mask : Unsigned_32;
      Cur : Natural;
      Prio, Cur_Prio : Unsigned_32;
   begin
      Prio := 16#ff#;

      for I in 0 .. (Nbr_Int / 32) - 1 loop
         Active := Dev.ACTIVE (I);
         if Active /= 0 then
            for J in 0 .. 31 loop
               Cur := I * 32 + J;
               Cur_Mask := Shift_Left (1, J);
               if (Active and Cur_Mask) /= 0 then
                  Cur_Prio := Get_Priority (Dev, Cur);
                  if Cur_Prio < Prio then
                     Prio := Cur_Prio;
                  end if;
                  Active := Active and not Cur_Mask;
                  exit when Active = 0;
               end if;
            end loop;
         end if;
      end loop;

      return Prio;
   end Compute_Highest_Priority_Active_Interrupt;

   function Get_Gic_Priority_Mask (Dev : GIC_Dev) return Unsigned_32 is
   begin
      return Shift_Left (16#ff#, Natural (Dev.BPR)) and 16#ff#;
   end Get_Gic_Priority_Mask;

   procedure Generate_Exceptions (Dev : in out GIC_Dev)
   is
      Irq : Boolean;
   begin
      Compute_Highest_Priority_Pending_Interrupt (Dev);

      if Dev.ACTIVE /= (Dev.ACTIVE'Range => 0) then
         Irq := True;
      elsif Dev.HPPIR_Prio < Dev.PMR
        and then (Dev.DCTLR and 1) = 1
        and then (Dev.CCTLR and 1) = 1
        and then Dev.HPPIR_Prio < (Dev.CRPR and Get_Gic_Priority_Mask (Dev))
      then
         Irq := True;
      else
         Irq := False;
      end if;

      if Irq /= Dev.Irq then
         Dev.Irq := Irq;
         Dev.Cpu.Set_Level (0, Irq);
         if Flag_Debug then
            if Irq then
               Put ("IRQ^");
            else
               Put ("IRQv");
            end if;
         end if;
      end if;
   end Generate_Exceptions;

   procedure Set_Level
     (Dev : in out GIC_Interrupt_Dev; Id : Natural; Level : Boolean)
   is
      D : GIC_Dev renames Dev.Parent.all;
   begin
      pragma Assert (Id in D.Lines'Range);

      if D.Lines (Id) = Level then
         return;
      end if;

      if False then
         Put ("IT");
         if Level then
            Put ('S');
         else
            Put ('C');
         end if;
         Put_Dec (Id);
         New_Line;
      end if;

      D.Lines (Id) := Level;

      if (Shift_Right (D.ICFGR (Id / 16), 2 * (Id mod 16) + 1) and 2) = 2 then
         --  Edge triggered.
         if not Level then
            return;
         end if;
         D.PEND (Id / 32) := D.PEND (Id / 32) or Shift_Left (1, Id mod 32);
      else
         --  Level triggered
         if Level then
            D.PEND (Id / 32) :=
              D.PEND (Id / 32) or Shift_Left (1, Id mod 32);
         else
            D.PEND (Id / 32) :=
              D.PEND (Id / 32) and not Shift_Left (1, Id mod 32);
         end if;
      end if;

      if (D.DCTLR and 1) = 0 then
         --  Distributor disabled.
         return;
      end if;

      if (D.ENABLE (Id / 32) and Shift_Left (1, Id mod 32)) = 0 then
         --  Interrupt not enabled
         return;
      end if;

      if (D.ACTIVE (Id / 32) and Shift_Left (1, Id mod 32)) = 1 then
         --  Interrupt already active
         return;
      end if;

      if False then
         Put ('{');
         if Level then
            Put ('r');
         else
            Put ('f');
         end if;
         Put_Hex4 (Unsigned_32 (Id));
         Put ('}');
      end if;

      Generate_Exceptions (D);
   end Set_Level;

   procedure Dump (Dev : GIC_Dev) is
   begin
      Put ("GIC: DCTLR: ");
      Put_Hex4 (Dev.DCTLR);
      Put (" CCTLR: ");
      Put_Hex4 (Dev.CCTLR);
      Put (" PMR: ");
      Put_Hex4 (Dev.PMR);
      Put (" RPR: ");
      Put_Hex4 (Dev.CRPR);
      New_Line;

      Put ("   PEND:");
      for I in Dev.PEND'Range loop
         Put (' ');
         Put_Hex4 (Dev.PEND (I));
      end loop;
      New_Line;

      Put (" ENABLE:");
      for I in Dev.ENABLE'Range loop
         Put (' ');
         Put_Hex4 (Dev.ENABLE (I));
      end loop;
      New_Line;

      Put (" ACTIVE:");
      for I in Dev.ACTIVE'Range loop
         Put (' ');
         Put_Hex4 (Dev.ACTIVE (I));
      end loop;
      New_Line;

      Put ("  HPPIR: ");
      Put_Dec (Natural (Dev.HPPIR));
      Put (", PRIO: ");
      Put_Hex4 (Dev.HPPIR_Prio);
      New_Line;

      Put (" Lines: ");
      for I in Dev.Lines'Range loop
         if Dev.Lines (I) then
            Put ('1');
         else
            Put ('0');
         end if;
      end loop;
      New_Line;
   end Dump;

end Emu_GIC;
