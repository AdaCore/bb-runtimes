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

   procedure Disp_Reg_Name (Off : Off_T) is
   begin
      case Off is
         when Reg_GICD_CTLR =>
            Log ("GICD_CTLR");
         when Reg_GICD_TYPER =>
            Log ("GICD_TYPER");
         when Reg_GICD_IIDR =>
            Log ("GICD_IIDR");
         when Reg_GICD_IGROUPRn =>
            Log ("GICD_IGROUPRn");
         when Reg_GICD_ISENABLER0 .. Reg_GICD_ISENABLER1 =>
            Log ("GICD_ISENABLER");
            Log_Dec (Natural (Off - Reg_GICD_ISENABLER0) / 4);
         when Reg_GICD_ICENABLER0 .. Reg_GICD_ICENABLER1 =>
            Log ("GICD_ICENABLER");
            Log_Dec (Natural (Off - Reg_GICD_ICENABLER0) / 4);
         when Reg_GICD_ISPENDRn =>
            Log ("GICD_ISPENDRn");
         when Reg_GICD_ICPENDRn =>
            Log ("GICD_ICPENDRn");
         when Reg_GICD_ISACTIVER0 .. Reg_GICD_ISACTIVER1 =>
            Log ("GICD_ISACTIVER");
            Log_Dec (Natural (Off - Reg_GICD_ISACTIVER0) / 4);
         when Reg_GICD_ICACTIVER0 .. Reg_GICD_ICACTIVER1 =>
            Log ("GICD_ICACTIVER");
            Log_Dec (Natural (Off - Reg_GICD_ICACTIVER0) / 4);
         when Reg_GICD_IPRIORITYR0 .. Reg_GICD_IPRIORITYR15 =>
            Log ("GICD_IPRIORITYR");
            Log_Dec (Natural (Off - Reg_GICD_IPRIORITYR0) / 4);
         when Reg_GICD_ITARGETSR0 .. Reg_GICD_ITARGETSR15 =>
            Log ("GICD_ITARGETSR");
            Log_Dec (Natural (Off - Reg_GICD_ITARGETSR0) / 4);
         when Reg_GICD_ICFGR0 .. Reg_GICD_ICFGR3 =>
            Log ("GICD_ICFGR");
            Log_Dec (Natural (Off - Reg_GICD_ICFGR0) / 4);
         when Reg_GICD_NSACRn =>
            Log ("GICD_NSACRn");
         when Reg_GICD_SGIR =>
            Log ("GICD_SGIR");
         when Reg_GICD_CPENDSGIRn =>
            Log ("GICD_CPENDSGIRn");
         when Reg_GICD_SPENDSGIRn =>
            Log ("GICD_SPENDSGIRn");

         when Reg_GICC_CTLR =>
            Log ("GICC_CTLR");
         when Reg_GICC_PMR =>
            Log ("GICC_PMR");
         when Reg_GICC_BPR =>
            Log ("GICC_BPR");
         when Reg_GICC_IAR =>
            Log ("GICC_IAR");
         when Reg_GICC_EOIR =>
            Log ("GICC_EOIR");
         when Reg_GICC_RPR =>
            Log ("GICC_RPR");
         when Reg_GICC_HPPIR =>
            Log ("GICC_HPPIR");
         when Reg_GICC_ABPR =>
            Log ("GICC_ABPR");
         when Reg_GICC_AIAR =>
            Log ("GICC_AIAR");
         when Reg_GICC_AEOIR =>
            Log ("GICC_AEOIR");
         when Reg_GICC_AHPPIR =>
            Log ("GICC_AHPPIR");
         when Reg_GICC_APRn =>
            Log ("GICC_APRn");
         when Reg_GICC_NSAPRn =>
            Log ("GICC_NSAPRn");
         when Reg_GICC_IIDR =>
            Log ("GICC_IIDR");
         when Reg_GICC_DIR =>
            Log ("GICC_DIR");

         when others =>
            Log_Hex4 (Unsigned_32 (Off));
      end case;
   end Disp_Reg_Name;

   protected body Prot_Gic is
      --  Get configuration (level or edge triggered) of interrupt ID.
      function Get_Icfg (Id : Natural) return Unsigned_32 is
      begin
         return Shift_Right (S.ICFGR (Id / 16), 2 * (Id mod 16)) and 3;
      end Get_Icfg;

      function Get_Priority (Id : Natural) return Unsigned_32 is
      begin
         return Shift_Left (S.PRIORITY (Id / 4), 8 * (Id mod 4)) and 16#ff#;
      end Get_Priority;

      procedure Compute_Highest_Priority_Pending_Interrupt
      is
         Pend : Unsigned_32;
         Cur_Mask : Unsigned_32;
         Id, Cur : Natural;
         Prio, Cur_Prio : Unsigned_32;
      begin
         Id := 1023;
         Prio := 16#ff#;

         for I in 0 .. (Nbr_Int / 32) - 1 loop
            Pend := S.PEND (I) and not S.ACTIVE (I);
            for J in 0 .. 31 loop
               Cur := I * 32 + J;
               Cur_Mask := Shift_Left (1, J);
               if (Pend and Cur_Mask) /= 0 then
               --  Interrupt is pending (and not active).
                  Cur_Prio := Get_Priority (Cur);
                  if Cur_Prio < Prio then
                     Id := Cur;
                     Prio := Cur_Prio;
                  end if;
                  Pend := Pend and not Cur_Mask;
                  exit when Pend = 0;
               end if;
            end loop;
         end loop;

         S.HPPIR := Unsigned_32 (Id);
         S.HPPIR_Prio := Prio;
      end Compute_Highest_Priority_Pending_Interrupt;

      function Compute_Highest_Priority_Active_Interrupt return Unsigned_32
      is
         Active : Unsigned_32;
         Cur_Mask : Unsigned_32;
         Cur : Natural;
         Prio, Cur_Prio : Unsigned_32;
      begin
         Prio := 16#ff#;

         for I in 0 .. (Nbr_Int / 32) - 1 loop
            Active := S.ACTIVE (I);
            if Active /= 0 then
               for J in 0 .. 31 loop
                  Cur := I * 32 + J;
                  Cur_Mask := Shift_Left (1, J);
                  if (Active and Cur_Mask) /= 0 then
                     Cur_Prio := Get_Priority (Cur);
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

      function Get_Gic_Priority_Mask return Unsigned_32 is
      begin
         return Shift_Left (16#ff#, Natural (S.BPR)) and 16#ff#;
      end Get_Gic_Priority_Mask;

      procedure Generate_Exceptions
      is
         Irq : Boolean;
      begin
         Compute_Highest_Priority_Pending_Interrupt;

         if S.ACTIVE /= (S.ACTIVE'Range => 0) then
            Irq := True;
         elsif S.HPPIR_Prio < S.PMR
           and then (S.DCTLR and 1) = 1
           and then (S.CCTLR and 1) = 1
           and then S.HPPIR_Prio < (S.CRPR and Get_Gic_Priority_Mask)
         then
            Irq := True;
         else
            Irq := False;
         end if;

         if Irq /= S.Irq then
            S.Irq := Irq;
            S.Cpu.Set_Level (0, Irq);
            if Flag_Debug then
               if Irq then
                  Log ("IRQ^");
               else
                  Log ("IRQv");
               end if;
            end if;
         end if;
      end Generate_Exceptions;

      procedure Set_Level (Id : Natural; Level : Boolean) is
      begin
         pragma Assert (Id in S.Lines'Range);

         if S.Lines (Id) = Level then
            return;
         end if;

         if False then
            Log ("IT");
            if Level then
               Log ('S');
            else
               Log ('C');
            end if;
            Log_Dec (Id);
            Log_Line;
         end if;

         S.Lines (Id) := Level;

         --  ICFGR are defined only for SPI.
         if Id > 31
           and then
             (Shift_Right (S.ICFGR (Id / 16), 2 * (Id mod 16)) and 2) = 2
         then
            --  Edge triggered.
            if not Level then
               return;
            end if;
            S.PEND (Id / 32) := S.PEND (Id / 32) or Shift_Left (1, Id mod 32);
         else
            --  Level triggered
            if Level then
               S.PEND (Id / 32) :=
                 S.PEND (Id / 32) or Shift_Left (1, Id mod 32);
            else
               S.PEND (Id / 32) :=
                 S.PEND (Id / 32) and not Shift_Left (1, Id mod 32);
            end if;
         end if;

         if (S.DCTLR and 1) = 0 then
            --  Distributor disabled.
            return;
         end if;

         if (S.ENABLE (Id / 32) and Shift_Left (1, Id mod 32)) = 0 then
            --  Interrupt not enabled
            return;
         end if;

         if (S.ACTIVE (Id / 32) and Shift_Left (1, Id mod 32)) = 1 then
            --  Interrupt already active
            return;
         end if;

         if False then
            Log ('{');
            if Level then
               Log ('r');
            else
               Log ('f');
            end if;
            Log_Hex4 (Unsigned_32 (Id));
            Log ('}');
         end if;

         Generate_Exceptions;
      end Set_Level;

      procedure Read32 (Off : Off_T; Res : out Unsigned_32; Ack : out Integer)
      is
      begin
         Ack := -1;

         case Off is
         when Reg_GICD_CTLR =>
            Res := S.DCTLR;
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
            Res := S.ICFGR (Natural (Off - Reg_GICD_ICFGRn) / 4);
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
            Res := S.PRIORITY (Natural (Off - Reg_GICD_IPRIORITYRn) / 4);
         when Reg_GICD_ISACTIVER0
            | Reg_GICD_ISACTIVER1 =>
            Res := S.ACTIVE (Natural (Off - Reg_GICD_ISACTIVERn) / 4);
         when Reg_GICD_ICACTIVER0
            | Reg_GICD_ICACTIVER1 =>
            Res := S.ACTIVE (Natural (Off - Reg_GICD_ICACTIVERn) / 4);
         when Reg_GICD_ISENABLER0
            | Reg_GICD_ISENABLER1 =>
            Res := S.ENABLE (Natural (Off - Reg_GICD_ISENABLERn) / 4);
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            Res := S.ENABLE (Natural (Off - Reg_GICD_ICENABLERn) / 4);

         when Reg_GICC_CTLR =>
            Res := S.CCTLR;
         when Reg_GICC_PMR =>
            Res := S.PMR;

         when Reg_GICC_IAR =>
            Res := S.HPPIR;
            declare
               Id : constant Natural := Natural (Res and 16#3ff#);
            begin
               if Id /= 1023 then
                  --  Acknowledge interrupt.
                  --  interrupt is active.
                  Set_Enable
                    (S.ACTIVE (Id / 32), Shift_Left (1, Id mod 32));
                  --  If it is edge-triggered, disable pending bit.
                  if Id < 16 then
                     --  SGI
                     null;
                  elsif Id < 32 then
                     --  PPI
                     Ack := Id;
                  else
                     --  SPI
                     if (Get_Icfg (Id) and 2) = 2 then
                        Set_Disable
                          (S.PEND (Id / 32), Shift_Left (1, Id mod 32));
                     end if;
                  end if;
                  --  Change running priority
                  S.CRPR := S.HPPIR_Prio;
                  --  Recompute highest pending interrupt.
                  Compute_Highest_Priority_Pending_Interrupt;
               end if;
            end;

         when others =>
            Log ("gic.read ");
            Disp_Reg_Name (Off);
            Log_Line;
            raise Program_Error;
         end case;
      end Read32;

      procedure Write32_Mask
        (Off : Off_T;
         Val : Unsigned_32;
         Mask : Unsigned_32)
      is
         Idx : Natural;
      begin
         case Off is
         when Reg_GICD_CTLR =>
            Update (S.DCTLR, Val and 16#01#, Mask);
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
            Update (S.ICFGR (Idx), Val, Mask);
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
            Update (S.PRIORITY (Idx), Val, Mask);
            Generate_Exceptions;
         when Reg_GICD_ISACTIVER0
            | Reg_GICD_ISACTIVER1 =>
            Idx := Natural (Off - Reg_GICD_ISACTIVERn) / 4;
            Set_Enable (S.ACTIVE (Idx), Val);
            --  Update
         when Reg_GICD_ICACTIVER0
            | Reg_GICD_ICACTIVER1 =>
            Idx := Natural (Off - Reg_GICD_ICACTIVERn) / 4;
            Set_Disable (S.ACTIVE (Idx), Val);
            --  Update.
         when Reg_GICD_ISENABLER0
            | Reg_GICD_ISENABLER1 =>
            Idx := Natural (Off - Reg_GICD_ISENABLERn) / 4;
            Set_Enable (S.ENABLE (Idx), Val);
            Generate_Exceptions;
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            Idx := Natural (Off - Reg_GICD_ICENABLERn) / 4;
            Set_Disable (S.ENABLE (Idx), Val);
            Generate_Exceptions;

         when Reg_GICC_CTLR =>
            Update (S.CCTLR, Val and 1, Mask);
            Generate_Exceptions;
         when Reg_GICC_PMR =>
            Update (S.PMR, Val and 16#ff#, Mask);
            Generate_Exceptions;

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
               Set_Disable (S.ACTIVE (Id / 32), Shift_Left (1, Id mod 32));

               --  Set running priority to the highest active interrupt
               Nprio := Compute_Highest_Priority_Active_Interrupt;

               S.CRPR := Nprio;

               Generate_Exceptions;
            end;

         when others =>
            Log ("gic.write ");
            Disp_Reg_Name (Off);
            Log_Line;
            raise Program_Error;
         end case;
      end Write32_Mask;

      procedure Init (Cpu : Interrupt_Dev_Acc) is
      begin
         S := (Cpu => Cpu,
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

      procedure Dump is
      begin
         Log ("GIC: DCTLR: ");
         Log_Hex4 (S.DCTLR);
         Log (" CCTLR: ");
         Log_Hex4 (S.CCTLR);
         Log (" PMR: ");
         Log_Hex4 (S.PMR);
         Log (" RPR: ");
         Log_Hex4 (S.CRPR);
         Log_Line;

         Log ("   PEND:");
         for I in S.PEND'Range loop
            Log (' ');
            Log_Hex4 (S.PEND (I));
         end loop;
         Log_Line;

         Log (" ENABLE:");
         for I in S.ENABLE'Range loop
            Log (' ');
            Log_Hex4 (S.ENABLE (I));
         end loop;
         Log_Line;

         Log (" ACTIVE:");
         for I in S.ACTIVE'Range loop
            Log (' ');
            Log_Hex4 (S.ACTIVE (I));
         end loop;
         Log_Line;

         Log ("  HPPIR: ");
         Log_Dec (Natural (S.HPPIR));
         Log (", PRIO: ");
         Log_Hex4 (S.HPPIR_Prio);
         Log_Line;

         Log (" Lines: ");
         for I in S.Lines'Range loop
            if S.Lines (I) then
               Log ('1');
            else
               Log ('0');
            end if;
         end loop;
         Log_Line;
      end Dump;
   end Prot_Gic;

   procedure Init (Dev : access GIC_Dev; Cpu : Interrupt_Dev_Acc) is
   begin
      Dev.IT := (Parent => GIC_Dev_Acc (Dev));
      Dev.PPI_Acks := (others => null);
      Dev.P.Init (Cpu);
   end Init;

   function Get_Interrupt_Dev (Dev : access GIC_Dev) return Interrupt_Dev_Acc
   is
   begin
      return Dev.IT'Access;
   end Get_Interrupt_Dev;

   procedure Set_Level
     (Dev : in out GIC_Interrupt_Dev; Id : Natural; Level : Boolean)
   is
      D : GIC_Dev renames Dev.Parent.all;
   begin
      D.P.Set_Level (Id, Level);
   end Set_Level;

   procedure Set_Ack_Cb
     (Dev : in out GIC_Interrupt_Dev; Id : Natural; Cb : Interrupt_Ack_Cb_Acc)
   is
      D : GIC_Dev renames Dev.Parent.all;
   begin
      if D.PPI_Acks (Id) /= null then
         --  Must not be overriden.
         raise Program_Error;
      end if;

      D.PPI_Acks (Id) := Cb;
   end Set_Ack_Cb;

   procedure Dump (Dev : in out GIC_Dev) is
   begin
      Dev.P.Dump;
   end Dump;

   function Read32 (Dev : in out GIC_Dev; Off : Off_T)
                    return Unsigned_32
   is
      Res : Unsigned_32;
      Ack : Integer;
   begin
      Dev.P.Read32 (Off, Res, Ack);

      if Flag_Debug then
         Log ("RGIC: ");
         Disp_Reg_Name (Off);
         Log (" -> ");
         Log_Hex4 (Res);
         Log_Line;
      end if;

      if Ack >= 0 then
         --  Call the ack callback from the outside of the PO, as the callback
         --  may reenter the PO.
         Dev.PPI_Acks (Ack).Ack;
      end if;

      return Res;
   end Read32;

   procedure Write32_Mask
     (Dev : in out GIC_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32) is
   begin
      if Flag_Debug then
         Log ("WGIC: ");
         Disp_Reg_Name (Off);
         Log (" <- ");
         Log_Hex4 (Val);
         Log_Line;
      end if;

      Dev.P.Write32_Mask (Off, Val, Mask);
   end Write32_Mask;
end Emu_GIC;
