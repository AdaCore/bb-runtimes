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
with System.Storage_Elements;

package body Emu_GIC is
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
   Reg_GICD_ITARGETSRn  : constant := 16#800#;
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
            Put ("GICD_CTLR");
         when Reg_GICD_TYPER =>
            Put ("GICD_TYPER");
         when Reg_GICD_IIDR =>
            Put ("GICD_IIDR");
         when Reg_GICD_IGROUPRn =>
            Put ("GICD_IGROUPRn");
         when Reg_GICD_ISENABLERn =>
            Put ("GICD_ISENABLERn");
         when Reg_GICD_ICENABLERn =>
            Put ("GICD_ICENABLERn");
         when Reg_GICD_ISPENDRn =>
            Put ("GICD_ISPENDRn");
         when Reg_GICD_ICPENDRn =>
            Put ("GICD_ICPENDRn");
         when Reg_GICD_ISACTIVERn =>
            Put ("GICD_ISACTIVERn");
         when Reg_GICD_ICACTIVERn =>
            Put ("GICD_ICACTIVERn");
         when Reg_GICD_IPRIORITYRn =>
            Put ("GICD_IPRIORITYRn");
         when Reg_GICD_ITARGETSRn =>
            Put ("GICD_ITARGETSRn");
         when Reg_GICD_ICFGRn =>
            Put ("GICD_ICFGRn");
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
            Put ("???");
      end case;
   end Disp_Reg_Name;

   function Read32 (Dev : in out GIC_Dev; Off : Off_T)
                    return Unsigned_32
   is
      use System.Storage_Elements;
   begin
      case Off is
         when Reg_GICD_CTLR =>
            return Dev.ICTLR;
         when Reg_GICD_TYPER =>
            --  64 interrupts, 1 cpu, no security extension.
            return 2#00000_0_00_000_00001#;
         when Reg_GICD_ITARGETSR0
            | Reg_GICD_ITARGETSR1
            | Reg_GICD_ITARGETSR2
            | Reg_GICD_ITARGETSR3
            | Reg_GICD_ITARGETSR4
            | Reg_GICD_ITARGETSR5
            | Reg_GICD_ITARGETSR6
            | Reg_GICD_ITARGETSR7 =>
            return 16#01_01_01_01#;
         when Reg_GICD_ITARGETSR8
            | Reg_GICD_ITARGETSR9
            | Reg_GICD_ITARGETSR10
            | Reg_GICD_ITARGETSR11
            | Reg_GICD_ITARGETSR12
            | Reg_GICD_ITARGETSR13
            | Reg_GICD_ITARGETSR14
            | Reg_GICD_ITARGETSR15 =>
            return Dev.ITARGETS (Natural (Off - Reg_GICD_ITARGETSRn) / 4);
         when Reg_GICD_ICFGR0 =>
            --  SGIR: level-sensitive
            return 16#00000000#;
         when Reg_GICD_ICFGR1 =>
            --  Interrupt 14 (timer ns el1) is level-hi.
            return 16#00000000#;
         when Reg_GICD_ICFGR2
            | Reg_GICD_ICFGR3 =>
            return Dev.ICFGR (Natural (Off - Reg_GICD_ICFGRn) / 4);
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
            return Dev.PRIORITY (Natural (Off - Reg_GICD_IPRIORITYRn) / 4);
         when Reg_GICD_ISACTIVER0
            | Reg_GICD_ISACTIVER1 =>
            return Dev.ACTIVE (Natural (Off - Reg_GICD_ISACTIVERn) / 4);
         when Reg_GICD_ICACTIVER0
            | Reg_GICD_ICACTIVER1 =>
            return Dev.ACTIVE (Natural (Off - Reg_GICD_ICACTIVERn) / 4);
         when Reg_GICD_ISENABLER0
            | Reg_GICD_ISENABLER1 =>
            return Dev.ENABLE (Natural (Off - Reg_GICD_ISENABLERn) / 4);
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            return Dev.ENABLE (Natural (Off - Reg_GICD_ICENABLERn) / 4);

         when Reg_GICC_CTLR =>
            return Dev.CCTLR;
         when Reg_GICC_PMR =>
            return Dev.PMR;

         when others =>
            Put ("gic.read ");
            Disp_Reg_Name (Off);
            New_Line;
            Hulls.Dump_Cpu (Dev.Ctxt.all);
            raise Program_Error;
            return 0;
      end case;
   end Read32;

   procedure Write32_Mask
     (Dev : in out GIC_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32)
   is
      use System.Storage_Elements;
      Idx : Natural;
   begin
      case Off is
         when Reg_GICD_CTLR =>
            Update (Dev.ICTLR, Val and 16#01#, Mask);
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
            Idx := Natural (Off - Reg_GICD_ITARGETSRn) / 4;
            Update (Dev.ITARGETS (Idx), Val and 16#01_01_01_01#, Mask);
            --  FIXME: Update output
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
            --  Update.
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
            --  Update
         when Reg_GICD_ICENABLER0
            | Reg_GICD_ICENABLER1 =>
            Idx := Natural (Off - Reg_GICD_ICENABLERn) / 4;
            Set_Disable (Dev.ENABLE (Idx), Val);
            --  Update.

         when Reg_GICC_CTLR =>
            Update (Dev.CCTLR, Val and 1, Mask);
            --  Update.
         when Reg_GICC_PMR =>
            Update (Dev.PMR, Val and 16#ff#, Mask);
            --  Update.
         when others =>
            Put ("gic.write ");
            Disp_Reg_Name (Off);
            New_Line;
            Hulls.Dump_Cpu (Dev.Ctxt.all);
            raise Program_Error;
      end case;
   end Write32_Mask;

   procedure Init (Dev : access GIC_Dev; Ctxt : Hulls.Hull_Context_Acc) is
   begin
      Dev.all := (Ctxt => Ctxt,
                  ICTLR => 0,
                  ITARGETS => (others => 0),
                  ICFGR => (others => 0),
                  PRIORITY => (others => 0),
                  ACTIVE => (others => 0),
                  ENABLE => (others => 0),

                  CCTLR => 0,
                  PMR => 0);
   end Init;

end Emu_GIC;
