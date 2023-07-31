------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . A R M _ G I C                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2022, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;           use Interfaces;
with System.BB.Parameters;
with System.Machine_Code;  use System.Machine_Code;

package body System.ARM_GIC is

   use System.BB.Interrupts;

   subtype PRI is Unsigned_8;
   --  Type for GIC interrupt priorities. Note that 0 is the highest
   --  priority, which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest. We assume the
   --  PRIGROUP setting is such that the 4 most significant bits determine
   --  the priority group used for preemption. However, if less bits are
   --  implemented, this should still work.

   function To_PRI (P : Integer) return PRI with Inline;
   --  Return the PRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.

   function To_Priority (P : PRI) return Interrupt_Priority with Inline;
   --  Given an ARM interrupt priority (PRI value), determine the Ada
   --  priority. While the value 0 is reserved for the kernel and has no Ada
   --  priority that represents it, Interrupt_Priority'Last is closest.

   subtype Banked_Interrupt is Interrupt_ID range 0 .. 31;
   Banked_Interrupts_Enabled  : array (Banked_Interrupt) of Boolean :=
                                  (others => False);
   Banked_Interrupts_Priority : array (Banked_Interrupt) of PRI;

   type Bit         is mod 2**1  with Size => 1;
   type Unsigned_2  is mod 2**2  with Size => 2;
   type Unsigned_3  is mod 2**3  with Size => 3;
   type Unsigned_4  is mod 2**4  with Size => 4;
   type Unsigned_5  is mod 2**5  with Size => 5;
   type Unsigned_6  is mod 2**6  with Size => 6;
   type Unsigned_7  is mod 2**7  with Size => 7;
   type Unsigned_10 is mod 2**10 with Size => 10;
   type Unsigned_20 is mod 2**20 with Size => 20;
   type Unsigned_23 is mod 2**23 with Size => 23;
   type Unsigned_24 is mod 2**24 with Size => 24;
   type Unsigned_28 is mod 2**28 with Size => 28;
   type Unsigned_29 is mod 2**29 with Size => 29;

   ----------------------------------------------
   -- GIC-600 Distributor Register Definitions --
   ----------------------------------------------

   --  See:
   --    * Table 4-2 of the Arm CoreLink GIC-600 Generic Interrupt Controller
   --      Technical Reference Manual
   --    * Table 12-25 of the Arm Generic Interrupt Controller Architecture
   --      Specification GIC architecture version 3 and version 4

   type PRI_Array_4 is array (Natural range 0 .. 3) of PRI with
     Pack, Size => 32, Volatile_Full_Access;

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;

   type GICD_CTLR_Register is record
      EnableGrp0   : Bit;
      EnableGrp1NS : Bit;
      EnableGrp1S  : Bit;
      Reserved_1   : Bit;
      ARE_S        : Bit;
      ARE_NS       : Bit;
      DS           : Bit;
      E1NWF        : Bit;
      Reserved_2   : Unsigned_23;
      RWP          : Bit;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICD_CTLR_Register use record
      EnableGrp0   at 0 range  0 ..  0;
      EnableGrp1NS at 0 range  1 ..  1;
      EnableGrp1S  at 0 range  2 ..  2;
      Reserved_1   at 0 range  3 ..  3;
      ARE_S        at 0 range  4 ..  4;
      ARE_NS       at 0 range  5 ..  5;
      DS           at 0 range  6 ..  6;
      E1NWF        at 0 range  7 ..  7;
      Reserved_2   at 0 range  8 .. 30;
      RWP          at 0 range 31 .. 31;
   end record;

   type GICD_TYPER_Register is record
      ITLineNumber : Unsigned_5;
      Reserved_1   : Unsigned_5;
      SecurityExtn : Bit;
      Reserved_2   : Unsigned_5;
      MBIS         : Bit;
      LPIS         : Bit;
      DVIS         : Bit;
      IDbits       : Unsigned_5;
      A3V          : Bit;
      No1N         : Bit;
      Reserved_3   : Unsigned_6;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICD_TYPER_Register use record
      ITLineNumber at 0 range 0 .. 4;
      Reserved_1   at 0 range 5 .. 9;
      SecurityExtn at 0 range 10 .. 10;
      Reserved_2   at 0 range 11 .. 15;
      MBIS         at 0 range 16 .. 16;
      LPIS         at 0 range 17 .. 17;
      DVIS         at 0 range 18 .. 18;
      IDbits       at 0 range 19 .. 23;
      A3V          at 0 range 24 .. 24;
      No1N         at 0 range 25 .. 25;
      Reserved_3   at 0 range 26 .. 31;
   end record;

   subtype GICD_IIDR_Register is Unsigned_32;

   type GICD_FCTLR_Register is record
      SIP        : Bit;
      Reserved_1 : Unsigned_3;
      CGO        : Unsigned_10;
      Reserved_2 : Unsigned_2;
      NSACR      : Unsigned_2;
      Reserved_3 : Unsigned_3;
      DCC        : Bit;
      Reserved_4 : Unsigned_3;
      QDENY      : Bit;
      POS        : Bit;
      Reserved_5 : Unsigned_5;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICD_FCTLR_Register use record
      SIP        at 0 range  0 ..  0;
      Reserved_1 at 0 range  1 ..  3;
      CGO        at 0 range  4 .. 13;
      Reserved_2 at 0 range 14 .. 15;
      NSACR      at 0 range 16 .. 17;
      Reserved_3 at 0 range 18 .. 20;
      DCC        at 0 range 21 .. 21;
      Reserved_4 at 0 range 22 .. 24;
      QDENY      at 0 range 25 .. 25;
      POS        at 0 range 26 .. 26;
      Reserved_5 at 0 range 27 .. 31;
   end record;

   type GICD_SAC_Register is record
      DSL      : Bit;
      GICTNS   : Bit;
      GICPNS   : Bit;
      Reserved : Unsigned_29;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICD_SAC_Register use record
      DSL      at 0 range 0 ..  0;
      GICTNS   at 0 range 1 ..  1;
      GICPNS   at 0 range 2 ..  2;
      Reserved at 0 range 3 .. 31;
   end record;

   type GICD_IROUTER_Register is record
      Aff0                   : Unsigned_8;
      Aff1                   : Unsigned_8;
      Aff2                   : Unsigned_8;
      Reserved_1             : Unsigned_7;
      Interrupt_Routing_Mode : Bit;
      Aff3                   : Unsigned_8;
      Reserved_2             : Unsigned_24;
   end record with
     Size => 64,
     Volatile_Full_Access;

   for GICD_IROUTER_Register use record
      Aff0                   at 0 range  0 ..  7;
      Aff1                   at 0 range  8 .. 15;
      Aff2                   at 0 range 16 .. 23;
      Reserved_1             at 0 range 24 .. 30;
      Interrupt_Routing_Mode at 0 range 31 .. 31;
      Aff3                   at 0 range 32 .. 39;
      Reserved_2             at 0 range 40 .. 63;
   end record;

   type GICD_IGROUPR_Registers    is new Unsigned_32_Array (0 .. 31);
   type GICD_ISENABLER_Registers  is new Unsigned_32_Array (0 .. 31);
   type GICD_ICENABLER_Registers  is new Unsigned_32_Array (0 .. 31);
   type GICD_ISPENDR_Registers    is new Unsigned_32_Array (0 .. 31);
   type GICD_ICPENDR_Registers    is new Unsigned_32_Array (0 .. 31);
   type GICD_ISACTIVER_Registers  is new Unsigned_32_Array (0 .. 31);
   type GICD_ICACTIVER_Registers  is new Unsigned_32_Array (0 .. 31);
   type GICD_IPRIORITYR_Registers is array (0 .. 255) of PRI_Array_4;
   type GICD_ICFGR_Registers      is new Unsigned_32_Array (0 .. 63);
   type GICD_IGRPMODR_Registers   is new Unsigned_32_Array (0 .. 31);
   type GICD_NSACR_Registers      is new Unsigned_32_Array (0 .. 63);

   type GICD_IROUTER_Registers is array (32 .. 1019) of GICD_IROUTER_Register;

   type GICD_Peripheral is record
      CTLR        : GICD_CTLR_Register;
      TYPER       : GICD_TYPER_Register;
      IIDR        : GICD_IIDR_Register;
      Reserved_1  : Unsigned_32_Array (0 .. 4);
      FCTLR       : GICD_FCTLR_Register;
      SAC         : GICD_SAC_Register;
      Reserved_2  : Unsigned_32_Array (0 .. 5);
      SETSPI_NSR  : Unsigned_32;
      Reserved_3  : Unsigned_32;
      CLRSPI_NSR  : Unsigned_32;
      Reserved_4  : Unsigned_32;
      SETSPI_SR   : Unsigned_32;
      Reserved_5  : Unsigned_32;
      CLRSPI_SR   : Unsigned_32;
      Reserved_6  : Unsigned_32_Array (0 .. 8);
      IGROUPR     : GICD_IGROUPR_Registers;
      ISENABLER   : GICD_ISENABLER_Registers;
      ICENABLER   : GICD_ICENABLER_Registers;
      ISPENDR     : GICD_ISPENDR_Registers;
      ICPENDR     : GICD_ICPENDR_Registers;
      ISACTIVER   : GICD_ISACTIVER_Registers;
      ICACTIVER   : GICD_ICACTIVER_Registers;
      IPRIORITYR  : GICD_IPRIORITYR_Registers;
      Reserved_7  : Unsigned_32_Array (0 .. 255);
      ICFGR       : GICD_ICFGR_Registers;
      IGRPMODR    : GICD_IGRPMODR_Registers;
      Reserved_8  : Unsigned_32_Array (0 .. 31);
      NSACR       : GICD_NSACR_Registers;
      Reserved_9  : Unsigned_32_Array (0 .. 5247);
      IROUTER     : GICD_IROUTER_Registers;
      Reserved_10 : Unsigned_32_Array (0 .. 4103);
      CHIPSR      : Unsigned_32;
      DCHIPR      : Unsigned_32;
      CHIPR       : Unsigned_32_Array (0 .. 31);
      Reserved_11 : Unsigned_32_Array (0 .. 2015);
      ICLAR       : Unsigned_32_Array (2 .. 63);
      Reserved_12 : Unsigned_32_Array (0 .. 1);
      IERR        : Unsigned_32_Array (1 .. 30);
      Reserved_13 : Unsigned_32_Array (0 .. 927);
      CFGID       : Unsigned_64;
      Reserved_14 : Unsigned_32_Array (0 .. 1009);
      PIDR4       : Unsigned_32;
      PIDR5       : Unsigned_32;
      PIDR6       : Unsigned_32;
      PIDR7       : Unsigned_32;
      PIDR0       : Unsigned_32;
      PIDR1       : Unsigned_32;
      PIDR2       : Unsigned_32;
      PIDR3       : Unsigned_32;
      CIDR0       : Unsigned_32;
      CIDR1       : Unsigned_32;
      CIDR2       : Unsigned_32;
      CIDR3       : Unsigned_32;
   end record with
     Volatile,
     Size => 64 * 1024 * 8;

   for GICD_Peripheral use record
      CTLR        at 16#0000# range 0 .. 31;
      TYPER       at 16#0004# range 0 .. 31;
      IIDR        at 16#0008# range 0 .. 31;
      Reserved_1  at 16#000C# range 0 .. 159;
      FCTLR       at 16#0020# range 0 .. 31;
      SAC         at 16#0024# range 0 .. 31;
      Reserved_2  at 16#0028# range 0 .. 191;
      SETSPI_NSR  at 16#0040# range 0 .. 31;
      Reserved_3  at 16#0044# range 0 .. 31;
      CLRSPI_NSR  at 16#0048# range 0 .. 31;
      Reserved_4  at 16#004C# range 0 .. 31;
      SETSPI_SR   at 16#0050# range 0 .. 31;
      Reserved_5  at 16#0054# range 0 .. 31;
      CLRSPI_SR   at 16#0058# range 0 .. 31;
      Reserved_6  at 16#005C# range 0 .. 287;
      IGROUPR     at 16#0080# range 0 .. 1023;
      ISENABLER   at 16#0100# range 0 .. 1023;
      ICENABLER   at 16#0180# range 0 .. 1023;
      ISPENDR     at 16#0200# range 0 .. 1023;
      ICPENDR     at 16#0280# range 0 .. 1023;
      ISACTIVER   at 16#0300# range 0 .. 1023;
      ICACTIVER   at 16#0380# range 0 .. 1023;
      IPRIORITYR  at 16#0400# range 0 .. 8191;
      Reserved_7  at 16#0800# range 0 .. 8191;
      ICFGR       at 16#0C00# range 0 .. 2047;
      IGRPMODR    at 16#0D00# range 0 .. 1023;
      Reserved_8  at 16#0D80# range 0 .. 1023;
      NSACR       at 16#0E00# range 0 .. 2047;
      Reserved_9  at 16#0F00# range 0 .. 167_935;
      IROUTER     at 16#6100# range 0 .. 63_231;
      Reserved_10 at 16#7FE0# range 0 .. 131_327;
      CHIPSR      at 16#C000# range 0 .. 31;
      DCHIPR      at 16#C004# range 0 .. 31;
      CHIPR       at 16#C008# range 0 .. 1023;
      Reserved_11 at 16#C088# range 0 .. 64_511;
      ICLAR       at 16#E008# range 0 .. 1983;
      Reserved_12 at 16#E100# range 0 .. 63;
      IERR        at 16#E108# range 0 .. 959;
      Reserved_13 at 16#E180# range 0 .. 29_695;
      CFGID       at 16#F000# range 0 .. 63;
      Reserved_14 at 16#F008# range 0 .. 32_319;
      PIDR4       at 16#FFD0# range 0 .. 31;
      PIDR5       at 16#FFD4# range 0 .. 31;
      PIDR6       at 16#FFD8# range 0 .. 31;
      PIDR7       at 16#FFDC# range 0 .. 31;
      PIDR0       at 16#FFE0# range 0 .. 31;
      PIDR1       at 16#FFE4# range 0 .. 31;
      PIDR2       at 16#FFE8# range 0 .. 31;
      PIDR3       at 16#FFEC# range 0 .. 31;
      CIDR0       at 16#FFF0# range 0 .. 31;
      CIDR1       at 16#FFF4# range 0 .. 31;
      CIDR2       at 16#FFF8# range 0 .. 31;
      CIDR3       at 16#FFFC# range 0 .. 31;
   end record;

   GICD : GICD_Peripheral with
     Import,
     Address => System'To_Address (BB.Parameters.GIC_Base_Address);

   ---------------------------------------------------------------------
   -- GIC-600 Redistributor Register Definitions for Control and LPIs --
   ---------------------------------------------------------------------

   --  See:
   --    * Table 4-21 of the Arm CoreLink GIC-600 Generic Interrupt Controller
   --      Technical Reference Manual
   --    * Table 12-27 of the Arm Generic Interrupt Controller Architecture
   --      Specification GIC architecture version 3 and version 4

   type GICR_CTLR_Register is record
      EnableLPIs : Bit;
      CES        : Bit;
      IR         : Bit;
      RWP        : Bit;
      Reserved_1 : Unsigned_20;
      DPG0       : Bit;
      DPG1NS     : Bit;
      DPG1S      : Bit;
      Reserved_2 : Unsigned_4;
      UWP        : Bit;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICR_CTLR_Register use record
      EnableLPIs at 0 range  0 ..  0;
      CES        at 0 range  1 ..  1;
      IR         at 0 range  2 ..  2;
      RWP        at 0 range  3 ..  3;
      Reserved_1 at 0 range  4 .. 23;
      DPG0       at 0 range 24 .. 24;
      DPG1NS     at 0 range 25 .. 25;
      DPG1S      at 0 range 26 .. 26;
      Reserved_2 at 0 range 27 .. 30;
      UWP        at 0 range 31 .. 31;
   end record;

   type GICR_TYPER_Register is record
      PLPIS           : Bit;
      VLPIS           : Bit;
      Reserved_1      : Bit;
      DirectLPI       : Bit;
      Last            : Bit;
      DPGS            : Bit;
      Reserved_2      : Unsigned_2;
      ProcessorNumber : Unsigned_16;
      CommonLPIAff    : Unsigned_2;
      Reserved_3      : Unsigned_6;
      AffinityValue   : Unsigned_32;
   end record with
     Size => 64, Volatile_Full_Access;

   for GICR_TYPER_Register use record
      PLPIS           at 0 range  0 ..  0;
      VLPIS           at 0 range  1 ..  1;
      Reserved_1      at 0 range  2 ..  2;
      DirectLPI       at 0 range  3 ..  3;
      Last            at 0 range  4 ..  4;
      DPGS            at 0 range  5 ..  5;
      Reserved_2      at 0 range  6 ..  7;
      ProcessorNumber at 0 range  8 .. 23;
      CommonLPIAff    at 0 range 24 .. 25;
      Reserved_3      at 0 range 26 .. 31;
      AffinityValue   at 0 range 32 .. 63;
   end record;

   type GICR_WAKER_Register is record
      Sleep          : Bit;
      ProcessorSleep : Bit;
      ChildrenAsleep : Bit;
      Reserved       : Unsigned_28;
      Quiescent      : Bit;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICR_WAKER_Register use record
      Sleep          at 0 range  0 ..  0;
      ProcessorSleep at 0 range  1 ..  1;
      ChildrenAsleep at 0 range  2 ..  2;
      Reserved       at 0 range  3 .. 30;
      Quiescent      at 0 range 31 .. 31;
   end record;

   type GICR_FCTLR_Register is record
      SIP        : Bit;
      Reserved_1 : Unsigned_3;
      CGO        : Unsigned_3;
      Reserved_2 : Unsigned_24;
      QD         : Bit;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICR_FCTLR_Register use record
      SIP        at 0 range  0 ..  0;
      Reserved_1 at 0 range  1 ..  3;
      CGO        at 0 range  4 ..  6;
      Reserved_2 at 0 range  7 .. 30;
      QD         at 0 range 31 .. 31;
   end record;

   type GICR_PWRR_Register is record
      RDPD       : Bit;
      RDAG       : Bit;
      RDGPD      : Bit;
      RDGPO      : Bit;
      Reserved_1 : Unsigned_4;
      RDGO       : Unsigned_8;
      RDG        : Unsigned_8;
      Reserved_2 : Unsigned_8;
   end record with
     Size => 32,
     Volatile_Full_Access;

   for GICR_PWRR_Register use record
      RDPD       at 0 range  0 ..  0;
      RDAG       at 0 range  1 ..  1;
      RDGPD      at 0 range  2 ..  2;
      RDGPO      at 0 range  3 ..  3;
      Reserved_1 at 0 range  4 ..  7;
      RDGO       at 0 range  8 .. 15;
      RDG        at 0 range 16 .. 23;
      Reserved_2 at 0 range 24 .. 31;
   end record;

   type GICR_RD_Page is record
      CTLR       : GICR_CTLR_Register;
      IIDR       : Unsigned_32;
      TYPER      : GICR_TYPER_Register;
      Reserved_1 : Unsigned_32;
      WAKER      : GICR_WAKER_Register;
      MPAMIDR    : Unsigned_32;
      PARTIDR    : Unsigned_32;
      FCTLR      : GICR_FCTLR_Register;
      PWRR       : GICR_PWRR_Register;
      CLASS      : Unsigned_32;
      Reserved_3 : Unsigned_32_Array (0 .. 4);
      SETLPIR    : Unsigned_64;
      CLRLPIR    : Unsigned_64;
      Reserved_4 : Unsigned_32_Array (0 .. 7);
      PROPBASER  : Unsigned_64;
      PENDBASER  : Unsigned_64;
      Reserved_5 : Unsigned_32_Array (0 .. 7);
      INVLPIR    : Unsigned_64;
      Reserved_6 : Unsigned_32_Array (0 .. 1);
      INVALLR    : Unsigned_64;
      Reserved_7 : Unsigned_32_Array (0 .. 1);
      SYNCR      : Unsigned_32;
      Reserved_8 : Unsigned_32_Array (0 .. 16322);
      PIDR4      : Unsigned_32;
      PIDR5      : Unsigned_32;
      PIDR6      : Unsigned_32;
      PIDR7      : Unsigned_32;
      PIDR0      : Unsigned_32;
      PIDR1      : Unsigned_32;
      PIDR2      : Unsigned_32;
      PIDR3      : Unsigned_32;
      CIDR0      : Unsigned_32;
      CIDR1      : Unsigned_32;
      CIDR2      : Unsigned_32;
      CIDR3      : Unsigned_32;
   end record with Size => 64 * 1024 * 8;
   --  Redistributor registers for control and physical LPIs

   for GICR_RD_Page use record
      CTLR       at 16#0000# range 0 .. 31;
      IIDR       at 16#0004# range 0 .. 31;
      TYPER      at 16#0008# range 0 .. 63;
      Reserved_1 at 16#0010# range 0 .. 31;
      WAKER      at 16#0014# range 0 .. 31;
      MPAMIDR    at 16#0018# range 0 .. 31;
      PARTIDR    at 16#001C# range 0 .. 31;
      FCTLR      at 16#0020# range 0 .. 31;
      PWRR       at 16#0024# range 0 .. 31;
      CLASS      at 16#0028# range 0 .. 31;
      Reserved_3 at 16#002C# range 0 .. 159;
      SETLPIR    at 16#0040# range 0 .. 63;
      CLRLPIR    at 16#0048# range 0 .. 63;
      Reserved_4 at 16#0050# range 0 .. 255;
      PROPBASER  at 16#0070# range 0 .. 63;
      PENDBASER  at 16#0078# range 0 .. 63;
      Reserved_5 at 16#0080# range 0 .. 255;
      INVLPIR    at 16#00A0# range 0 .. 63;
      Reserved_6 at 16#00A8# range 0 .. 63;
      INVALLR    at 16#00B0# range 0 .. 63;
      Reserved_7 at 16#00B8# range 0 .. 63;
      SYNCR      at 16#00C0# range 0 .. 31;
      Reserved_8 at 16#00C4# range 0 .. 522_335;
      PIDR4      at 16#FFD0# range 0 .. 31;
      PIDR5      at 16#FFD4# range 0 .. 31;
      PIDR6      at 16#FFD8# range 0 .. 31;
      PIDR7      at 16#FFDC# range 0 .. 31;
      PIDR0      at 16#FFE0# range 0 .. 31;
      PIDR1      at 16#FFE4# range 0 .. 31;
      PIDR2      at 16#FFE8# range 0 .. 31;
      PIDR3      at 16#FFEC# range 0 .. 31;
      CIDR0      at 16#FFF0# range 0 .. 31;
      CIDR1      at 16#FFF4# range 0 .. 31;
      CIDR2      at 16#FFF8# range 0 .. 31;
      CIDR3      at 16#FFFC# range 0 .. 31;
   end record;

   --------------------------------------------------------
   -- GIC-600 Redistributor Register Definitions for SGI --
   --------------------------------------------------------

   --  See:
   --    * Table 4-29 of the Arm CoreLink GIC-600 Generic Interrupt Controller
   --      Technical Reference Manual
   --    * Table 12-29 of the Arm Generic Interrupt Controller Architecture
   --      Specification GIC architecture version 3 and version 4

   type GICR_IPRIORITYR_Registers is array (0 .. 7) of PRI_Array_4;

   type GICR_SGI_Page is record
      Reserved_1  : Unsigned_32_Array (0 .. 31);
      IGROUPR0    : Unsigned_32;
      Reserved_2  : Unsigned_32_Array (0 .. 30);
      ISENABLER0  : Unsigned_32;
      Reserved_3  : Unsigned_32_Array (0 .. 30);
      ISPENDR0    : Unsigned_32;
      Reserved_4  : Unsigned_32_Array (0 .. 14);
      ICPENDR0    : Unsigned_32;
      Reserved_5  : Unsigned_32_Array (0 .. 14);
      ISACTIVER0  : Unsigned_32;
      Reserved_6  : Unsigned_32_Array (0 .. 14);
      ICACTIVER0  : Unsigned_32;
      Reserved_7  : Unsigned_32_Array (0 .. 14);
      IPRIORITYR  : GICR_IPRIORITYR_Registers;
      Reserved_8  : Unsigned_32_Array (0 .. 503);
      ICFGR       : Unsigned_32_Array (0 .. 1);
      Reserved_9  : Unsigned_32_Array (0 .. 61);
      IGRPMODR0   : Unsigned_32;
      Reserved_10 : Unsigned_32_Array (0 .. 30);
      NSACR       : Unsigned_32;
      Reserved_11 : Unsigned_32_Array (0 .. 11_390);
      MISCSTATUSR : Unsigned_32;
      Reserved_12 : Unsigned_32;
      IERRVR      : Unsigned_32;
      Reserved_13 : Unsigned_32;
      SGIDR       : Unsigned_64;
      Reserved_14 : Unsigned_32_Array (0 .. 3065);
      CFGID0      : Unsigned_32;
      CFGID1      : Unsigned_32;
      Reserved_15 : Unsigned_32_Array (0 .. 1021);
   end record with Size => 64 * 1024 * 8;

   for GICR_SGI_Page use record
      Reserved_1  at 16#0000# range 0 .. 1023;
      IGROUPR0    at 16#0080# range 0 .. 31;
      Reserved_2  at 16#0084# range 0 .. 991;
      ISENABLER0  at 16#0100# range 0 .. 31;
      Reserved_3  at 16#0104# range 0 .. 991;
      ISPENDR0    at 16#0200# range 0 .. 31;
      Reserved_4  at 16#0204# range 0 .. 479;
      ICPENDR0    at 16#0280# range 0 .. 31;
      Reserved_5  at 16#0284# range 0 .. 479;
      ISACTIVER0  at 16#0300# range 0 .. 31;
      Reserved_6  at 16#0304# range 0 .. 479;
      ICACTIVER0  at 16#0380# range 0 .. 31;
      Reserved_7  at 16#0384# range 0 .. 479;
      IPRIORITYR  at 16#0400# range 0 .. 255;
      Reserved_8  at 16#0420# range 0 .. 16_127;
      ICFGR       at 16#0C00# range 0 .. 63;
      Reserved_9  at 16#0C08# range 0 .. 1983;
      IGRPMODR0   at 16#0D00# range 0 .. 31;
      Reserved_10 at 16#0D04# range 0 .. 991;
      NSACR       at 16#0E00# range 0 .. 31;
      Reserved_11 at 16#0E04# range 0 .. 364_511;
      MISCSTATUSR at 16#C000# range 0 .. 31;
      Reserved_12 at 16#C004# range 0 .. 31;
      IERRVR      at 16#C008# range 0 .. 31;
      Reserved_13 at 16#C00C# range 0 .. 31;
      SGIDR       at 16#C010# range 0 .. 63;
      Reserved_14 at 16#C018# range 0 .. 98_111;
      CFGID0      at 16#F000# range 0 .. 31;
      CFGID1      at 16#F004# range 0 .. 31;
      Reserved_15 at 16#F008# range 0 .. 32_703;
   end record;

   -------------------------------------------------
   -- GIC-600 Redistributor Peripheral Definition --
   -------------------------------------------------

   --  The GIC-600 redistributor consists of two 64 KB pages

   type GICR_Peripheral is record
      RD  : GICR_RD_Page;
      SGI : GICR_SGI_Page;
   end record with
     Size => 64 * 1024 * 8 * 2,
     Volatile;

   for GICR_Peripheral use record
      RD  at 16#0_0000# range 0 .. 524_287;
      SGI at 16#1_0000# range 0 .. 524_287;
   end record;

   --  There is one redistributor per CPU
   GICR : array (System.Multiprocessors.CPU) of GICR_Peripheral with
     Import, Address => System'To_Address (BB.Parameters.GICR_Base_Address);

   --------------------------------------------
   -- GIC CPU Interface Register Definitions --
   --------------------------------------------

   procedure Set_ICC_BPR1_EL1 (Value : Unsigned_64) with Inline_Always;
   procedure Set_ICC_EOIR1_EL1 (Value : Unsigned_64) with Inline_Always;
   procedure Set_ICC_IGRPEN1_EL1 (Value : Unsigned_64) with Inline_Always;
   procedure Set_ICC_PMR_EL1 (Value : Unsigned_64) with Inline_Always;
   procedure Set_ICC_SRE_EL1 (Value : Unsigned_64) with Inline_Always;
   --  Set the value of the respective CPU interface system registers

   function Get_ICC_IAR1_EL1 return Unsigned_64 with Inline_Always;
   function Get_ICC_SRE_EL1 return Unsigned_64 with Inline_Always;
   --  Get the value of the respective CPU interface system registers

   ----------------------------
   -- Other System Registers --
   ----------------------------

   type MPIDR_Register is record
      Aff0       : Unsigned_8;
      Aff1       : Unsigned_8;
      Aff2       : Unsigned_8;
      MT         : Boolean;
      Reserved_1 : Unsigned_5;
      U          : Boolean;
      Reserved_2 : Boolean;
      Aff3       : Unsigned_8;
      Reserved_3 : Unsigned_24;
   end record with Size => 64;
   --  Layout of the MPIDR system register

   for MPIDR_Register use record
      Aff0       at 0 range 0 .. 7;
      Aff1       at 0 range 8 .. 15;
      Aff2       at 0 range 16 .. 23;
      MT         at 0 range 24 .. 24;
      Reserved_1 at 0 range 25 .. 29;
      U          at 0 range 30 .. 30;
      Reserved_2 at 0 range 31 .. 31;
      Aff3       at 0 range 32 .. 39;
      Reserved_3 at 0 range 40 .. 63;
   end record;

   function MPIDR return MPIDR_Register;

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return System.Multiprocessors.CPU is
      use System.BB.Parameters;

      MPIDR_Value : constant MPIDR_Register := MPIDR;
      CPU_Index   : constant Natural :=
                        Natural (MPIDR_Value.Aff0) +
                        (Natural (MPIDR_Value.Aff1) * Num_PEs_Per_CPU) +
                        (Natural (MPIDR_Value.Aff2) * Num_PEs_Per_Cluster);
   begin
      return System.Multiprocessors.CPU (CPU_Index + 1);
   end Current_CPU;

   -------------------------
   -- Define_IRQ_Triggers --
   -------------------------

   procedure Define_IRQ_Triggers (Config : ICFGR) is
   begin
      for I in Config'Range loop
         GICD.ICFGR (I) := Config (I);
      end loop;
   end Define_IRQ_Triggers;

   ----------------------
   -- Get_ICC_IAR1_EL1 --
   ----------------------

   function Get_ICC_IAR1_EL1 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mrs %0, s3_0_c12_c12_0",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Volatile => True);
      return Value;
   end Get_ICC_IAR1_EL1;

   ----------------------
   -- Get_ICC_SRE_EL1 --
   ----------------------

   function Get_ICC_SRE_EL1 return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("mrs %0, s3_0_c12_c12_5",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Volatile => True);
      return Value;
   end Get_ICC_SRE_EL1;

   ---------------------
   -- Initialize_GICD --
   ---------------------

   procedure Initialize_GICD is
   begin
      GICD.CTLR := (GICD.CTLR with delta
                    EnableGrp0   => 0,
                    EnableGrp1NS => 0,
                    EnableGrp1S  => 0,
                    DS           => 1);

      --  Disable all shared interrupts

      for I in 1 .. GICD.ICENABLER'Last loop
         GICD.ICENABLER (I) := 16#FFFF_FFFF#;
      end loop;

      --  Set default priority (maximum priority)

      for I in GICD.IPRIORITYR'Range loop
         GICD.IPRIORITYR (I) := (others => 0);
      end loop;

      --  Enable group 1 interrupts (Non-Secure)

      GICD.CTLR.EnableGrp1NS := 1;
   end Initialize_GICD;

   --------------------
   -- Initialize_ICC --
   --------------------

   procedure Initialize_ICC is
      Int_Mask : Unsigned_32 := 0;
      SRE      : Unsigned_64;

      RDGPD : Bit;
      RDGPO : Bit;

      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;

   begin
      --  Power on the redistributor. See Section 3.6.1 of the GIC-600
      --  Generic Interrupt Controller Technical Reference Manual.

      loop
         loop
            RDGPD := GICR (CPU_Id).RD.PWRR.RDGPD;
            RDGPO := GICR (CPU_Id).RD.PWRR.RDGPO;
            exit when RDGPD = RDGPO;
         end loop;

         GICR (CPU_Id).RD.PWRR := (GICR (CPU_Id).RD.PWRR with delta
                                   RDAG => 1,
                                   RDPD => 0);

         exit when GICR (CPU_Id).RD.PWRR.RDPD = 0;
      end loop;

      --  Notify the redistributor that this processor is awake

      GICR (CPU_Id).RD.WAKER := (GICR (CPU_Id).RD.WAKER with delta
                                 Sleep          => 0,
                                 ProcessorSleep => 0);

      --  Each CPU has its own GIC CPU Interface, so has to be configured
      --  each time. The PPI and SGI exceptions are also CPU-specific so are
      --  banked. see 12.1.12 in the ARM GIC Architecture Specification v3/v4
      --  document.

      --  Enable system register access for this CPU (set ICC_SRE_EL1.SRE = 1)

      SRE := Get_ICC_SRE_EL1;
      SRE := SRE or 1;
      Set_ICC_SRE_EL1 (SRE);

      --  Mask all interrupts

      Set_ICC_PMR_EL1 (0);

      --  Enable group 1 interrupts

      Set_ICC_IGRPEN1_EL1 (1);

      --  Binary point register:
      --  The register defines the point at which the priority value fields
      --  split into two parts.

      Set_ICC_BPR1_EL1 (3);

      --  Disable banked interrupts by default

      GICD.ICENABLER (0) := 16#FFFF_FFFF#;

      --  All SGIs and PPIs are non-secure group 1

      GICR (CPU_Id).SGI.IGROUPR0 := 16#FFFF_FFFF#;

      --  Enable the CPU-specific interrupts that have a handler registered.
      --
      --  On CPU0, no interrupt is registered for now so this has no effect.
      --  On the other CPUs, as interrupts are registered via a call to
      --  Interrupts.Install_Interrupt_Handler before the CPUs are started,
      --  the following properly takes care of initializing the interrupt mask
      --  and priorities for those.

      for J in Banked_Interrupts_Enabled'Range loop
         if Banked_Interrupts_Enabled (J) then
            Int_Mask := Int_Mask or 2 ** J;

            GICR (CPU_Id).SGI.IPRIORITYR (J / 4)(J mod 4) :=
              Unsigned_8 (Banked_Interrupts_Priority (J));
         end if;
      end loop;

      if Int_Mask /= 0 then
         GICR (CPU_Id).SGI.ISENABLER0 := Int_Mask;
      end if;
   end Initialize_ICC;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Interrupt : Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      Reg_Idx  : constant Natural     := Interrupt / 32;
      Int_Mask : constant Unsigned_32 := 2**(Interrupt mod 32);
      Int_PRI : constant PRI         := To_PRI (Prio);
   begin
      --  Route the interrupt to core 0 in cluster 0, except for PPIs and SGIs

      if Natural (Interrupt) in GICD.IROUTER'Range then
         GICD.IROUTER (Natural (Interrupt)) :=
           (Aff0                   => 0,
            Aff1                   => 0,
            Aff2                   => 0,
            Reserved_1             => 0,
            Interrupt_Routing_Mode => 0,
            Aff3                   => 0,
            Reserved_2             => 0);
      end if;

      --  Set priority and enable interrupt

      GICD.IPRIORITYR (Interrupt / 4)(Interrupt mod 4) := To_PRI (Prio);
      GICD.ISENABLER  (Reg_Idx) := Int_Mask;

      --  Handlers are registered before the CPUs are awaken (only the CPU 0
      --  executes Install_Interrupt_Handler.
      --  So we save the registered interrupts to properly initialize the
      --  other CPUs for banked interrupts.

      if Interrupt in Banked_Interrupt then
         Banked_Interrupts_Priority (Interrupt) := Int_PRI;
         Banked_Interrupts_Enabled (Interrupt)  := True;

         --  Enable the interrupt on CPU 0 now

         GICR (1).SGI.IPRIORITYR (Interrupt / 4)(Interrupt mod 4) := Int_PRI;
         GICR (1).SGI.ISENABLER0 := 2**Interrupt;
      end if;
   end Install_Interrupt_Handler;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler is
      Int_ID : constant Unsigned_64 := Get_ICC_IAR1_EL1;
   begin
      if Int_ID = 16#3FF# then
         --  Spurious interrupt
         return;
      end if;

      Interrupt_Wrapper (Interrupt_ID (Int_ID));

      --  Clear interrupt request
      Set_ICC_EOIR1_EL1 (Int_ID);
   end IRQ_Handler;

   -----------
   -- MPIDR --
   -----------

   function MPIDR return MPIDR_Register is
      R : MPIDR_Register;
   begin
      Asm ("mrs %0, mpidr_el1",
           Outputs  => MPIDR_Register'Asm_Output ("=r", R),
           Volatile => True);
      return R;
   end MPIDR;

   --------------
   -- Poke_CPU --
   --------------

   procedure Poke_CPU
     (CPU_Id         : System.Multiprocessors.CPU;
      Poke_Interrupt : Interrupt_ID)
   is
   begin
      --  Multiple processors is not supported yet

      null;
   end Poke_CPU;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Power_Down;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority
   is
      Reg_Index   : constant Natural := Interrupt / 4;
      Field_Index : constant Natural := Interrupt mod 4;

   begin
      return To_Priority (GICD.IPRIORITYR (Reg_Index)(Field_Index));
   end Priority_Of_Interrupt;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      Set_ICC_PMR_EL1 (Unsigned_64 (To_PRI (Priority)));
   end Set_Current_Priority;

   ----------------------
   -- Set_ICC_BPR1_EL1 --
   ----------------------

   procedure Set_ICC_BPR1_EL1 (Value : Unsigned_64) is
   begin
      Asm ("msr s3_0_c12_c12_3, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Set_ICC_BPR1_EL1;

   ----------------------
   -- Set_ICC_EOIR1_EL1 --
   ----------------------

   procedure Set_ICC_EOIR1_EL1 (Value : Unsigned_64) is
   begin
      Asm ("msr s3_0_c12_c12_1, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Set_ICC_EOIR1_EL1;

   -------------------------
   -- Set_ICC_IGRPEN1_EL1 --
   -------------------------

   procedure Set_ICC_IGRPEN1_EL1 (Value : Unsigned_64) is
   begin
      Asm ("msr s3_0_c12_c12_7, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Set_ICC_IGRPEN1_EL1;

   ---------------------
   -- Set_ICC_PMR_EL1 --
   ---------------------

   procedure Set_ICC_PMR_EL1 (Value : Unsigned_64) is
   begin
      Asm ("msr s3_0_c4_c6_0, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Set_ICC_PMR_EL1;

   ---------------------
   -- Set_ICC_SRE_EL1 --
   ---------------------

   procedure Set_ICC_SRE_EL1 (Value : Unsigned_64) is
   begin
      Asm ("msr s3_0_c12_c12_5, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Set_ICC_SRE_EL1;

   ------------
   -- To_PRI --
   ------------

   function To_PRI (P : Integer) return PRI is
   begin
      if P < Interrupt_Priority'First then
         --  Do not mask any interrupt
         return 255;
      else
         --  change range 240 .. 254
         return PRI (Interrupt_Priority'Last - P) * 16;
      end if;
   end To_PRI;

   -----------------
   -- To_Priority --
   -----------------

   function To_Priority (P : PRI) return Interrupt_Priority is
   begin
      if P = 0 then
         return Interrupt_Priority'Last;
      else
         return Interrupt_Priority'Last - Any_Priority'Base (P / 16);
      end if;
   end To_Priority;

end System.ARM_GIC;
