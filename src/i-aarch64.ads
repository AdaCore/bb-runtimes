------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     I N T E R F A C E S . A A R C H 6 4                  --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                         Copyright (C) 2017, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
------------------------------------------------------------------------------

with System;

package Interfaces.AArch64 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   --  Counters and timers

   function Get_CPACR_EL1 return Interfaces.Unsigned_64 with Inline_Always;
   procedure Set_CPACR_EL1 (Val : Interfaces.Unsigned_64) with Inline_Always;
   --  Low-level access to CPACR_EL1 register

   CPACR_FPEN : constant := 16#100000#;
   --  FPU enable bit of CPACR

   function Get_CPTR_EL2 return Interfaces.Unsigned_64 with Inline_Always;
   procedure Set_CPTR_EL2 (Val : Interfaces.Unsigned_64) with Inline_Always;
   --  Low-level access to CPTR_EL2 register

   CPTR_TFP : constant := 16#400#;
   --  FPU trap bit of CPTR

   procedure Set_CNTP_CTL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTHP_CTL_EL2 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTP_CTL register

   procedure Set_CNTV_CTL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTV_CTL_EL0 register

   procedure Set_CNTP_TVAL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTHP_TVAL_EL2 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTP_TVAL register

   procedure Set_CNTP_CVAL_EL0 (Val : Interfaces.Unsigned_64)
     with Inline_Always;
   procedure Set_CNTHP_CVAL_EL2 (Val : Interfaces.Unsigned_64)
     with Inline_Always;
   --  Set the CNTP_CVAL register

   function Get_CNTPCT_EL0 return Interfaces.Unsigned_64
     with Inline_Always;
   --  Get the CNTPCT register

   --  MMU

   TCR_PS_4GB      : constant := 2#000# * 2**16;
   TCR_TG0_4KB     : constant := 2#00# * 2**14;
   TCR_SH0_OS      : constant := 2#10# * 2**12;
   TCR_ORGN0_WBWAC : constant := 2#01# * 2**10;
   TCR_IRGN0_WBWAC : constant := 2#01# * 2**8;
   TCR_SL0_00      : constant := 2#00# * 2**6;
   TCR_SL0_01      : constant := 2#01# * 2**6;
   TCR_SL0_10      : constant := 2#10# * 2**6;
   TCR_T0SZ        : constant := 2**0;

   HCR_RW    : constant := 2**31;
   HCR_TACR  : constant := 2**21;
   HCR_TIDCP : constant := 2**20;
   HCR_TSC   : constant := 2**19;
   HCR_TID3  : constant := 2**18;
   HCR_TID2  : constant := 2**17;
   HCR_TID1  : constant := 2**16;
   HCR_TID0  : constant := 2**15;
   HCR_TWE   : constant := 2**14;
   HCR_TWI   : constant := 2**13;
   HCR_DC    : constant := 2**12;
   HCR_AMO   : constant := 2**5;
   HCR_IMO   : constant := 2**4;
   HCR_FMO   : constant := 2**3;
   HCR_VM    : constant := 2**0;

   function Get_Current_EL return Unsigned_32 with Inline_Always;

   --  EL3 registers

   function Get_ELR_EL3 return Unsigned_64 with Inline_Always;
   procedure Set_ELR_EL3 (V : Unsigned_64) with Inline_Always;
   function Get_SPSR_EL3 return Unsigned_32 with Inline_Always;
   function Get_ESR_EL3 return Unsigned_32 with Inline_Always;
   function Get_FAR_EL3 return Unsigned_64 with Inline_Always;

   --  EL2 registers

   function Get_ELR_EL2 return Unsigned_64 with Inline_Always;
   procedure Set_ELR_EL2 (V : Unsigned_64) with Inline_Always;
   function Get_SPSR_EL2 return Unsigned_32 with Inline_Always;
   function Get_ESR_EL2 return Unsigned_32 with Inline_Always;
   function Get_FAR_EL2 return Unsigned_64 with Inline_Always;
   function Get_HPFAR_EL2 return Unsigned_64 with Inline_Always;
   function Get_SP_EL2 return Unsigned_64 with Inline_Always;
   function Get_HCR_EL2 return Unsigned_64 with Inline_Always;
   procedure Set_HCR_EL2 (V : Unsigned_64) with Inline_Always;
   function Get_VTCR_EL2 return Unsigned_64 with Inline_Always;
   function Get_VTTBR_EL2 return Unsigned_64 with Inline_Always;
   procedure Set_VTTBR_EL2 (V : Unsigned_64) with Inline_Always;
   function Get_SCTLR_EL2 return Unsigned_32 with Inline_Always;
   procedure Set_VPIDR_EL2 (V : Unsigned_32) with Inline_Always;
   procedure Set_VMPIDR_EL2 (V : Unsigned_64) with Inline_Always;

   --  EL1 registers
   function Get_ELR_EL1 return Unsigned_64 with Inline_Always;
   procedure Set_ELR_EL1 (V : Unsigned_64) with Inline_Always;
   function Get_SPSR_EL1 return Unsigned_32 with Inline_Always;
   function Get_VBAR_EL1 return Unsigned_64 with Inline_Always;
   function Get_ESR_EL1 return Unsigned_32 with Inline_Always;
   function Get_FAR_EL1 return Unsigned_64 with Inline_Always;
   function Get_SP_EL1 return Unsigned_64 with Inline_Always;
   function Get_SCTLR_EL1 return Unsigned_32 with Inline_Always;
   function Get_TCR_EL1 return Unsigned_64 with Inline_Always;
   function Get_TTBR0_EL1 return Unsigned_64 with Inline_Always;
   function Get_TTBR1_EL1 return Unsigned_64 with Inline_Always;
   function Get_MPIDR_EL1 return Unsigned_32 with Inline_always;

   --  EL0 registers
   function Get_SP_EL0 return Unsigned_64 with Inline_Always;

   --  ID registers

   function Get_ID_AA64MMFR0_EL1 return Unsigned_64 with Inline_always;
   function Get_ID_AA64MMFR1_EL1 return Unsigned_64 with Inline_always;

   --  Cache control

   procedure DC_CVAU (Addr : System.Address) with Inline_Always;
   --  Clean D-cache by virtual address to point of unification

   procedure DC_CVAC (Addr : System.Address) with Inline_Always;
   --  Clean D-cache by virtual address to point of coherence

   procedure IC_IVAU (Addr : System.Address) with Inline_Always;
   --  Invalidate I-cache by virtual address

   --  Barriers

   procedure DSB_ISH with Inline_Always;
   --  Data Synchronization Barrier

   procedure ISB with Inline_Always;
   --  Instruction Synchronization Barrier

   --  TLB

   procedure TLBI_VMALLS12E1 with Inline_Always;

end Interfaces.AArch64;
