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

with Interfaces;               use Interfaces;
with System.BB.Parameters;
with System.Machine_Code;

package body System.ARM_GIC is

   use System.BB.Interrupts;

   subtype PRI is Unsigned_8;
   --  Type for GIC interrupt priorities. Note that 0 is the highest
   --  priority, which is reserved for the kernel and has no corresponding
   --  Interrupt_Priority value, and 255 is the lowest. We assume the
   --  PRIGROUP setting is such that the 4 most significant bits determine
   --  the priority group used for preemption. However, if less bits are
   --  implemented, this should still work.

   subtype Banked_Interrupt is Interrupt_ID range 0 .. 31;
   Interrupts_Enabled  : array (Banked_Interrupt) of Boolean :=
                           (others => False);
   Interrupts_Priority : array (Banked_Interrupt) of PRI;

   subtype GIC_Interrupts is Interrupt_ID;

   --  Supports up to 192 interrupts, MaxIRQs need to be a aligned on 32
   --  Number_of_IRQs = Interrupt_ID'Last + 1
   --  Max_IRQs = (Number_Of_IRQs + 31) / 32 * 32
   Max_IRQs : constant Natural :=
     Natural'Min (192,
                  ((Interrupt_ID'Last / 32 + 1) * 32));

   function Reg_Num_32 (Intnum : GIC_Interrupts) return GIC_Interrupts
   is (Intnum / 32);

   type Reg32 is new Unsigned_32 with Volatile;
   type Reg8  is new Unsigned_8  with Volatile;

   --  32-bit registers set
   --  Accessed by 32 bits chunks
   type Bits32_Register_Array is array
     (Natural range 0 .. Max_IRQs / 32 - 1) of Reg32
     with Pack;

   --  Byte registers set
   --  Accessed by 8 bits chunks
   type Byte_Register_Array is array
     (Natural range 0 .. Max_IRQs - 1) of Reg8
     with Pack;

   --  Byte registers set, accessed by 32-bit chunks
   type Byte_Register_Array32 is array
     (Natural range 0 .. Max_IRQs / 4 - 1) of Reg32
     with Pack;

   type GICD_Peripheral is record
      CTLR       : Reg32;
      ISENABLER  : Bits32_Register_Array;
      ICENABLER  : Bits32_Register_Array;
      IPRIORITYR : Byte_Register_Array;
      ITARGETSR  : Byte_Register_Array32;
      ICFGR      : Byte_Register_Array32;
      SGIR       : Reg32;
   end record;

   for GICD_Peripheral use record
      CTLR       at 16#000# range 0 .. 31;
      ISENABLER  at 16#100# range 0 .. Max_IRQs - 1;
      ICENABLER  at 16#180# range 0 .. Max_IRQs - 1;
      IPRIORITYR at 16#400# range 0 .. Max_IRQs * 8 - 1;
      ITARGETSR  at 16#800# range 0 .. Max_IRQs * 8 - 1;
      ICFGR      at 16#C00# range 0 .. Max_IRQs * 8 - 1;
      SGIR       at 16#F00# range 0 .. 31;
   end record;

   type GICC_Peripheral is record
      CTLR : Reg32;
      PMR  : Reg32;
      BPR  : Reg32;
      IAR  : Reg32;
      EOIR : Reg32;
   end record;

   for GICC_Peripheral use record
      CTLR at 16#00# range 0 .. 31;
      PMR  at 16#04# range 0 .. 31;
      BPR  at 16#08# range 0 .. 31;
      IAR  at 16#0C# range 0 .. 31;
      EOIR at 16#10# range 0 .. 31;
   end record;

   GICD : GICD_Peripheral
     with Import, Address => BB.Parameters.GICD_Base_Address;
   GICC : GICC_Peripheral
     with Import, Address => BB.Parameters.GICC_Base_Address;

   function To_PRI (P : Integer) return PRI with Inline;
   --  Return the PRI mask for the given Ada priority. Note that the zero
   --  value here means no mask, so no interrupts are masked.

   function To_Priority (P : PRI) return Interrupt_Priority with Inline;
   --  Given an ARM interrupt priority (PRI value), determine the Ada
   --  priority.
   --  While the value 0 is reserved for the kernel and has no Ada
   --  priority that represents it, Interrupt_Priority'Last is closest.

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

   ---------------------
   -- Initialize_GICC --
   ---------------------

   procedure Initialize_GICC
   is
      Int_Mask : Unsigned_32 := 0;
   begin
      --  Core-specific part of the GIC configuration:
      --  The GICC (CPU Interface) is banked for each CPU, so has to be
      --  configured each time.
      --  The PPI and SGI exceptions are also CPU-specific so are banked.
      --  see 4.1.4 in the ARM GIC Architecture Specification v2 document

      --  Mask all interrupts

      GICC.PMR := 0;

      --  Binary point register:
      --  The register defines the point at which the priority value fields
      --  split into two parts.

      GICC.BPR := 3;

      --  Disable banked interrupts by default

      GICD.ICENABLER (0) := 16#FFFF_FFFF#;

      --  Enable the CPU-specific interrupts that have a handler registered.
      --
      --  On CPU0, no interrupt is registered for now so this has no effect.
      --  On the other CPUs, as interrupts are registered via a call to
      --  Interrupts.Install_Interrupt_Handler before the CPUs are started,
      --  the following properly takes care of initializing the interrupt mask
      --  and priorities for those.

      for J in Interrupts_Enabled'Range loop
         if Interrupts_Enabled (J) then
            Int_Mask := Int_Mask or 2 ** J;
            GICD.IPRIORITYR (J) := Reg8 (Interrupts_Priority (J));
         end if;
      end loop;

      if Int_Mask /= 0 then
         GICD.ISENABLER (0) := Reg32 (Int_Mask);
      end if;

      --  Set the Enable Group1 bit to the GICC CTLR register.
      --  The view we have here is a GICv1 or GICv2 version with Security
      --  extension, from a non-secure mode.

      GICC.CTLR := 1;
   end Initialize_GICC;

   ---------------------
   -- Initialize_GICD --
   ---------------------

   procedure Initialize_GICD
   is
   begin
      GICD.CTLR := 0;

      --  Disable all shared Interrupts
      for J in 1 .. GICD.ICENABLER'Last loop
         GICD.ICENABLER (J) := 16#FFFF_FFFF#;
      end loop;

      --  default priority
      for J in GICD.IPRIORITYR'Range loop
         GICD.IPRIORITYR (J) := 0;
      end loop;

      --  Set the target cpu

      --  ITARGETSR(0) is read-only and redirects all IRQs to the currently
      --  running CPU. We initialize the shared interrupts targets to the
      --  same value so that we're sure to receive them.
      for Reg_Num in 8 .. GICD.ITARGETSR'Last loop
         GICD.ITARGETSR (Reg_Num) := GICD.ITARGETSR (0);
      end loop;

      GICD.CTLR := 3;
   end Initialize_GICD;

   -------------------------
   -- Define_IRQ_Triggers --
   -------------------------

   procedure Define_IRQ_Triggers (Config : ICFGR)
   is
   begin
      for J in Config'Range loop
         GICD.ICFGR (J) := Reg32 (Config (J));
      end loop;
   end Define_IRQ_Triggers;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler
   is
      IAR    : constant Unsigned_32 := Unsigned_32 (GICC.IAR);
      Int_Id : constant Unsigned_32 := IAR and 16#3FF#;
   begin
      if Int_Id = 16#3FF# then
         --  Spurious interrupt
         return;
      end if;

      Interrupt_Wrapper (Interrupt_ID (Int_Id));

      --  Clear interrupt request
      GICC.EOIR := Reg32 (IAR);
   end IRQ_Handler;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Interrupt : Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
   begin
      GICD.IPRIORITYR (Interrupt) := Reg8 (To_PRI (Prio));
      GICD.ISENABLER (Reg_Num_32 (Interrupt)) := 2 ** (Interrupt mod 32);

      --  Handlers are registered before the CPUs are awaken (only the CPU 0
      --  executes Install_Interrupt_Handler.
      --  So we save the registered interrupts to properly initialize the
      --  other CPUs for banked interrupts.
      if Interrupt in Banked_Interrupt then
         Interrupts_Priority (Interrupt) := To_PRI (Prio);
         Interrupts_Enabled (Interrupt)  := True;
      end if;
   end Install_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority
   is
   begin
      return To_Priority (Unsigned_8 (GICD.IPRIORITYR (Interrupt)));
   end Priority_Of_Interrupt;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      GICC.PMR := Reg32 (To_PRI (Priority));
   end Set_Current_Priority;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      System.Machine_Code.Asm ("wfi", Volatile => True);
   end Power_Down;

   --------------
   -- Poke_CPU --
   --------------

   procedure Poke_CPU (CPU_Id         : System.Multiprocessors.CPU;
                       Poke_Interrupt : Interrupt_ID)
   is
   begin
      GICD.SGIR :=
        2 ** (15 + Natural (CPU_Id)) + Reg32 (Poke_Interrupt);
   end Poke_CPU;

end System.ARM_GIC;
