------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . R I S C V _ P L I C                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements the RISC-V PLIC interface from the SiFive E3 PLIC

with Interfaces.FE310;      use Interfaces.FE310;
with Interfaces.FE310.PLIC; use Interfaces.FE310.PLIC;

with System.BB.CPU_Specific;

package body System.BB.RISCV_PLIC is

   use type CPU_Specific.Register_Word;

   Claimed_Table : array (BBI.Any_Interrupt_ID) of Boolean := (others => False)
     with Ghost;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Disable all interrupts
      PLIC_Periph.TARGET_ENABLE.ENABLE :=
        (others => (As_Array => True, Arr => (others => False)));

      --  Set all priority to zero
      for Prio of PLIC_Periph.PRIORITY loop
         Prio := (PRIORITY => 0, others => <>);
      end loop;
   end Initialize;

   -------------
   -- Pending --
   -------------

   function Pending return Boolean is
   begin
      return (CPU_Specific.Mip and CPU_Specific.Mip_MEIP) /= 0;
   end Pending;

   ------------
   -- Enable --
   ------------

   procedure Enable (Interrupt : BBI.Interrupt_ID) is
      Reg_Id : constant Natural := Natural (Interrupt) / 32;
      Int_Id : constant Natural := Natural (Interrupt) mod 32;
   begin
      PLIC_Periph.TARGET_ENABLE.ENABLE (Reg_Id).Arr (Int_Id) := True;
   end Enable;

   -------------
   -- Claimed --
   -------------

   function Claimed (Interrupt : BBI.Interrupt_ID) return Boolean
   is (Claimed_Table (Interrupt));

   ------------
   --  Claim --
   ------------

   function Claim return BBI.Any_Interrupt_ID is

      Number : constant UInt32 := PLIC_Periph.TARGET.CLAIM;

      Id : BBI.Any_Interrupt_ID;
   begin

      if Number
           not in
         UInt32 (BBI.Interrupt_ID'First) .. UInt32 (BBI.Interrupt_ID'Last)
      then
         Id := BBI.No_Interrupt;
      else
         Id := BBI.Interrupt_ID (Number);
      end if;

      Claimed_Table (Id) := True;
      return Id;
   end Claim;

   --------------
   -- Complete --
   --------------

   procedure Complete (Interrupt : BBI.Interrupt_ID) is
   begin

      --  Writing to the CLAIM register to signal completion
      PLIC_Periph.TARGET.COMPLETE := UInt32 (Interrupt);

      Claimed_Table (Interrupt) := False;
   end Complete;

   ----------------------------
   -- Set_Priority_Threshold --
   ----------------------------

   procedure Set_Priority_Threshold (Priority : Integer) is
      subtype THRESHOLD_Field is THRESHOLD_TARGET_THRESHOLD_Field;

      Thresh : THRESHOLD_TARGET_Register renames PLIC_Periph.TARGET.THRESHOLD;

      Int_Priority : constant Integer :=
        Priority - Interrupt_Priority'First + 1;

   begin
      if Int_Priority > Integer (THRESHOLD_Field'Last) then
         Thresh := (THRESHOLD_Field'Last, others => <>);
      elsif Int_Priority < Integer (THRESHOLD_Field'First) then
         Thresh := (THRESHOLD_Field'First, others => <>);
      else
         Thresh := (THRESHOLD_TARGET_THRESHOLD_Field (Int_Priority),
                    others => <>);
      end if;
   end Set_Priority_Threshold;

   ---------------
   -- Threshold --
   ---------------

   function Threshold return Integer is
   begin
      return Integer (PLIC_Periph.TARGET.THRESHOLD.THRESHOLD);
   end Threshold;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (Interrupt : System.BB.Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      Int_Prio : constant Natural :=
        Natural (Prio) - Natural (Interrupt_Priority'First) + 1;
      --  On the PLIC, priority zero is reserved to mean "never interrupt"

      Int_Id : constant Natural := Natural (Interrupt);
   begin
      PLIC_Periph.PRIORITY (Int_Id).PRIORITY := UInt3 (Int_Prio);
   end Set_Priority;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority
   is
      Int_Id : constant Natural := Natural (Interrupt);
   begin
      return Natural (PLIC_Periph.PRIORITY (Int_Id).PRIORITY) +
        Interrupt_Priority'First - 1;
   end Priority_Of_Interrupt;

end System.BB.RISCV_PLIC;
