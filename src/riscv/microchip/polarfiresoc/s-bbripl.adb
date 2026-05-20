------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . R I S C V _ P L I C                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with Interfaces; use Interfaces;

with System.BB.CPU_Specific;

with System.BB.Board_Parameters; use System.BB.Board_Parameters;

package body System.BB.RISCV_PLIC is

   use type CPU_Specific.Register_Word;

   ---------------------
   -- Source Priority --
   ---------------------

   type Priority_Type is mod 2**PLIC_Priority_Bits
     with Size => PLIC_Priority_Bits;

   type Priority_Register is record
      P : Priority_Type;
   end record
     with Volatile_Full_Access, Size => 32;

   for Priority_Register use record
      P at 0 range 0 .. PLIC_Priority_Bits - 1;
   end record;

   type Priority_Array is array (1 .. PLIC_Nbr_Of_Sources) of
     Priority_Register;

   Source_Priority : Priority_Array
     with Address => PLIC_Base_Address + 4;

   ----------------------
   -- Enable Registers --
   ----------------------

   type Enable_Flags is array (0 .. 31) of Boolean
     with Volatile_Full_Access, Pack, Size => 32;

   type Enable_Array is array (0 .. PLIC_Nbr_Of_Mask_Regs - 1) of Enable_Flags
     with Pack, Size => 32 * PLIC_Nbr_Of_Mask_Regs;

   pragma Warnings (Off, "832 bits of ""Hart_Enables"" unused");
   type Hart_Enables is record
      M_Mode   : Enable_Array;
      S_Mode   : Enable_Array;
   end record
     with Size => 8 * 256;
   pragma Warnings (On, "832 bits of ""Hart_Enables"" unused");

   for Hart_Enables use record
      M_Mode   at 16#00# range 0 .. (32 * PLIC_Nbr_Of_Mask_Regs) - 1;
      S_Mode   at 16#80# range 0 .. (32 * PLIC_Nbr_Of_Mask_Regs) - 1;
   end record;

   type Hart_Enables_Array is array (1 .. PLIC_Nbr_Of_Harts - 1) of
     Hart_Enables;

   Hart_0_M_Mode_Enables : Enable_Array
     with Address => PLIC_Base_Address + 16#0000_2000#;
   --  Hart 0 is a special case because there is no S-Mode and the address is
   --  does not follow the same pattern as the other harts.

   Harts_Enables : Hart_Enables_Array
     with Address => PLIC_Base_Address + 16#0000_2080#;
   --  Other harts enables

   -----------------------------------
   -- Hart Threshold/Claim/Complete --
   -----------------------------------

   pragma Warnings (Off, "32704 bits of ""Hart_Control"" unused");
   type Hart_Control is record
      Threshold      : Priority_Register;
      Claim_Complete : Unsigned_32;
   end record
     with Size => 8 * 4096;
   pragma Warnings (On, "32704 bits of ""Hart_Control"" unused");

   for Hart_Control use record
      Threshold      at 0 range 0 .. 31;
      Claim_Complete at 4 range 0 .. 31;
   end record;

   type Hart_Control_Array is array (0 .. PLIC_Nbr_Of_Harts - 1) of
     Hart_Control
     with Size => 8 * 4096 * PLIC_Nbr_Of_Harts;

   Harts_Control : Hart_Control_Array
     with Address => PLIC_Base_Address + 16#0020_0000#;

   Claimed_Table : array (BBI.Any_Interrupt_ID) of Boolean := (others => False)
     with Ghost;

   Use_Hart_0 : constant Boolean := PLIC_Hart_Id = 0;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Disable all interrupts
      Hart_0_M_Mode_Enables := (others => (others => False));
      Harts_Enables := (others => (M_Mode => (others => (others => False)),
                                   S_Mode => (others => (others => False)))
                       );

      --  Set all priority to zero
      for Prio of Source_Priority loop
         Prio := (P => 0);
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
      if Use_Hart_0 then
         Hart_0_M_Mode_Enables (Reg_Id) (Int_Id) := True;
      else
         Harts_Enables (PLIC_Hart_Id).M_Mode (Reg_Id) (Int_Id) := True;
      end if;
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

      Number : constant Unsigned_32 :=
        Harts_Control (PLIC_Hart_Id).Claim_Complete;

      Id : BBI.Any_Interrupt_ID;
   begin

      if Number
           not in
         Unsigned_32 (BBI.Interrupt_ID'First) ..
        Unsigned_32 (BBI.Interrupt_ID'Last)
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
      Harts_Control (PLIC_Hart_Id).Claim_Complete := Unsigned_32 (Interrupt);

      Claimed_Table (Interrupt) := False;
   end Complete;

   ----------------------------
   -- Set_Priority_Threshold --
   ----------------------------

   procedure Set_Priority_Threshold (Priority : Integer) is
      Hart : Hart_Control renames Harts_Control (PLIC_Hart_Id);

      Int_Priority : constant Integer :=
        Priority - Interrupt_Priority'First + 1;

   begin
      if Int_Priority > Integer (Priority_Type'Last) then
         Hart.Threshold := (P => Priority_Type'Last);
      elsif Int_Priority < Integer (Priority_Type'First) then
         Hart.Threshold := (P => Priority_Type'First);
      else
         Hart.Threshold := (P => Priority_Type (Int_Priority));
      end if;
   end Set_Priority_Threshold;

   ---------------
   -- Threshold --
   ---------------

   function Threshold return Integer is
   begin
      return Integer (Harts_Control (PLIC_Hart_Id).Threshold.P);
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
      Source_Priority (Int_Id).P := Priority_Type (Int_Prio);
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
      return Natural (Source_Priority (Int_Id).P) +
        Interrupt_Priority'First - 1;
   end Priority_Of_Interrupt;

end System.BB.RISCV_PLIC;
