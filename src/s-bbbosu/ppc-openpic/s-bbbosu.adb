------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with System.BB.Board_Parameters;
with System.BB.CPU_Specific;
with System.Machine_Code;

pragma Warnings (off);
--  Vectors and priorities are defined in Ada.Interrupts.Names, which is not
--  preelaborated. Ignore this issue as we only reference static constants.

with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

pragma Warnings (on);

with Interfaces; use Interfaces;

package body System.BB.Board_Support is

   procedure Interrupt_Handler;
   --  Called by low-level handler in case of external interrupt

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use 4;
   type Unsigned_1 is mod 2 ** 1;
   for Unsigned_1'Size use 1;

   type Vector_Priority_Register is record
      Vector   : Unsigned_16;
      Priority : Unsigned_4;
      Activity : Unsigned_1;
      Mask     : Unsigned_1;
   end record with Size => 32;

   for Vector_Priority_Register'Bit_Order use Low_Order_First;
   for Vector_Priority_Register use record
      Vector   at 0 range 0 .. 15;
      Priority at 0 range 16 .. 19;
      Activity at 0 range 30 .. 30;
      Mask     at 0 range 31 .. 31;
   end record;

   procedure Set_Vpr
     (Offset    : Address;
      Interrupt : Ada.Interrupts.Interrupt_ID;
      Priority  : Interrupt_Priority);
   --  Set a VPR register of the OpenPIC at address Offset. Enable interrupt

   function Get_Vpr (Offset : Address) return Vector_Priority_Register;
   --  Return a VPR register of the OpenPIC at address Offset

   function Vpr_Offset
     (Interrupt : Ada.Interrupts.Interrupt_ID) return Address;
   --  Return offset of the vector priority for the given interrupt

   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Implementation of Time.Clear_Alarm_Interrupt

   -------------
   -- Set_Vpr --
   -------------

   procedure Set_Vpr
     (Offset    : Address;
      Interrupt : Ada.Interrupts.Interrupt_ID;
      Priority  : Interrupt_Priority)
   is
      Vpr : Vector_Priority_Register;
      for Vpr'Address use System.BB.Board_Parameters.CCSRBAR + Offset;
      pragma Volatile (Vpr);
      pragma Import (Ada, Vpr);
   begin
      --  We store the interrupt Id in Vpr.Vector, IACK will be set with this
      --  value when the interrupt is trigger. This allows us to get the ID of
      --  the triggered interrupt.

      Vpr := (Vector   => Unsigned_16 (Interrupt),
              Priority => Unsigned_4 (Priority - Interrupt_Priority'First),
              Activity => 0,
              Mask     => 0);
   end Set_Vpr;

   -------------
   -- Get_Vpr --
   -------------

   function Get_Vpr (Offset : Address) return Vector_Priority_Register is
      Vpr : Vector_Priority_Register;
      for Vpr'Address use System.BB.Board_Parameters.CCSRBAR + Offset;
      pragma Volatile (Vpr);
      pragma Import (Ada, Vpr);

   begin
      return Vpr;
   end Get_Vpr;

   ----------------
   -- Vpr_Offset --
   ----------------

   function Vpr_Offset
     (Interrupt : Ada.Interrupts.Interrupt_ID) return Address
   is
      Base   : Address;
      Step   : Address;
      Index  : Natural;
   begin

      --  Step between 2 Vpr registers
      if Interrupt in IPI_Interrupt_ID then
         Step := 16#10#;
      else
         Step := 16#20#;
      end if;

      case Interrupt is
         when IPI_Interrupt_ID =>
            Base := 16#4_10A0#;
            Index := Natural (Interrupt - IPI_Interrupt_ID'First);

         when External_Interrupt_ID =>
            Base := 16#5_0000#;
            Index := Natural (Interrupt - External_Interrupt_ID'First);

         when Internal_Interrupt_ID =>
            Base := 16#5_0200#;
            Index := Natural (Interrupt - Internal_Interrupt_ID'First);

         when Messaging_Interrupt_ID => null;
            Base := 16#5_1600#;
            Index := Natural (Interrupt - Messaging_Interrupt_ID'First);

         when Shared_Message_Interrupt_ID => null;
            Base := 16#5_1C00#;
            Index := Natural (Interrupt - Shared_Message_Interrupt_ID'First);

         when others =>
            raise Program_Error;
      end case;

      return Base + Step * Address (Index);
   end Vpr_Offset;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Initialize the OpenPIC

      declare
         Svr : Unsigned_32;
         for Svr'Address use
           System.BB.Board_Parameters.CCSRBAR + 16#4_10E0#;
         pragma Volatile (Svr);
         pragma Import (Ada, Svr);

      begin
         --  Set the spurious vector register as No_Interrupt (0), so that
         --  spurious interrupts can be easily discarded.

         Svr := 16#0000#;
      end;

      --  Enable IPI

      Set_Vpr
        (16#4_10A0#,
         Ada.Interrupts.Names.Interprocessor_Interrupt_0,
         Ada.Interrupts.Names.Interprocessor_Interrupt_0_Priority);
      Set_Vpr
        (16#4_10B0#,
         Ada.Interrupts.Names.Interprocessor_Interrupt_1,
         Ada.Interrupts.Names.Interprocessor_Interrupt_1_Priority);
      Set_Vpr
        (16#4_10C0#,
         Ada.Interrupts.Names.Interprocessor_Interrupt_2,
         Ada.Interrupts.Names.Interprocessor_Interrupt_2_Priority);
      Set_Vpr
        (16#4_10D0#,
         Ada.Interrupts.Names.Interprocessor_Interrupt_3,
         Ada.Interrupts.Names.Interprocessor_Interrupt_3_Priority);

      --  Mask interrupts

      Interrupts.Set_Current_Priority (Interrupt_Priority'Last - 1);

      CPU_Specific.Install_Exception_Handler
        (Interrupt_Handler'Address, CPU_Specific.External_Interrupt_Excp);
   end Initialize_Board;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
      use System.Machine_Code;

   begin
      if System.BB.CPU_Specific.PowerPC_Book_E then
         --  Clear TSR[DIS] on Book-E CPUs (e500)

         Asm ("mtspr 336,%0",
              Inputs => Unsigned_32'Asm_Input ("r", 2 ** (63 - 36)),
              Volatile => True);
      end if;
   end Clear_Alarm_Interrupt;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   procedure Interrupt_Handler is
      IACK : Unsigned_32
        with Import, Volatile,
        Address => Board_Parameters.CCSRBAR + 16#4_00A0#;
      --  Interrupt acknowledge register

      EOI : Unsigned_32
        with Import, Volatile,
        Address => System.BB.Board_Parameters.CCSRBAR + 16#4_00B0#;

      Iack_Val : Unsigned_32;
   begin
      Iack_Val := IACK;

      --  Spurious interrupts

      if Iack_Val = 0 then
         return;
      end if;

      BB.Interrupts.Interrupt_Wrapper (BB.Interrupts.Interrupt_ID (Iack_Val));

      EOI := 0;
   end Interrupt_Handler;

   package body Interrupts is
      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
         Offset : constant Address :=
           Vpr_Offset (Ada.Interrupts.Interrupt_ID (Interrupt));
      begin
         Set_Vpr (Offset, Ada.Interrupts.Interrupt_ID (Interrupt), Prio);
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
      is
         Vpr : constant Vector_Priority_Register :=
           Get_Vpr (Vpr_Offset (Ada.Interrupts.Interrupt_ID (Interrupt)));
      begin
         return System.Interrupt_Priority'First +
           System.Any_Priority (Vpr.Priority);
      end Priority_Of_Interrupt;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
         use System.Machine_Code;
         POW : constant Unsigned_32 := 2 ** 18;
         Msr : Unsigned_32;
      begin
         --  See sequences on:
         --
         --  PowerPc e500 Core Familiy Reference Manual
         --  Section 6.4.1 Software Considerations for Power Management

         if System.BB.CPU_Specific.PowerPC_Book_E then

            --  Read MSR and set POW/WE bit

            Asm ("mfmsr %0",
                 Outputs => Unsigned_32'Asm_Output ("=r", Msr),
                 Volatile => True);
            Msr := Msr or POW;

            Asm ("msync", Volatile => True);

            --  Set MSR

            Asm ("mtmsr %0",
                 Inputs => Unsigned_32'Asm_Input ("r", Msr),
                 Volatile => True);

            Asm ("isync", Volatile => True);
         end if;
      end Power_Down;

      --------------------------
      -- Set_Current_Priority --
      --------------------------

      procedure Set_Current_Priority (Priority : Integer) is
         CTPR : Unsigned_32;
         for CTPR'Address use System.BB.Board_Parameters.CCSRBAR + 16#4_0080#;
         pragma Volatile (CTPR);
         pragma Import (Ada, CTPR);

      begin
         --  Note that Priority cannot be the last one, as this procedure is
         --  unable to disable the decrementer interrupt.

         pragma Assert (Priority /= Interrupt_Priority'Last);

         if Priority in Interrupt_Priority then
            CTPR := Unsigned_32 (Priority - Interrupt_Priority'First);
         else
            CTPR := 0;
         end if;
      end Set_Current_Priority;
   end Interrupts;

   package body Time is separate;

   package body Multiprocessors is separate;
end System.BB.Board_Support;
