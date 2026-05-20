------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              I N T E R F A C E S . X 8 6 _ 6 4 . I O A P I C             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2021, Free Software Foundation, Inc.            --
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
------------------------------------------------------------------------------

with System.BB.Parameters; use System.BB.Parameters;

package body Interfaces.X86_64.IO_APIC is

   use Ada.Interrupts;

   pragma Warnings (Off, "*not referenced");
   --  Suppress warning for unreferenced enumeration values

   ----------------------
   -- Direct Registers --
   ----------------------

   --  Addresses

   Index_Address : constant := 16#00#;
   Data_Address  : constant := 16#10#;

   --  Registers

   subtype IOAPIC_Index is Natural range 0 .. 16#3F#;

   Index : IOAPIC_Index
     with Size => 8,
       Address => System'To_Address (IO_APIC_Base_Address + Index_Address);

   Data : Unsigned_32
     with Size => 32,
       Address => System'To_Address (IO_APIC_Base_Address + Data_Address);

   ------------------------
   -- Indirect Registers --
   ------------------------

   --  Indirect Register Index

   ID       : constant := 16#00#;
   Version  : constant := 16#01#;
   Redirection_Base : constant := 16#10#;

   --  Registers

   type APIC_ID is new Interfaces.Unsigned_8;

   type Delivery_Status_Type is (Idle, Pending);

   type Remote_IRR_Type is (Reset_On_EOI, Reset_On_Ack);

   type Destination_Type is (Physical, Logical);

   type Delivery_Mode_Type is
     (Fixed, Lowest_Priority, SMI, NMI, INIT, ExtINT);

   for Delivery_Mode_Type use
     (Fixed           => 2#000#,
      Lowest_Priority => 2#001#,
      SMI             => 2#010#,
      NMI             => 2#100#,
      INIT            => 2#101#,
      ExtINT          => 2#111#);

   type Redirection_Entry (DWord_View : Boolean := False) is record
      case DWord_View is
         when False =>
            Destination      : APIC_ID;
            Masked           : Boolean;
            Trigger          : Trigger_Mode;
            Remote_IRR       : Remote_IRR_Type;
            Pin_Polarity     : Polarity;
            Delivery_Status  : Delivery_Status_Type;
            Destination_Mode : Destination_Type;
            Delivery_Mode    : Delivery_Mode_Type;
            Vector           : Interrupt_ID;
         when True =>
            Low, High        : Unsigned_32;
      end case;
   end record with Unchecked_Union, Size => 64;

   for Redirection_Entry use record
      Destination      at 0 range 56 .. 63;
      Masked           at 0 range 16 .. 16;
      Trigger          at 0 range 15 .. 15;
      Remote_IRR       at 0 range 14 .. 14;
      Pin_Polarity     at 0 range 13 .. 13;
      Delivery_Status  at 0 range 12 .. 12;
      Destination_Mode at 0 range 11 .. 11;
      Delivery_Mode    at 0 range  8 .. 10;
      Vector           at 0 range  0 .. 7;
      Low              at 0 range  0 .. 31;
      High             at 0 range 32 .. 63;
   end record;

   pragma Warnings (On, "*not referenced");

   ----------
   -- Mask --
   ----------

   procedure Mask (Interrupt : GSI_ID) is
      IRQ_Entry : Redirection_Entry;
   begin
      Index := Redirection_Base + Interrupt;

      IRQ_Entry.High := Data;
      IRQ_Entry.Low  := Data;

      IRQ_Entry.Masked := True;

      Data  := IRQ_Entry.High;
      Data  := IRQ_Entry.Low;
   end Mask;

   -----------
   -- Route --
   -----------

   procedure Route
     (Interrupt    : GSI_ID;
      To_Vector    : Ada.Interrupts.Interrupt_ID;
      IRQ_Trigger  : Trigger_Mode := Level;
      IRQ_Polarity : Polarity := Active_High)
   is
      IRQ_Entry : constant Redirection_Entry :=
        (DWord_View       => False,
         Destination      => 0,
         Masked           => True,
         Trigger          => IRQ_Trigger,
         Remote_IRR       => Reset_On_EOI,
         Pin_Polarity     => IRQ_Polarity,
         Delivery_Status  => Idle,
         Delivery_Mode    => Fixed,
         Destination_Mode => Physical,
         Vector           => To_Vector);
   begin
      Index := Redirection_Base + Interrupt;
      Data  := IRQ_Entry.High;
      Data  := IRQ_Entry.Low;
   end Route;

   ------------
   -- Unmask --
   ------------

   procedure Unmask (Interrupt : GSI_ID) is
      IRQ_Entry : Redirection_Entry;
   begin
      Index := Redirection_Base + Interrupt;

      IRQ_Entry.High := Data;
      IRQ_Entry.Low  := Data;

      IRQ_Entry.Masked := False;

      Data  := IRQ_Entry.High;
      Data  := IRQ_Entry.Low;
   end Unmask;

end Interfaces.X86_64.IO_APIC;
