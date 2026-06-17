------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              I N T E R F A C E S . X 8 6 _ 6 4 . I O A P I C             --
--                                                                          --
--                                 S p e c                                  --
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

--  This package facilitates the mapping of Global System Interrupts (GSI)
--  from an I/O APIC to Local APIC Vectors. Only a single I/O APIC is supported
--  and all GSI are routed to the Local APIC with the ID of 0 (usually the
--  Local APIC for the boot CPU).

with Ada.Interrupts;

package Interfaces.X86_64.IO_APIC is

   subtype GSI_ID is Natural range 0 .. 23;
   --  Global System Interrupts IDs

   IRQ0  : constant GSI_ID := 0;
   IRQ1  : constant GSI_ID := 1;
   IRQ2  : constant GSI_ID := 2;
   IRQ3  : constant GSI_ID := 3;
   IRQ4  : constant GSI_ID := 4;
   IRQ5  : constant GSI_ID := 5;
   IRQ6  : constant GSI_ID := 6;
   IRQ7  : constant GSI_ID := 7;
   IRQ8  : constant GSI_ID := 8;
   IRQ9  : constant GSI_ID := 9;
   IRQ10 : constant GSI_ID := 10;
   IRQ11 : constant GSI_ID := 11;
   IRQ12 : constant GSI_ID := 12;
   IRQ13 : constant GSI_ID := 13;
   IRQ14 : constant GSI_ID := 14;
   IRQ15 : constant GSI_ID := 15;
   IRQ16 : constant GSI_ID := 16;
   IRQ17 : constant GSI_ID := 17;
   IRQ18 : constant GSI_ID := 18;
   IRQ19 : constant GSI_ID := 19;
   IRQ20 : constant GSI_ID := 20;
   IRQ21 : constant GSI_ID := 21;
   IRQ22 : constant GSI_ID := 22;
   IRQ23 : constant GSI_ID := 23;
   --  Interrupt names

   type Trigger_Mode is (Edge, Level);
   --  Triggering mode for the GSI

   type Polarity is (Active_High, Active_Low);
   --  The pin polarity for a level trigged interrupt

   procedure Route
     (Interrupt    : GSI_ID;
      To_Vector    : Ada.Interrupts.Interrupt_ID;
      IRQ_Trigger  : Trigger_Mode := Level;
      IRQ_Polarity : Polarity := Active_High);
   --  Route the GSI to Local APIC Vector. The interrupt is masked and needs to
   --  be explicitly unmasked using the corresponding procedure. The interrupt
   --  trigger mode and pin polarity can be configured. Interrupts are
   --  configured to be routed to the Local APIC with with the ID of 0 (usually
   --  the Local APIC for the boot CPU).

   procedure Mask (Interrupt : GSI_ID);
   procedure Unmask (Interrupt : GSI_ID);
   --  Mask and unmask the specified interrupt in the I/O APIC

end Interfaces.X86_64.IO_APIC;
