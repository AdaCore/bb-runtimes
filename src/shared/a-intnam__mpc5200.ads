------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2011-2020, Free Software Foundation, Inc.         --
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

--  Definitions for the MPC5200B

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Interrupt IDs are assigned in order of the mask register fields, with
   --  Main and Critical Interrupts following Peripheral Interrupts. See
   --  Table 7-4 and Table 7-9 of the MPC5200B User Manual.

   subtype Peripheral_Interrupt_ID is Interrupt_ID range 0 .. 23;
   subtype Main_Interrupt_ID is Interrupt_ID range 24 .. 40;
   subtype Critical_Interrupt_ID is Interrupt_ID range 41 .. 44;

   BestComm            : constant Peripheral_Interrupt_ID := 0;
   PSC1                : constant Peripheral_Interrupt_ID := 1;
   PSC2                : constant Peripheral_Interrupt_ID := 2;
   PSC3                : constant Peripheral_Interrupt_ID := 3;
   PSC6                : constant Peripheral_Interrupt_ID := 4;
   Ethernet            : constant Peripheral_Interrupt_ID := 5;
   USB                 : constant Peripheral_Interrupt_ID := 6;
   ATA                 : constant Peripheral_Interrupt_ID := 7;
   PCI_Control_Module  : constant Peripheral_Interrupt_ID := 8;
   PCI_SC_Initiator_RX : constant Peripheral_Interrupt_ID := 9;
   PCI_SC_Initiator_TX : constant Peripheral_Interrupt_ID := 10;
   PSC4                : constant Peripheral_Interrupt_ID := 11;
   PSC5                : constant Peripheral_Interrupt_ID := 12;
   SPI_MODF            : constant Peripheral_Interrupt_ID := 13;
   SPI_SPIF            : constant Peripheral_Interrupt_ID := 14;
   I2C1                : constant Peripheral_Interrupt_ID := 15;
   I2C2                : constant Peripheral_Interrupt_ID := 16;
   CAN1                : constant Peripheral_Interrupt_ID := 17;
   CAN2                : constant Peripheral_Interrupt_ID := 18;
   XLB_Arbiter         : constant Peripheral_Interrupt_ID := 21;
   BDLC                : constant Peripheral_Interrupt_ID := 22;
   BestComm_LocalPlus  : constant Peripheral_Interrupt_ID := 23;

   Slice_Timer_1 : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 0;
   IRQ1          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 1;
   IRQ2          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 2;
   IRQ3          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 3;
   LO_INT        : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 4;
   --  Note: LO_INT is reserved for runtime use only
   RTC_PINT      : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 5;
   RTC_SINT      : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 6;
   GPIO_STD      : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 7;
   GPIO_WKUP     : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 8;
   TMR0          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 9;
   TMR1          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 10;
   TMR2          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 11;
   TMR3          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 12;
   TMR4          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 13;
   TMR5          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 14;
   TMR6          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 15;
   TMR7          : constant Main_Interrupt_ID := Main_Interrupt_ID'First + 16;

   IRQ0          : constant Critical_Interrupt_ID :=
                     Critical_Interrupt_ID'First + 0;
   Slice_Timer_0 : constant Critical_Interrupt_ID :=
                     Critical_Interrupt_ID'First + 1;
   HI_INT        : constant Critical_Interrupt_ID :=
                     Critical_Interrupt_ID'First + 2;
   --  Note: HI_INT is reserved for runtime use only
   Wake_Up       : constant Critical_Interrupt_ID :=
                     Critical_Interrupt_ID'First + 3;

end Ada.Interrupts.Names;
