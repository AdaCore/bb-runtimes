------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2012-2020, Free Software Foundation, Inc.         --
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

--  This package version is specific to the zynq target

pragma Restrictions (No_Elaboration_Code);

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Software Generated Interrupts (SGI)

   SGI_0  : constant Interrupt_ID := 0;  --  Reserved by the runtime
   SGI_1  : constant Interrupt_ID := 1;
   SGI_2  : constant Interrupt_ID := 2;
   SGI_3  : constant Interrupt_ID := 3;
   SGI_4  : constant Interrupt_ID := 4;
   SGI_5  : constant Interrupt_ID := 5;
   SGI_6  : constant Interrupt_ID := 6;
   SGI_7  : constant Interrupt_ID := 7;
   SGI_8  : constant Interrupt_ID := 8;
   SGI_9  : constant Interrupt_ID := 9;
   SGI_10 : constant Interrupt_ID := 10;
   SGI_11 : constant Interrupt_ID := 11;
   SGI_12 : constant Interrupt_ID := 12;
   SGI_13 : constant Interrupt_ID := 13;
   SGI_14 : constant Interrupt_ID := 14;
   SGI_15 : constant Interrupt_ID := 15;

   --  Private Peripheral Interrupts (PPI)

   Global_Timer_Interrupt      : constant Interrupt_ID := 27;
   FIQ_Interrupt               : constant Interrupt_ID := 28;
   CPU_Private_Timer_Interrupt : constant Interrupt_ID := 29;
   Private_Watchdog_Interrupt  : constant Interrupt_ID := 30;
   IRQ_Interrupt               : constant Interrupt_ID := 31;

   --  PS and PL Shared Peripheral Interrupts
   APU_CPU0_Interrupt     : constant Interrupt_ID := 32;
   APU_CPU1_Interrupt     : constant Interrupt_ID := 33;
   L2_Cache_Interrupt     : constant Interrupt_ID := 34;
   OCM_Interrupt          : constant Interrupt_ID := 35;
   Reserved_36_Interrupt  : constant Interrupt_ID := 36;
   PMU0_Interrupt         : constant Interrupt_ID := 37;
   PMU1_Interrupt         : constant Interrupt_ID := 38;
   XADC_Interrupt         : constant Interrupt_ID := 39;
   DEVC_Interrupt         : constant Interrupt_ID := 40;
   SWDT_Interrupt         : constant Interrupt_ID := 41;
   TTC0_0_Interrupt       : constant Interrupt_ID := 42;
   TTC0_1_Interrupt       : constant Interrupt_ID := 43;
   TTC0_2_Interrupt       : constant Interrupt_ID := 44;
   DMAC_Abort_Interrupt   : constant Interrupt_ID := 45;
   DMAC_0_Interrupt       : constant Interrupt_ID := 46;
   DMAC_1_Interrupt       : constant Interrupt_ID := 47;
   DMAC_2_Interrupt       : constant Interrupt_ID := 48;
   DMAC_3_Interrupt       : constant Interrupt_ID := 49;
   SMC_Interrupt          : constant Interrupt_ID := 50;
   QSPI_Interrupt         : constant Interrupt_ID := 51;
   GPIO_Interrupt         : constant Interrupt_ID := 52;
   USB0_Interrupt         : constant Interrupt_ID := 53;
   ETH0_Interrupt         : constant Interrupt_ID := 54;
   ETH0_Wake_Up_Interrupt : constant Interrupt_ID := 55;
   SDIO0_Interrupt        : constant Interrupt_ID := 56;
   I2C0_Interrupt         : constant Interrupt_ID := 57;
   SPI0_Interrupt         : constant Interrupt_ID := 58;
   UART0_Interrupt        : constant Interrupt_ID := 59;
   CAN0_Interrupt         : constant Interrupt_ID := 60;
   PL0_Interrupt          : constant Interrupt_ID := 61;
   PL1_Interrupt          : constant Interrupt_ID := 62;
   PL2_Interrupt          : constant Interrupt_ID := 63;
   PL3_Interrupt          : constant Interrupt_ID := 64;
   PL4_Interrupt          : constant Interrupt_ID := 65;
   PL5_Interrupt          : constant Interrupt_ID := 66;
   PL6_Interrupt          : constant Interrupt_ID := 67;
   PL7_Interrupt          : constant Interrupt_ID := 68;
   TTC1_0_Interrupt       : constant Interrupt_ID := 69;
   TTC1_1_Interrupt       : constant Interrupt_ID := 70;
   TTC1_2_Interrupt       : constant Interrupt_ID := 71;
   DMAC_4_Interrupt       : constant Interrupt_ID := 72;
   DMAC_5_Interrupt       : constant Interrupt_ID := 73;
   DMAC_6_Interrupt       : constant Interrupt_ID := 74;
   DMAC_7_Interrupt       : constant Interrupt_ID := 75;
   USB1_Interrupt         : constant Interrupt_ID := 76;
   ETH1_Interrupt         : constant Interrupt_ID := 77;
   ETH1_Wake_Up_Interrupt : constant Interrupt_ID := 78;
   SDIO1_Interrupt        : constant Interrupt_ID := 79;
   I2C1_Interrupt         : constant Interrupt_ID := 80;
   SPI1_Interrupt         : constant Interrupt_ID := 81;
   UART1_Interrupt        : constant Interrupt_ID := 82;
   CAN1_Interrupt         : constant Interrupt_ID := 83;
   PL8_Interrupt          : constant Interrupt_ID := 84;
   PL9_Interrupt          : constant Interrupt_ID := 85;
   PL10_Interrupt         : constant Interrupt_ID := 86;
   PL11_Interrupt         : constant Interrupt_ID := 87;
   PL12_Interrupt         : constant Interrupt_ID := 88;
   PL13_Interrupt         : constant Interrupt_ID := 89;
   PL14_Interrupt         : constant Interrupt_ID := 90;
   PL15_Interrupt         : constant Interrupt_ID := 91;
   SCU_Parity_Interrupt   : constant Interrupt_ID := 92;
   Reserved_93_Interrupt  : constant Interrupt_ID := 93;
   Reserved_94_Interrupt  : constant Interrupt_ID := 94;
   Reserved_95_Interrupt  : constant Interrupt_ID := 95;
end Ada.Interrupts.Names;
