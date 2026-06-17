------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2012-2017, Free Software Foundation, Inc.         --
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

--  This is the Xilinx Ultrascale+ MPSoC version of this package

pragma Restrictions (No_Elaboration_Code);

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Software Generated Interrupts (SGI)

   SGI_0  : constant Interrupt_ID := 0; --  Reserved by the runtime
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
   Virtual_Maintenance_Interrupt      : constant Interrupt_ID := 25;
   Hypervisor_Timer_Interrupt         : constant Interrupt_ID := 26;
   Virtual_Timer_Interrupt            : constant Interrupt_ID := 27;
   Legacy_FIQ_Interrupt               : constant Interrupt_ID := 28;
   Secure_Physical_Timer_Interrupt    : constant Interrupt_ID := 29;
   Non_Secure_Physical_Time_Interrupt : constant Interrupt_ID := 30;
   Legacy_IRQ_Interrupt               : constant Interrupt_ID := 31;

   --  System Interrupts
   RPU0_AMP_Interrupt                 : constant Interrupt_ID := 40;
   RPU1_AMP_Interrupt                 : constant Interrupt_ID := 41;
   OCM_Interrupt                      : constant Interrupt_ID := 42;
   LPD_APB_Interrupt                  : constant Interrupt_ID := 43;
   RPU0_ECC_Interrupt                 : constant Interrupt_ID := 44;
   RPU1_ECC_Interrupt                 : constant Interrupt_ID := 45;
   NAND_Interrupt                     : constant Interrupt_ID := 46;
   QSPI_Interrupt                     : constant Interrupt_ID := 47;
   GPIO_Interrupt                     : constant Interrupt_ID := 48;
   I2C0_Interrupt                     : constant Interrupt_ID := 49;
   I2C1_Interrupt                     : constant Interrupt_ID := 50;
   SPI0_Interrupt                     : constant Interrupt_ID := 51;
   SPI1_Interrupt                     : constant Interrupt_ID := 52;
   UART0_Interrupt                    : constant Interrupt_ID := 53;
   UART1_Interrupt                    : constant Interrupt_ID := 54;
   CAN0_Interrupt                     : constant Interrupt_ID := 55;
   CAN1_Interrupt                     : constant Interrupt_ID := 56;
   LPD_APM_Interrupt                  : constant Interrupt_ID := 57;
   RTC_Alarm_Interrupt                : constant Interrupt_ID := 58;
   RTC_Seconds_Interrupt              : constant Interrupt_ID := 59;
   ClkMon_Interrupt                   : constant Interrupt_ID := 60;
   IPI_Ch7_Interrupt                  : constant Interrupt_ID := 61;
   IPI_Ch8_Interrupt                  : constant Interrupt_ID := 62;
   IPI_Ch9_Interrupt                  : constant Interrupt_ID := 63;
   IPI_Ch10_Interrupt                 : constant Interrupt_ID := 64;
   IPI_Ch2_Interrupt                  : constant Interrupt_ID := 65;
   IPI_Ch1_Interrupt                  : constant Interrupt_ID := 66;
   IPI_Ch0_Interrupt                  : constant Interrupt_ID := 67;
   TTC0_0_Interrupt                   : constant Interrupt_ID := 68;
   TTC0_1_Interrupt                   : constant Interrupt_ID := 69;
   TTC0_2_Interrupt                   : constant Interrupt_ID := 70;
   TTC1_0_Interrupt                   : constant Interrupt_ID := 71;
   TTC1_1_Interrupt                   : constant Interrupt_ID := 72;
   TTC1_2_Interrupt                   : constant Interrupt_ID := 73;
   TTC2_0_Interrupt                   : constant Interrupt_ID := 74;
   TTC2_1_Interrupt                   : constant Interrupt_ID := 75;
   TTC2_2_Interrupt                   : constant Interrupt_ID := 76;
   TTC3_0_Interrupt                   : constant Interrupt_ID := 77;
   TTC3_1_Interrupt                   : constant Interrupt_ID := 78;
   TTC3_2_Interrupt                   : constant Interrupt_ID := 79;
   SDIO0_Interrupt                    : constant Interrupt_ID := 80;
   SDIO1_Interrupt                    : constant Interrupt_ID := 81;
   SDIO0_Wakeup_Interrupt             : constant Interrupt_ID := 82;
   SDIO1_Wakeup_Interrupt             : constant Interrupt_ID := 83;
   LPD_SWDT_Interrupt                 : constant Interrupt_ID := 84;
   CSU_SWDT_Interrupt                 : constant Interrupt_ID := 85;
   LPD_ATB_Interrupt                  : constant Interrupt_ID := 86;
   AIB_Interrupt                      : constant Interrupt_ID := 87;
   SysMon_Interrupt                   : constant Interrupt_ID := 88;
   GEM0_Interrupt                     : constant Interrupt_ID := 89;
   GEM0_Wakeup_Interrupt              : constant Interrupt_ID := 90;
   GEM1_Interrupt                     : constant Interrupt_ID := 91;
   GEM1_Wakeup_Interrupt              : constant Interrupt_ID := 92;
   GEM2_Interrupt                     : constant Interrupt_ID := 93;
   GEM2_Wakeup_Interrupt              : constant Interrupt_ID := 94;
   GEM3_Interrupt                     : constant Interrupt_ID := 95;
   GEM3_Wakeup_Interrupt              : constant Interrupt_ID := 96;
   USB0_Endpoint0_Interrupt           : constant Interrupt_ID := 97;
   USB0_Endpoint1_Interrupt           : constant Interrupt_ID := 98;
   USB0_Endpoint2_Interrupt           : constant Interrupt_ID := 99;
   USB0_Endpoint3_Interrupt           : constant Interrupt_ID := 100;
   USB0_OTG_Interrupt                 : constant Interrupt_ID := 101;
   USB1_Endpoint0_Interrupt           : constant Interrupt_ID := 102;
   USB1_Endpoint1_Interrupt           : constant Interrupt_ID := 103;
   USB1_Endpoint2_Interrupt           : constant Interrupt_ID := 104;
   USB1_Endpoint3_Interrupt           : constant Interrupt_ID := 105;
   USB1_OTG_Interrupt                 : constant Interrupt_ID := 106;
   USB0_Wakeup_Interrupt              : constant Interrupt_ID := 107;
   USB1_Wakeup_Interrupt              : constant Interrupt_ID := 108;
   LPD_DMA_Ch0_Interrupt              : constant Interrupt_ID := 109;
   LPD_DMA_Ch1_Interrupt              : constant Interrupt_ID := 110;
   LPD_DMA_Ch2_Interrupt              : constant Interrupt_ID := 111;
   LPD_DMA_Ch3_Interrupt              : constant Interrupt_ID := 112;
   LPD_DMA_Ch4_Interrupt              : constant Interrupt_ID := 113;
   LPD_DMA_Ch5_Interrupt              : constant Interrupt_ID := 114;
   LPD_DMA_Ch6_Interrupt              : constant Interrupt_ID := 115;
   LPD_DMA_Ch7_Interrupt              : constant Interrupt_ID := 116;
   CSU_Interrupt                      : constant Interrupt_ID := 117;
   CSU_DMA_Interrupt                  : constant Interrupt_ID := 118;
   eFuse_Interrupt                    : constant Interrupt_ID := 119;
   XPPU_Interrupt                     : constant Interrupt_ID := 120;
   PL_PS_0_Interrupt                  : constant Interrupt_ID := 121;
   PL_PS_1_Interrupt                  : constant Interrupt_ID := 122;
   PL_PS_2_Interrupt                  : constant Interrupt_ID := 123;
   PL_PS_3_Interrupt                  : constant Interrupt_ID := 124;
   PL_PS_4_Interrupt                  : constant Interrupt_ID := 125;
   PL_PS_5_Interrupt                  : constant Interrupt_ID := 126;
   PL_PS_6_Interrupt                  : constant Interrupt_ID := 127;
   PL_PS_7_Interrupt                  : constant Interrupt_ID := 128;
   --  7 reserved interrupts from 129 to 135
   PL_PS_8_Interrupt                  : constant Interrupt_ID := 136;
   PL_PS_9_Interrupt                  : constant Interrupt_ID := 137;
   PL_PS_10_Interrupt                 : constant Interrupt_ID := 138;
   PL_PS_11_Interrupt                 : constant Interrupt_ID := 139;
   PL_PS_12_Interrupt                 : constant Interrupt_ID := 140;
   PL_PS_13_Interrupt                 : constant Interrupt_ID := 141;
   PL_PS_14_Interrupt                 : constant Interrupt_ID := 142;
   PL_PS_15_Interrupt                 : constant Interrupt_ID := 143;
   DDR_Interrupt                      : constant Interrupt_ID := 144;
   FPD_SWDT_Interrupt                 : constant Interrupt_ID := 145;
   PCIe_MSI0_Interrupt                : constant Interrupt_ID := 146;
   PCIe_MSI1_Interrupt                : constant Interrupt_ID := 147;
   PCIe_INTx_Interrupt                : constant Interrupt_ID := 148;
   PCIe_DMA_Interrupt                 : constant Interrupt_ID := 149;
   PCIe_MSC_Interrupt                 : constant Interrupt_ID := 150;
   DisplayPort_Interrupt              : constant Interrupt_ID := 151;
   FPD_APB_Interrupt                  : constant Interrupt_ID := 152;
   FPD_DTB_Interrupt                  : constant Interrupt_ID := 153;
   DPDMA_Interrupt                    : constant Interrupt_ID := 154;
   FPD_ATM_Interrupt                  : constant Interrupt_ID := 155;
   FPD_DMA_Ch0_Interrupt              : constant Interrupt_ID := 156;
   FPD_DMA_Ch1_Interrupt              : constant Interrupt_ID := 157;
   FPD_DMA_Ch2_Interrupt              : constant Interrupt_ID := 158;
   FPD_DMA_Ch3_Interrupt              : constant Interrupt_ID := 159;
   FPD_DMA_Ch4_Interrupt              : constant Interrupt_ID := 160;
   FPD_DMA_Ch5_Interrupt              : constant Interrupt_ID := 161;
   FPD_DMA_Ch6_Interrupt              : constant Interrupt_ID := 162;
   FPD_DMA_Ch7_Interrupt              : constant Interrupt_ID := 163;
   GPU_Interrupt                      : constant Interrupt_ID := 164;
   SATA_Interrupt                     : constant Interrupt_ID := 165;
   FPD_XMPU_Interrupt                 : constant Interrupt_ID := 166;
   APU_VCPUMNT_0_Interrupt            : constant Interrupt_ID := 167;
   APU_VCPUMNT_1_Interrupt            : constant Interrupt_ID := 168;
   APU_VCPUMNT_2_Interrupt            : constant Interrupt_ID := 169;
   APU_VCPUMNT_3_Interrupt            : constant Interrupt_ID := 170;
   CPU_CTI_0_Interrupt                : constant Interrupt_ID := 171;
   CPU_CTI_1_Interrupt                : constant Interrupt_ID := 172;
   CPU_CTI_2_Interrupt                : constant Interrupt_ID := 173;
   CPU_CTI_3_Interrupt                : constant Interrupt_ID := 174;
   PMU_Comm_0_Interrupt               : constant Interrupt_ID := 175;
   PMU_Comm_1_Interrupt               : constant Interrupt_ID := 176;
   PMU_Comm_2_Interrupt               : constant Interrupt_ID := 177;
   PMU_Comm_3_Interrupt               : constant Interrupt_ID := 178;
   APU_Comm_0_Interrupt               : constant Interrupt_ID := 179;
   APU_Comm_1_Interrupt               : constant Interrupt_ID := 180;
   APU_Comm_2_Interrupt               : constant Interrupt_ID := 181;
   APU_Comm_3_Interrupt               : constant Interrupt_ID := 182;
   L2_Cache_Interrupt                 : constant Interrupt_ID := 183;
   APU_ExtError_Interrupt             : constant Interrupt_ID := 184;
   APU_RegError_Interrupt             : constant Interrupt_ID := 185;
   CCI_Interrupt                      : constant Interrupt_ID := 186;
   SMMU_Interrupt                     : constant Interrupt_ID := 187;

end Ada.Interrupts.Names;
