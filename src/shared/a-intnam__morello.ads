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

--  This is the Morello version of this package. See section 5.3.1 of the
--  Arm Morello System Development Platform Technical Reference Manual.

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
   SPE_Interrupt                        : constant Interrupt_ID := 21;
   Debug_Communications_Interrupt       : constant Interrupt_ID := 22;
   PMU_Interrupt                        : constant Interrupt_ID := 23;
   CTI_Interrupt                        : constant Interrupt_ID := 24;
   Virtual_Maintenance_Interrupt        : constant Interrupt_ID := 25;
   Non_Secure_PL2_Timer_Event_Interrupt : constant Interrupt_ID := 26;
   Virtual_Timer_Event                  : constant Interrupt_ID := 27;
   Secure_PL1_Timer_Event_Interrupt     : constant Interrupt_ID := 29;
   Non_Secure_PL1_Timer_Event_Interrupt : constant Interrupt_ID := 30;

   --  Shared Peripheral Interrupts
   DMC0_PMU_Event_Interrupt                     : constant Interrupt_ID := 32;
   DMC0_Combined_Error_Overflow_Interrupt       : constant Interrupt_ID := 33;
   DMC0_Access_Error_Interrupt                  : constant Interrupt_ID := 34;
   DMC0_ECC_Error_Interrupt                     : constant Interrupt_ID := 35;
   DMC1_PMU_Event_Interrupt                     : constant Interrupt_ID := 36;
   DMC1_Combined_Error_Overflow_Interrupt       : constant Interrupt_ID := 37;
   DMC1_Access_Error_Interrupt                  : constant Interrupt_ID := 38;
   DMC1_ECC_Error_Interrupt                     : constant Interrupt_ID := 39;
   --  27 reserved interrupts from 40 .. 66
   MHU_Non_Secure_Interrupt                     : constant Interrupt_ID := 67;
   --  1 reserved interrupt at 68
   MCU_Secure_Interrupt                         : constant Interrupt_ID := 69;
   --  1 reserved interrupt at 70
   ETRBUFINT_Interrupt                          : constant Interrupt_ID := 71;
   PCIe_TCU_PRI_Interrupt                       : constant Interrupt_ID := 72;
   CCIX_TCU_PRI_Interrupt                       : constant Interrupt_ID := 73;
   --  4 reserved interrupts from 74 .. 77
   PMU_Count_Overflow_Interrupt                 : constant Interrupt_ID := 78;
   --  4 reserved interrupts from 79 .. 82
   STM500_Synchronization_Interrupt             : constant Interrupt_ID := 83;
   CTI2_Trigger_Output_6                        : constant Interrupt_ID := 84;
   CTI2_Trigger_Output_7                        : constant Interrupt_ID := 85;
   Trusted_Watchdog_Interrupt                   : constant Interrupt_ID := 86;
   AP_Secure_UART_Interrupt                     : constant Interrupt_ID := 87;
   --  3 reserved interrupts from 88 .. 90
   AP_REFCLK_Generic_Timer_Interrupt_Secure     : constant Interrupt_ID := 91;
   AP_REFCLK_Generic_Timer_Interrupt_Non_Secure : constant Interrupt_ID := 92;
   Watchdog_WS0_Interrupt                       : constant Interrupt_ID := 93;
   Watchdog_WS1_Interrupt                       : constant Interrupt_ID := 94;
   AP_UART0_Interrupt                           : constant Interrupt_ID := 95;
   AP_UART2_Interrupt                           : constant Interrupt_ID := 96;
   GPU_Interrupt                                : constant Interrupt_ID := 97;
   GPU_Job_Interrupt                            : constant Interrupt_ID := 98;
   GPU_MMU_Interrupt                            : constant Interrupt_ID := 99;
   GPU_Event_Interrupt                          : constant Interrupt_ID := 100;
   DPU_Global_Control_Unit_Interrupt            : constant Interrupt_ID := 101;
   DPU_AFBC_DMA_Interrupt                       : constant Interrupt_ID := 102;
   --  1 reserved interrupt at 103
   TBU_PMU_IRPT_Interrupt                       : constant Interrupt_ID := 104;
   --  2 reserved interrupts from 105 .. 106
   TCU_Event_Queue_Secure_Interrupt             : constant Interrupt_ID := 107;
   TCU_Event_Queue_Non_Secure_Interrupt         : constant Interrupt_ID := 108;
   --  1 reserved interrupt at 109
   TCU_Sync_Complete_Non_Secure_Interrupt       : constant Interrupt_ID := 110;
   TCU_Sync_Complete_Secure_Interrupt           : constant Interrupt_ID := 111;
   TCU_Global_Non_Secure_Interrupt              : constant Interrupt_ID := 112;
   TCU_Global_Secure_Interupt                   : constant Interrupt_ID := 113;
   --  1 reserved interrupt at 114
   TCU_PMU_Interrupt                            : constant Interrupt_ID := 115;
   --  10 reserved interrupts at 116 .. 125
   DSU_PMU_Interrupt_1                          : constant Interrupt_ID := 126;
   DSU_PMU_Interrupt_2                          : constant Interrupt_ID := 127;
   AP_External_Interrupt                        : constant Interrupt_ID := 128;
   AP_External_Ethernet_Interrupt               : constant Interrupt_ID := 129;
   --  38 reserved interrupts from 130 .. 167
   GPIO_IOFPGA_Combined_Interrupt_1             : constant Interrupt_ID := 168;
   GPIO_IOFPGA_Combined_Interrupt_2             : constant Interrupt_ID := 169;
   GPIO_IOFPGA_Combined_Interrupt_3             : constant Interrupt_ID := 170;
   GPIO_IOFPGA_Combined_Interrupt_4             : constant Interrupt_ID := 171;
   GPIO_IOFPGA_Combined_Interrupt_5             : constant Interrupt_ID := 172;
   GPIO_IOFPGA_Combined_Interrupt_6             : constant Interrupt_ID := 173;
   GPIO_IOFPGA_Combined_Interrupt_7             : constant Interrupt_ID := 174;
   GPIO_IOFPGA_Combined_Interrupt_8             : constant Interrupt_ID := 175;
   GPIO_IOFPGA_Combined_Interrupt_9             : constant Interrupt_ID := 176;
   --  34 reserved interrupts from 177 .. 200
   PCIe_INTA_Out_Interrupt                      : constant Interrupt_ID := 201;
   PCIe_INTB_Out_Interrupt                      : constant Interrupt_ID := 202;
   PCIe_INTC_Out_Interrupt                      : constant Interrupt_ID := 203;
   PCIe_INTD_Out_Interrupt                      : constant Interrupt_ID := 204;
   PCIe_PHY_Interrupt_Out_Interrupt             : constant Interrupt_ID := 205;
   PCIe_AER_Interrupt                           : constant Interrupt_ID := 206;
   PCIe_Link_Down_Reset_Out_Interrupt           : constant Interrupt_ID := 207;
   PCIe_Local_Interrupt_Reset_Interrupt         : constant Interrupt_ID := 208;
   PCIe_Performance_Data_Threshold_Interrupt    : constant Interrupt_ID := 209;
   PCIe_Negotiated_Speed_Change_Interrupt       : constant Interrupt_ID := 210;
   PCIe_Link_Training_Done_Interrupt            : constant Interrupt_ID := 211;
   PCIe_PLL_status_Rise_Interrupt               : constant Interrupt_ID := 212;
   PCIe_Message_FIFO_Interrupt                  : constant Interrupt_ID := 213;
   PCIe_Local_Interrupt_RAS_Interrupt           : constant Interrupt_ID := 214;
   PCIe_PHY_Lane_Interrupt                      : constant Interrupt_ID := 215;
   --  16 reserved interrupts from 216 .. 231
   CCIX_Bus_Device_Change_Interrupt             : constant Interrupt_ID := 232;
   CCIX_INTA_Out_Interrupt                      : constant Interrupt_ID := 233;
   CCIX_INTB_Out_Interrupt                      : constant Interrupt_ID := 234;
   CCIX_INTC_Out_Interrupt                      : constant Interrupt_ID := 235;
   CCIX_INTD_Out_Interrupt                      : constant Interrupt_ID := 236;
   CCIX_PHY_Interrupt_Out_Interrupt             : constant Interrupt_ID := 237;
   CCIX_AER_Interrupt                           : constant Interrupt_ID := 238;
   CCIX_Link_Down_Reset_Out_Interrupt           : constant Interrupt_ID := 239;
   CCIX_Local_Interrupt_Reset_Interrupt         : constant Interrupt_ID := 240;
   CCIX_Performance_Data_Threshold_Interrupt    : constant Interrupt_ID := 241;
   CCIX_Negotiated_Speed_Change_Interrupt       : constant Interrupt_ID := 242;
   CCIX_Link_Training_Done_Interrupt            : constant Interrupt_ID := 243;
   CCIX_PLL_Status_Rise_Interrupt               : constant Interrupt_ID := 244;
   CCIX_Message_FIFO_Interrupt                  : constant Interrupt_ID := 245;
   CCIX_Local_Interrupt_RAS_Interrupt           : constant Interrupt_ID := 246;
   CCIX_Hot_Reset_Interrupt                     : constant Interrupt_ID := 247;
   CCIX_FLR_Reset_Interrupt                     : constant Interrupt_ID := 248;
   CCIX_Power_State_Change_Interrupt            : constant Interrupt_ID := 249;
   CCIX_PHY_Lane_Interrupt                      : constant Interrupt_ID := 250;
   --  5 reserved interrupts from 251 .. 255
   MMUTCU1_PMU_Interrupt                        : constant Interrupt_ID := 256;
   MMUTCU1_Event_Queue_Secure_Interrupt         : constant Interrupt_ID := 257;
   MMUTCU1_SYNC_Complete_Secure_Interrupt       : constant Interrupt_ID := 258;
   MMUTCU1_Global_Secure_Interrupt              : constant Interrupt_ID := 259;
   MMUTCU1_Event_Queue_Non_Secure_Interrupt     : constant Interrupt_ID := 260;
   MMUTCU1_SYNC_Complete_Non_Secure_Interrupt   : constant Interrupt_ID := 261;
   MMUTCU1_Global_Non_Secure_Interrupt          : constant Interrupt_ID := 262;
   MMUTCU2_PMU_Interrupt                        : constant Interrupt_ID := 263;
   MMUTCU2_Event_Queue_Secure_Interrupt         : constant Interrupt_ID := 264;
   MMUTCU2_SYNC_Complete_Secure_Interrupt       : constant Interrupt_ID := 265;
   MMUTCU2_Global_Secure_Interrupt              : constant Interrupt_ID := 266;
   MMUTCU2_Event_Queue_Non_Secure_Interrupt     : constant Interrupt_ID := 267;
   MMUTCU2_SYNC_Complete_Non_Secure_Interrupt   : constant Interrupt_ID := 268;
   MMUTCU2_Global_Non_Secure_Interrupt          : constant Interrupt_ID := 269;
   --  50 reserved interrupts from 270 .. 319
   MMUTBU_PMU_IRPT0_Interrupt                   : constant Interrupt_ID := 320;
   MMUTBU_PMU_IRPT1_Interrupt                   : constant Interrupt_ID := 321;
   MMUTBU_PMU_IRPT2_Interrupt                   : constant Interrupt_ID := 322;
   MMUTBU_PMU_IRPT3_Interrupt                   : constant Interrupt_ID := 323;
   --  188 reserved interrupts from 324 .. 511
   CLUSTER0_SCP_To_AP_MHU_Non_Secure_Interrupt  : constant Interrupt_ID := 512;
   CLUSTER0_SCP_To_AP_MHU_Secure_Interrupt      : constant Interrupt_ID := 513;
   CLUSTER1_SCP_To_AP_MHU_Non_Secure_Interrupt  : constant Interrupt_ID := 514;
   CLUSTER1_SCP_To_AP_MHU_Secure_Interrupt      : constant Interrupt_ID := 515;
   --  60 reserved interrupts from 516 .. 575
   P0_REFCLK_Generic_Secure_Timer_Interrupt_0   : constant Interrupt_ID := 576;
   P0_REFCLK_Generic_Secure_Timer_Interrupt_1   : constant Interrupt_ID := 577;

end Ada.Interrupts.Names;
