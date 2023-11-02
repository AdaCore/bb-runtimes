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

--  This is the TI AM64x/AM263x Arm Cortex-R5 version of this package

pragma Restrictions (No_Elaboration_Code);

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   RTI8_INTR_WWD_0                              : constant Interrupt_ID := 0;
   --  R5FSS0 Core 0

   RTI9_INTR_WWD_0                              : constant Interrupt_ID := 0;
   --  R5FSS0 Core 1

   RTI10_INTR_WWD_0                             : constant Interrupt_ID := 0;
   --  R5FSS1 Core 0

   RTI11_INTR_WWD_0                             : constant Interrupt_ID := 0;
   --  R5FSS1 Core 1

   DMSC0_AES_0_HIB_PUBLIC_0                     : constant Interrupt_ID := 1;
   DMSC0_AES_0_HIB_SECURE_0                     : constant Interrupt_ID := 2;
   DMSC0_DBG_AUTH_0_DEBUG_AUTH_INTR_0           : constant Interrupt_ID := 3;
   --  All cores

   R5FSS0_CORE_0_EXP_INTR_0                     : constant Interrupt_ID := 4;
   R5FSS0_COMMON0_COMMRX_LEVEL_0_0              : constant Interrupt_ID := 5;
   R5FSS0_COMMON0_COMMTX_LEVEL_0_0              : constant Interrupt_ID := 6;
   --  R5FSS0 Core 0

   R5FSS0_CORE_1_EXP_INTR_0                     : constant Interrupt_ID := 4;
   R5FSS0_COMMON0_COMMRX_LEVEL_1_0              : constant Interrupt_ID := 5;
   R5FSS0_COMMON0_COMMTX_LEVEL_1_0              : constant Interrupt_ID := 6;
   --  R5FSS0 Core 1

   R5FSS1_CORE_0_EXP_INTR_0                     : constant Interrupt_ID := 4;
   R5FSS1_COMMON0_COMMRX_LEVEL_0_0              : constant Interrupt_ID := 5;
   R5FSS1_COMMON0_COMMTX_LEVEL_0_0              : constant Interrupt_ID := 6;
   --  R5FSS1 Core 0

   R5FSS1_CORE_1_EXP_INTR_0                     : constant Interrupt_ID := 4;
   R5FSS1_COMMON0_COMMRX_LEVEL_1_0              : constant Interrupt_ID := 5;
   R5FSS1_COMMON0_COMMTX_LEVEL_1_0              : constant Interrupt_ID := 6;
   --  R5FSS1 Core 1

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_72       : constant Interrupt_ID := 8;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_73       : constant Interrupt_ID := 9;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_74       : constant Interrupt_ID := 10;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_75       : constant Interrupt_ID := 11;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_76       : constant Interrupt_ID := 12;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_77       : constant Interrupt_ID := 13;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_78       : constant Interrupt_ID := 14;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_79       : constant Interrupt_ID := 15;
   --  R5FSS0 Core 0

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_80       : constant Interrupt_ID := 8;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_81       : constant Interrupt_ID := 9;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_82       : constant Interrupt_ID := 10;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_83       : constant Interrupt_ID := 11;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_84       : constant Interrupt_ID := 12;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_85       : constant Interrupt_ID := 13;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_86       : constant Interrupt_ID := 14;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_87       : constant Interrupt_ID := 15;
   --  R5FSS0 Core 1

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_120      : constant Interrupt_ID := 8;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_121      : constant Interrupt_ID := 9;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_122      : constant Interrupt_ID := 10;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_123      : constant Interrupt_ID := 11;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_124      : constant Interrupt_ID := 12;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_125      : constant Interrupt_ID := 13;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_126      : constant Interrupt_ID := 14;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_127      : constant Interrupt_ID := 15;
   --  R5FSS1 Core 0

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_128      : constant Interrupt_ID := 8;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_129      : constant Interrupt_ID := 9;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_130      : constant Interrupt_ID := 10;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_131      : constant Interrupt_ID := 11;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_132      : constant Interrupt_ID := 12;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_133      : constant Interrupt_ID := 13;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_134      : constant Interrupt_ID := 14;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_135      : constant Interrupt_ID := 15;
   --  R5FSS1 Core 1

   FSIRX0_FSI_RX_OINT1_0                        : constant Interrupt_ID := 16;
   FSIRX0_FSI_RX_OINT2_0                        : constant Interrupt_ID := 17;
   FSIRX1_FSI_RX_OINT1_0                        : constant Interrupt_ID := 18;
   FSIRX1_FSI_RX_OINT2_0                        : constant Interrupt_ID := 19;
   FSIRX2_FSI_RX_OINT1_0                        : constant Interrupt_ID := 20;
   FSIRX2_FSI_RX_OINT2_0                        : constant Interrupt_ID := 21;
   FSIRX3_FSI_RX_OINT1_0                        : constant Interrupt_ID := 22;
   FSIRX3_FSI_RX_OINT2_0                        : constant Interrupt_ID := 23;
   FSIRX4_FSI_RX_OINT1_0                        : constant Interrupt_ID := 24;
   FSIRX4_FSI_RX_OINT2_0                        : constant Interrupt_ID := 25;
   FSIRX5_FSI_RX_OINT1_0                        : constant Interrupt_ID := 26;
   FSIRX5_FSI_RX_OINT2_0                        : constant Interrupt_ID := 27;
   FSITX0_FSI_TX_OINT1_0                        : constant Interrupt_ID := 28;
   FSITX0_FSI_TX_OINT2_0                        : constant Interrupt_ID := 29;
   FSITX1_FSI_TX_OINT1_0                        : constant Interrupt_ID := 30;
   FSITX1_FSI_TX_OINT2_0                        : constant Interrupt_ID := 31;
   --  All cores

   MAIN_GPIOMUX_INTROUTER0_OUTP_0               : constant Interrupt_ID := 32;
   MAIN_GPIOMUX_INTROUTER0_OUTP_1               : constant Interrupt_ID := 33;
   MAIN_GPIOMUX_INTROUTER0_OUTP_2               : constant Interrupt_ID := 34;
   MAIN_GPIOMUX_INTROUTER0_OUTP_3               : constant Interrupt_ID := 35;
   MAIN_GPIOMUX_INTROUTER0_OUTP_4               : constant Interrupt_ID := 36;
   MAIN_GPIOMUX_INTROUTER0_OUTP_5               : constant Interrupt_ID := 37;
   MAIN_GPIOMUX_INTROUTER0_OUTP_6               : constant Interrupt_ID := 38;
   MAIN_GPIOMUX_INTROUTER0_OUTP_7               : constant Interrupt_ID := 39;
   MAIN_GPIOMUX_INTROUTER0_OUTP_8               : constant Interrupt_ID := 40;
   MAIN_GPIOMUX_INTROUTER0_OUTP_9               : constant Interrupt_ID := 41;
   MAIN_GPIOMUX_INTROUTER0_OUTP_10              : constant Interrupt_ID := 42;
   MAIN_GPIOMUX_INTROUTER0_OUTP_11              : constant Interrupt_ID := 43;
   MAIN_GPIOMUX_INTROUTER0_OUTP_12              : constant Interrupt_ID := 44;
   MAIN_GPIOMUX_INTROUTER0_OUTP_13              : constant Interrupt_ID := 45;
   MAIN_GPIOMUX_INTROUTER0_OUTP_14              : constant Interrupt_ID := 46;
   MAIN_GPIOMUX_INTROUTER0_OUTP_15              : constant Interrupt_ID := 47;
   --  All cores

   CMPEVENT_INTROUTER0_OUTP_16                  : constant Interrupt_ID := 48;
   CMPEVENT_INTROUTER0_OUTP_17                  : constant Interrupt_ID := 49;
   CMPEVENT_INTROUTER0_OUTP_18                  : constant Interrupt_ID := 50;
   CMPEVENT_INTROUTER0_OUTP_19                  : constant Interrupt_ID := 51;
   CMPEVENT_INTROUTER0_OUTP_20                  : constant Interrupt_ID := 52;
   CMPEVENT_INTROUTER0_OUTP_21                  : constant Interrupt_ID := 53;
   CMPEVENT_INTROUTER0_OUTP_22                  : constant Interrupt_ID := 54;
   CMPEVENT_INTROUTER0_OUTP_23                  : constant Interrupt_ID := 55;
   --  R5FSS0 Core 0 and Core 1

   CMPEVENT_INTROUTER0_OUTP_24                  : constant Interrupt_ID := 48;
   CMPEVENT_INTROUTER0_OUTP_25                  : constant Interrupt_ID := 49;
   CMPEVENT_INTROUTER0_OUTP_26                  : constant Interrupt_ID := 50;
   CMPEVENT_INTROUTER0_OUTP_27                  : constant Interrupt_ID := 51;
   CMPEVENT_INTROUTER0_OUTP_28                  : constant Interrupt_ID := 52;
   CMPEVENT_INTROUTER0_OUTP_29                  : constant Interrupt_ID := 53;
   CMPEVENT_INTROUTER0_OUTP_30                  : constant Interrupt_ID := 54;
   CMPEVENT_INTROUTER0_OUTP_31                  : constant Interrupt_ID := 55;
   --  R5FSS1 Core 0 and Core 1

   DMSC0_CORTEX_M3_0_SEC_OUT_0                  : constant Interrupt_ID := 56;
   DMSC0_CORTEX_M3_0_SEC_OUT_1                  : constant Interrupt_ID := 57;
   --  All cores

   R5FSS0_CORE0_PMU_0                           : constant Interrupt_ID := 58;
   R5FSS0_CORE0_VALFIQ_0                        : constant Interrupt_ID := 59;
   R5FSS0_CORE0_VALIRQ_0                        : constant Interrupt_ID := 60;
   --  R5FSS0 Core 0

   R5FSS0_CORE1_PMU_0                           : constant Interrupt_ID := 58;
   R5FSS0_CORE1_VALFIQ_0                        : constant Interrupt_ID := 59;
   R5FSS0_CORE1_VALIRQ_0                        : constant Interrupt_ID := 60;
   --  R5FSS0 Core 1

   R5FSS1_CORE0_PMU_0                           : constant Interrupt_ID := 58;
   R5FSS1_CORE0_VALFIQ_0                        : constant Interrupt_ID := 59;
   R5FSS1_CORE0_VALIRQ_0                        : constant Interrupt_ID := 60;
   --  R5FSS0 Core 0

   R5FSS1_CORE1_PMU_0                           : constant Interrupt_ID := 58;
   R5FSS1_CORE1_VALFIQ_0                        : constant Interrupt_ID := 59;
   R5FSS1_CORE1_VALIRQ_0                        : constant Interrupt_ID := 60;
   --  R5FSS1 Core 1

   MCU_I2C0_POINTRPEND_0                        : constant Interrupt_ID := 61;
   MCU_I2C1_POINTRPEND_0                        : constant Interrupt_ID := 62;
   MCSPI3_INTR_SPI_0                            : constant Interrupt_ID := 63;
   --  All Cores

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_40       : constant Interrupt_ID := 64;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_41       : constant Interrupt_ID := 65;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_42       : constant Interrupt_ID := 66;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_43       : constant Interrupt_ID := 67;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_44       : constant Interrupt_ID := 68;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_45       : constant Interrupt_ID := 69;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_46       : constant Interrupt_ID := 70;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_47       : constant Interrupt_ID := 71;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_48       : constant Interrupt_ID := 72;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_49       : constant Interrupt_ID := 73;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_50       : constant Interrupt_ID := 74;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_51       : constant Interrupt_ID := 75;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_52       : constant Interrupt_ID := 76;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_53       : constant Interrupt_ID := 77;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_54       : constant Interrupt_ID := 78;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_55       : constant Interrupt_ID := 79;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_56       : constant Interrupt_ID := 80;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_57       : constant Interrupt_ID := 81;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_58       : constant Interrupt_ID := 82;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_59       : constant Interrupt_ID := 83;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_60       : constant Interrupt_ID := 84;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_61       : constant Interrupt_ID := 85;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_62       : constant Interrupt_ID := 86;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_63       : constant Interrupt_ID := 87;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_64       : constant Interrupt_ID := 88;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_65       : constant Interrupt_ID := 89;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_66       : constant Interrupt_ID := 90;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_67       : constant Interrupt_ID := 91;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_68       : constant Interrupt_ID := 92;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_69       : constant Interrupt_ID := 93;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_70       : constant Interrupt_ID := 94;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_71       : constant Interrupt_ID := 95;
   --  R5FSS0 Core 0 and Core 1

   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_88       : constant Interrupt_ID := 64;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_89       : constant Interrupt_ID := 65;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_90       : constant Interrupt_ID := 66;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_91       : constant Interrupt_ID := 67;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_92       : constant Interrupt_ID := 68;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_93       : constant Interrupt_ID := 69;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_94       : constant Interrupt_ID := 70;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_95       : constant Interrupt_ID := 71;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_96       : constant Interrupt_ID := 72;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_97       : constant Interrupt_ID := 73;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_98       : constant Interrupt_ID := 74;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_99       : constant Interrupt_ID := 75;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_100      : constant Interrupt_ID := 76;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_101      : constant Interrupt_ID := 77;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_102      : constant Interrupt_ID := 78;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_103      : constant Interrupt_ID := 79;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_104      : constant Interrupt_ID := 80;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_105      : constant Interrupt_ID := 81;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_106      : constant Interrupt_ID := 82;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_107      : constant Interrupt_ID := 83;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_108      : constant Interrupt_ID := 84;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_109      : constant Interrupt_ID := 85;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_110      : constant Interrupt_ID := 86;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_111      : constant Interrupt_ID := 87;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_112      : constant Interrupt_ID := 88;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_113      : constant Interrupt_ID := 89;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_114      : constant Interrupt_ID := 90;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_115      : constant Interrupt_ID := 91;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_116      : constant Interrupt_ID := 92;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_117      : constant Interrupt_ID := 93;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_118      : constant Interrupt_ID := 94;
   DMASS0_INTAGGR_0_INTAGGR_VINTR_PEND_119      : constant Interrupt_ID := 95;
   --  R5FSS1 Core 0 and Core 1

   MAILBOX0_MAILBOX_CLUSTER_0_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 96;
   MAILBOX0_MAILBOX_CLUSTER_1_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 97;
   MAILBOX0_MAILBOX_CLUSTER_2_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 98;
   MAILBOX0_MAILBOX_CLUSTER_3_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 99;
   MAILBOX0_MAILBOX_CLUSTER_6_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 100;
   --  R5FSS0 Core 0

   MAILBOX0_MAILBOX_CLUSTER_0_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 96;
   MAILBOX0_MAILBOX_CLUSTER_1_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 97;
   MAILBOX0_MAILBOX_CLUSTER_2_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 98;
   MAILBOX0_MAILBOX_CLUSTER_3_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 99;
   MAILBOX0_MAILBOX_CLUSTER_6_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 100;
   --  R5FSS0 Core 1

   MAILBOX0_MAILBOX_CLUSTER_0_MAILBOX_CLUSTER_PEND_2 :
     constant Interrupt_ID := 96;
   MAILBOX0_MAILBOX_CLUSTER_1_MAILBOX_CLUSTER_PEND_2 :
     constant Interrupt_ID := 97;
   MAILBOX0_MAILBOX_CLUSTER_4_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 98;
   MAILBOX0_MAILBOX_CLUSTER_5_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 99;
   MAILBOX0_MAILBOX_CLUSTER_7_MAILBOX_CLUSTER_PEND_0 :
     constant Interrupt_ID := 100;
   --  R5FSS1 Core 0

   MAILBOX0_MAILBOX_CLUSTER_0_MAILBOX_CLUSTER_PEND_3 :
     constant Interrupt_ID := 96;
   MAILBOX0_MAILBOX_CLUSTER_1_MAILBOX_CLUSTER_PEND_3 :
     constant Interrupt_ID := 97;
   MAILBOX0_MAILBOX_CLUSTER_4_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 98;
   MAILBOX0_MAILBOX_CLUSTER_5_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 99;
   MAILBOX0_MAILBOX_CLUSTER_7_MAILBOX_CLUSTER_PEND_1 :
     constant Interrupt_ID := 100;
   --  R5FSS1 Core 0

   GLUELOGIC_MAINRESET_REQUEST_GLUE_MAIN_RESETZ_SYNC_STRETCH_0 :
     constant Interrupt_ID := 101;
   GLUELOGIC_MAINRESET_REQUEST_GLUE_MAIN_PORZ_SYNC_STRETCH_0 :
     constant Interrupt_ID := 102;
   --  All cores

   PCIE0_PCIE_DPA_PULSE_0                       : constant Interrupt_ID := 103;
   --  All cores

   MCU_MCU_GPIOMUX_INTROUTER0_OUTP_0            : constant Interrupt_ID := 104;
   MCU_MCU_GPIOMUX_INTROUTER0_OUTP_1            : constant Interrupt_ID := 105;
   MCU_MCU_GPIOMUX_INTROUTER0_OUTP_2            : constant Interrupt_ID := 106;
   MCU_MCU_GPIOMUX_INTROUTER0_OUTP_3            : constant Interrupt_ID := 107;
   --  All cores

   EPWM0_EPWM_ETINT_0                           : constant Interrupt_ID := 108;
   EPWM0_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 109;
   EPWM1_EPWM_ETINT_0                           : constant Interrupt_ID := 110;
   EPWM1_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 111;
   EPWM2_EPWM_ETINT_0                           : constant Interrupt_ID := 112;
   EPWM2_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 113;
   EPWM3_EPWM_ETINT_0                           : constant Interrupt_ID := 114;
   EPWM3_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 115;
   EPWM4_EPWM_ETINT_0                           : constant Interrupt_ID := 116;
   EPWM4_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 117;
   EPWM5_EPWM_ETINT_0                           : constant Interrupt_ID := 118;
   --  All cores

   CTRL_MMR0_ACCESS_ERR_0                       : constant Interrupt_ID := 119;
   --  All cores

   PRU_ICSSG0_PR1_HOST_INTR_PEND_0              : constant Interrupt_ID := 120;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_1              : constant Interrupt_ID := 121;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_2              : constant Interrupt_ID := 122;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_3              : constant Interrupt_ID := 123;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_4              : constant Interrupt_ID := 124;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_5              : constant Interrupt_ID := 125;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_6              : constant Interrupt_ID := 126;
   PRU_ICSSG0_PR1_HOST_INTR_PEND_7              : constant Interrupt_ID := 127;
   --  All cores

   ADC0_GEN_LEVEL_0                             : constant Interrupt_ID := 128;
   CPTS0_EVNT_PEND_0                            : constant Interrupt_ID := 129;
   MCU_CTRL_MMR0_ACCESS_ERR_0                   : constant Interrupt_ID := 130;
   PADCFG_CTRL0_ACCESS_ERR_0                    : constant Interrupt_ID := 131;
   MCU_PADCFG_CTRL0_ACCESS_ERR_0                : constant Interrupt_ID := 132;
   GLUELOGIC_CBASS_INTR_OR_GLUE_MAIN_CBASS_AGG_ERR_INTR_0 :
     constant Interrupt_ID := 133;
   CPSW0_EVNT_PEND_0                            : constant Interrupt_ID := 134;
   CPSW0_MDIO_PEND_0                            : constant Interrupt_ID := 135;
   CPSW0_STAT_PEND_0                            : constant Interrupt_ID := 136;
   MCU_DCC0_INTR_DONE_LEVEL_0                   : constant Interrupt_ID := 137;
   EPWM5_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 139;
   ECAP0_ECAP_INT_0                             : constant Interrupt_ID := 140;
   ECAP1_ECAP_INT_0                             : constant Interrupt_ID := 141;
   ECAP2_ECAP_INT_0                             : constant Interrupt_ID := 142;
   EQEP0_EQEP_INT_0                             : constant Interrupt_ID := 143;
   EQEP1_EQEP_INT_0                             : constant Interrupt_ID := 144;
   EQEP2_EQEP_INT_0                             : constant Interrupt_ID := 145;
   EPWM6_EPWM_ETINT_0                           : constant Interrupt_ID := 146;
   EPWM6_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 147;
   EPWM7_EPWM_ETINT_0                           : constant Interrupt_ID := 148;
   EPWM7_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 149;
   EPWM8_EPWM_ETINT_0                           : constant Interrupt_ID := 150;
   DDR16SS0_DDRSS_CONTROLLER_0                  : constant Interrupt_ID := 151;
   --  All cores

   TIMER0_INTR_PEND_0                           : constant Interrupt_ID := 152;
   TIMER1_INTR_PEND_0                           : constant Interrupt_ID := 153;
   TIMER2_INTR_PEND_0                           : constant Interrupt_ID := 154;
   TIMER3_INTR_PEND_0                           : constant Interrupt_ID := 155;
   --  Reserved for runtime use

   TIMER4_INTR_PEND_0                           : constant Interrupt_ID := 156;
   TIMER5_INTR_PEND_0                           : constant Interrupt_ID := 157;
   TIMER6_INTR_PEND_0                           : constant Interrupt_ID := 158;
   TIMER7_INTR_PEND_0                           : constant Interrupt_ID := 159;
   TIMER8_INTR_PEND_0                           : constant Interrupt_ID := 160;
   TIMER9_INTR_PEND_0                           : constant Interrupt_ID := 161;
   TIMER10_INTR_PEND_0                          : constant Interrupt_ID := 162;
   TIMER11_INTR_PEND_0                          : constant Interrupt_ID := 163;
   --  All cores

   ELM0_ELM_POROCPSINTERRUPT_LVL_0              : constant Interrupt_ID := 164;
   MMCSD0_EMMCSS_INTR_0                         : constant Interrupt_ID := 165;
   MMCSD1_EMMCSDSS_INTR_0                       : constant Interrupt_ID := 166;
   ESM0_ESM_INT_CFG_LVL_0                       : constant Interrupt_ID := 167;
   ESM0_ESM_INT_HI_LVL_0                        : constant Interrupt_ID := 168;
   ESM0_ESM_INT_LOW_LVL_0                       : constant Interrupt_ID := 169;
   PRU_ICSSG0_ISO_RESET_PROTCOL_ACK_0           : constant Interrupt_ID := 170;
   FSS0_OSPI_0_OSPI_LVL_INTR_0                  : constant Interrupt_ID := 171;
   PRU_ICSSG1_ISO_RESET_PROTCOL_ACK_0           : constant Interrupt_ID := 172;
   GICSS0_GIC_PWR0_WAKE_REQUEST_0               : constant Interrupt_ID := 173;
   GICSS0_GIC_PWR0_WAKE_REQUEST_1               : constant Interrupt_ID := 174;
   --  All cores

   R5FSS0_CORE0_CTI_0                           : constant Interrupt_ID := 175;
   R5FSS0_CORE1_CTI_0                           : constant Interrupt_ID := 176;
   --  R5FSS0 Core 0 and Core 1

   R5FSS1_CORE0_CTI_0                           : constant Interrupt_ID := 175;
   R5FSS1_CORE1_CTI_0                           : constant Interrupt_ID := 176;
   --  R5FSS1 Core 0 and Core 1

   DDPA0_DDPA_INTR_0                            : constant Interrupt_ID := 177;
   EPWM8_EPWM_TRIPZINT_0                        : constant Interrupt_ID := 178;
   SERDES_10G0_PHY_PWR_TIMEOUT_LVL_0            : constant Interrupt_ID := 180;
   GLUELOGIC_DCC_DONE_GLUE_MAIN_DCC_DONE_0      : constant Interrupt_ID := 182;
   --  All cores

   VTM0_THERM_LVL_GT_TH1_INTR_0                 : constant Interrupt_ID := 183;
   VTM0_THERM_LVL_GT_TH2_INTR_0                 : constant Interrupt_ID := 184;
   VTM0_THERM_LVL_LT_TH0_INTR_0                 : constant Interrupt_ID := 185;
   --  All cores

   MCAN0_MCANSS_EXT_TS_ROLLOVER_LVL_INT_0       : constant Interrupt_ID := 186;
   MCAN0_MCANSS_MCAN_LVL_INT_0                  : constant Interrupt_ID := 187;
   MCAN0_MCANSS_MCAN_LVL_INT_1                  : constant Interrupt_ID := 188;
   MCAN1_MCANSS_EXT_TS_ROLLOVER_LVL_INT_0       : constant Interrupt_ID := 189;
   MCAN1_MCANSS_MCAN_LVL_INT_0                  : constant Interrupt_ID := 190;
   MCAN1_MCANSS_MCAN_LVL_INT_1                  : constant Interrupt_ID := 191;
   --  All cores

   MCU_MCRC64_0_INT_MCRC_0                      : constant Interrupt_ID := 192;
   --  All cores

   I2C0_POINTRPEND_0                            : constant Interrupt_ID := 193;
   I2C1_POINTRPEND_0                            : constant Interrupt_ID := 194;
   I2C2_POINTRPEND_0                            : constant Interrupt_ID := 195;
   I2C3_POINTRPEND_0                            : constant Interrupt_ID := 196;
   --  All cores

   PCIE0_PCIE_PWR_STATE_PULSE_0                 : constant Interrupt_ID := 197;
   GLUELOGIC_GLUE_EXT_INTN_OUT_0                : constant Interrupt_ID := 198;
   --  All cores
   SA2_UL0_SA_UL_PKA_0                          : constant Interrupt_ID := 199;
   SA2_UL0_SA_UL_TRNG_0                         : constant Interrupt_ID := 200;
   --  All cores

   DEBUGSS0_AQCMPINTR_LEVEL_0                   : constant Interrupt_ID := 201;
   DEBUGSS0_CTM_LEVEL_0                         : constant Interrupt_ID := 202;
   --  All cores

   PSC0_PSC_ALLINT_0                            : constant Interrupt_ID := 203;
   --  All cores

   MCSPI0_INTR_SPI_0                            : constant Interrupt_ID := 204;
   MCSPI1_INTR_SPI_0                            : constant Interrupt_ID := 205;
   MCSPI2_INTR_SPI_0                            : constant Interrupt_ID := 206;
   MCSPI4_INTR_SPI_0                            : constant Interrupt_ID := 207;
   MCU_MCSPI0_INTR_SPI_0                        : constant Interrupt_ID := 208;
   MCU_MCSPI1_INTR_SPI_0                        : constant Interrupt_ID := 209;
   --  All cores

   UART0_USART_IRQ_0                            : constant Interrupt_ID := 210;
   UART1_USART_IRQ_0                            : constant Interrupt_ID := 211;
   UART2_USART_IRQ_0                            : constant Interrupt_ID := 212;
   UART3_USART_IRQ_0                            : constant Interrupt_ID := 213;
   UART4_USART_IRQ_0                            : constant Interrupt_ID := 214;
   UART5_USART_IRQ_0                            : constant Interrupt_ID := 215;
   UART6_USART_IRQ_0                            : constant Interrupt_ID := 216;
   MCU_UART0_USART_IRQ_0                        : constant Interrupt_ID := 217;
   MCU_UART1_USART_IRQ_0                        : constant Interrupt_ID := 218;
   --  All cores

   USB0_HOST_SYSTEM_ERROR_0                     : constant Interrupt_ID := 219;
   USB0_IRQ_0                                   : constant Interrupt_ID := 220;
   USB0_IRQ_1                                   : constant Interrupt_ID := 221;
   USB0_IRQ_2                                   : constant Interrupt_ID := 222;
   USB0_IRQ_3                                   : constant Interrupt_ID := 223;
   USB0_IRQ_4                                   : constant Interrupt_ID := 224;
   USB0_IRQ_5                                   : constant Interrupt_ID := 225;
   USB0_IRQ_6                                   : constant Interrupt_ID := 226;
   USB0_IRQ_7                                   : constant Interrupt_ID := 227;
   USB0_OTGIRQ_0                                : constant Interrupt_ID := 228;
   --  All cores

   PCIE0_PCIE_CPTS_PEND_0                       : constant Interrupt_ID := 229;
   PCIE0_PCIE_DOWNSTREAM_PULSE_0                : constant Interrupt_ID := 230;
   PCIE0_PCIE_FLR_PULSE_0                       : constant Interrupt_ID := 231;
   PCIE0_PCIE_ERROR_PULSE_0                     : constant Interrupt_ID := 232;
   PCIE0_PCIE_HOT_RESET_PULSE_0                 : constant Interrupt_ID := 233;
   PCIE0_PCIE_LEGACY_PULSE_0                    : constant Interrupt_ID := 234;
   PCIE0_PCIE_LINK_STATE_PULSE_0                : constant Interrupt_ID := 235;
   PCIE0_PCIE_LOCAL_LEVEL_0                     : constant Interrupt_ID := 236;
   PCIE0_PCIE_PHY_LEVEL_0                       : constant Interrupt_ID := 237;
   PCIE0_PCIE_PTM_VALID_PULSE_0                 : constant Interrupt_ID := 238;
   --  All cores

   GPMC0_GPMC_SINTERRUPT_0                      : constant Interrupt_ID := 239;
   --  All cores

   PRU_ICSSG1_PR1_RX_SOF_INTR_REQ_0             : constant Interrupt_ID := 240;
   PRU_ICSSG1_PR1_RX_SOF_INTR_REQ_1             : constant Interrupt_ID := 241;
   PRU_ICSSG1_PR1_TX_SOF_INTR_REQ_0             : constant Interrupt_ID := 242;
   PRU_ICSSG1_PR1_TX_SOF_INTR_REQ_1             : constant Interrupt_ID := 243;
   PRU_ICSSG0_PR1_RX_SOF_INTR_REQ_0             : constant Interrupt_ID := 244;
   PRU_ICSSG0_PR1_RX_SOF_INTR_REQ_1             : constant Interrupt_ID := 245;
   PRU_ICSSG0_PR1_TX_SOF_INTR_REQ_0             : constant Interrupt_ID := 246;
   PRU_ICSSG0_PR1_TX_SOF_INTR_REQ_1             : constant Interrupt_ID := 247;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_0              : constant Interrupt_ID := 248;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_1              : constant Interrupt_ID := 249;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_2              : constant Interrupt_ID := 250;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_3              : constant Interrupt_ID := 251;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_4              : constant Interrupt_ID := 252;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_5              : constant Interrupt_ID := 253;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_6              : constant Interrupt_ID := 254;
   PRU_ICSSG1_PR1_HOST_INTR_PEND_7              : constant Interrupt_ID := 255;
   --  All cores

end Ada.Interrupts.Names;
