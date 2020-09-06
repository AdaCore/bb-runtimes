------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2020, Free Software Foundation, Inc.         --
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

--  This is the version for Cortex R4F TMS570 targets
--  See Table 4-3 in the TMS570LS3137 datasheet, TI document SPNS162A.

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   RTI_Compare_Interrupt_0              : constant Interrupt_ID := 2;
   RTI_Compare_Interrupt_1              : constant Interrupt_ID := 3;
   RTI_Compare_Interrupt_2              : constant Interrupt_ID := 4;
   RTI_Compare_Interrupt_3              : constant Interrupt_ID := 5;
   RTI_Overflow_Interrupt_0             : constant Interrupt_ID := 6;
   RTI_Overflow_Interrupt_1             : constant Interrupt_ID := 7;
   RTI_Timebase_Interrupt               : constant Interrupt_ID := 8;

   GIO_High_Level_Interrupt             : constant Interrupt_ID := 9;
   N2HET1_Level_0_Interrupt             : constant Interrupt_ID := 10;
   HET_TU1_Level_0_Interrupt            : constant Interrupt_ID := 11;
   MIBSPI1_Level_0_Interrupt            : constant Interrupt_ID := 12;
   LIN_Level_0_Interrupt                : constant Interrupt_ID := 13;
   MIBADC1_Event_Group_Interrupt        : constant Interrupt_ID := 14;
   MIBADC1_SW_Group_1_Interrupt         : constant Interrupt_ID := 15;
   DCAN1_Level_0_Interrupt              : constant Interrupt_ID := 16;
   SPI2_Level_0_Interrupt               : constant Interrupt_ID := 17;
   FlexRay_Level_0_Interrupt            : constant Interrupt_ID := 18;
   CRC_Interrupt                        : constant Interrupt_ID := 19;
   ESM_Low_Level_Interrupt              : constant Interrupt_ID := 20;
   Software_Interrupt                   : constant Interrupt_ID := 21;
   PMU_Interrupt                        : constant Interrupt_ID := 22;
   GIO_Low_Level_Interrupt              : constant Interrupt_ID := 23;
   N2HET1_Level_1_Interrupt             : constant Interrupt_ID := 24;
   HET_TU1_Level_1_Interrupt            : constant Interrupt_ID := 25;
   MIBSPI1_Level_1_Interrupt            : constant Interrupt_ID := 26;
   LIN_Level_1_Interrupt                : constant Interrupt_ID := 27;
   MIBADC1_SW_Group_2_Interrupt         : constant Interrupt_ID := 28;
   DCAN1_Level_1_Interrupt              : constant Interrupt_ID := 29;
   SPI2_Level_1_Interrupt               : constant Interrupt_ID := 30;
   MIBADC1_Magnitude_Compare_Interrupt  : constant Interrupt_ID := 31;
   FlexRay_Level_1_Interrupt            : constant Interrupt_ID := 32;
   FTCA_Interrupt                       : constant Interrupt_ID := 33;
   LFSA_Interrupt                       : constant Interrupt_ID := 34;
   DCAN2_Level_0_Interrupt              : constant Interrupt_ID := 35;
   DMM_Level_0_Interrupt                : constant Interrupt_ID := 36;
   MIBSPI3_Level_0_Interrupt            : constant Interrupt_ID := 37;
   MIBSPI3_Level_1_Interrupt            : constant Interrupt_ID := 38;
   HBCA_Interrupt                       : constant Interrupt_ID := 39;
   BTCA_Interrupt                       : constant Interrupt_ID := 40;
   AEMIFINT3                            : constant Interrupt_ID := 41;
   DCAN2_Level_1_Interrupt              : constant Interrupt_ID := 42;
   DMM_Level_1_Interrupt                : constant Interrupt_ID := 43;
   DCAN1_IF3_Interrupt                  : constant Interrupt_ID := 44;
   DCAN3_Level_0_Interrupt              : constant Interrupt_ID := 45;
   DCAN2_IF3_Interrupt                  : constant Interrupt_ID := 46;
   FPU_Interrupt                        : constant Interrupt_ID := 47;
   FlexRay_TU_Transfer_Status_Interrupt : constant Interrupt_ID := 48;

   SPI4_Level_0_Interrupt               : constant Interrupt_ID := 49;
   MIBADC2_Event_Group_Interrupt        : constant Interrupt_ID := 50;
   MIBADC2_SW_Group_1_Interrupt         : constant Interrupt_ID := 51;
   FlexRay_T0C_Interrupt                : constant Interrupt_ID := 52;
   MIBSPI5_Level_0_Interrupt            : constant Interrupt_ID := 53;
   SPI4_Level_1_Interrupt               : constant Interrupt_ID := 54;
   DCAN3_Level_1_Interrupt              : constant Interrupt_ID := 55;
   MIBSPI5_Level_1_Interrupt            : constant Interrupt_ID := 56;
   MIBADC2_SW_Group_2_Interrupt         : constant Interrupt_ID := 57;
   FlexRay_TU_Error_Interrupt           : constant Interrupt_ID := 58;
   MIBADC2_Magnitude_Compare_Interrupt  : constant Interrupt_ID := 59;
   DCAN3_IF3_Interrupt                  : constant Interrupt_ID := 60;
   FSM_Done_Interrupt                   : constant Interrupt_ID := 61;
   FlexRay_T1C_Interrupt                : constant Interrupt_ID := 62;
   NHET2_Level_0_Interrupt              : constant Interrupt_ID := 63;
   SCI_Level_0_Interrupt                : constant Interrupt_ID := 64;
   HET_TU2_Level_0_Interrupt            : constant Interrupt_ID := 65;
   I2C_Level_0_Interrupt                : constant Interrupt_ID := 66;

   N2HET2_Level_1_Interrupt             : constant Interrupt_ID := 73;
   SCI_Level_1_Interrupt                : constant Interrupt_ID := 74;
   HET_TU2_Level_1_Interrupt            : constant Interrupt_ID := 75;
   C0_Misc_Pulse_Interrupt              : constant Interrupt_ID := 76;
   C0_Tx_Pulse_Interrupt                : constant Interrupt_ID := 77;
   C0_Thresh_Pulse_Interrupt            : constant Interrupt_ID := 78;
   C0_RX_Pulse_Interrupt                : constant Interrupt_ID := 79;
   HWAG1_Int_Req_H_Interrupt            : constant Interrupt_ID := 80;
   HWAG2_Int_Req_H_Interrupt            : constant Interrupt_ID := 81;
   DCC_Done_Interrupt                   : constant Interrupt_ID := 82;
   DCC2_Done_Interrupt                  : constant Interrupt_ID := 83;

   HWAG1_Int_Req_L_Interrupt            : constant Interrupt_ID := 88;
   HWAG2_Int_Req_L_Interrupt            : constant Interrupt_ID := 89;
end Ada.Interrupts.Names;
