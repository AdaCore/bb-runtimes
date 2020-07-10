--
--  Copyright (C) 2020, AdaCore
--

--  Copyright (c) 2018 Microchip Technology Inc.
--
--  SPDX-License-Identifier: Apache-2.0
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

--  This spec has been automatically generated from ATSAMD21G18AU.svd

pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings (Off, "*bits *");

with System;

package Interfaces.SAM.SERCOM is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   subtype SERCOM_CTRLA_SERCOM_I2CM_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_ENABLE_Field is Interfaces.SAM.Bit;

   --  Operating Mode
   type CTRLA_MODESelect is
     (--  USART mode with external clock
      Usart_Ext_Clk,
      --  USART mode with internal clock
      Usart_Int_Clk,
      --  SPI mode with external clock
      Spi_Slave,
      --  SPI mode with internal clock
      Spi_Master,
      --  I2C mode with external clock
      I2C_Slave,
      --  I2C mode with internal clock
      I2C_Master)
     with Size => 3;
   for CTRLA_MODESelect use
     (Usart_Ext_Clk => 0,
      Usart_Int_Clk => 1,
      Spi_Slave => 2,
      Spi_Master => 3,
      I2C_Slave => 4,
      I2C_Master => 5);

   subtype SERCOM_CTRLA_SERCOM_I2CM_RUNSTDBY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_PINOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SDAHOLD_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CM_MEXTTOEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SEXTTOEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SPEED_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CM_SCLSM_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CM_INACTOUT_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CM_LOWTOUTEN_Field is Interfaces.SAM.Bit;

   --  I2CM Control A
   type SERCOM_CTRLA_SERCOM_I2CM_Register is record
      --  Software Reset
      SWRST          : SERCOM_CTRLA_SERCOM_I2CM_SWRST_Field := 16#0#;
      --  Enable
      ENABLE         : SERCOM_CTRLA_SERCOM_I2CM_ENABLE_Field := 16#0#;
      --  Operating Mode
      MODE           : CTRLA_MODESelect :=
                        Interfaces.SAM.SERCOM.Usart_Ext_Clk;
      --  unspecified
      Reserved_5_6   : Interfaces.SAM.UInt2 := 16#0#;
      --  Run in Standby
      RUNSTDBY       : SERCOM_CTRLA_SERCOM_I2CM_RUNSTDBY_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : Interfaces.SAM.Byte := 16#0#;
      --  Pin Usage
      PINOUT         : SERCOM_CTRLA_SERCOM_I2CM_PINOUT_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : Interfaces.SAM.UInt3 := 16#0#;
      --  SDA Hold Time
      SDAHOLD        : SERCOM_CTRLA_SERCOM_I2CM_SDAHOLD_Field := 16#0#;
      --  Master SCL Low Extend Timeout
      MEXTTOEN       : SERCOM_CTRLA_SERCOM_I2CM_MEXTTOEN_Field := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOEN       : SERCOM_CTRLA_SERCOM_I2CM_SEXTTOEN_Field := 16#0#;
      --  Transfer Speed
      SPEED          : SERCOM_CTRLA_SERCOM_I2CM_SPEED_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : Interfaces.SAM.Bit := 16#0#;
      --  SCL Clock Stretch Mode
      SCLSM          : SERCOM_CTRLA_SERCOM_I2CM_SCLSM_Field := 16#0#;
      --  Inactive Time-Out
      INACTOUT       : SERCOM_CTRLA_SERCOM_I2CM_INACTOUT_Field := 16#0#;
      --  SCL Low Timeout Enable
      LOWTOUTEN      : SERCOM_CTRLA_SERCOM_I2CM_LOWTOUTEN_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_I2CM_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PINOUT         at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SDAHOLD        at 0 range 20 .. 21;
      MEXTTOEN       at 0 range 22 .. 22;
      SEXTTOEN       at 0 range 23 .. 23;
      SPEED          at 0 range 24 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      SCLSM          at 0 range 27 .. 27;
      INACTOUT       at 0 range 28 .. 29;
      LOWTOUTEN      at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_I2CM_SMEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_I2CM_QCEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_I2CM_CMD_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLB_SERCOM_I2CM_ACKACT_Field is Interfaces.SAM.Bit;

   --  I2CM Control B
   type SERCOM_CTRLB_SERCOM_I2CM_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.SAM.Byte := 16#0#;
      --  Smart Mode Enable
      SMEN           : SERCOM_CTRLB_SERCOM_I2CM_SMEN_Field := 16#0#;
      --  Quick Command Enable
      QCEN           : SERCOM_CTRLB_SERCOM_I2CM_QCEN_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : Interfaces.SAM.UInt6 := 16#0#;
      --  Write-only. Command
      CMD            : SERCOM_CTRLB_SERCOM_I2CM_CMD_Field := 16#0#;
      --  Acknowledge Action
      ACKACT         : SERCOM_CTRLB_SERCOM_I2CM_ACKACT_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_I2CM_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SMEN           at 0 range 8 .. 8;
      QCEN           at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      CMD            at 0 range 16 .. 17;
      ACKACT         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype SERCOM_BAUD_SERCOM_I2CM_BAUD_Field is Interfaces.SAM.Byte;
   subtype SERCOM_BAUD_SERCOM_I2CM_BAUDLOW_Field is Interfaces.SAM.Byte;
   subtype SERCOM_BAUD_SERCOM_I2CM_HSBAUD_Field is Interfaces.SAM.Byte;
   subtype SERCOM_BAUD_SERCOM_I2CM_HSBAUDLOW_Field is Interfaces.SAM.Byte;

   --  I2CM Baud Rate
   type SERCOM_BAUD_SERCOM_I2CM_Register is record
      --  Baud Rate Value
      BAUD      : SERCOM_BAUD_SERCOM_I2CM_BAUD_Field := 16#0#;
      --  Baud Rate Value Low
      BAUDLOW   : SERCOM_BAUD_SERCOM_I2CM_BAUDLOW_Field := 16#0#;
      --  High Speed Baud Rate Value
      HSBAUD    : SERCOM_BAUD_SERCOM_I2CM_HSBAUD_Field := 16#0#;
      --  High Speed Baud Rate Value Low
      HSBAUDLOW : SERCOM_BAUD_SERCOM_I2CM_HSBAUDLOW_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_SERCOM_I2CM_Register use record
      BAUD      at 0 range 0 .. 7;
      BAUDLOW   at 0 range 8 .. 15;
      HSBAUD    at 0 range 16 .. 23;
      HSBAUDLOW at 0 range 24 .. 31;
   end record;

   subtype SERCOM_INTENCLR_SERCOM_I2CM_MB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_I2CM_SB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_I2CM_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CM Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt Disable
      MB           : SERCOM_INTENCLR_SERCOM_I2CM_MB_Field := 16#0#;
      --  Slave On Bus Interrupt Disable
      SB           : SERCOM_INTENCLR_SERCOM_I2CM_SB_Field := 16#0#;
      --  unspecified
      Reserved_2_6 : Interfaces.SAM.UInt5 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : SERCOM_INTENCLR_SERCOM_I2CM_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTENSET_SERCOM_I2CM_MB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_I2CM_SB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_I2CM_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CM Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt Enable
      MB           : SERCOM_INTENSET_SERCOM_I2CM_MB_Field := 16#0#;
      --  Slave On Bus Interrupt Enable
      SB           : SERCOM_INTENSET_SERCOM_I2CM_SB_Field := 16#0#;
      --  unspecified
      Reserved_2_6 : Interfaces.SAM.UInt5 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : SERCOM_INTENSET_SERCOM_I2CM_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTFLAG_SERCOM_I2CM_MB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_I2CM_SB_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_I2CM_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CM Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_I2CM_Register is record
      --  Master On Bus Interrupt
      MB           : SERCOM_INTFLAG_SERCOM_I2CM_MB_Field := 16#0#;
      --  Slave On Bus Interrupt
      SB           : SERCOM_INTFLAG_SERCOM_I2CM_SB_Field := 16#0#;
      --  unspecified
      Reserved_2_6 : Interfaces.SAM.UInt5 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : SERCOM_INTFLAG_SERCOM_I2CM_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_I2CM_Register use record
      MB           at 0 range 0 .. 0;
      SB           at 0 range 1 .. 1;
      Reserved_2_6 at 0 range 2 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_STATUS_SERCOM_I2CM_BUSERR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_ARBLOST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_RXNACK_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_BUSSTATE_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_STATUS_SERCOM_I2CM_LOWTOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_CLKHOLD_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_MEXTTOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_SEXTTOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CM_LENERR_Field is Interfaces.SAM.Bit;

   --  I2CM Status
   type SERCOM_STATUS_SERCOM_I2CM_Register is record
      --  Bus Error
      BUSERR         : SERCOM_STATUS_SERCOM_I2CM_BUSERR_Field := 16#0#;
      --  Arbitration Lost
      ARBLOST        : SERCOM_STATUS_SERCOM_I2CM_ARBLOST_Field := 16#0#;
      --  Read-only. Received Not Acknowledge
      RXNACK         : SERCOM_STATUS_SERCOM_I2CM_RXNACK_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : Interfaces.SAM.Bit := 16#0#;
      --  Bus State
      BUSSTATE       : SERCOM_STATUS_SERCOM_I2CM_BUSSTATE_Field := 16#0#;
      --  SCL Low Timeout
      LOWTOUT        : SERCOM_STATUS_SERCOM_I2CM_LOWTOUT_Field := 16#0#;
      --  Read-only. Clock Hold
      CLKHOLD        : SERCOM_STATUS_SERCOM_I2CM_CLKHOLD_Field := 16#0#;
      --  Master SCL Low Extend Timeout
      MEXTTOUT       : SERCOM_STATUS_SERCOM_I2CM_MEXTTOUT_Field := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOUT       : SERCOM_STATUS_SERCOM_I2CM_SEXTTOUT_Field := 16#0#;
      --  Length Error
      LENERR         : SERCOM_STATUS_SERCOM_I2CM_LENERR_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : Interfaces.SAM.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_I2CM_Register use record
      BUSERR         at 0 range 0 .. 0;
      ARBLOST        at 0 range 1 .. 1;
      RXNACK         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      BUSSTATE       at 0 range 4 .. 5;
      LOWTOUT        at 0 range 6 .. 6;
      CLKHOLD        at 0 range 7 .. 7;
      MEXTTOUT       at 0 range 8 .. 8;
      SEXTTOUT       at 0 range 9 .. 9;
      LENERR         at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
   end record;

   subtype SERCOM_SYNCBUSY_SERCOM_I2CM_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_I2CM_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_I2CM_SYSOP_Field is Interfaces.SAM.Bit;

   --  I2CM Syncbusy
   type SERCOM_SYNCBUSY_SERCOM_I2CM_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : SERCOM_SYNCBUSY_SERCOM_I2CM_SWRST_Field;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : SERCOM_SYNCBUSY_SERCOM_I2CM_ENABLE_Field;
      --  Read-only. System Operation Synchronization Busy
      SYSOP         : SERCOM_SYNCBUSY_SERCOM_I2CM_SYSOP_Field;
      --  unspecified
      Reserved_3_31 : Interfaces.SAM.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_I2CM_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      SYSOP         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SERCOM_ADDR_SERCOM_I2CM_ADDR_Field is Interfaces.SAM.UInt11;
   subtype SERCOM_ADDR_SERCOM_I2CM_LENEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_ADDR_SERCOM_I2CM_HS_Field is Interfaces.SAM.Bit;
   subtype SERCOM_ADDR_SERCOM_I2CM_TENBITEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_ADDR_SERCOM_I2CM_LEN_Field is Interfaces.SAM.Byte;

   --  I2CM Address
   type SERCOM_ADDR_SERCOM_I2CM_Register is record
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_I2CM_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : Interfaces.SAM.UInt2 := 16#0#;
      --  Length Enable
      LENEN          : SERCOM_ADDR_SERCOM_I2CM_LENEN_Field := 16#0#;
      --  High Speed Mode
      HS             : SERCOM_ADDR_SERCOM_I2CM_HS_Field := 16#0#;
      --  Ten Bit Addressing Enable
      TENBITEN       : SERCOM_ADDR_SERCOM_I2CM_TENBITEN_Field := 16#0#;
      --  Length
      LEN            : SERCOM_ADDR_SERCOM_I2CM_LEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_I2CM_Register use record
      ADDR           at 0 range 0 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      LENEN          at 0 range 13 .. 13;
      HS             at 0 range 14 .. 14;
      TENBITEN       at 0 range 15 .. 15;
      LEN            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SERCOM_DBGCTRL_SERCOM_I2CM_DBGSTOP_Field is Interfaces.SAM.Bit;

   --  I2CM Debug Control
   type SERCOM_DBGCTRL_SERCOM_I2CM_Register is record
      --  Debug Mode
      DBGSTOP      : SERCOM_DBGCTRL_SERCOM_I2CM_DBGSTOP_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_I2CM_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  I2C Master Mode
   type SercomI2cm_Cluster is record
      --  I2CM Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_I2CM_Register;
      --  I2CM Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_I2CM_Register;
      --  I2CM Baud Rate
      BAUD     : aliased SERCOM_BAUD_SERCOM_I2CM_Register;
      --  I2CM Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_I2CM_Register;
      --  I2CM Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_I2CM_Register;
      --  I2CM Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_I2CM_Register;
      --  I2CM Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_I2CM_Register;
      --  I2CM Syncbusy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_I2CM_Register;
      --  I2CM Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_I2CM_Register;
      --  I2CM Data
      DATA     : aliased Interfaces.SAM.Byte;
      --  I2CM Debug Control
      DBGCTRL  : aliased SERCOM_DBGCTRL_SERCOM_I2CM_Register;
   end record
     with Size => 416;

   for SercomI2cm_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      BAUD     at 16#C# range 0 .. 31;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 7;
      DBGCTRL  at 16#30# range 0 .. 7;
   end record;

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   subtype SERCOM_CTRLA_SERCOM_I2CS_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_RUNSTDBY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_PINOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SDAHOLD_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SEXTTOEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SPEED_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_I2CS_SCLSM_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_I2CS_LOWTOUTEN_Field is Interfaces.SAM.Bit;

   --  I2CS Control A
   type SERCOM_CTRLA_SERCOM_I2CS_Register is record
      --  Software Reset
      SWRST          : SERCOM_CTRLA_SERCOM_I2CS_SWRST_Field := 16#0#;
      --  Enable
      ENABLE         : SERCOM_CTRLA_SERCOM_I2CS_ENABLE_Field := 16#0#;
      --  Operating Mode
      MODE           : CTRLA_MODESelect :=
                        Interfaces.SAM.SERCOM.Usart_Ext_Clk;
      --  unspecified
      Reserved_5_6   : Interfaces.SAM.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : SERCOM_CTRLA_SERCOM_I2CS_RUNSTDBY_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : Interfaces.SAM.Byte := 16#0#;
      --  Pin Usage
      PINOUT         : SERCOM_CTRLA_SERCOM_I2CS_PINOUT_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : Interfaces.SAM.UInt3 := 16#0#;
      --  SDA Hold Time
      SDAHOLD        : SERCOM_CTRLA_SERCOM_I2CS_SDAHOLD_Field := 16#0#;
      --  unspecified
      Reserved_22_22 : Interfaces.SAM.Bit := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOEN       : SERCOM_CTRLA_SERCOM_I2CS_SEXTTOEN_Field := 16#0#;
      --  Transfer Speed
      SPEED          : SERCOM_CTRLA_SERCOM_I2CS_SPEED_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : Interfaces.SAM.Bit := 16#0#;
      --  SCL Clock Stretch Mode
      SCLSM          : SERCOM_CTRLA_SERCOM_I2CS_SCLSM_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : Interfaces.SAM.UInt2 := 16#0#;
      --  SCL Low Timeout Enable
      LOWTOUTEN      : SERCOM_CTRLA_SERCOM_I2CS_LOWTOUTEN_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_I2CS_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PINOUT         at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      SDAHOLD        at 0 range 20 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      SEXTTOEN       at 0 range 23 .. 23;
      SPEED          at 0 range 24 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      SCLSM          at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      LOWTOUTEN      at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_I2CS_SMEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_I2CS_GCMD_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_I2CS_AACKEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_I2CS_AMODE_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLB_SERCOM_I2CS_CMD_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLB_SERCOM_I2CS_ACKACT_Field is Interfaces.SAM.Bit;

   --  I2CS Control B
   type SERCOM_CTRLB_SERCOM_I2CS_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.SAM.Byte := 16#0#;
      --  Smart Mode Enable
      SMEN           : SERCOM_CTRLB_SERCOM_I2CS_SMEN_Field := 16#0#;
      --  PMBus Group Command
      GCMD           : SERCOM_CTRLB_SERCOM_I2CS_GCMD_Field := 16#0#;
      --  Automatic Address Acknowledge
      AACKEN         : SERCOM_CTRLB_SERCOM_I2CS_AACKEN_Field := 16#0#;
      --  unspecified
      Reserved_11_13 : Interfaces.SAM.UInt3 := 16#0#;
      --  Address Mode
      AMODE          : SERCOM_CTRLB_SERCOM_I2CS_AMODE_Field := 16#0#;
      --  Write-only. Command
      CMD            : SERCOM_CTRLB_SERCOM_I2CS_CMD_Field := 16#0#;
      --  Acknowledge Action
      ACKACT         : SERCOM_CTRLB_SERCOM_I2CS_ACKACT_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : Interfaces.SAM.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_I2CS_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SMEN           at 0 range 8 .. 8;
      GCMD           at 0 range 9 .. 9;
      AACKEN         at 0 range 10 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      AMODE          at 0 range 14 .. 15;
      CMD            at 0 range 16 .. 17;
      ACKACT         at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype SERCOM_INTENCLR_SERCOM_I2CS_PREC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_I2CS_AMATCH_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_I2CS_DRDY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_I2CS_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CS Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt Disable
      PREC         : SERCOM_INTENCLR_SERCOM_I2CS_PREC_Field := 16#0#;
      --  Address Match Interrupt Disable
      AMATCH       : SERCOM_INTENCLR_SERCOM_I2CS_AMATCH_Field := 16#0#;
      --  Data Interrupt Disable
      DRDY         : SERCOM_INTENCLR_SERCOM_I2CS_DRDY_Field := 16#0#;
      --  unspecified
      Reserved_3_6 : Interfaces.SAM.UInt4 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : SERCOM_INTENCLR_SERCOM_I2CS_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTENSET_SERCOM_I2CS_PREC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_I2CS_AMATCH_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_I2CS_DRDY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_I2CS_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CS Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt Enable
      PREC         : SERCOM_INTENSET_SERCOM_I2CS_PREC_Field := 16#0#;
      --  Address Match Interrupt Enable
      AMATCH       : SERCOM_INTENSET_SERCOM_I2CS_AMATCH_Field := 16#0#;
      --  Data Interrupt Enable
      DRDY         : SERCOM_INTENSET_SERCOM_I2CS_DRDY_Field := 16#0#;
      --  unspecified
      Reserved_3_6 : Interfaces.SAM.UInt4 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : SERCOM_INTENSET_SERCOM_I2CS_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTFLAG_SERCOM_I2CS_PREC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_I2CS_AMATCH_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_I2CS_DRDY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_I2CS_ERROR_Field is Interfaces.SAM.Bit;

   --  I2CS Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_I2CS_Register is record
      --  Stop Received Interrupt
      PREC         : SERCOM_INTFLAG_SERCOM_I2CS_PREC_Field := 16#0#;
      --  Address Match Interrupt
      AMATCH       : SERCOM_INTFLAG_SERCOM_I2CS_AMATCH_Field := 16#0#;
      --  Data Interrupt
      DRDY         : SERCOM_INTFLAG_SERCOM_I2CS_DRDY_Field := 16#0#;
      --  unspecified
      Reserved_3_6 : Interfaces.SAM.UInt4 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : SERCOM_INTFLAG_SERCOM_I2CS_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_I2CS_Register use record
      PREC         at 0 range 0 .. 0;
      AMATCH       at 0 range 1 .. 1;
      DRDY         at 0 range 2 .. 2;
      Reserved_3_6 at 0 range 3 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_STATUS_SERCOM_I2CS_BUSERR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_COLL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_RXNACK_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_DIR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_SR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_LOWTOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_CLKHOLD_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_SEXTTOUT_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_I2CS_HS_Field is Interfaces.SAM.Bit;

   --  I2CS Status
   type SERCOM_STATUS_SERCOM_I2CS_Register is record
      --  Bus Error
      BUSERR         : SERCOM_STATUS_SERCOM_I2CS_BUSERR_Field := 16#0#;
      --  Transmit Collision
      COLL           : SERCOM_STATUS_SERCOM_I2CS_COLL_Field := 16#0#;
      --  Read-only. Received Not Acknowledge
      RXNACK         : SERCOM_STATUS_SERCOM_I2CS_RXNACK_Field := 16#0#;
      --  Read-only. Read/Write Direction
      DIR            : SERCOM_STATUS_SERCOM_I2CS_DIR_Field := 16#0#;
      --  Read-only. Repeated Start
      SR             : SERCOM_STATUS_SERCOM_I2CS_SR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : Interfaces.SAM.Bit := 16#0#;
      --  SCL Low Timeout
      LOWTOUT        : SERCOM_STATUS_SERCOM_I2CS_LOWTOUT_Field := 16#0#;
      --  Read-only. Clock Hold
      CLKHOLD        : SERCOM_STATUS_SERCOM_I2CS_CLKHOLD_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : Interfaces.SAM.Bit := 16#0#;
      --  Slave SCL Low Extend Timeout
      SEXTTOUT       : SERCOM_STATUS_SERCOM_I2CS_SEXTTOUT_Field := 16#0#;
      --  High Speed
      HS             : SERCOM_STATUS_SERCOM_I2CS_HS_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : Interfaces.SAM.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_I2CS_Register use record
      BUSERR         at 0 range 0 .. 0;
      COLL           at 0 range 1 .. 1;
      RXNACK         at 0 range 2 .. 2;
      DIR            at 0 range 3 .. 3;
      SR             at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      LOWTOUT        at 0 range 6 .. 6;
      CLKHOLD        at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      SEXTTOUT       at 0 range 9 .. 9;
      HS             at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
   end record;

   subtype SERCOM_SYNCBUSY_SERCOM_I2CS_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_I2CS_ENABLE_Field is Interfaces.SAM.Bit;

   --  I2CS Syncbusy
   type SERCOM_SYNCBUSY_SERCOM_I2CS_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : SERCOM_SYNCBUSY_SERCOM_I2CS_SWRST_Field;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : SERCOM_SYNCBUSY_SERCOM_I2CS_ENABLE_Field;
      --  unspecified
      Reserved_2_31 : Interfaces.SAM.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_I2CS_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype SERCOM_ADDR_SERCOM_I2CS_GENCEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_ADDR_SERCOM_I2CS_ADDR_Field is Interfaces.SAM.UInt10;
   subtype SERCOM_ADDR_SERCOM_I2CS_TENBITEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_ADDR_SERCOM_I2CS_ADDRMASK_Field is Interfaces.SAM.UInt10;

   --  I2CS Address
   type SERCOM_ADDR_SERCOM_I2CS_Register is record
      --  General Call Address Enable
      GENCEN         : SERCOM_ADDR_SERCOM_I2CS_GENCEN_Field := 16#0#;
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_I2CS_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : Interfaces.SAM.UInt4 := 16#0#;
      --  Ten Bit Addressing Enable
      TENBITEN       : SERCOM_ADDR_SERCOM_I2CS_TENBITEN_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : Interfaces.SAM.Bit := 16#0#;
      --  Address Mask
      ADDRMASK       : SERCOM_ADDR_SERCOM_I2CS_ADDRMASK_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : Interfaces.SAM.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_I2CS_Register use record
      GENCEN         at 0 range 0 .. 0;
      ADDR           at 0 range 1 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      TENBITEN       at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      ADDRMASK       at 0 range 17 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  I2C Slave Mode
   type SercomI2cs_Cluster is record
      --  I2CS Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_I2CS_Register;
      --  I2CS Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_I2CS_Register;
      --  I2CS Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_I2CS_Register;
      --  I2CS Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_I2CS_Register;
      --  I2CS Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_I2CS_Register;
      --  I2CS Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_I2CS_Register;
      --  I2CS Syncbusy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_I2CS_Register;
      --  I2CS Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_I2CS_Register;
      --  I2CS Data
      DATA     : aliased Interfaces.SAM.Byte;
   end record
     with Size => 352;

   for SercomI2cs_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 7;
   end record;

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   subtype SERCOM_CTRLA_SERCOM_SPI_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_RUNSTDBY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_IBON_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_DOPO_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_SPI_DIPO_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_SPI_FORM_Field is Interfaces.SAM.UInt4;
   subtype SERCOM_CTRLA_SERCOM_SPI_CPHA_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_CPOL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_SPI_DORD_Field is Interfaces.SAM.Bit;

   --  SPI Control A
   type SERCOM_CTRLA_SERCOM_SPI_Register is record
      --  Software Reset
      SWRST          : SERCOM_CTRLA_SERCOM_SPI_SWRST_Field := 16#0#;
      --  Enable
      ENABLE         : SERCOM_CTRLA_SERCOM_SPI_ENABLE_Field := 16#0#;
      --  Operating Mode
      MODE           : CTRLA_MODESelect :=
                        Interfaces.SAM.SERCOM.Usart_Ext_Clk;
      --  unspecified
      Reserved_5_6   : Interfaces.SAM.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : SERCOM_CTRLA_SERCOM_SPI_RUNSTDBY_Field := 16#0#;
      --  Immediate Buffer Overflow Notification
      IBON           : SERCOM_CTRLA_SERCOM_SPI_IBON_Field := 16#0#;
      --  unspecified
      Reserved_9_15  : Interfaces.SAM.UInt7 := 16#0#;
      --  Data Out Pinout
      DOPO           : SERCOM_CTRLA_SERCOM_SPI_DOPO_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.SAM.UInt2 := 16#0#;
      --  Data In Pinout
      DIPO           : SERCOM_CTRLA_SERCOM_SPI_DIPO_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : Interfaces.SAM.UInt2 := 16#0#;
      --  Frame Format
      FORM           : SERCOM_CTRLA_SERCOM_SPI_FORM_Field := 16#0#;
      --  Clock Phase
      CPHA           : SERCOM_CTRLA_SERCOM_SPI_CPHA_Field := 16#0#;
      --  Clock Polarity
      CPOL           : SERCOM_CTRLA_SERCOM_SPI_CPOL_Field := 16#0#;
      --  Data Order
      DORD           : SERCOM_CTRLA_SERCOM_SPI_DORD_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_SPI_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      IBON           at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      DOPO           at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      DIPO           at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      FORM           at 0 range 24 .. 27;
      CPHA           at 0 range 28 .. 28;
      CPOL           at 0 range 29 .. 29;
      DORD           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_SPI_CHSIZE_Field is Interfaces.SAM.UInt3;
   subtype SERCOM_CTRLB_SERCOM_SPI_PLOADEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_SPI_SSDE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_SPI_MSSEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_SPI_AMODE_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLB_SERCOM_SPI_RXEN_Field is Interfaces.SAM.Bit;

   --  SPI Control B
   type SERCOM_CTRLB_SERCOM_SPI_Register is record
      --  Character Size
      CHSIZE         : SERCOM_CTRLB_SERCOM_SPI_CHSIZE_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : Interfaces.SAM.UInt3 := 16#0#;
      --  Data Preload Enable
      PLOADEN        : SERCOM_CTRLB_SERCOM_SPI_PLOADEN_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : Interfaces.SAM.UInt2 := 16#0#;
      --  Slave Select Low Detect Enable
      SSDE           : SERCOM_CTRLB_SERCOM_SPI_SSDE_Field := 16#0#;
      --  unspecified
      Reserved_10_12 : Interfaces.SAM.UInt3 := 16#0#;
      --  Master Slave Select Enable
      MSSEN          : SERCOM_CTRLB_SERCOM_SPI_MSSEN_Field := 16#0#;
      --  Address Mode
      AMODE          : SERCOM_CTRLB_SERCOM_SPI_AMODE_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : Interfaces.SAM.Bit := 16#0#;
      --  Receiver Enable
      RXEN           : SERCOM_CTRLB_SERCOM_SPI_RXEN_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : Interfaces.SAM.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_SPI_Register use record
      CHSIZE         at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      PLOADEN        at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      SSDE           at 0 range 9 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      MSSEN          at 0 range 13 .. 13;
      AMODE          at 0 range 14 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      RXEN           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype SERCOM_INTENCLR_SERCOM_SPI_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_SPI_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_SPI_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_SPI_SSL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_SPI_ERROR_Field is Interfaces.SAM.Bit;

   --  SPI Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_SPI_Register is record
      --  Data Register Empty Interrupt Disable
      DRE          : SERCOM_INTENCLR_SERCOM_SPI_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt Disable
      TXC          : SERCOM_INTENCLR_SERCOM_SPI_TXC_Field := 16#0#;
      --  Receive Complete Interrupt Disable
      RXC          : SERCOM_INTENCLR_SERCOM_SPI_RXC_Field := 16#0#;
      --  Slave Select Low Interrupt Disable
      SSL          : SERCOM_INTENCLR_SERCOM_SPI_SSL_Field := 16#0#;
      --  unspecified
      Reserved_4_6 : Interfaces.SAM.UInt3 := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : SERCOM_INTENCLR_SERCOM_SPI_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTENSET_SERCOM_SPI_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_SPI_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_SPI_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_SPI_SSL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_SPI_ERROR_Field is Interfaces.SAM.Bit;

   --  SPI Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_SPI_Register is record
      --  Data Register Empty Interrupt Enable
      DRE          : SERCOM_INTENSET_SERCOM_SPI_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt Enable
      TXC          : SERCOM_INTENSET_SERCOM_SPI_TXC_Field := 16#0#;
      --  Receive Complete Interrupt Enable
      RXC          : SERCOM_INTENSET_SERCOM_SPI_RXC_Field := 16#0#;
      --  Slave Select Low Interrupt Enable
      SSL          : SERCOM_INTENSET_SERCOM_SPI_SSL_Field := 16#0#;
      --  unspecified
      Reserved_4_6 : Interfaces.SAM.UInt3 := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : SERCOM_INTENSET_SERCOM_SPI_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTFLAG_SERCOM_SPI_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_SPI_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_SPI_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_SPI_SSL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_SPI_ERROR_Field is Interfaces.SAM.Bit;

   --  SPI Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_SPI_Register is record
      --  Read-only. Data Register Empty Interrupt
      DRE          : SERCOM_INTFLAG_SERCOM_SPI_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt
      TXC          : SERCOM_INTFLAG_SERCOM_SPI_TXC_Field := 16#0#;
      --  Read-only. Receive Complete Interrupt
      RXC          : SERCOM_INTFLAG_SERCOM_SPI_RXC_Field := 16#0#;
      --  Slave Select Low Interrupt Flag
      SSL          : SERCOM_INTFLAG_SERCOM_SPI_SSL_Field := 16#0#;
      --  unspecified
      Reserved_4_6 : Interfaces.SAM.UInt3 := 16#0#;
      --  Combined Error Interrupt
      ERROR        : SERCOM_INTFLAG_SERCOM_SPI_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_SPI_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      SSL          at 0 range 3 .. 3;
      Reserved_4_6 at 0 range 4 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_STATUS_SERCOM_SPI_BUFOVF_Field is Interfaces.SAM.Bit;

   --  SPI Status
   type SERCOM_STATUS_SERCOM_SPI_Register is record
      --  unspecified
      Reserved_0_1  : Interfaces.SAM.UInt2 := 16#0#;
      --  Buffer Overflow
      BUFOVF        : SERCOM_STATUS_SERCOM_SPI_BUFOVF_Field := 16#0#;
      --  unspecified
      Reserved_3_15 : Interfaces.SAM.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_SPI_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      BUFOVF        at 0 range 2 .. 2;
      Reserved_3_15 at 0 range 3 .. 15;
   end record;

   subtype SERCOM_SYNCBUSY_SERCOM_SPI_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_SPI_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_SPI_CTRLB_Field is Interfaces.SAM.Bit;

   --  SPI Syncbusy
   type SERCOM_SYNCBUSY_SERCOM_SPI_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : SERCOM_SYNCBUSY_SERCOM_SPI_SWRST_Field;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : SERCOM_SYNCBUSY_SERCOM_SPI_ENABLE_Field;
      --  Read-only. CTRLB Synchronization Busy
      CTRLB         : SERCOM_SYNCBUSY_SERCOM_SPI_CTRLB_Field;
      --  unspecified
      Reserved_3_31 : Interfaces.SAM.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_SPI_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      CTRLB         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SERCOM_ADDR_SERCOM_SPI_ADDR_Field is Interfaces.SAM.Byte;
   subtype SERCOM_ADDR_SERCOM_SPI_ADDRMASK_Field is Interfaces.SAM.Byte;

   --  SPI Address
   type SERCOM_ADDR_SERCOM_SPI_Register is record
      --  Address Value
      ADDR           : SERCOM_ADDR_SERCOM_SPI_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : Interfaces.SAM.Byte := 16#0#;
      --  Address Mask
      ADDRMASK       : SERCOM_ADDR_SERCOM_SPI_ADDRMASK_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.SAM.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_ADDR_SERCOM_SPI_Register use record
      ADDR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      ADDRMASK       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SERCOM_DATA_SERCOM_SPI_DATA_Field is Interfaces.SAM.UInt9;

   --  SPI Data
   type SERCOM_DATA_SERCOM_SPI_Register is record
      --  Data Value
      DATA          : SERCOM_DATA_SERCOM_SPI_DATA_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : Interfaces.SAM.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_DATA_SERCOM_SPI_Register use record
      DATA          at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype SERCOM_DBGCTRL_SERCOM_SPI_DBGSTOP_Field is Interfaces.SAM.Bit;

   --  SPI Debug Control
   type SERCOM_DBGCTRL_SERCOM_SPI_Register is record
      --  Debug Mode
      DBGSTOP      : SERCOM_DBGCTRL_SERCOM_SPI_DBGSTOP_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_SPI_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  SPI Mode
   type SercomSpi_Cluster is record
      --  SPI Control A
      CTRLA    : aliased SERCOM_CTRLA_SERCOM_SPI_Register;
      --  SPI Control B
      CTRLB    : aliased SERCOM_CTRLB_SERCOM_SPI_Register;
      --  SPI Baud Rate
      BAUD     : aliased Interfaces.SAM.Byte;
      --  SPI Interrupt Enable Clear
      INTENCLR : aliased SERCOM_INTENCLR_SERCOM_SPI_Register;
      --  SPI Interrupt Enable Set
      INTENSET : aliased SERCOM_INTENSET_SERCOM_SPI_Register;
      --  SPI Interrupt Flag Status and Clear
      INTFLAG  : aliased SERCOM_INTFLAG_SERCOM_SPI_Register;
      --  SPI Status
      STATUS   : aliased SERCOM_STATUS_SERCOM_SPI_Register;
      --  SPI Syncbusy
      SYNCBUSY : aliased SERCOM_SYNCBUSY_SERCOM_SPI_Register;
      --  SPI Address
      ADDR     : aliased SERCOM_ADDR_SERCOM_SPI_Register;
      --  SPI Data
      DATA     : aliased SERCOM_DATA_SERCOM_SPI_Register;
      --  SPI Debug Control
      DBGCTRL  : aliased SERCOM_DBGCTRL_SERCOM_SPI_Register;
   end record
     with Size => 416;

   for SercomSpi_Cluster use record
      CTRLA    at 16#0# range 0 .. 31;
      CTRLB    at 16#4# range 0 .. 31;
      BAUD     at 16#C# range 0 .. 7;
      INTENCLR at 16#14# range 0 .. 7;
      INTENSET at 16#16# range 0 .. 7;
      INTFLAG  at 16#18# range 0 .. 7;
      STATUS   at 16#1A# range 0 .. 15;
      SYNCBUSY at 16#1C# range 0 .. 31;
      ADDR     at 16#24# range 0 .. 31;
      DATA     at 16#28# range 0 .. 31;
      DBGCTRL  at 16#30# range 0 .. 7;
   end record;

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   subtype SERCOM_CTRLA_SERCOM_USART_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_RUNSTDBY_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_IBON_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_SAMPR_Field is Interfaces.SAM.UInt3;
   subtype SERCOM_CTRLA_SERCOM_USART_TXPO_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_RXPO_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_SAMPA_Field is Interfaces.SAM.UInt2;
   subtype SERCOM_CTRLA_SERCOM_USART_FORM_Field is Interfaces.SAM.UInt4;
   subtype SERCOM_CTRLA_SERCOM_USART_CMODE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_CPOL_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLA_SERCOM_USART_DORD_Field is Interfaces.SAM.Bit;

   --  USART Control A
   type SERCOM_CTRLA_SERCOM_USART_Register is record
      --  Software Reset
      SWRST          : SERCOM_CTRLA_SERCOM_USART_SWRST_Field := 16#0#;
      --  Enable
      ENABLE         : SERCOM_CTRLA_SERCOM_USART_ENABLE_Field := 16#0#;
      --  Operating Mode
      MODE           : CTRLA_MODESelect :=
                        Interfaces.SAM.SERCOM.Usart_Ext_Clk;
      --  unspecified
      Reserved_5_6   : Interfaces.SAM.UInt2 := 16#0#;
      --  Run during Standby
      RUNSTDBY       : SERCOM_CTRLA_SERCOM_USART_RUNSTDBY_Field := 16#0#;
      --  Immediate Buffer Overflow Notification
      IBON           : SERCOM_CTRLA_SERCOM_USART_IBON_Field := 16#0#;
      --  unspecified
      Reserved_9_12  : Interfaces.SAM.UInt4 := 16#0#;
      --  Sample
      SAMPR          : SERCOM_CTRLA_SERCOM_USART_SAMPR_Field := 16#0#;
      --  Transmit Data Pinout
      TXPO           : SERCOM_CTRLA_SERCOM_USART_TXPO_Field := 16#0#;
      --  unspecified
      Reserved_18_19 : Interfaces.SAM.UInt2 := 16#0#;
      --  Receive Data Pinout
      RXPO           : SERCOM_CTRLA_SERCOM_USART_RXPO_Field := 16#0#;
      --  Sample Adjustment
      SAMPA          : SERCOM_CTRLA_SERCOM_USART_SAMPA_Field := 16#0#;
      --  Frame Format
      FORM           : SERCOM_CTRLA_SERCOM_USART_FORM_Field := 16#0#;
      --  Communication Mode
      CMODE          : SERCOM_CTRLA_SERCOM_USART_CMODE_Field := 16#0#;
      --  Clock Polarity
      CPOL           : SERCOM_CTRLA_SERCOM_USART_CPOL_Field := 16#0#;
      --  Data Order
      DORD           : SERCOM_CTRLA_SERCOM_USART_DORD_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : Interfaces.SAM.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLA_SERCOM_USART_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      MODE           at 0 range 2 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      RUNSTDBY       at 0 range 7 .. 7;
      IBON           at 0 range 8 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      SAMPR          at 0 range 13 .. 15;
      TXPO           at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      RXPO           at 0 range 20 .. 21;
      SAMPA          at 0 range 22 .. 23;
      FORM           at 0 range 24 .. 27;
      CMODE          at 0 range 28 .. 28;
      CPOL           at 0 range 29 .. 29;
      DORD           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SERCOM_CTRLB_SERCOM_USART_CHSIZE_Field is Interfaces.SAM.UInt3;
   subtype SERCOM_CTRLB_SERCOM_USART_SBMODE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_COLDEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_SFDE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_ENC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_PMODE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_TXEN_Field is Interfaces.SAM.Bit;
   subtype SERCOM_CTRLB_SERCOM_USART_RXEN_Field is Interfaces.SAM.Bit;

   --  USART Control B
   type SERCOM_CTRLB_SERCOM_USART_Register is record
      --  Character Size
      CHSIZE         : SERCOM_CTRLB_SERCOM_USART_CHSIZE_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : Interfaces.SAM.UInt3 := 16#0#;
      --  Stop Bit Mode
      SBMODE         : SERCOM_CTRLB_SERCOM_USART_SBMODE_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.SAM.Bit := 16#0#;
      --  Collision Detection Enable
      COLDEN         : SERCOM_CTRLB_SERCOM_USART_COLDEN_Field := 16#0#;
      --  Start of Frame Detection Enable
      SFDE           : SERCOM_CTRLB_SERCOM_USART_SFDE_Field := 16#0#;
      --  Encoding Format
      ENC            : SERCOM_CTRLB_SERCOM_USART_ENC_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : Interfaces.SAM.UInt2 := 16#0#;
      --  Parity Mode
      PMODE          : SERCOM_CTRLB_SERCOM_USART_PMODE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : Interfaces.SAM.UInt2 := 16#0#;
      --  Transmitter Enable
      TXEN           : SERCOM_CTRLB_SERCOM_USART_TXEN_Field := 16#0#;
      --  Receiver Enable
      RXEN           : SERCOM_CTRLB_SERCOM_USART_RXEN_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : Interfaces.SAM.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_CTRLB_SERCOM_USART_Register use record
      CHSIZE         at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      SBMODE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      COLDEN         at 0 range 8 .. 8;
      SFDE           at 0 range 9 .. 9;
      ENC            at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      PMODE          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      TXEN           at 0 range 16 .. 16;
      RXEN           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype SERCOM_BAUD_FRAC_MODE_SERCOM_USART_BAUD_Field is
     Interfaces.SAM.UInt13;
   subtype SERCOM_BAUD_FRAC_MODE_SERCOM_USART_FP_Field is Interfaces.SAM.UInt3;

   --  USART Baud Rate
   type SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register is record
      --  Baud Rate Value
      BAUD : SERCOM_BAUD_FRAC_MODE_SERCOM_USART_BAUD_Field := 16#0#;
      --  Fractional Part
      FP   : SERCOM_BAUD_FRAC_MODE_SERCOM_USART_FP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register use record
      BAUD at 0 range 0 .. 12;
      FP   at 0 range 13 .. 15;
   end record;

   subtype SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_BAUD_Field is
     Interfaces.SAM.UInt13;
   subtype SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_FP_Field is
     Interfaces.SAM.UInt3;

   --  USART Baud Rate
   type SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register is record
      --  Baud Rate Value
      BAUD : SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_BAUD_Field := 16#0#;
      --  Fractional Part
      FP   : SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_FP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register use record
      BAUD at 0 range 0 .. 12;
      FP   at 0 range 13 .. 15;
   end record;

   subtype SERCOM_INTENCLR_SERCOM_USART_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_RXS_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_CTSIC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_RXBRK_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENCLR_SERCOM_USART_ERROR_Field is Interfaces.SAM.Bit;

   --  USART Interrupt Enable Clear
   type SERCOM_INTENCLR_SERCOM_USART_Register is record
      --  Data Register Empty Interrupt Disable
      DRE          : SERCOM_INTENCLR_SERCOM_USART_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt Disable
      TXC          : SERCOM_INTENCLR_SERCOM_USART_TXC_Field := 16#0#;
      --  Receive Complete Interrupt Disable
      RXC          : SERCOM_INTENCLR_SERCOM_USART_RXC_Field := 16#0#;
      --  Receive Start Interrupt Disable
      RXS          : SERCOM_INTENCLR_SERCOM_USART_RXS_Field := 16#0#;
      --  Clear To Send Input Change Interrupt Disable
      CTSIC        : SERCOM_INTENCLR_SERCOM_USART_CTSIC_Field := 16#0#;
      --  Break Received Interrupt Disable
      RXBRK        : SERCOM_INTENCLR_SERCOM_USART_RXBRK_Field := 16#0#;
      --  unspecified
      Reserved_6_6 : Interfaces.SAM.Bit := 16#0#;
      --  Combined Error Interrupt Disable
      ERROR        : SERCOM_INTENCLR_SERCOM_USART_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENCLR_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTENSET_SERCOM_USART_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_RXS_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_CTSIC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_RXBRK_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTENSET_SERCOM_USART_ERROR_Field is Interfaces.SAM.Bit;

   --  USART Interrupt Enable Set
   type SERCOM_INTENSET_SERCOM_USART_Register is record
      --  Data Register Empty Interrupt Enable
      DRE          : SERCOM_INTENSET_SERCOM_USART_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt Enable
      TXC          : SERCOM_INTENSET_SERCOM_USART_TXC_Field := 16#0#;
      --  Receive Complete Interrupt Enable
      RXC          : SERCOM_INTENSET_SERCOM_USART_RXC_Field := 16#0#;
      --  Receive Start Interrupt Enable
      RXS          : SERCOM_INTENSET_SERCOM_USART_RXS_Field := 16#0#;
      --  Clear To Send Input Change Interrupt Enable
      CTSIC        : SERCOM_INTENSET_SERCOM_USART_CTSIC_Field := 16#0#;
      --  Break Received Interrupt Enable
      RXBRK        : SERCOM_INTENSET_SERCOM_USART_RXBRK_Field := 16#0#;
      --  unspecified
      Reserved_6_6 : Interfaces.SAM.Bit := 16#0#;
      --  Combined Error Interrupt Enable
      ERROR        : SERCOM_INTENSET_SERCOM_USART_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTENSET_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_INTFLAG_SERCOM_USART_DRE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_TXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_RXC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_RXS_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_CTSIC_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_RXBRK_Field is Interfaces.SAM.Bit;
   subtype SERCOM_INTFLAG_SERCOM_USART_ERROR_Field is Interfaces.SAM.Bit;

   --  USART Interrupt Flag Status and Clear
   type SERCOM_INTFLAG_SERCOM_USART_Register is record
      --  Read-only. Data Register Empty Interrupt
      DRE          : SERCOM_INTFLAG_SERCOM_USART_DRE_Field := 16#0#;
      --  Transmit Complete Interrupt
      TXC          : SERCOM_INTFLAG_SERCOM_USART_TXC_Field := 16#0#;
      --  Read-only. Receive Complete Interrupt
      RXC          : SERCOM_INTFLAG_SERCOM_USART_RXC_Field := 16#0#;
      --  Write-only. Receive Start Interrupt
      RXS          : SERCOM_INTFLAG_SERCOM_USART_RXS_Field := 16#0#;
      --  Clear To Send Input Change Interrupt
      CTSIC        : SERCOM_INTFLAG_SERCOM_USART_CTSIC_Field := 16#0#;
      --  Break Received Interrupt
      RXBRK        : SERCOM_INTFLAG_SERCOM_USART_RXBRK_Field := 16#0#;
      --  unspecified
      Reserved_6_6 : Interfaces.SAM.Bit := 16#0#;
      --  Combined Error Interrupt
      ERROR        : SERCOM_INTFLAG_SERCOM_USART_ERROR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_INTFLAG_SERCOM_USART_Register use record
      DRE          at 0 range 0 .. 0;
      TXC          at 0 range 1 .. 1;
      RXC          at 0 range 2 .. 2;
      RXS          at 0 range 3 .. 3;
      CTSIC        at 0 range 4 .. 4;
      RXBRK        at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      ERROR        at 0 range 7 .. 7;
   end record;

   subtype SERCOM_STATUS_SERCOM_USART_PERR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_USART_FERR_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_USART_BUFOVF_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_USART_CTS_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_USART_ISF_Field is Interfaces.SAM.Bit;
   subtype SERCOM_STATUS_SERCOM_USART_COLL_Field is Interfaces.SAM.Bit;

   --  USART Status
   type SERCOM_STATUS_SERCOM_USART_Register is record
      --  Parity Error
      PERR          : SERCOM_STATUS_SERCOM_USART_PERR_Field := 16#0#;
      --  Frame Error
      FERR          : SERCOM_STATUS_SERCOM_USART_FERR_Field := 16#0#;
      --  Buffer Overflow
      BUFOVF        : SERCOM_STATUS_SERCOM_USART_BUFOVF_Field := 16#0#;
      --  Read-only. Clear To Send
      CTS           : SERCOM_STATUS_SERCOM_USART_CTS_Field := 16#0#;
      --  Inconsistent Sync Field
      ISF           : SERCOM_STATUS_SERCOM_USART_ISF_Field := 16#0#;
      --  Collision Detected
      COLL          : SERCOM_STATUS_SERCOM_USART_COLL_Field := 16#0#;
      --  unspecified
      Reserved_6_15 : Interfaces.SAM.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_STATUS_SERCOM_USART_Register use record
      PERR          at 0 range 0 .. 0;
      FERR          at 0 range 1 .. 1;
      BUFOVF        at 0 range 2 .. 2;
      CTS           at 0 range 3 .. 3;
      ISF           at 0 range 4 .. 4;
      COLL          at 0 range 5 .. 5;
      Reserved_6_15 at 0 range 6 .. 15;
   end record;

   subtype SERCOM_SYNCBUSY_SERCOM_USART_SWRST_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_USART_ENABLE_Field is Interfaces.SAM.Bit;
   subtype SERCOM_SYNCBUSY_SERCOM_USART_CTRLB_Field is Interfaces.SAM.Bit;

   --  USART Syncbusy
   type SERCOM_SYNCBUSY_SERCOM_USART_Register is record
      --  Read-only. Software Reset Synchronization Busy
      SWRST         : SERCOM_SYNCBUSY_SERCOM_USART_SWRST_Field;
      --  Read-only. SERCOM Enable Synchronization Busy
      ENABLE        : SERCOM_SYNCBUSY_SERCOM_USART_ENABLE_Field;
      --  Read-only. CTRLB Synchronization Busy
      CTRLB         : SERCOM_SYNCBUSY_SERCOM_USART_CTRLB_Field;
      --  unspecified
      Reserved_3_31 : Interfaces.SAM.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SERCOM_SYNCBUSY_SERCOM_USART_Register use record
      SWRST         at 0 range 0 .. 0;
      ENABLE        at 0 range 1 .. 1;
      CTRLB         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SERCOM_DATA_SERCOM_USART_DATA_Field is Interfaces.SAM.UInt9;

   --  USART Data
   type SERCOM_DATA_SERCOM_USART_Register is record
      --  Data Value
      DATA          : SERCOM_DATA_SERCOM_USART_DATA_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : Interfaces.SAM.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 16,
          Bit_Order => System.Low_Order_First;

   for SERCOM_DATA_SERCOM_USART_Register use record
      DATA          at 0 range 0 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
   end record;

   subtype SERCOM_DBGCTRL_SERCOM_USART_DBGSTOP_Field is Interfaces.SAM.Bit;

   --  USART Debug Control
   type SERCOM_DBGCTRL_SERCOM_USART_Register is record
      --  Debug Mode
      DBGSTOP      : SERCOM_DBGCTRL_SERCOM_USART_DBGSTOP_Field := 16#0#;
      --  unspecified
      Reserved_1_7 : Interfaces.SAM.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 8,
          Bit_Order => System.Low_Order_First;

   for SERCOM_DBGCTRL_SERCOM_USART_Register use record
      DBGSTOP      at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   type SercomUsart_Disc is
     (Default,
      Frac_Mode,
      Fracfp_Mode,
      Usartfp_Mode);

   --  USART Mode
   type SercomUsart_Cluster
     (Discriminent : SercomUsart_Disc := Default)
   is record
      --  USART Control A
      CTRLA             : aliased SERCOM_CTRLA_SERCOM_USART_Register;
      --  USART Control B
      CTRLB             : aliased SERCOM_CTRLB_SERCOM_USART_Register;
      --  USART Receive Pulse Length
      RXPL              : aliased Interfaces.SAM.Byte;
      --  USART Interrupt Enable Clear
      INTENCLR          : aliased SERCOM_INTENCLR_SERCOM_USART_Register;
      --  USART Interrupt Enable Set
      INTENSET          : aliased SERCOM_INTENSET_SERCOM_USART_Register;
      --  USART Interrupt Flag Status and Clear
      INTFLAG           : aliased SERCOM_INTFLAG_SERCOM_USART_Register;
      --  USART Status
      STATUS            : aliased SERCOM_STATUS_SERCOM_USART_Register;
      --  USART Syncbusy
      SYNCBUSY          : aliased SERCOM_SYNCBUSY_SERCOM_USART_Register;
      --  USART Data
      DATA              : aliased SERCOM_DATA_SERCOM_USART_Register;
      --  USART Debug Control
      DBGCTRL           : aliased SERCOM_DBGCTRL_SERCOM_USART_Register;
      case Discriminent is
         when Default =>
            --  USART Baud Rate
            BAUD : aliased Interfaces.SAM.UInt16;
         when Frac_Mode =>
            --  USART Baud Rate
            BAUD_FRAC_MODE : aliased SERCOM_BAUD_FRAC_MODE_SERCOM_USART_Register;
         when Fracfp_Mode =>
            --  USART Baud Rate
            BAUD_FRACFP_MODE : aliased SERCOM_BAUD_FRACFP_MODE_SERCOM_USART_Register;
         when Usartfp_Mode =>
            --  USART Baud Rate
            BAUD_USARTFP_MODE : aliased Interfaces.SAM.UInt16;
      end case;
   end record
     with Unchecked_Union, Size => 416;

   for SercomUsart_Cluster use record
      CTRLA             at 16#0# range 0 .. 31;
      CTRLB             at 16#4# range 0 .. 31;
      RXPL              at 16#E# range 0 .. 7;
      INTENCLR          at 16#14# range 0 .. 7;
      INTENSET          at 16#16# range 0 .. 7;
      INTFLAG           at 16#18# range 0 .. 7;
      STATUS            at 16#1A# range 0 .. 15;
      SYNCBUSY          at 16#1C# range 0 .. 31;
      DATA              at 16#28# range 0 .. 15;
      DBGCTRL           at 16#30# range 0 .. 7;
      BAUD              at 16#C# range 0 .. 15;
      BAUD_FRAC_MODE    at 16#C# range 0 .. 15;
      BAUD_FRACFP_MODE  at 16#C# range 0 .. 15;
      BAUD_USARTFP_MODE at 16#C# range 0 .. 15;
   end record;

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   ------------------------------------
   -- SercomI2cm cluster's Registers --
   ------------------------------------

   ------------------------------------
   -- SercomI2cs cluster's Registers --
   ------------------------------------

   -----------------------------------
   -- SercomSpi cluster's Registers --
   -----------------------------------

   -------------------------------------
   -- SercomUsart cluster's Registers --
   -------------------------------------

   -----------------
   -- Peripherals --
   -----------------

   type SERCOM0_Disc is
     (I2Cm,
      I2Cs,
      Spi,
      Usart);

   --  Serial Communication Interface 0
   type SERCOM_Peripheral
     (Discriminent : SERCOM0_Disc := I2Cm)
   is record
      case Discriminent is
         when I2Cm =>
            --  I2C Master Mode
            SERCOM_I2CM : aliased SercomI2cm_Cluster;
         when I2Cs =>
            --  I2C Slave Mode
            SERCOM_I2CS : aliased SercomI2cs_Cluster;
         when Spi =>
            --  SPI Mode
            SERCOM_SPI : aliased SercomSpi_Cluster;
         when Usart =>
            --  USART Mode
            SERCOM_USART : aliased SercomUsart_Cluster;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for SERCOM_Peripheral use record
      SERCOM_I2CM  at 0 range 0 .. 415;
      SERCOM_I2CS  at 0 range 0 .. 351;
      SERCOM_SPI   at 0 range 0 .. 415;
      SERCOM_USART at 0 range 0 .. 415;
   end record;

   --  Serial Communication Interface 0
   SERCOM0_Periph : aliased SERCOM_Peripheral
     with Import, Address => SERCOM0_Base;

   --  Serial Communication Interface 1
   SERCOM1_Periph : aliased SERCOM_Peripheral
     with Import, Address => SERCOM1_Base;

   --  Serial Communication Interface 2
   SERCOM2_Periph : aliased SERCOM_Peripheral
     with Import, Address => SERCOM2_Base;

   --  Serial Communication Interface 3
   SERCOM3_Periph : aliased SERCOM_Peripheral
     with Import, Address => SERCOM3_Base;

   --  Serial Communication Interface 4
   SERCOM4_Periph : aliased SERCOM_Peripheral
     with Import, Address => SERCOM4_Base;

   --  Serial Communication Interface 5
   SERCOM5_Periph : aliased SERCOM_Peripheral (Discriminent => Usart)
     with Import, Address => SERCOM5_Base;

end Interfaces.SAM.SERCOM;
