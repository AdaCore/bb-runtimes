------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                I N T E R F A C E S . R A S P B E R R Y _ P I             --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                       Copyright (C) 2016-2017, AdaCore                   --
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

--  This package provides declarations for hardware registers on Raspberry-Pi 2

pragma Ada_2012;
--  Uses aspects

with System;

package Interfaces.Raspberry_Pi is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   IO_Base : constant := 16#3f00_0000#;
   --  IO peripherals base address. Depends on the Raspberry Pi version:
   --   16#2000_000# for RPi1
   --   16#3f00_000# for RPi2 and RPi3

   --  Local peripherals defined in Quad-A7 control (QA7_rev3.4.pdf)

   type Core_Unsigned_32 is array (1 .. 4) of Unsigned_32;
   type Core_Mailbox_Unsigned_32 is array (1 .. 4, 0 .. 3) of Unsigned_32;

   type Local_Registers_Type is record
      --  At 0x00:
      Control              : Unsigned_32;
      Unused_1             : Unsigned_32;
      Core_Timer_Prescaler : Unsigned_32;
      GPU_Int_Routing      : Unsigned_32;

      --  At 0x10:
      PMU_Int_Routing_Set : Unsigned_32;
      PMU_Int_Routing_Clr : Unsigned_32;
      Unused_2            : Unsigned_32;
      Core_Timer_LS       : Unsigned_32;

      --  At 0x20:
      Core_Timer_MS     : Unsigned_32;
      Local_Int_Routing : Unsigned_32;
      Unused_3          : Unsigned_32;
      AXI_Counters      : Unsigned_32;

      --  At 0x30:
      AXI_Int           : Unsigned_32;
      Local_Timer_Ctr   : Unsigned_32;
      Local_Timer_Flags : Unsigned_32;
      Unused_4          : Unsigned_32;

      --  At 0x40 - 0x7f:
      Cores_Timer_Int_Ctr     : Core_Unsigned_32;
      Cores_Mailboxes_Int_Ctr : Core_Unsigned_32;
      Cores_IRQ_Source        : Core_Unsigned_32;
      Cores_FIQ_Source        : Core_Unsigned_32;

      --  At 0x80:
      Cores_Mailboxes_Write_Set : Core_Mailbox_Unsigned_32;

      --  At 0xc0:
      Cores_Mailboxes_Read_Clr  : Core_Mailbox_Unsigned_32;
   end record;

   Local_Registers_Base : constant := 16#4000_0000#;

   Local_Registers : Local_Registers_Type
     with Import, Volatile,
     Address => System'To_Address (Local_Registers_Base);

   --  GPIO registers

   GP_Base : constant := IO_Base + 16#20_0000#;

   type GPIO_Registers_Type is record
      GPFSEL0 : Unsigned_32;
      GPFSEL1 : Unsigned_32;
      GPFSEL2 : Unsigned_32;
      GPFSEL3 : Unsigned_32;

      GPFSEL4 : Unsigned_32;
      GPFSEL5 : Unsigned_32;
      Pad_18  : Unsigned_32;
      GPSET0  : Unsigned_32;

      GPSET1  : Unsigned_32;
      Pad_24  : Unsigned_32;
      GPCLR0  : Unsigned_32;
      GPCLR1  : Unsigned_32;

      Pad_30  : Unsigned_32;
      GPLEV0  : Unsigned_32;
      GPLEV1  : Unsigned_32;
      Pad_3c  : Unsigned_32;

      GPEDS0  : Unsigned_32;
      GPEDS1  : Unsigned_32;
      Pad_48  : Unsigned_32;
      GPREN0  : Unsigned_32;

      GPREN1  : Unsigned_32;
      Pad_54  : Unsigned_32;
      GPFEN0  : Unsigned_32;
      GPFEN1  : Unsigned_32;

      Pad_60  : Unsigned_32;
      GPHEN0  : Unsigned_32;
      GPHEN1  : Unsigned_32;
      Pad_6c  : Unsigned_32;

      GPLEN0  : Unsigned_32;
      GPLEN1  : Unsigned_32;
      Pad_78  : Unsigned_32;
      GPAREN0 : Unsigned_32;

      GPAREN1 : Unsigned_32;
      Pad_84  : Unsigned_32;
      GPAFEN0 : Unsigned_32;
      GPAFEN1 : Unsigned_32;

      Pad_90  : Unsigned_32;
      GPPUD   : Unsigned_32;
      GPPUDCLK0 : Unsigned_32;
      GPPUDCLK1 : Unsigned_32;

      Pad_A0  : Unsigned_32;
      Pad_A4  : Unsigned_32;
      Pad_A8  : Unsigned_32;
      Pad_Ac  : Unsigned_32;

      Test   : Unsigned_32;
   end record;

   GPIO_Registers : GPIO_Registers_Type
     with Address => System'To_Address (GP_Base), Volatile, Import;

   --  Mini-UART registers

   MU_Base : constant := IO_Base + 16#21_5000#;

   Aux_ENB : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#04#), Import, Volatile;
   MU_IO : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#40#), Import, Volatile;
   MU_IIR : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#44#), Import, Volatile;
   MU_IER : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#48#), Import, Volatile;
   MU_LCR : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#4c#), Import, Volatile;
   MU_LSR : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#54#), Import, Volatile;
   MU_CNTL : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#60#), Import, Volatile;
   MU_BAUD : Unsigned_32
     with Address => System'To_Address (MU_Base + 16#68#), Import, Volatile;

   --  Mailboxes

   Mail_Base_Addr : constant := IO_Base + 16#00_b880#;

   Mail_Read_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#00#),
     Volatile, Import;
   Mail_Status_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#18#),
     Volatile, Import;
   Mail_Write_Reg : Unsigned_32
     with Address => System'To_Address (Mail_Base_Addr + 16#20#),
     Volatile, Import;

   --  For status:
   Mail_Empty : constant Unsigned_32 := 16#4000_0000#;  -- Cannot read
   Mail_Full  : constant Unsigned_32 := 16#8000_0000#;  -- Cannot write

   --  Peripheral interrupt controller

   Arm_Interrupt_Base : constant := IO_Base + 16#00_b200#;

   type Arm_Interrupt_Type is record
     Irq_Basic_Pending : Unsigned_32;
     Irq_Pending_1 : Unsigned_32;
     Irq_Pending_2 : Unsigned_32;
     Fiq_Control : Unsigned_32;

     Enable_Irq_1 : Unsigned_32;
     Enable_Irq_2 : Unsigned_32;
     Enable_Basic_Irq : Unsigned_32;
     Disable_Irq_1 : Unsigned_32;

     Disable_Irq_2 : Unsigned_32;
     Disable_Basic_Irq : Unsigned_32;
   end record;

   Arm_Interrupts : Arm_Interrupt_Type
     with Import, Volatile,
     Address => System'To_Address (Arm_Interrupt_Base);

   --  Peripheral timer

   type Arm_Timer_Type is record
      Load : Unsigned_32;
      Value : Unsigned_32;
      Ctl : Unsigned_32;
      Irq_Ack : Unsigned_32;

      Raw_Irq : Unsigned_32;
      Masked_Irq : Unsigned_32;
      Reload : Unsigned_32;
      Divider : Unsigned_32;

      Free_Counter : Unsigned_32;
   end record;

   Arm_Timer_Base : constant := IO_Base + 16#00_b400#;

   Arm_Timer : Arm_Timer_Type
     with Import, Volatile,
     Address => System'To_Address (Arm_Timer_Base);

   --  EMMC
   type EMMC_Registers_Type is record
      --  At 0x00
      ACMD         : Unsigned_32;
      BLKSIZECNT   : Unsigned_32;
      Arg1         : Unsigned_32;
      CMDTM        : Unsigned_32;

      --  At 0x10
      RSP0         : Unsigned_32;
      RSP1         : Unsigned_32;
      RSP2         : Unsigned_32;
      RSP3         : Unsigned_32;

      --  At 0x20
      Data         : Unsigned_32;
      Status       : Unsigned_32;
      Control0     : Unsigned_32;
      Control1     : Unsigned_32;

      --  At 0x30
      Interrupt    : Unsigned_32;
      IRPT_Mask    : Unsigned_32;
      IRPT_En      : Unsigned_32;
      Control2     : Unsigned_32;

      --  At 0x40
      Pad_40       : Unsigned_32;
      Pad_44       : Unsigned_32;
      Pad_48       : Unsigned_32;
      Pad_4c       : Unsigned_32;

      --  At 0x50
      Force_IRPT   : Unsigned_32;
      Pad_54       : Unsigned_32;
      Pad_58       : Unsigned_32;
      Pad_5c       : Unsigned_32;

      --  At 0x60
      Pad_60       : Unsigned_32;
      Pad_64       : Unsigned_32;
      Pad_68       : Unsigned_32;
      Pad_6c       : Unsigned_32;

      --  At 0x70
      Boot_Timeout : Unsigned_32;
      DBG_Sel      : Unsigned_32;
      Pad_78       : Unsigned_32;
      Pad_7c       : Unsigned_32;

      --  At 0x80
      EXRDFIFO_CFG : Unsigned_32;
      EXRDFIFO_En : Unsigned_32;
      Tune_Step : Unsigned_32;
      Tune_Steps_STD : Unsigned_32;

      --  At 0x90
      Tune_Steps_DDR : Unsigned_32;
      Pad_94       : Unsigned_32;
      Pad_98       : Unsigned_32;
      Pad_9c       : Unsigned_32;

      --  At 0xa0
      Pad_a0       : Unsigned_32;
      Pad_a4       : Unsigned_32;
      Pad_a8       : Unsigned_32;
      Pad_ac       : Unsigned_32;

      --  At 0xb0
      Pad_b0       : Unsigned_32;
      Pad_b4       : Unsigned_32;
      Pad_b8       : Unsigned_32;
      Pad_bc       : Unsigned_32;

      --  At 0xc0
      Pad_c0       : Unsigned_32;
      Pad_c4       : Unsigned_32;
      Pad_c8       : Unsigned_32;
      Pad_cc       : Unsigned_32;

      --  At 0xd0
      Pad_d0       : Unsigned_32;
      Pad_d4       : Unsigned_32;
      Pad_d8       : Unsigned_32;
      Pad_dc       : Unsigned_32;

      --  At 0xe0
      Pad_e0       : Unsigned_32;
      Pad_e4       : Unsigned_32;
      Pad_e8       : Unsigned_32;
      Pad_ec       : Unsigned_32;

      --  At 0xf0
      Spi_Int_Spt : Unsigned_32;
      Pad_f4       : Unsigned_32;
      Pad_f8       : Unsigned_32;
      SlotISR_Ver : Unsigned_32;
   end record;

   package EMMC_Bits is
      --  Status
      CMD_INHIBIT : constant := 2**0;
      DAT_INHIBIT : constant := 2**1;

      --  Control 1
      SRST_DATA  : constant := 2**26;
      SRST_CMD   : constant := 2**25;
      SRST_HC    : constant := 2**24;
      CLK_INTLEN : constant := 2**0;
      CLK_STABLE : constant := 2**1;
      CLK_EN     : constant := 2**2;

      --  Interrupt
      CMD_DONE : constant := 2**0;
      DATA_DONE : constant := 2**1;
      WRITE_RDY : constant := 2**4;
      READ_RDY : constant := 2**5;
      ERR : constant := 2**15;
   end EMMC_Bits;

   EMMC_Base : constant := IO_Base + 16#30_0000#;

   EMMC : EMMC_Registers_Type
     with Import, Volatile, Address => System'To_Address (EMMC_Base);

   --  Mailbox interface with VC

   package Mailbox_Interfaces is

      --  Channels
      Channel_Frame_Buffer   : constant Unsigned_32 := 1;
      Channel_Tags_ARM_To_VC : constant Unsigned_32 := 8;

      --  Header of message is aligned on 16 bytes, and contains:
      --    Size : Unsigned_32;
      --    Code : Unsigned_32;  --  Request or answer

      Request_Code     : constant Unsigned_32 := 16#0#;
      Response_Success : constant Unsigned_32 := 16#8000_0000#;
      Response_Error   : constant Unsigned_32 := 16#8000_0001#;

      --  The header is then followed by tags and then by a null word:
      --    Tag : Unsigned_32;
      --    Size : Unsigned_32;  --  In bytes
      --    Indicator : Unsigned_32;

      Request_Indicator  : constant Unsigned_32 := 0;
      Response_Indicator : constant Unsigned_32 := 16#8000_0000#;

      --  Tags
      Tag_Get_Board_Serial : constant Unsigned_32 := 16#1_0004#;
      Tag_Get_ARM_Memory   : constant Unsigned_32 := 16#1_0005#;
      Tag_Get_VC_Memory    : constant Unsigned_32 := 16#1_0006#;
      Tag_Get_Power_State  : constant Unsigned_32 := 16#2_0001#;
      Tag_Set_Power_State  : constant Unsigned_32 := 16#2_8001#;
      Tag_Get_Clock_Rate   : constant Unsigned_32 := 16#3_0002#;
      Tag_Allocate_Buffer  : constant Unsigned_32 := 16#4_0001#;
      Tag_Release_Buffer   : constant Unsigned_32 := 16#4_8001#;
      Tag_Blank_Screen     : constant Unsigned_32 := 16#4_0002#;
      Tag_Set_Physical_Size : constant Unsigned_32 := 16#4_8003#;
      Tag_Set_Virtual_Size  : constant Unsigned_32 := 16#4_8004#;
      Tag_Set_Depth         : constant Unsigned_32 := 16#4_8005#;
      Tag_Get_Pitch         : constant Unsigned_32 := 16#4_0008#;

      --  Power Id
      Power_Id_SDCard : constant Unsigned_32 := 0;
      Power_Id_UART0  : constant Unsigned_32 := 1;
      Power_Id_UART1  : constant Unsigned_32 := 2;
      Power_Id_USB    : constant Unsigned_32 := 3;
      Power_Id_I2C0   : constant Unsigned_32 := 4;
      Power_Id_I2C1   : constant Unsigned_32 := 5;
      Power_Id_I2C2   : constant Unsigned_32 := 6;
      Power_Id_SPI    : constant Unsigned_32 := 7;
      Power_Id_CCP2TX : constant Unsigned_32 := 8;

      --  Clocks Id
      Clock_Id_EMMC : constant Unsigned_32 := 1;
      Clock_Id_UART : constant Unsigned_32 := 2;
      Clock_Id_ARM  : constant Unsigned_32 := 3;
      Clock_Id_Core : constant Unsigned_32 := 4;
      Clock_Id_V3D  : constant Unsigned_32 := 5;
      Clock_Id_H264 : constant Unsigned_32 := 6;
      Clock_Id_ISP  : constant Unsigned_32 := 7;
      Clock_Id_SDRAM : constant Unsigned_32 := 8;
      Clock_Id_PIXEL : constant Unsigned_32 := 9;
      Clock_Id_PWM  : constant Unsigned_32 := 10;
   end Mailbox_Interfaces;

   --  PL011  (UART0)

   type Pl011_Registers_Type is record
      DR     : Unsigned_32;
      RSRECR : Unsigned_32;
      Pad08  : Unsigned_32;
      Pad0c  : Unsigned_32;

      Pad10  : Unsigned_32;
      Pad14  : Unsigned_32;
      FR     : Unsigned_32;
      Pad1c  : Unsigned_32;

      ILPR   : Unsigned_32;
      IBRD   : Unsigned_32;
      FBRD   : Unsigned_32;
      LCRH   : Unsigned_32;

      CR     : Unsigned_32;
      IFLS   : Unsigned_32;
      IMSC   : Unsigned_32;
      RIS    : Unsigned_32;

      MIS    : Unsigned_32;
      ICR    : Unsigned_32;
      DMACR  : Unsigned_32;
      Pad4c  : Unsigned_32;
   end record;

   PL011_Base : constant := IO_Base + 16#20_1000#;

   PL011_Registers : Pl011_Registers_Type
     with Address => System'To_Address (PL011_Base), Volatile, Import;

   package PL011_Bits is
      FR_TXFF : constant := 2**5;
      FR_RXFE : constant := 2**4;
      FR_BUSY : constant := 2**3;

      MASK_RT : constant := 2**6;
      MASK_TX : constant := 2**5;
      MASK_RX : constant := 2**4;
   end PL011_Bits;

end Interfaces.Raspberry_Pi;
