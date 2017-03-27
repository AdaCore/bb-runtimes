------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                           (No Tasking Version)                           --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  This is the Raspberry Pi 2 & 3 version of this file
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Basic interrupts. Numbered from 0 to 11.
   --  Those are core-specific interrupts

   --  Core-related interrupts

   --  Timers (64-bit)
   CNTPSIRQ_Interrupt        : constant Interrupt_ID := 0;
   CNTPNSIRQ_Interrupt       : constant Interrupt_ID := 1;
   CNTHPIRQ_Interrupt        : constant Interrupt_ID := 2;
   CNTVIRQ_Interrupt         : constant Interrupt_ID := 3;
   --  Mailboxes
   Mailbox_0_Interrupt       : constant Interrupt_ID := 4;
   Mailbox_1_Interrupt       : constant Interrupt_ID := 5;
   Mailbox_2_Interrupt       : constant Interrupt_ID := 6;
   Mailbox_3_Interrupt       : constant Interrupt_ID := 7;

   --  Any GPU IRQ will trigger first one of the core's IRQ8.
   --  As the routing to the proper handler is performed by the runtime,
   --  attaching a user-defined handler to IRQ8 would produce no effect

   --  Performance monitor
   PMU_Interrupt             : constant Interrupt_ID := 9;

   --  Core-unrelated interrupts
   AXI_Outstanding_Interrupt : constant Interrupt_ID := 10; --  Core0 only
   Local_Timer_Interrupt     : constant Interrupt_ID := 11;

   --  GPU interrupts. Those are routed to the Core 1 by default.
   --  The IDs range from 12 .. 12 + 63
   GPU_Int_Base_0            : constant Interrupt_ID := 12;
   Timer0_Interrupt          : constant Interrupt_ID := 0 + GPU_Int_Base_0;
   Timer1_Interrupt          : constant Interrupt_ID := 1 + GPU_Int_Base_0;
   Timer2_Interrupt          : constant Interrupt_ID := 2 + GPU_Int_Base_0;
   Timer3_Interrupt          : constant Interrupt_ID := 3 + GPU_Int_Base_0;
   VC_CODEC0_Interrupt       : constant Interrupt_ID := 4 + GPU_Int_Base_0;
   VC_CODEC1_Interrupt       : constant Interrupt_ID := 5 + GPU_Int_Base_0;
   VC_CODEC2_Interrupt       : constant Interrupt_ID := 6 + GPU_Int_Base_0;
   VC_JPEG_Interrupt         : constant Interrupt_ID := 7 + GPU_Int_Base_0;
   ISP_Interrupt             : constant Interrupt_ID := 8 + GPU_Int_Base_0;
   VC_USB_Interrupt          : constant Interrupt_ID := 9 + GPU_Int_Base_0;
   VC_3D_Interrupt           : constant Interrupt_ID := 10 + GPU_Int_Base_0;
   Transposer_Interrupt      : constant Interrupt_ID := 11 + GPU_Int_Base_0;
   MC_SYNC0_Interrupt        : constant Interrupt_ID := 12 + GPU_Int_Base_0;
   MC_SYNC1_Interrupt        : constant Interrupt_ID := 13 + GPU_Int_Base_0;
   MC_SYNC2_Interrupt        : constant Interrupt_ID := 14 + GPU_Int_Base_0;
   MC_SYNC3_Interrupt        : constant Interrupt_ID := 15 + GPU_Int_Base_0;
   DMA0_Interrupt            : constant Interrupt_ID := 16 + GPU_Int_Base_0;
   DMA1_Interrupt            : constant Interrupt_ID := 17 + GPU_Int_Base_0;
   DMA2_Interrupt            : constant Interrupt_ID := 18 + GPU_Int_Base_0;
   DMA3_Interrupt            : constant Interrupt_ID := 19 + GPU_Int_Base_0;
   DMA4_Interrupt            : constant Interrupt_ID := 20 + GPU_Int_Base_0;
   DMA5_Interrupt            : constant Interrupt_ID := 21 + GPU_Int_Base_0;
   DMA6_Interrupt            : constant Interrupt_ID := 22 + GPU_Int_Base_0;
   DMA7_Interrupt            : constant Interrupt_ID := 23 + GPU_Int_Base_0;
   DMA8_Interrupt            : constant Interrupt_ID := 24 + GPU_Int_Base_0;
   DMA9_Interrupt            : constant Interrupt_ID := 25 + GPU_Int_Base_0;
   DMA10_Interrupt           : constant Interrupt_ID := 26 + GPU_Int_Base_0;
   DMA11_14_Interrupt        : constant Interrupt_ID := 27 + GPU_Int_Base_0;
   DMAALL_Interrupt          : constant Interrupt_ID := 28 + GPU_Int_Base_0;
   AUX_Interrupt             : constant Interrupt_ID := 29 + GPU_Int_Base_0;
   ARM_Interrupt             : constant Interrupt_ID := 30 + GPU_Int_Base_0;
   VPUDMA_Interrupt          : constant Interrupt_ID := 31 + GPU_Int_Base_0;

   GPU_Int_Base_1            : constant Interrupt_ID := GPU_Int_Base_0 + 32;
   HOSTPORT_Interrupt        : constant Interrupt_ID := 0 + GPU_Int_Base_1;
   VIDEOSCALER_Interrupt     : constant Interrupt_ID := 1 + GPU_Int_Base_1;
   CCP2TX_Interrupt          : constant Interrupt_ID := 2 + GPU_Int_Base_1;
   SDC_Interrupt             : constant Interrupt_ID := 3 + GPU_Int_Base_1;
   DSI0_Interrupt            : constant Interrupt_ID := 4 + GPU_Int_Base_1;
   AVE_Interrupt             : constant Interrupt_ID := 5 + GPU_Int_Base_1;
   CAM0_Interrupt            : constant Interrupt_ID := 6 + GPU_Int_Base_1;
   CAM1_Interrupt            : constant Interrupt_ID := 7 + GPU_Int_Base_1;
   HDMI0_Interrupt           : constant Interrupt_ID := 8 + GPU_Int_Base_1;
   HDMI1_Interrupt           : constant Interrupt_ID := 9 + GPU_Int_Base_1;
   PIXEL_VALVE1_Interrupt    : constant Interrupt_ID := 10 + GPU_Int_Base_1;
   I2C_SPI_SLV_Interrupt     : constant Interrupt_ID := 11 + GPU_Int_Base_1;
   DSI1_Interrupt            : constant Interrupt_ID := 12 + GPU_Int_Base_1;
   PWA0_Interrupt            : constant Interrupt_ID := 13 + GPU_Int_Base_1;
   PWA1_Interrupt            : constant Interrupt_ID := 14 + GPU_Int_Base_1;
   CPR_Interrupt             : constant Interrupt_ID := 15 + GPU_Int_Base_1;
   SMI_Interrupt             : constant Interrupt_ID := 16 + GPU_Int_Base_1;
   GPIO_0_Interupt           : constant Interrupt_ID := 17 + GPU_Int_Base_1;
   GPIO_1_Interupt           : constant Interrupt_ID := 18 + GPU_Int_Base_1;
   GPIO_2_Interupt           : constant Interrupt_ID := 19 + GPU_Int_Base_1;
   GPIO_3_Interupt           : constant Interrupt_ID := 20 + GPU_Int_Base_1;
   I2C_Interrupt             : constant Interrupt_ID := 21 + GPU_Int_Base_1;
   SPI_Interrupt             : constant Interrupt_ID := 22 + GPU_Int_Base_1;
   I2S_PCM_Interrupt         : constant Interrupt_ID := 23 + GPU_Int_Base_1;
   SDIO_Interrupt            : constant Interrupt_ID := 24 + GPU_Int_Base_1;
   UART_Interrupt            : constant Interrupt_ID := 25 + GPU_Int_Base_1;
   SLIMBUS_Interrupt         : constant Interrupt_ID := 26 + GPU_Int_Base_1;
   VEC_Interrupt             : constant Interrupt_ID := 27 + GPU_Int_Base_1;
   CPG_Interrupt             : constant Interrupt_ID := 28 + GPU_Int_Base_1;
   RNG_Interrupt             : constant Interrupt_ID := 29 + GPU_Int_Base_1;
   VC_ARASANSDIO_Interrupt   : constant Interrupt_ID := 30 + GPU_Int_Base_1;
   AVSPMON_Interrupt         : constant Interrupt_ID := 31 + GPU_Int_Base_1;

end Ada.Interrupts.Names;
