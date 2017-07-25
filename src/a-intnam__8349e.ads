------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . I N T E R R U P T S . N A M E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
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
------------------------------------------------------------------------------

--  Definitions for the MPC8349E PowerQUICC II Pro

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   IPI_Int_Internal_32 : constant Interrupt_ID := 64;
   IPI_Int_Internal_33 : constant Interrupt_ID := 65;
   IPI_Int_Internal_34 : constant Interrupt_ID := 66;
   IPI_Int_Internal_35 : constant Interrupt_ID := 67;
   RTC_ALR             : constant Interrupt_ID := 68;
   MU                  : constant Interrupt_ID := 69;
   SBA                 : constant Interrupt_ID := 70;
   DMA                 : constant Interrupt_ID := 71;
   TSEC1_Tx            : constant Interrupt_ID := 32;
   TSEC1_Rx            : constant Interrupt_ID := 33;
   TSEC1_Err           : constant Interrupt_ID := 34;
   TSEC2_Tx            : constant Interrupt_ID := 35;
   TSEC2_Rx            : constant Interrupt_ID := 36;
   TSEC2_Err           : constant Interrupt_ID := 37;
   USB_DR              : constant Interrupt_ID := 38;
   USB_MPH             : constant Interrupt_ID := 39;
   IPP_Ind_Ext_Int_0   : constant Interrupt_ID := 48;
   IPP_Ind_Ext_Int_1   : constant Interrupt_ID := 17;
   IPP_Ind_Ext_Int_2   : constant Interrupt_ID := 18;
   IPP_Ind_Ext_Int_3   : constant Interrupt_ID := 19;
   IRQ4                : constant Interrupt_ID := 20;
   IRQ5                : constant Interrupt_ID := 21;
   IRQ6                : constant Interrupt_ID := 22;
   IRQ7                : constant Interrupt_ID := 23;
   UART1               : constant Interrupt_ID := 9;
   UART2               : constant Interrupt_ID := 10;
   SEC                 : constant Interrupt_ID := 11;
   I2C1                : constant Interrupt_ID := 14;
   I2C2                : constant Interrupt_ID := 15;
   SPI                 : constant Interrupt_ID := 16;
   GTM4                : constant Interrupt_ID := 72;
   GTM8                : constant Interrupt_ID := 73;
   GPIO1               : constant Interrupt_ID := 74;
   GPIO2               : constant Interrupt_ID := 75;
   DDR                 : constant Interrupt_ID := 76;
   LBC                 : constant Interrupt_ID := 77;
   GTM2                : constant Interrupt_ID := 78;
   GTM6                : constant Interrupt_ID := 79;
   PMC                 : constant Interrupt_ID := 80;
   GTM3                : constant Interrupt_ID := 84;
   GTM7                : constant Interrupt_ID := 85;
   GTM1                : constant Interrupt_ID := 90;
   GTM5                : constant Interrupt_ID := 91;
end Ada.Interrupts.Names;
