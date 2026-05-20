------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2017, Free Software Foundation, Inc.         --
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

--  This is the version for LEON targets of this package

with System;

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   -------------------------
   -- External Interrupts --
   -------------------------

   External_Interrupt_3           : constant Interrupt_ID := 7;
   External_Interrupt_3_Priority  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 6;

   External_Interrupt_2           : constant Interrupt_ID := 6;
   External_Interrupt_2_Priority  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 5;

   External_Interrupt_1           : constant Interrupt_ID := 5;
   External_Interrupt_1_Priority  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 4;

   External_Interrupt_0           : constant Interrupt_ID := 4;
   External_Interrupt_0_Priority  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 3;

   ----------------------
   -- Timer Interrupts --
   ----------------------

   Real_Time_Clock                : constant Interrupt_ID := 9;
   Real_Time_Clock_Priority       : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 8;

   General_Purpose_Timer          : constant Interrupt_ID := 8;
   General_Purpose_Timer_Priority : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 7;

   ---------------------
   -- UART Interrupts --
   ---------------------

   UART_1_RX_TX                   : constant Interrupt_ID := 3;
   UART_1_RX_TX_Priority          : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 2;

   UART_2_RX_TX                   : constant Interrupt_ID := 2;
   UART_2_RX_TX_Priority          : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 1;

   -----------------------
   -- Unused Interrupts --
   -----------------------

   Unused_Interrupt_10            : constant Interrupt_ID := 10;
   Unused_Interrupt_10_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 9;

   Unused_Interrupt_12            : constant Interrupt_ID := 12;
   Unused_Interrupt_12_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 11;

   Unused_Interrupt_13            : constant Interrupt_ID := 13;
   Unused_Interrupt_13_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 12;

   -----------------------------
   -- Miscelaneous Interrupts --
   -----------------------------

   Internal_Bus_Error             : constant Interrupt_ID := 1;
   Internal_Bus_Error_Priority    : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First;

   DSU                            : constant Interrupt_ID := 11;
   DSU_Priority                   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 10;

   PCI                            : constant Interrupt_ID := 14;
   PCI_Priority                   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 13;

   EDAC_Interrupt                 : constant Interrupt_ID := 15;
   EDAC_Priority                  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 14;

end Ada.Interrupts.Names;
