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

--  This is the version for ERC32 targets of this package

with System;

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   -------------------------
   -- External Interrupts --
   -------------------------

   External_Interrupt_4            : constant Interrupt_ID := 14;
   External_Interrupt_4_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 13;

   External_Interrupt_3            : constant Interrupt_ID := 11;
   External_Interrupt_3_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 10;

   External_Interrupt_2            : constant Interrupt_ID := 10;
   External_Interrupt_2_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 9;

   External_Interrupt_1            : constant Interrupt_ID := 3;
   External_Interrupt_1_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 2;

   External_Interrupt_0            : constant Interrupt_ID := 2;
   External_Interrupt_0_Priority   : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 1;

   ----------------------
   -- Timer Interrupts --
   ----------------------

   Watch_Dog_Time_Out              : constant Interrupt_ID := 15;
   Watch_Dog_Time_Out_Priority     : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 14;

   Real_Time_Clock                 : constant Interrupt_ID := 13;
   Real_Time_Clock_Priority        : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 12;

   General_Purpose_Timer           : constant Interrupt_ID := 12;
   General_Purpose_Timer_Priority  : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 11;

   --------------------
   -- DMA Interrupts --
   --------------------

   DMA_Time_Out                    : constant Interrupt_ID := 9;
   DMA_Time_Out_Priority           : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 8;

   DMA_Access_Error                : constant Interrupt_ID := 8;
   DMA_Access_Error_Priority       : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 7;

   ---------------------
   -- UART Interrupts --
   ---------------------

   UART_Error                      : constant Interrupt_ID := 7;
   UART_Error_Priority             : constant System.Interrupt_Priority
                                     := System.Interrupt_Priority'First + 6;

   UART_B_Ready                    : constant Interrupt_ID := 5;
   UART_B_Ready_Priority           : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 4;

   UART_A_Ready                    : constant Interrupt_ID := 4;
   UART_A_Ready_Priority           : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First + 3;

   -----------------------------
   -- Miscelaneous Interrupts --
   -----------------------------

   Correctable_Error_In_Memory     : constant Interrupt_ID := 6;
   Correctable_Error_In_Memory_Priority : constant System.Interrupt_Priority
     := System.Interrupt_Priority'First + 5;

   Masked_Hardware_Errors          : constant Interrupt_ID := 1;
   Masked_Hardware_Errors_Priority : constant System.Interrupt_Priority
                                      := System.Interrupt_Priority'First;

end Ada.Interrupts.Names;
