------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2020, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --

--                                                                          --
------------------------------------------------------------------------------

--  This package implements PowerPC architecture specific support for the GNAT
--  Ravenscar run time.

with System.Machine_Code;

package body System.BB.CPU_Specific is

   type Exception_Handler_Array is array (Vector_Id) of Address;

   procedure FP_Unavailable_Error_Handler (Trap : Vector_Id);
   procedure GNAT_Error_Handler (Trap : Vector_Id);
   --  PowerPC core Interrupt Handlers

   Exception_Handlers : Exception_Handler_Array;
   pragma Export (C, Exception_Handlers, "__gnat_powerpc_exception_handlers");

   -------------------------------
   -- Finish_Initialize_Context --
   -------------------------------

   procedure Finish_Initialize_Context
     (Buffer : not null access Context_Buffer)
   is
   begin
      null;
   end Finish_Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      use System.Machine_Code;

      Addr : Address;
      --  Interrupt vector table prefix

      TCR : Timer_Control_Register;
   begin
      --  Enable decrementer interrupt via the TCR

      Asm ("mftcr %0",
           Outputs => Timer_Control_Register'Asm_Output ("=r", TCR),
           Volatile => True);
      TCR.Decrementer_Interrupt_Enable := True;
      Asm ("mttcr %0",
           Inputs => Timer_Control_Register'Asm_Input ("r", TCR),
           Volatile => True);

      --  Set Exception_Handler table

      Exception_Handlers := (others => GNAT_Error_Handler'Address);

      --  Set IVPR

      Asm ("lis %0,handler_0@h",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 63, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR registers to point to our handler routines in handler.S

      Asm
        ("li       %%r3,handler_0@l"                    & ASCII.LF & ASCII.HT &
         "mtivor0  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_1@l"                    & ASCII.LF & ASCII.HT &
         "mtivor1  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_2@l"                    & ASCII.LF & ASCII.HT &
         "mtivor2  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_3@l"                    & ASCII.LF & ASCII.HT &
         "mtivor3  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_4@l"                    & ASCII.LF & ASCII.HT &
         "mtivor4  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_5@l"                    & ASCII.LF & ASCII.HT &
         "mtivor5  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_6@l"                    & ASCII.LF & ASCII.HT &
         "mtivor6  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_7@l"                    & ASCII.LF & ASCII.HT &
         "mtivor7  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_8@l"                    & ASCII.LF & ASCII.HT &
         "mtivor8  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_9@l"                    & ASCII.LF & ASCII.HT &
         "mtivor9  %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_10@l"                   & ASCII.LF & ASCII.HT &
         "mtivor10 %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_11@l"                   & ASCII.LF & ASCII.HT &
         "mtivor11 %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_12@l"                   & ASCII.LF & ASCII.HT &
         "mtivor12 %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_13@l"                   & ASCII.LF & ASCII.HT &
         "mtivor13 %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_14@l"                   & ASCII.LF & ASCII.HT &
         "mtivor14 %%r3"                                & ASCII.LF & ASCII.HT &
         "li       %%r3,handler_15@l"                   & ASCII.LF & ASCII.HT &
         "mtivor15 %%r3",
         Volatile => True,
         Clobber  => "r3");

   end Initialize_CPU;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Exception_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id)
   is
   begin
      Exception_Handlers (Vector) := Service_Routine;
   end Install_Exception_Handler;

   ----------------------------------
   -- FP_Unavailable_Error_Handler --
   ----------------------------------

   procedure FP_Unavailable_Error_Handler (Trap : Vector_Id) is
   begin
      raise Program_Error with "Floating Point Unit is not enabled";
   end FP_Unavailable_Error_Handler;

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Vector_Id) is
   begin
      raise Program_Error with "unhandled trap " & Trap'Image;
   end GNAT_Error_Handler;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      Install_Exception_Handler (FP_Unavailable_Error_Handler'Address,
                                 FP_Unavailable);
   end Install_Error_Handlers;
end System.BB.CPU_Specific;
