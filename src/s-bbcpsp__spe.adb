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
--                     Copyright (C) 2003-2017, AdaCore                     --
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

   procedure GNAT_Error_Handler (Trap : Vector_Id);

   Exception_Handlers : Exception_Handler_Array :=
                          (others => GNAT_Error_Handler'Address);
   pragma Export (C, Exception_Handlers, "__gnat_powerpc_exception_handlers");

   -------------------------------
   -- Finish_Initialize_Context --
   -------------------------------

   procedure Finish_Initialize_Context
     (Buffer : not null access Context_Buffer)
   is
      use System.Machine_Code;
      use Interfaces;

      Spefpscr : Unsigned_32;

   begin
      --  Copy the current value of SPEFPSCR
      Asm ("mfspr %0, 512",
           Outputs  => Unsigned_32'Asm_Output ("=r", Spefpscr),
           Volatile => True);
      Buffer.SPEFPSCR := Spefpscr;
   end Finish_Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      use System.Machine_Code;
      use Interfaces;

      Addr : Address;
      --  Interrupt vector table prefix

      DIE : constant Unsigned_32 := 16#0400_0000#;
      --  Decrementer interrupt enable

   begin
      --  Set TCR

      Asm ("mtspr 340, %0",
           Inputs => Unsigned_32'Asm_Input ("r", DIE),
           Volatile => True);

      --  Set IVPR

      Asm ("lis %0,handler_0@h",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 63, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR10 (decrementer)

      Asm ("li %0,handler_10@l",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 410, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR4 (External interrupt)

      Asm ("li %0,handler_4@l",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 404, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR33 (spe data interrupt)

      Asm ("li %0,handler_33@l",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 529, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

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

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Vector_Id) is
   begin
      case Trap is
         when Floatting_Point_Data_Excp =>
            raise Constraint_Error with "floating point exception";
         when others =>
            raise Program_Error with "unhandled trap";
      end case;
   end GNAT_Error_Handler;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      Install_Exception_Handler (GNAT_Error_Handler'Address,
                                 Floatting_Point_Data_Excp);
   end Install_Error_Handlers;
end System.BB.CPU_Specific;
