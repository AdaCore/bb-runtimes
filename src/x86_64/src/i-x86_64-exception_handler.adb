------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    I N T E R F A C E S . X 8 6 _ 6 4                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2020-2021, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;         use Ada.Text_IO;

package body Interfaces.X86_64.Exception_Handler is

   use System.BB.CPU_Specific;
   use System.BB.Interrupts;

   procedure Os_Abort
     with Import, External_Name => "abort";

   procedure Fatal_Exception (ID : SBI.Interrupt_ID; Code : SBC.Error_Code) is
   begin
      Put ("FATAL PROCESSOR EXCEPTION RAISED: ");
      case ID is
         when NMI_Interrupt =>
            Put ("unhandled NMI interrupt.");
         when Invalid_Opcode_Exception =>
            Put ("Invalid Opcode.");
         when Device_Not_Available_Exception =>
            Put ("Device Not Available.");
         when Double_Fault_Exception =>
            Put ("Double Fault.");
         when Invalid_TSS_Exception =>
            Put ("Invalid TSS.");
         when General_Protection_Exception =>
            Put ("General Protection Exception.");
         when Alignment_Check_Exception =>
            Put ("Alignment Check Exception.");
         when Machine_Check_Exception =>
            Put ("Machine Check Exception.");
         when Virtualization_Exception =>
            Put ("Virtualization Exception.");
         when Control_Protection_Exception =>
            Put ("Control Protection Exception.");
         when others =>
            Put ("Unknown exception raised. Number " & ID'Image & ".");
      end case;

      Put (" Error Code: " & Code'Image);
      New_Line;
      Put_Line ("Rebooting...");
      Os_Abort;
   end Fatal_Exception;
end Interfaces.X86_64.Exception_Handler;
