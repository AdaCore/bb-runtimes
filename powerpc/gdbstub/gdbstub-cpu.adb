------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

with System.Machine_Code;
with Interfaces; use Interfaces;

package body Gdbstub.CPU is

   type Gpr_Context is array (0 .. 31) of Unsigned_32;
   type Fpr_Context is array (0 .. 31) of IEEE_Float_64;

   type Cpu_Context is record
      Gpr   : Gpr_Context;
      Fpr   : Fpr_Context;

      PC    : Unsigned_32;
      MSR   : Unsigned_32;
      CR    : Unsigned_32;
      LR    : Unsigned_32;
      CTR   : Unsigned_32;
      XER   : Unsigned_32;
      FPSCR : Unsigned_32;
   end record;

   type Cpu_Context_Acc is access all Cpu_Context;

   Regs : Cpu_Context_Acc;
   MSR_SE : constant Unsigned_32 := 2 ** (31 - 21);
   SIGTRAP : constant Integer := 5;

   procedure Exception_Handler (Val : Integer; Context : Cpu_Context_Acc);

   procedure Exception_Handler (Val : Integer; Context : Cpu_Context_Acc)
   is
      pragma Unreferenced (Val);
   begin
      Regs := Context;
      Gdbstub.Registers_Area := Context.all'Address;
      Gdbstub.Registers_Size := Cpu_Context'Size / 8;

      Gdbstub.Handle_Exception (SIGTRAP);
   end Exception_Handler;

   procedure Get_Register_Area (Reg : Natural;
                                Area : out Address;
                                Size : out Storage_Count) is
   begin
      case Reg is
         when 0 .. 31 =>
            Area := Regs.Gpr (Reg)'Address;
            Size := 4;
         when 32 .. 63 =>
            Area := Regs.Fpr (Reg - 32)'Address;
            Size := 8;
         when 64 =>
            Area := Regs.PC'Address;
            Size := 4;
         when 65 =>
            Area := Regs.MSR'Address;
            Size := 4;
         when 66 =>
            Area := Regs.CR'Address;
            Size := 4;
         when 67 =>
            Area := Regs.LR'Address;
            Size := 4;
         when 68 =>
            Area := Regs.CTR'Address;
            Size := 4;
         when 69 =>
            Area := Regs.XER'Address;
            Size := 4;
         when 70 =>
            Area := Regs.FPSCR'Address;
            Size := 4;
         when others =>
            Area := Null_Address;
            Size := 0;
      end case;
   end Get_Register_Area;

   type Vector_Id is range 0 .. 16#2fff#;

   System_Reset_Excp       : constant Vector_Id := 16#100#;
   Machine_Check_Excp      : constant Vector_Id := 16#200#;
   DSI_Excp                : constant Vector_Id := 16#300#;
   ISI_Excp                : constant Vector_Id := 16#400#;
   External_Interrupt_Excp : constant Vector_Id := 16#500#;
   Alignment_Excp          : constant Vector_Id := 16#600#;
   Program_Excp            : constant Vector_Id := 16#700#;
   FP_Unavailable_Excp     : constant Vector_Id := 16#800#;
   Decrementer_Excp        : constant Vector_Id := 16#900#;
   System_Call_Excp        : constant Vector_Id := 16#C00#;
   Trace_Excp              : constant Vector_Id := 16#D00#;
   FP_Assist_Excp          : constant Vector_Id := 16#E00#;

   pragma Unreferenced (Alignment_Excp);
   pragma Unreferenced (System_Reset_Excp);
   pragma Unreferenced (Machine_Check_Excp);
   pragma Unreferenced (DSI_Excp);
   pragma Unreferenced (ISI_Excp);
   pragma Unreferenced (External_Interrupt_Excp);
   pragma Unreferenced (FP_Assist_Excp);
   pragma Unreferenced (FP_Unavailable_Excp);
   pragma Unreferenced (System_Call_Excp);
   pragma Unreferenced (Decrementer_Excp);

   procedure Copy_Debug_Handler (Handler : Address;
                                 Vector : Vector_Id;
                                 Param : Integer);
   pragma Import (C, Copy_Debug_Handler);

   procedure Setup_Handlers is
   begin
      Copy_Debug_Handler (Exception_Handler'Address, Program_Excp, 0);
      Copy_Debug_Handler (Exception_Handler'Address, Trace_Excp, 1);
   end Setup_Handlers;

   procedure Breakpoint is
      procedure Debug_Trap;
      pragma Import (C, Debug_Trap);
   begin
      if True then
         Debug_Trap;
      else
         System.Machine_Code.Asm ("trap", Volatile => True);
      end if;
   end Breakpoint;

   procedure Invalidate_Icache (Start : Address; Len : Storage_Offset) is
   begin
      for I in 0 .. Len - 1 loop
         System.Machine_Code.Asm
           ("icbi 0,%0",
            Inputs => Address'Asm_Input ("r", Start + I),
            Volatile => True);
      end loop;
   end Invalidate_Icache;

   procedure Set_Trace_Flag (Trace : Boolean) is
   begin
      if Trace then
         Regs.MSR := Regs.MSR or MSR_SE;
      else
         Regs.MSR := Regs.MSR and not MSR_SE;
      end if;
   end Set_Trace_Flag;
end Gdbstub.CPU;
