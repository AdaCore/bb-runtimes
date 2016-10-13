------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with System.Text_IO; use System.Text_IO;
with System.Machine_Code; use System.Machine_Code;
with System.Machine_Reset;
with Ada.Unchecked_Conversion;

package body Trap_Dump is
   procedure Put (Item : Character);
   procedure Put (Item : String);
   procedure New_Line;
   procedure Get (C : out Character);

   procedure Put_Hex8 (V : Unsigned_64);
   procedure Put_Hex4 (V : Unsigned_32);
   procedure Put_Hex1 (V : Unsigned_8);
   procedure Put_02 (N : Natural);

   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Put (Item : Character) is
   begin
      while not System.Text_IO.Is_Tx_Ready loop
         null;
      end loop;

      System.Text_IO.Put (Item);
   end Put;

   procedure Put (Item : String) is
   begin
      for J in Item'Range loop
         Put (Item (J));
      end loop;
   end Put;

   procedure New_Line is
   begin
      if System.Text_IO.Use_Cr_Lf_For_New_Line then
         Put (ASCII.CR);
      end if;

      Put (ASCII.LF);
   end New_Line;

   procedure Put_Hex8 (V : Unsigned_64) is
      Res : String (1 .. 16);
   begin
      for I in Res'Range loop
         Res (I) :=
           Hex_Digits (Natural (Shift_Right (V, 4 * (16 - I)) and 15));
      end loop;
      Put (Res);
   end Put_Hex8;

   procedure Put_Hex4 (V : Unsigned_32) is
      Res : String (1 .. 8);
   begin
      for I in Res'Range loop
         Res (I) :=
           Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;
      Put (Res);
   end Put_Hex4;

   procedure Put_Hex1 (V : Unsigned_8) is
      Res : String (1 .. 2);
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (2 - I)) and 15));
      end loop;
      Put (Res);
   end Put_Hex1;

   procedure Put_02 (N : Natural) is
      Res : String (1 .. 2);
   begin
      Res (1) := Character'Val (48 + N / 10);
      Res (2) := Character'Val (48 + N mod 10);
      Put (Res);
   end Put_02;

   procedure Get (C : out Character) is
   begin
      while not Is_Rx_Ready loop
         null;
      end loop;

      C := System.Text_IO.Get;
   end Get;

   --  Access to registers
   function Get_Current_EL return Unsigned_32;
   function Get_ELR_EL3 return Unsigned_64;
   procedure Set_ELR_EL3 (V : Unsigned_64);
   function Get_SPSR_EL3 return Unsigned_64;
   function Get_ESR_EL3 return Unsigned_32;
   function Get_FAR_EL3 return Unsigned_64;

   function Get_ELR_EL2 return Unsigned_64;
   procedure Set_ELR_EL2 (V : Unsigned_64);
   function Get_SPSR_EL2 return Unsigned_64;
   function Get_ESR_EL2 return Unsigned_32;
   function Get_FAR_EL2 return Unsigned_64;
   function Get_SP_EL2 return Unsigned_64;

   function Get_ELR_EL1 return Unsigned_64;
   procedure Set_ELR_EL1 (V : Unsigned_64);
   function Get_SPSR_EL1 return Unsigned_64;
   function Get_ESR_EL1 return Unsigned_32;
   function Get_FAR_EL1 return Unsigned_64;
   function Get_SP_EL1 return Unsigned_64;

   function Get_SP_EL0 return Unsigned_64;

   procedure Print_ESR (ESR : Unsigned_32);

   function Get_Current_EL return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, currentel",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Current_EL;

   function Get_ELR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL3;

   procedure Set_ELR_EL3 (V : Unsigned_64) is
   begin
      Asm ("msr elr_el3, %0",
           Inputs => Unsigned_64'Asm_Input ("r", V),
           Volatile => True);
   end Set_ELR_EL3;

   function Get_SPSR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, spsr_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR_EL3;

   function Get_ESR_EL3 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, esr_el3",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ESR_EL3;

   function Get_FAR_EL3 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, far_el3",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_FAR_EL3;

   function Get_ELR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL2;

   procedure Set_ELR_EL2 (V : Unsigned_64) is
   begin
      Asm ("msr elr_el2, %0",
           Inputs => Unsigned_64'Asm_Input ("r", V),
           Volatile => True);
   end Set_ELR_EL2;

   function Get_SPSR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, spsr_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR_EL2;

   function Get_ESR_EL2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, esr_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ESR_EL2;

   function Get_FAR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, far_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_FAR_EL2;

   function Get_SP_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL2;

   function Get_ELR_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL1;

   procedure Set_ELR_EL1 (V : Unsigned_64) is
   begin
      Asm ("msr elr_el1, %0",
           Inputs => Unsigned_64'Asm_Input ("r", V),
           Volatile => True);
   end Set_ELR_EL1;

   function Get_SPSR_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, spsr_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR_EL1;

   function Get_ESR_EL1 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, esr_el1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ESR_EL1;

   function Get_FAR_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, far_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_FAR_EL1;

   function Get_SP_EL1 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el1",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL1;

   function Get_SP_EL0 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, sp_el0",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SP_EL0;

   procedure Print_ESR (ESR : Unsigned_32) is
   begin
      Put ("ESR:");
      Put_Hex4 (ESR);
      Put (" (EC:");
      Put_Hex1 (Unsigned_8 (Shift_Right (ESR, 26)));
      Put (')');
   end Print_ESR;

   procedure Dump (Regs : Registers_List_Acc; Id : Natural)
   is
      function To_Unsigned_64 is new Ada.Unchecked_Conversion
        (Registers_List_Acc, Unsigned_64);
      EL : Unsigned_32;
      C : Character;
   begin
      --  Initialize console in case of very early crash
      if not Initialized then
         Initialize;
      end if;

      Put ("X registers:");
      New_Line;
      for I in 0 .. 30 loop
         if I mod 4 /= 0 then
            Put (' ');
         end if;
         Put_02 (I);
         Put (':');
         Put_Hex8 (Regs.Xr (I));
         if (I mod 4 = 3) or I = 30 then
            New_Line;
         end if;
      end loop;

      Put ("id: ");
      Put_Hex1 (Unsigned_8 (Id));
      if (Id mod 4) = 1 then
         Put (" synchronous exception");
      end if;

      EL := Get_Current_EL;
      Put ("  Current_EL: ");
      Put_Hex1 (Unsigned_8 (EL / 4));
      Put ("    SP: ");
      Put_Hex8 (To_Unsigned_64 (Regs));
      New_Line;

      if EL = 3 * 4 then
         Put ("EL3 ELR:");
         Put_Hex8 (Get_ELR_EL3);
         Put (", SPSR:");
         Put_Hex8 (Get_SPSR_EL3);
         New_Line;
         Put ("EL3 ");
         Print_ESR (Get_ESR_EL3);
         Put (", FAR:");
         Put_Hex8 (Get_FAR_EL3);
         Put (", EL2 SP:");
         Put_Hex8 (Get_SP_EL2);
         New_Line;
      end if;

      if EL >= 2 * 4 then
         Put ("EL2 ELR:");
         Put_Hex8 (Get_ELR_EL2);
         Put (", SPSR:");
         Put_Hex8 (Get_SPSR_EL2);
         New_Line;
         Put ("EL2 ");
         Print_ESR (Get_ESR_EL2);
         Put (", FAR:");
         Put_Hex8 (Get_FAR_EL2);
         Put (", EL1 SP:");
         Put_Hex8 (Get_SP_EL1);
         New_Line;
      end if;

      if EL >= 1 * 4 then
         Put ("EL1 ELR:");
         Put_Hex8 (Get_ELR_EL1);
         Put (", SPSR:");
         Put_Hex8 (Get_SPSR_EL1);
         New_Line;
         Put ("EL1 ");
         Print_ESR (Get_ESR_EL1);
         Put (", FAR:");
         Put_Hex8 (Get_FAR_EL1);
         Put (", EL0 SP:");
         Put_Hex8 (Get_SP_EL0);
         New_Line;
      end if;

      --  Interractive (!!) session.
      Put ("TMON: (C)ont/(R)eset/(N)ext ?");

      loop
         Get (C);
         Put (C);

         case C is
            when 'c' | 'C' =>
               New_Line;
               return;
            when 'n' | 'N' =>
               case EL is
                  when 1 * 4 =>
                     Set_ELR_EL1 (Get_ELR_EL1 + 4);
                  when 2 * 4 =>
                     Set_ELR_EL2 (Get_ELR_EL2 + 4);
                  when 3 * 4 =>
                     Set_ELR_EL3 (Get_ELR_EL3 + 4);
                  when others =>
                     null;
               end case;
               New_Line;
               return;
            when 'r' | 'R' =>
               System.Machine_Reset.Stop;
            when others =>
               Put ('?');
         end case;
      end loop;
   end Dump;
end Trap_Dump;
