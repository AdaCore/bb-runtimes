------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

pragma Warnings (Off, "* internal GNAT unit");
with System.Text_IO; use System.Text_IO;
pragma Warnings (On, "* internal GNAT unit");
with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;

package body Trap_Handler is
   procedure Put (Item : Character);
   procedure Put (Item : String);
   procedure New_Line;
   procedure Get (C : out Character);

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

   pragma Unreferenced (Put_Hex1, Get);

   function Get_CPSR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0,cpsr",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_CPSR;

   function Get_SPSR return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0,spsr",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR;

   procedure Set_TLB_Data_Read_Operation_Reg (V : Unsigned_32) is
   begin
      Asm ("mcr p15, #3, %0, c15, c4, #2",
           Inputs => Unsigned_32'Asm_Input ("r", V),
           Volatile => True);
   end Set_TLB_Data_Read_Operation_Reg;

   function Get_Data_Register_0 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #3, %0, c15, c0, #0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Data_Register_0;

   function Get_Data_Register_1 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #3, %0, c15, c0, #1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Data_Register_1;

   function Get_Data_Register_2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #3, %0, c15, c0, #2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Data_Register_2;

   function Get_Data_Register_3 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, #3, %0, c15, c0, #3",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Data_Register_3;

   procedure Traps_Handler (Ctxt : Trap_Context_Acc; Num : Natural)
   is
      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (Trap_Context_Acc, Unsigned_32);
   begin
      --  Initialize console in case of very early crash
      if not Initialized then
         Initialize;
      end if;

      for I in 0 .. 14 loop
         if I mod 4 /= 0 then
            Put (' ');
         end if;
         if I = 13 then
            Put ("SP:");
         elsif I = 14 then
            Put ("LR:");
         else
            Put_02 (I);
            Put (':');
         end if;
         Put_Hex4 (Ctxt.R (I));
         if (I mod 4) = 3 then
            New_Line;
         end if;
      end loop;
      Put (" PC:");
      Put_Hex4 (Ctxt.PC);
      New_Line;

      Put ("SPSR:");
      Put_Hex4 (Get_SPSR);

      Put ("  CPSR:");
      Put_Hex4 (Get_CPSR);

      Put ("  CurSP:");
      Put_Hex4 (To_Unsigned_32 (Ctxt));

      Put ("  Trap #");
      Put_02 (Num);
      New_Line;

      if (Get_CPSR and 16#1f#) = 16#16# then
         Put ("TLB:  PA  VA");
         New_Line;
         for I in Unsigned_32 range 0 .. 3 loop
            for J in Unsigned_32 range 0 .. 127 loop
               declare
                  function SL (V : Unsigned_32; S : Natural) return Unsigned_32
                    renames Shift_Left;
                  function SR (V : Unsigned_32; S : Natural) return Unsigned_32
                    renames Shift_Right;
                  En : constant Unsigned_32 := I * 2**30 + J;
                  D0, D1, D2 : Unsigned_32;
               begin
                  Set_TLB_Data_Read_Operation_Reg (I * 2**30 + J);
                  D0 := Get_Data_Register_0;

                  if (D0 and 1) = 1 then
                     D2 := Get_Data_Register_2;
                     D1 := Get_Data_Register_1;
                     Put_Hex4 (En);
                     Put (": ");
                     Put_Hex4 (Get_Data_Register_3);
                     Put (' ');
                     Put_Hex4 (D2);
                     Put (' ');
                     Put_Hex4 (D1);
                     Put (' ');
                     Put_Hex4 (D0);
                     Put ("  ");
                     Put_Hex4 (SL (SR (D2 and 16#3fff_fffc#, 2), 12));
                     Put (' ');
                     Put_Hex4 (SL (SR (D0 and 16#7fff_fffc#, 2), 12));
                     Put (' ');
                     case SR (D1, 24) and 7 is
                        when 2#000#
                          |  2#001# => Put (" 4K");
                        when 2#010#
                          |  2#011# => Put ("64K");
                        when 2#100# => Put (" 1M");
                        when 2#101# => Put (" 2M");
                        when 2#110# => Put ("16M");
                        when 2#111# => Put (".5G");
                        when others => null;
                     end case;
                     New_Line;
                  end if;
               end;
            end loop;
         end loop;
      end if;

      loop
         null;
      end loop;
   end Traps_Handler;

   procedure Undef_Trap_Mon;
   pragma Import (C, Undef_Trap_Mon, "__gnat_undef_trap_mon");

   procedure Smc_Trap_Mon;
   pragma Import (C, Smc_Trap_Mon, "__gnat_smc_trap_mon");

   procedure Pabt_Trap_Mon;
   pragma Import (C, Pabt_Trap_Mon, "__gnat_pabt_trap_mon");

   procedure Dabt_Trap_Mon;
   pragma Import (C, Dabt_Trap_Mon, "__gnat_dabt_trap_mon");

   procedure Irq_Trap_Mon;
   pragma Import (C, Irq_Trap_Mon, "__gnat_irq_trap_mon");

   procedure Fiq_Trap_Mon;
   pragma Import (C, Fiq_Trap_Mon, "__gnat_fiq_trap_mon");

   procedure Smc_Switch_Mon;
   pragma Import (C, Smc_Switch_Mon, "__gnat_smc_switch_mon");

   procedure Install_Monitor_Handlers
   is
      use System;

      Except : array (0 .. 7) of Unsigned_32
        with Import, Volatile, Address => System'To_Address (0);
      Vectors : array (0 .. 7) of Address
        with Import, Volatile, Address => System'To_Address (16#20#);
      Ldr_Pc_Pc_20 : constant Unsigned_32 := 16#e59ff018#;
   begin
      Except (0 .. 7) := (others => Ldr_Pc_Pc_20);
      Vectors (0 .. 7) := (Null_Address,
                           Undef_Trap_Mon'Address,
                           Smc_Trap_Mon'Address,
                           Pabt_Trap_Mon'Address,
                           Dabt_Trap_Mon'Address,
                           Null_Address,
                           Irq_Trap_Mon'Address,
                           Fiq_Trap_Mon'Address);
      for I in 0 .. 7 loop
         --  DCCIMVAC
         Asm ("mcr p15, #0, %0, c7, c14, #1",
              Inputs => Address'Asm_Input ("r", Except (I)'Address),
              Volatile => True);

         --  ICIMVAU
         Asm ("mcr p15, #0, %0, c7, c5, #1",
              Inputs => Address'Asm_Input ("r", Except (I)'Address),
              Volatile => True);
      end loop;

      Asm ("dsb", Volatile => True);
      Asm ("isb", Volatile => True);

      if False then
         --  That doesn't work if cache is enabled!
         Put ("Switch to monitor from cpsr ");
         Put_Hex4 (Get_CPSR);
         New_Line;
         declare
            R : Unsigned_32;
         begin
            Vectors (2) := Smc_Switch_Mon'Address;
            Asm ("mov %0, sp",
                 Outputs => Unsigned_32'Asm_Output ("=r", R),
                 Volatile => True);
            Put ("SP=");
            Put_Hex4 (R);
            Asm ("dsb; isb; smc #0; mov %0, r1",
                 Outputs => Unsigned_32'Asm_Output ("=r", R),
                 Volatile => True,
                 Clobber => "r1, r2");
            Put (" R=");
            Put_Hex4 (R);
            Asm ("mov %0, sp",
                 Outputs => Unsigned_32'Asm_Output ("=r", R),
                 Volatile => True);
            Put (" SP=");
            Put_Hex4 (R);
            New_Line;
         end;
         Vectors (2) := Smc_Trap_Mon'Address;
      end if;
   end Install_Monitor_Handlers;
end Trap_Handler;
