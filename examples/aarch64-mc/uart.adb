------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Interfaces.Raspberry_Pi; use Interfaces.Raspberry_Pi;
with System.Machine_Code;
pragma Warnings (Off);
with System.Text_IO;
with System.Machine_Reset;
pragma Warnings (On);
with Interfaces.AArch64; use Interfaces.AArch64;

package body Uart is
   Use_Mini_Uart : constant Boolean := False;

   type Char_Emu_Acc_Arr is array (Natural range <>) of Char_Emu_Acc;

   protected Prot is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler;
      pragma Attach_Handler (Handler, (if Use_Mini_Uart then 41 else 69));

      procedure Init;

      procedure Register_Client (Client : Char_Emu_Acc);
   private
      In_Meta : Boolean := False;
      Clients : Char_Emu_Acc_Arr (0 .. 7);
      Nbr_Clients : Natural := 0;
      Cur_Client : Natural := 0;
   end Prot;

   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Put (C : Character) is
   begin
      while not System.Text_IO.Is_Tx_Ready loop
         null;
      end loop;

      System.Text_IO.Put (C);
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

   procedure Put_Dec (N : Natural) is
      D, R : Natural;
   begin
      if N < 10 then
         R := N;
      else
         D := N / 10;
         R := N - D * 10;
         Put_Dec (D);
      end if;
      Put (Character'Val (Character'Pos ('0') + R));
   end Put_Dec;

   protected body Prot is
      procedure Send_Char (C : Unsigned_32) is
      begin
         if Cur_Client < Nbr_Clients then
            Clients (Cur_Client).Put (C);
         end if;
      end Send_Char;

      procedure Handler
      is
         C : Character;
      begin
         --  Read character
         C := System.Text_IO.Get;

         if C = Character'Val (20) then
            Put ("EL2 ELR:");
            Put_Hex8 (Get_ELR_EL2);
            Put (", SPSR:");
            Put_Hex4 (Get_SPSR_EL2);
            Put (", ESR:");
            Put_Hex4 (Get_ESR_EL2);
            New_Line;
            In_Meta := True;
            return;
         end if;

         if In_Meta then
            if C = Character'Val (18) then
               --  C-r: reboot
               System.Machine_Reset.Stop;
               return;
            elsif C = Character'Val (2) then
               --  C-b: send break
               Send_Char (Break);
               return;
            elsif C = Character'Val (8) then
               --  C-h: monitor
               System.Machine_Code.Asm ("smc #1", Volatile => True);
            elsif C = 'n' then
               Cur_Client := Cur_Client + 1;
               if Cur_Client = Nbr_Clients then
                  Cur_Client := 0;
               end if;
               return;
            end if;
         end if;

         In_Meta := False;

         Send_Char (Character'Pos (C));
      end Handler;

      procedure Init is
      begin
         --  Enable receive interrupt (note that doc is incorrect, bits 0
         --  and 1 are swapped!).
         if Use_Mini_Uart then
            MU_IIR := 1;
         else
            PL011_Registers.IFLS := 0;
            PL011_Registers.IMSC :=
              PL011_Registers.IMSC or PL011_Bits.MASK_RT;
         end if;
      end Init;

      procedure Register_Client (Client : Char_Emu_Acc) is
      begin
         if Nbr_Clients = Clients'Last then
            raise Constraint_Error;
         end if;
         Clients (Nbr_Clients) := Client;
         Nbr_Clients := Nbr_Clients + 1;
      end Register_Client;
   end Prot;

   procedure Get (C : out Character) is
   begin
      raise Program_Error;
   end Get;

   procedure Init is
   begin
      if not System.Text_IO.Initialized then
         System.Text_IO.Initialize;
      end if;

      Prot.Init;
   end Init;

   procedure Dump_Status is
   begin
      Put ("CR: ");
      Put_Hex4 (PL011_Registers.CR);
      Put (", FR: ");
      Put_Hex4 (PL011_Registers.FR);
      Put (", RIS: ");
      Put_Hex4 (PL011_Registers.RIS);
      Put (", MIS: ");
      Put_Hex4 (PL011_Registers.MIS);
      Put (", IMSC: ");
      Put_Hex4 (PL011_Registers.IMSC);
      New_Line;
   end Dump_Status;

   procedure Put (Emu : in out Uart_Emu_Type; C : Unsigned_32)
   is
      pragma Unreferenced (Emu);
   begin
      if C <= 255 then
         Put (Character'Val (C));
      end if;
   end Put;

   procedure Register_Client (Emu : Char_Emu_Acc) is
   begin
      Prot.Register_Client (Emu);
   end Register_Client;

end Uart;
