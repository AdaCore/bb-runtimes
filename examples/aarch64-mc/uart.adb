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

with Interfaces.Raspberry_Pi; use Interfaces.Raspberry_Pi; use Interfaces;
with System.Text_IO;
with System.Machine_Code; use System.Machine_Code;

package body Uart is
   protected Prot is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler;
      pragma Attach_Handler (Handler, 41);

      procedure Init;
   end Prot;

   function Get_ELR_EL2 return Unsigned_64
   is
      Res : Unsigned_64;
   begin
      Asm ("mrs %0, elr_el2",
           Outputs => Unsigned_64'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ELR_EL2;

   function Get_SPSR_EL2 return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Asm ("mrs %0, spsr_el2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SPSR_EL2;

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

   protected body Prot is
      procedure Handler
      is
         C : Character;
      begin
         C := System.Text_IO.Get;
         System.Text_IO.Put (C);

         Put ("EL2 ELR:");
         Put_Hex8 (Get_ELR_EL2);
         Put (", SPSR:");
         Put_Hex4 (Get_SPSR_EL2);
         New_Line;
      end Handler;

      procedure Init is
      begin
         --  Enable receive interrupt (note that doc is incorrect, bits 0
         --  and 1 are swapped!).
         MU_IIR := 1;
      end Init;
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
end Uart;
