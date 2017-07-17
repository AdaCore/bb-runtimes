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

with System;
with Interfaces.Raspberry_Pi; use Interfaces;

package body Timer is

   type Timer_Handler is new Interrupt_Ack_Cb with record
      C : CPU;
      IT_Dev : Interrupt_Dev_Acc;
      IT_Id : Natural;
   end record;
   procedure Ack (This : Timer_Handler);

   type Timer_Handler_Arr is array (CPU) of aliased Timer_Handler;

   Handlers : Timer_Handler_Arr;

   procedure Set_Handler (Target_CPU : CPU;
                          IT_Dev : Interrupt_Dev_Acc;
                          IT_Id : Natural)
   is
      Hand : Timer_Handler renames Handlers (Target_CPU);
   begin
      if Hand.IT_Dev /= null then
         --  Cannot override.
         raise Program_Error;
      end if;

      Hand := (Target_CPU, IT_Dev, IT_Id);
      IT_Dev.Set_Ack_Cb (IT_Id, Hand'Access);
   end Set_Handler;

   protected Prot_Cntv is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler;
      pragma Attach_Handler (Handler, 3);

      procedure Enable (C : CPU);
   end Prot_Cntv;

   protected body Prot_Cntv is
      procedure Disable (C : CPU) is
         use Interfaces.Raspberry_Pi;
         Idx : constant Natural := Natural (C);
      begin
         Local_Registers.Cores_Timer_Int_Ctr (Idx) :=
           Local_Registers.Cores_Timer_Int_Ctr (Idx) and 2#0111#;
      end Disable;

      procedure Enable (C : CPU) is
         use Interfaces.Raspberry_Pi;
         Idx : constant Natural := Natural (C);
      begin
         Local_Registers.Cores_Timer_Int_Ctr (Idx) :=
           Local_Registers.Cores_Timer_Int_Ctr (Idx) or 2#1000#;
      end Enable;

      procedure Handler
      is
         Cur_Cpu : constant CPU := Current_CPU;
      begin
         --  Put ("Timer");

         --  Mask the interrupt until it is delivered.
         Disable (Cur_Cpu);

         --  Signal the interrupt.
         declare
            Hand : Timer_Handler renames Handlers (Cur_Cpu);
         begin
            Set_Level (Hand.IT_Dev.all, Hand.IT_Id, True);
         end;
      end Handler;
   end Prot_Cntv;

   procedure Ack (This : Timer_Handler) is
   begin
      Set_Level (This.IT_Dev.all, This.IT_Id, False);

      Prot_Cntv.Enable (This.C);
   end Ack;

   pragma Unreferenced (Prot_Cntv);
end Timer;
