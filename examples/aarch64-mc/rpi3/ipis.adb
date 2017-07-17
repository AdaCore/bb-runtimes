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
with IOEmu;

package body IPIs is

   protected Prot is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      --  Use mailbox 2 for IPIs.
      procedure Handler;
      pragma Attach_Handler (Handler, 6);
   end Prot;

   protected body Prot is
      procedure Handler
      is
         Cur_CPU : constant CPU := IOEmu.Current_CPU;
      begin
         Local_Registers.Cores_Mailboxes_Read_Clr (Natural (Cur_CPU), 2) := 1;
      end Handler;
   end Prot;

   pragma Unreferenced (Prot);

   procedure Send_Ipi (Target : CPU) is
   begin
      Local_Registers.Cores_Mailboxes_Write_Set (Natural (Target), 2) := 1;
   end Send_Ipi;
end IPIs;
