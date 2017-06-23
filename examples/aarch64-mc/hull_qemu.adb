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

with Timer;
with Emu_GIC; use Emu_GIC;

package body Hull_Qemu is
   procedure Init (H : Hull_Qemu_Acc) is
   begin
      H.Iomap :=
        (0 => (System'To_Address (16#0900_0000#), 16#1000#,
               H.Uart'Unchecked_Access),
         1 => (System'To_Address (16#0800_0000#), 16#2_0000#,
               H.GIC'Unchecked_Access));
      H.Debug := (Parent => H);

      Emu_PL011.Init (H.Uart'Access,
                      Get_Interrupt_Dev (H.GIC'Access), 33,
                      H.Debug'Access);
      Emu_GIC.Init (H.GIC'Access, H.Interrupt_Dev'Access);

      Timer.Set_Handler (Current_CPU,
                         Get_Interrupt_Dev (H.GIC'Access), 16 + 11);
   end Init;

   procedure Find_IO
     (Hull : Hull_Qemu_Type;
      Addr : Address;
      Dev : out IOEmu_Dev_Acc;
      Off : out Off_T) is
   begin
      Find_IO (Hull.Iomap, Addr, Dev, Off);
   end Find_IO;

   procedure Debug (Dev : in out Qemu_Debug_Dev) is
   begin
      Dev.Parent.GIC.Dump;
      Dev.Parent.Uart.Dump;

      Dump_Cpu (Dev.Parent.all);
   end Debug;
end Hull_Qemu;
