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

with System.Machine_Code; use System.Machine_Code;
with Ada.Text_IO; use Ada.Text_IO;

package body Hulls is
   procedure Start_Hull (Mmu_Map : Address; Start : Address);
   pragma Import (Asm, Start_Hull);

   procedure DC_CVAU (Addr : Address) is
   begin
      Asm ("dc cvau, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end DC_CVAU;

   procedure IC_IVAU (Addr : Address) is
   begin
      Asm ("ic ivau, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);
   end IC_IVAU;

   procedure DSB is
   begin
      Asm ("dsb ish", Volatile => True);
   end DSB;

   procedure ISB is
   begin
      Asm ("isb", Volatile => True);
   end ISB;

   procedure Cache_Sync_By_Range
     (Start : System.Address;
      Len   : System.Storage_Elements.Storage_Count)
   is
      Line_Size : constant := 16;
      Line_Off : Storage_Count;
      Off : Storage_Count;
      Addr : Address;
   begin
      Line_Off := Start mod Line_Size;
      Addr := Start - Line_Off;
      Off := 0;
      loop
         DC_CVAU (Addr);
         IC_IVAU (Addr);
         Off := Off + Line_Size;
         exit when Off > Len + Line_Off;
         Addr := Addr + Line_Size;
      end loop;

      DSB;
      ISB;
   end Cache_Sync_By_Range;

   procedure Create_Hull (Desc : Hull_Desc) is
   begin
      --  Copy file
      if Desc.File_Base /= Null_Address then
         Put_Line ("Copying Hull......");

         if Desc.File_Size > Desc.Ram_Size then
            raise Constraint_Error;
         end if;

         declare
            Src : Storage_Array (1 .. Desc.File_Size);
            pragma Import (Ada, Src);
            for Src'Address use Desc.File_Base;

            Dest : Storage_Array (1 .. Desc.File_Size);
            pragma Import (Ada, Dest);
            for Dest'Address use Desc.Ram_Paddr;
         begin
            Dest := Src;

            Cache_Sync_By_Range (Dest'Address, Desc.File_Size);
         end;
      end if;

      Put_Line ("Starting Hull......");

      Start_Hull (Desc.Mmu_Base, Desc.Ram_Vaddr);
   end Create_Hull;
end Hulls;
