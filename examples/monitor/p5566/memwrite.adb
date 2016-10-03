------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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
with Cache; use Cache;
with System.Machine_Code; use System.Machine_Code;
with Flash;

package body Memwrite is
   Invalid_Addr : constant Unsigned_32 := 1;
   --  An invalid address to mark the buffer as uninitialized.

   Buffer_Addr : Unsigned_32 := Invalid_Addr;
   --  Address at which the buffer must be written in memory.  Use Invalid_Addr
   --  to mark the address invalid (in that case the buffer is considered as
   --  empty).

   Buffer_Len : constant := 32;
   Addr_Mask : constant Unsigned_32 := not (Buffer_Len - 1);

   Buffer : Storage_Array (0 .. Storage_Count (Buffer_Len - 1));
   for Buffer'Alignment use 4;
   --  Buffer of data to be written at Buffer_Addr

   procedure Init is
   begin
      Buffer_Addr := Invalid_Addr;
      Buffer := (others => 16#ff#);
   end Init;

   procedure Flush is
      use Flash;

      Off : Unsigned_32;
      Off1 : Storage_Count;
      In_Flash : constant Boolean := Flash.In_Flash (Buffer_Addr);
   begin
      if In_Flash then
         --  Invalidate corresponding cache entries (if any).
         --  Note that the flash is cache inhibited when unlocked
         Off := 0;
         while Off < Buffer'Length loop
            Asm ("dcbi 0,%0",
                 Inputs => Unsigned_32'Asm_Input ("r", Buffer_Addr + Off),
                 Volatile => True);
            Off := Off + Cache.Cache_Line;
         end loop;

         Flash_Start_Prog;
      end if;

      --  Write buffer.  Copy per word, for FLASH.
      Off1 := 0;
      while Off1 < Storage_Count (Buffer_Len) loop
         declare
            D : Unsigned_32;
            for D'Address use
              System'To_Address (Buffer_Addr + Unsigned_32 (Off1));
            pragma Import (Ada, D);
            pragma Volatile (D);

            pragma Warnings (Off, "specified address*");
            S : Unsigned_32;
            for S'Address use Buffer (Off1)'Address;
            pragma Warnings (On, "specified address*");
         begin
            D := S;
         end;
         Off1 := Off1 + 4;
      end loop;

      if In_Flash then
         Flash_Wait_Prog;
      end if;

      Buffer := (others => 16#ff#);

      --  Flush and sync caches
      --  Not strictly needed on mpc5566, as the cache is unified
      Cache.Cache_Flush_Range
        (System'To_Address (Buffer_Addr),
         System'To_Address (Buffer_Addr + Buffer_Len - 1));
   end Flush;

   procedure Write (Addr : Unsigned_32; Content : Storage_Array) is
      Cur_Addr : Unsigned_32 := Addr;
   begin
      for I in Content'Range loop
         if Buffer_Addr = Invalid_Addr then
            Buffer_Addr := Cur_Addr and Addr_Mask;
         elsif (Buffer_Addr and Addr_Mask) /= (Cur_Addr and Addr_Mask) then
            Flush;
            Buffer_Addr := Cur_Addr and Addr_Mask;
         end if;
         Buffer (Storage_Count (Cur_Addr and not Addr_Mask)) := Content (I);
         Cur_Addr := Cur_Addr + 1;
      end loop;
   end Write;
end Memwrite;
