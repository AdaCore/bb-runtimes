------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M .  M E M O R Y _ C O P Y                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2014, Free Software Foundation, Inc.       --
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

with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;

package body System.Memory_Copy is

   type IA is mod System.Memory_Size;
   --  The type used to provide the actual desired operations

   function To_IA is new Ada.Unchecked_Conversion (Address, IA);
   --  The operations are implemented by unchecked conversion to type IA,
   --  followed by doing the intrinsic operation on the IA values, followed
   --  by converting the result back to type Address.

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  Byte is the storage unit

   type Byte_Ptr is access Byte;
   --  Access to a byte

   function To_Byte_Ptr is new Ada.Unchecked_Conversion (IA, Byte_Ptr);
   --  Conversion between an integer address and access to byte

   Byte_Size : constant := 1;
   --  Number of storage unit in a byte

   type Word is mod 2 ** System.Word_Size;
   for Word'Size use System.Word_Size;
   --  Word is efficiently loaded and stored by the processor, but has
   --  alignment constraints.

   type Word_Ptr is access Word;
   --  Access to a word.

   function To_Word_Ptr is new Ada.Unchecked_Conversion (IA, Word_Ptr);
   --  Conversion from an integer adddress to word access

   Word_Size : constant := Word'Size / Storage_Unit;
   --  Number of storage unit per word

   ------------
   -- memcpy --
   ------------

   function memcpy
     (Dest : Address; Src : Address; N : size_t) return Address
   is
      D : IA     := To_IA (Dest);
      S : IA     := To_IA (Src);
      C : size_t := N;

   begin
      --  Try to copy per word, if alignment constraints are respected

      if ((D or S) and (Word'Alignment - 1)) = 0 then
         while C >= Word_Size loop
            To_Word_Ptr (D).all := To_Word_Ptr (S).all;
            D := D + Word_Size;
            S := S + Word_Size;
            C := C - Word_Size;
         end loop;
      end if;

      --  Copy the remaining byte per byte

      while C > 0 loop
         To_Byte_Ptr (D).all := To_Byte_Ptr (S).all;
         D := D + Byte_Size;
         S := S + Byte_Size;
         C := C - Byte_Size;
      end loop;

      return Dest;
   end memcpy;

end System.Memory_Copy;
