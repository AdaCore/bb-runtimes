------------------------------------------------------------------------------
--                              T M A L L O C                               --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
--                                                                          --
-- This is free software; you can redistribute it  and/or modify it         --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
with System.C.Malloc; use System.C.Malloc;
with System.C.Malloc.Ext; use System.C.Malloc.Ext;
use System;
use System.C;
with Heap;

procedure Tmalloc is
   subtype Array_Range is Integer range 1 .. 256;
   subtype Size_Range is Size_T range 0 .. 511;
   package Arr_Random is new Ada.Numerics.Discrete_Random (Array_Range);
   package Size_Random is new Ada.Numerics.Discrete_Random (Size_Range);
   type Addr_Array is array (Natural range <>) of Address;
   Addrs : Addr_Array (Array_Range) := (others => Null_Address);
   P : Array_Range;
   P_Gen : Arr_Random.Generator;
   S_Gen : Size_Random.Generator;
   S : size_t;
   Verbose : constant Boolean := True;
begin
   Arr_Random.Reset (P_Gen, 3);
   Size_Random.Reset (S_Gen, 0);
   for I in 1 .. 800_000 loop
      P := Arr_Random.Random (P_Gen);
      if Addrs (P) = Null_Address then
         S := Size_Random.Random (S_Gen);
         Addrs (P) := Alloc (S);
         if Verbose then
            Put_Line ("Alloc block" & Array_Range'Image (P)
                        & " = " & Image (Addrs (P)) & ", size ="
                        & size_t'Image (S));
         end if;
      else
         if Verbose then
            Put_Line ("Free  block" & Array_Range'Image (P)
                        & " (addr = " & Image (Addrs (P)) & ")");
         end if;
         Free (Addrs (P));
         Addrs (P) := Null_Address;
      end if;
      Check;
      --  Disp_Stats;
      --  Disp_Heap;
   end loop;

   Put_Line ("Freeing all blocks");

   for I in Addrs'Range loop
      Free (Addrs (I));
      Check;
   end loop;

   Put_Line ("Heap at end:");
   Disp_Heap;
end Tmalloc;
