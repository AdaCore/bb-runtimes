------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . C . M A L L O C                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
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

with System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body System.C.Malloc is
   package SSE renames System.Storage_Elements;
   use SSE;

   Heap_Start : Character;
   for Heap_Start'Alignment use Standard'Maximum_Alignment;
   pragma Import (C, Heap_Start, "__heap_start");
   --  The address of the variable is the start of the heap

   Heap_End : Character;
   pragma Import (C, Heap_End, "__heap_end");
   --  The address of the variable is the end of the heap

   function Get_Cell_Data (Cell : Cell_Acc) return Address;

   procedure Add_Free_Cell (Cell : Free_Cell_Acc);
   --  Add a cell to the free chain

   procedure Remove_Free_Cell (Cell : Free_Cell_Acc);
   --  Remove free cell from free chain

   function To_Cell_Acc is new Ada.Unchecked_Conversion
     (Address, Cell_Acc);
   function To_Cell_Acc is new Ada.Unchecked_Conversion
     (Free_Cell_Acc, Cell_Acc);
   function To_Address is new Ada.Unchecked_Conversion
     (Cell_Acc, Address);
   function To_Free_Cell_Acc is new Ada.Unchecked_Conversion
     (Cell_Acc, Free_Cell_Acc);
   function To_Free_Cell_Acc is new Ada.Unchecked_Conversion
     (Address, Free_Cell_Acc);

   Cell_Size : constant SSE.Storage_Offset :=
                 Cell_Type'Size / Storage_Unit;

   Free_Cell_Size : constant SSE.Storage_Offset :=
                      Free_Cell_Type'Size / Storage_Unit;

   -------------------
   -- Add_Free_Cell --
   -------------------

   procedure Add_Free_Cell (Cell : Free_Cell_Acc) is
      Next : Free_Cell_Acc;
      Cur  : Free_Cell_Acc;

   begin
      --  Follow the chain until NEXT is larger then CELL

      Next := Free_List;
      Cur := null;
      while Next /= null loop
         exit when Next.Cell.Size >= Cell.Cell.Size;
         Cur := Next;
         Next := Next.Next_Free;
      end loop;

      --  Insert

      Cell.Prev_Free := Cur;

      if Cur = null then
         Cell.Next_Free := Free_List;

         if Free_List /= null then
            Free_List.Prev_Free := Cell;
         end if;

         Free_List := Cell;

      else
         Cell.Next_Free := Next;

         if Next /= null then
            Next.Prev_Free := Cell;
         end if;

         Cur.Next_Free := Cell;
      end if;
   end Add_Free_Cell;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return Address is
      Rounded_Size : size_t;

   begin
      --  Return null address for zero length request

      if Size = 0 then
         return Null_Address;
      end if;

      --  Round size up

      Rounded_Size := (Size + Standard'Maximum_Alignment);
      Rounded_Size :=
        Rounded_Size - Rounded_Size rem Standard'Maximum_Alignment;

      --  Find a free cell

      declare
         Res           : Free_Cell_Acc;
         Next_Cell     : Free_Cell_Acc;
         New_Next_Cell : Free_Cell_Acc;

      begin
         Res := Free_List;

         while Res /= null loop

            --  The last cell is not a free cell

            pragma Assert (To_Cell_Acc (Res) /= Last_Cell);

            if Res.Cell.Size >= Rounded_Size then

               --  Remove it from the list

               Remove_Free_Cell (Res);

               --  Can we split it?

               if Res.Cell.Size - Rounded_Size >= size_t (Free_Cell_Size) then
                  Next_Cell :=
                    To_Free_Cell_Acc (Get_Next_Cell (To_Cell_Acc (Res)));

                  --  Create the new cell

                  New_Next_Cell :=
                    To_Free_Cell_Acc
                      (Get_Cell_Data (To_Cell_Acc (Res)) +
                         Storage_Offset (Rounded_Size));

                  New_Next_Cell.Cell :=
                    (Size => Res.Cell.Size - Rounded_Size - size_t (Cell_Size),
                     Prev => To_Cell_Acc (Res),
                     Free => True);

                  Next_Cell.Cell.Prev := To_Cell_Acc (New_Next_Cell);

                  --  Resize the returned cell

                  Res.Cell.Size := Rounded_Size;

                  --  Add the new cell to the free list

                  Add_Free_Cell (New_Next_Cell);
               end if;

               Res.Cell.Free := False;
               return Get_Cell_Data (To_Cell_Acc (Res));
            end if;

            Res := Res.Next_Free;
         end loop;
      end;

      --  No free block so create a new block

      declare
         Res : Cell_Acc;

      begin
         if Last_Cell = null then

            --  Do we need to check alignment ???

            Res := Get_First_Cell;

         else
            Res := Get_Next_Cell (Last_Cell);
         end if;

         Res.all := (Prev => Last_Cell,
                     Size => Rounded_Size,
                     Free => False);

         --  Check heap exhaustion, and if so return null address

         if To_Address (Get_Next_Cell (Res)) > Heap_End'Address then
            return Null_Address;
         end if;

         Last_Cell := Res;
         return Get_Cell_Data (Res);
      end;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : Address) is
      Cell : Cell_Acc;

   begin
      --  Nothing to do if null address passed

      if Ptr = Null_Address then
         return;
      end if;

      Cell := To_Cell_Acc (Ptr - Cell_Size);
      pragma Assert (not Cell.Free);
      Cell.Free := True;

      --  If Cell is the last one, free it directly

      if Cell = Last_Cell then
         Last_Cell := Cell.Prev;

         --  The one before the last may be free too

         if Last_Cell /= null and then Last_Cell.Free then

            --  Remove it from the free list

            Remove_Free_Cell (To_Free_Cell_Acc (Last_Cell));
            Last_Cell := Last_Cell.Prev;

            --  There can be only one free block before

            pragma Assert (Last_Cell = null or else not Last_Cell.Free);
         end if;

         return;
      end if;

      --  Merge with the next cell?

      if Cell /= Last_Cell then
         declare
            Next_Cell : constant Cell_Acc := Get_Next_Cell (Cell);

         begin
            if Next_Cell.Free then

               --  Remove it from the free list

               Remove_Free_Cell (To_Free_Cell_Acc (Next_Cell));

               --  Do the merge

               if Next_Cell /= Last_Cell then
                  Get_Next_Cell (Next_Cell).Prev := Cell;
               end if;

               Cell.Size := Cell.Size + Next_Cell.Size + size_t (Cell_Size);
            end if;
         end;
      end if;

      --  Merge with prev cell?

      if Cell.Prev /= null and then Cell.Prev.Free then
         declare
            Prev_Cell : constant Cell_Acc := Cell.Prev;

         begin
            Remove_Free_Cell (To_Free_Cell_Acc (Prev_Cell));

            --  Do the merge

            if Cell /= Last_Cell then
               Get_Next_Cell (Cell).Prev := Prev_Cell;
            end if;

            Prev_Cell.Size := Prev_Cell.Size + Cell.Size + size_t (Cell_Size);
            Cell := Prev_Cell;
         end;
      end if;

      Add_Free_Cell (To_Free_Cell_Acc (Cell));
   end Free;

   -------------------
   -- Get_Cell_Data --
   -------------------

   function Get_Cell_Data (Cell : Cell_Acc) return Address is
   begin
      return Cell.all'Address + Cell_Size;
   end Get_Cell_Data;

   --------------------
   -- Get_First_Cell --
   --------------------

   function Get_First_Cell return Cell_Acc is
   begin
      return To_Cell_Acc (Heap_Start'Address);
   end Get_First_Cell;

   -------------------
   -- Get_Next_Cell --
   -------------------

   function Get_Next_Cell (Cell : Cell_Acc) return Cell_Acc is
   begin
      return To_Cell_Acc (Get_Cell_Data (Cell) + Storage_Offset (Cell.Size));
   end Get_Next_Cell;

   -------------
   -- Realloc --
   -------------

   function Realloc (Ptr : Address; Size : size_t) return Address is
   begin
      --  Not yet implemented

      raise Program_Error;
      return Null_Address;
   end Realloc;

   ----------------------
   -- Remove_Free_Cell --
   ----------------------

   procedure Remove_Free_Cell (Cell : Free_Cell_Acc) is
   begin
      if Cell.Next_Free /= null then
         Cell.Next_Free.Prev_Free := Cell.Prev_Free;
      end if;

      if Cell.Prev_Free /= null then
         Cell.Prev_Free.Next_Free := Cell.Next_Free;
      else
         Free_List := Cell.Next_Free;
      end if;
   end Remove_Free_Cell;
end System.C.Malloc;
