------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . C . M A L L O C                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

--  A simple implementation of storage allocation (malloc etc) for ZFP use

pragma Restrictions (No_Elaboration_Code);

package System.C.Malloc is
   pragma Preelaborate;

   function Alloc (Size : size_t) return Address;
   pragma Export (C, Alloc, "malloc");

   procedure Free (Ptr : Address);
   pragma Export (C, Free, "free");

   function Realloc (Ptr : Address; Size : size_t) return Address;
   pragma Export (C, Realloc, "realloc");

private
   --  The basic implementation structures are made private in the spec so
   --  that a child package could add extensions (statistics, consistency
   --  checks...)

   type Cell_Type;
   --  A cell is the header before the chunk of memory. This implementation
   --  uses doubly-linked list of cells.

   type Cell_Acc is access Cell_Type;
   pragma No_Strict_Aliasing (Cell_Acc);
   --  Get rid of strict aliasing error message because we will convert this
   --  access type to address and Free_Cell_Acc.

   subtype Cell_Size_T is size_t
     range 0 .. 2 ** (Standard'Address_Size - 2);

   type Cell_Type is record
      Prev : Cell_Acc;
      --  The cell just before this one or null if this is the first cell.
      --  There is no Next as this can be deduced from Size.

      Size : Cell_Size_T;
      --  Size of this cell rounded up to multiple of Max_Alignment

      Free : Boolean;
      --  Status flag, used to coalize blocks
   end record;
   pragma Pack (Cell_Type);
   for Cell_Type'Size use 2 * Standard'Address_Size;
   for Cell_Type'Alignment use Standard'Maximum_Alignment;

   type Free_Cell_Type;
   type Free_Cell_Acc is access Free_Cell_Type;
   pragma No_Strict_Aliasing (Free_Cell_Acc);
   --  Get rid of strict aliasing error message because we will convert this
   --  access type to address and Cell_Acc.

   type Free_Cell_Type is record
      Cell : Cell_Type;
      --  Free cells have two additional fields over busy cells

      Prev_Free : Free_Cell_Acc;
      Next_Free : Free_Cell_Acc;
      --  Doubly linked list of free blocks
   end record;

   Free_List : Free_Cell_Acc;
   --  Linked list of free cells ordered by increasing size

   function Get_First_Cell return Cell_Acc;
   --  The first cell. Valid only if the heap is not empty (which can be
   --  checked with Last_Cell).

   Last_Cell : Cell_Acc;
   --  Last allocated cell (it must not be a free cell)

   function Get_Next_Cell (Cell : Cell_Acc) return Cell_Acc;
   --  Next adjacent cell
end System.C.Malloc;
