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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with IOEmu; use IOEmu;

package Hulls is
   type Memmap_Entry is record
      Vaddr : Address;
      Paddr : Address;
      Size  : Storage_Count;
   end record;
   pragma Convention (C, Memmap_Entry);

   type Memmap_Array is array (Natural) of Memmap_Entry;
   type Memmap_Arr_Acc is access Memmap_Array;

   type File_Entry is record
      Name : Address;
      Content : Address;
      Len : Storage_Count;
   end record;

   type File_Array is array (Natural) of File_Entry;
   type File_Arr_Acc is access File_Array;

   type Hull_Desc is record
      Mmu_Table : Address;
      Memmap : Memmap_Arr_Acc;
      Memmap_Nbr : Natural;
      Files : File_Arr_Acc;
      Files_Nbr : Natural;
   end record;
   pragma Convention (C, Hull_Desc);

   type Ioemu_Map_Array_Acc is access all IOEmu_Map_Array;

   type Unsigned_64_Array is array (Natural range <>) of Unsigned_64;

   type Hull_Context_AArch64 is private;

   type Hull_Context is abstract tagged record
      Cpu : aliased Hull_Context_AArch64;

      --  Machine independant
      Machine_Reset : Boolean;
   end record;

   --  Primitives

   --  Define the IO map: translate an address to an IO emulation.
   procedure Find_IO
     (Hull : Hull_Context;
      Addr : Address;
      Dev : out IOEmu_Dev_Acc;
      Off : out Off_T) is abstract;

   procedure Dump_Cpu (H : Hull_Context);

   type Hull_Context_Acc is access all Hull_Context'Class;

   procedure Create_Hull (Desc : Hull_Desc; Ctxt : Hull_Context_Acc);
private
   type Hull_Context_AArch64 is record
      --  EL1 registers

      Xregs : Unsigned_64_Array (0 .. 30);       --    0 .. 247
      Sp : Unsigned_64;      --  From sp_el1     --  248
      PC : Unsigned_64;      --  From elr_el2    --  256
      Pstate : Unsigned_32;  --  From spsr_el2   --  264

      Vbar : Unsigned_64;                        --  272

      --  EL0 registers

      Sp_El0 : Unsigned_64;                      --  280

      --  EL2 registers

      Esr : Unsigned_32;                         --  288
      Far : Unsigned_64;                         --  296
      Hpfar : Unsigned_64;                       --  304

      Vtcr : Unsigned_64;                        --  312
      Vttbr : Unsigned_64;                       --  320
      Hcr : Unsigned_64;                         --  328

      --  Fully virtualized. Not used by asm code.
      V_MDSCR_EL1 : Unsigned_64;
      V_OSLAR_EL1 : Unsigned_64;
   end record;
   pragma Convention (C, Hull_Context_AArch64);

end Hulls;
