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

--  ELF definitions (for loaders)

with Interfaces; use Interfaces;

package Elf32 is
   type Elf32_Ehdr is record
      E_Ident_Mag0 : Unsigned_8;
      E_Ident_Mag1 : Unsigned_8;
      E_Ident_Mag2 : Unsigned_8;
      E_Ident_Mag3 : Unsigned_8;
      E_Ident_Class : Unsigned_8;
      E_Ident_Data : Unsigned_8;
      E_Ident_Version : Unsigned_8;
      E_Ident_Pad7 : Unsigned_8;
      E_Ident_Pad8 : Unsigned_8;
      E_Ident_Pad9 : Unsigned_8;
      E_Ident_Pad10 : Unsigned_8;
      E_Ident_Pad11 : Unsigned_8;
      E_Ident_Pad12 : Unsigned_8;
      E_Ident_Pad13 : Unsigned_8;
      E_Ident_Pad14 : Unsigned_8;
      E_Ident_Pad15 : Unsigned_8;

      E_Type : Unsigned_16;
      E_Machine : Unsigned_16;
      E_Version : Unsigned_32;
      E_Entry : Unsigned_32;
      E_Phoff : Unsigned_32;
      E_Shoff : Unsigned_32;
      E_Flags : Unsigned_32;
      E_Ehsize : Unsigned_16;
      E_Phentsize : Unsigned_16;
      E_Phnum : Unsigned_16;
      E_Shentsize : Unsigned_16;
      E_Shnum : Unsigned_16;
      E_Shstrndx : Unsigned_16;
   end record;

   --  Values for e_ident
   Ei_Mag0 : constant := 16#7f#;
   Ei_Mag1 : constant := Character'Pos ('E');
   Ei_Mag2 : constant := Character'Pos ('L');
   Ei_Mag3 : constant := Character'Pos ('F');
   Elfclass32 : constant := 1;
   Elfdata2lsb : constant := 1;
   Elfdata2msb : constant := 2;

   --  Values for e_type
   Et_Exec : constant := 2;

   --  Values for e_version
   Ev_Current : constant := 1;

   type Elf32_Phdr is record
      P_Type : Unsigned_32;
      P_Offset : Unsigned_32;
      P_Vaddr : Unsigned_32;
      P_Paddr : Unsigned_32;
      P_Filesz : Unsigned_32;
      P_Memsz : Unsigned_32;
      P_Flags : Unsigned_32;
      P_Align : Unsigned_32;
   end record;

   --  Values for p_type
   Pt_Null : constant := 0;
   Pt_Load : constant := 1;
   Pt_Dynamic : constant := 2;
   Pt_Interp : constant := 3;
   Pt_Note : constant := 4;
   Pt_Shlib : constant := 5;
   Pt_Phdr : constant := 6;

   --  Value for p_flags
   Pf_X : constant := 1;
   Pf_W : constant := 2;
   Pf_R : constant := 3;
end Elf32;
