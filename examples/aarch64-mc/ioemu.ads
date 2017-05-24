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

with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

package IOEmu is
   subtype Off_T is Storage_Count;

   type IOEmu_Dev is abstract tagged null record;

   function Read8  (Dev : in out IOEmu_Dev; Off : Off_T) return Unsigned_8
     is abstract;
   function Read16 (Dev : in out IOEmu_Dev; Off : Off_T) return Unsigned_16
      is abstract;
   function Read32 (Dev : in out IOEmu_Dev; Off : Off_T) return Unsigned_32
      is abstract;
   function Read64 (Dev : in out IOEmu_Dev; Off : Off_T) return Unsigned_64
      is abstract;

   procedure Write8  (Dev : in out IOEmu_Dev; Off : Off_T; Val : Unsigned_8)
      is abstract;
   procedure Write16 (Dev : in out IOEmu_Dev; Off : Off_T; Val : Unsigned_16)
      is abstract;
   procedure Write32 (Dev : in out IOEmu_Dev; Off : Off_T; Val : Unsigned_32)
      is abstract;
   procedure Write64 (Dev : in out IOEmu_Dev; Off : Off_T; Val : Unsigned_64)
      is abstract;

   type IOEmu_Dev32 is abstract new IOEmu_Dev with null record;

   procedure Write32_Mask
     (Dev : in out IOEmu_Dev32;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32) is abstract;
   --  Write to DEV at offset OFF

   function Read8  (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_8;
   function Read16 (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_16;
   function Read64 (Dev : in out IOEmu_Dev32; Off : Off_T) return Unsigned_64;

   procedure Write8
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_8);
   procedure Write16
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_16);
   procedure Write32
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_32);
   procedure Write64
     (Dev : in out IOEmu_Dev32; Off : Off_T; Val : Unsigned_64);

end IOEmu;
