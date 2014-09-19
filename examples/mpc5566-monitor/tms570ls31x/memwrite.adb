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
with Ada.Unchecked_Conversion;

package body Memwrite is
   Flash_Limit : constant := 16#30_0000#;

   procedure Init is
   begin
      null;
   end Init;

   procedure Reset_To_Sram is
      SYS_CPURSTCR : Unsigned_32;
      for SYS_CPURSTCR'Address use System'To_Address (16#ffff_ffcc#);
      pragma Volatile (SYS_CPURSTCR);
      pragma Import (Ada, SYS_CPURSTCR);

      SYS_BMMCR1 : Unsigned_32;
      for SYS_BMMCR1'Address use System'To_Address (16#ffff_ffc4#);
      pragma Volatile (SYS_BMMCR1);
      pragma Import (Ada, SYS_BMMCR1);
   begin
      SYS_BMMCR1 := 5;
      SYS_CPURSTCR := 1;
      loop
         null;
      end loop;
   end Reset_To_Sram;

   procedure Flush is
      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (System.Address, Unsigned_32);
   begin
      if Exec_Addr < Flash_Limit then
         Exec_Addr := To_Unsigned_32 (Reset_To_Sram'Address);
      end if;
   end Flush;

   procedure Write (Addr : Unsigned_32; Content : Storage_Array) is
      Addr1 : Unsigned_32 := Addr;
   begin
      if Addr1 < Flash_Limit then
         --  Want to write in flash. Write in SRAM instead and we will remap
         --  sram to flash just before execution
         Addr1 := 16#0800_0000# + Addr1;
      end if;
      declare
         Ram_Content : Storage_Array (Content'Range);
         for Ram_Content'Address use System'To_Address (Addr1);
         pragma Import (Ada, Ram_Content);
      begin
         Ram_Content := Content;
         --  Sync cache ???
      end;
   end Write;
end Memwrite;
