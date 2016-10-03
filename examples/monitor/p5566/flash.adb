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
with Console; use Console;
with Mpc55xx; use Mpc55xx;
with System.Machine_Code; use System.Machine_Code;
with Cache; use Cache;
with Term; use Term;
with Dumps; use Dumps;
with Commands; use Commands;

package body Flash is
   function In_Flash (Addr : Unsigned_32) return Boolean is
   begin
      pragma Warnings (Off, "lower bound*");
      return Addr in Flash_First_Address .. Flash_Last_Address;
      pragma Warnings (On, "lower bound*");
   end In_Flash;

   FLASH_MCR : Unsigned_32;
   for FLASH_MCR'Address use System'To_Address (16#c3f8_8000#);
   pragma Import (Ada, FLASH_MCR);
   pragma Volatile (FLASH_MCR);

   FLASH_LMLR : Unsigned_32;
   for FLASH_LMLR'Address use System'To_Address (16#c3f8_8004#);
   pragma Import (Ada, FLASH_LMLR);
   pragma Volatile (FLASH_LMLR);

   FLASH_HLR : Unsigned_32;
   for FLASH_HLR'Address use System'To_Address (16#c3f8_8008#);
   pragma Import (Ada, FLASH_HLR);
   pragma Volatile (FLASH_HLR);

   FLASH_SLMLR : Unsigned_32;
   for FLASH_SLMLR'Address use System'To_Address (16#c3f8_800c#);
   pragma Import (Ada, FLASH_SLMLR);
   pragma Volatile (FLASH_SLMLR);

   FLASH_LMSR : Unsigned_32;
   for FLASH_LMSR'Address use System'To_Address (16#c3f8_8010#);
   pragma Import (Ada, FLASH_LMSR);
   pragma Volatile (FLASH_LMSR);

   FLASH_HSR : Unsigned_32;
   for FLASH_HSR'Address use System'To_Address (16#c3f8_8014#);
   pragma Import (Ada, FLASH_HSR);
   pragma Volatile (FLASH_HSR);

   procedure Flash_Unlock is
   begin
      FLASH_LMLR := 16#a1a1_1111#; --  Password
      FLASH_LMLR := 16#000c_ffc0#;
      FLASH_HLR := 16#b2b2_2222#; --  Password
      FLASH_HLR := 16#0ff_00000#;
      FLASH_SLMLR := 16#c3c3_3333#; --  Password
      FLASH_SLMLR := 16#000c_ffc0#;
   end Flash_Unlock;

   procedure Flash_Start_Prog is
   begin
      FLASH_MCR := 16#0000_0010#; -- PGM
   end Flash_Start_Prog;

   procedure Flash_Wait_Done is
   begin
      while (FLASH_MCR and 16#0000_0400#) = 0 loop
         --  Wait DONE
         null;
      end loop;
      if (FLASH_MCR and 16#0000_0200#) = 0 then
         Put_Line ("PEG error");
      end if;
   end Flash_Wait_Done;

   procedure Flash_Wait_Prog is
   begin
      FLASH_MCR := 16#0000_0011#; -- PGM + EHV
      Flash_Wait_Done;
      FLASH_MCR := 16#0000_0010#; -- PGM;
      FLASH_MCR := 16#0000_0000#;
   end Flash_Wait_Prog;

   procedure Flash_Wait_Erase is
   begin
      FLASH_MCR := 16#0000_0005#; -- ERS + EHV
      Flash_Wait_Done;
      FLASH_MCR := 16#0000_0004#; -- ERS;
      FLASH_MCR := 16#0000_0000#;
   end Flash_Wait_Erase;

   procedure Proc_Flash is
   begin
      --  MMU
      --  use tlb#1, Disable cache
      Set_Mas0 (2 ** 28 + 1 * 2 ** 16);
      Set_Mas1 (2 ** 31 + 2 ** 30 + 6 * 2 ** 8); -- TSIZE=4MB
      Set_Mas2 (16#0000_0008#); -- Not cachable
      Set_Mas3 (16#0000_003f#); -- RWX
      Asm ("tlbwe", Volatile => True);

      Cache_Flush_All;

      Next_Word;
      if Pos > Line_Len or else Line (Pos .. End_Pos) = "info" then
         Put ("LMLR : ");
         Put (Image8 (FLASH_LMLR));
         New_Line;
         Put ("HLR  : ");
         Put (Image8 (FLASH_HLR));
         New_Line;
         Put ("SLMLR: ");
         Put (Image8 (FLASH_SLMLR));
         New_Line;
         Put ("MCR  : ");
         Put (Image8 (FLASH_MCR));
         New_Line;
      elsif Line (Pos .. End_Pos) = "unlock" then
         Flash_Unlock;
      elsif Line (Pos .. End_Pos) = "write" then
         Flash_Start_Prog;
         declare
            V : Unsigned_8;
            for V'Address use System'To_Address (0);
            pragma Import (Ada, V);
            pragma Volatile (V);
         begin
            V := 16#ab#;
         end;
         Flash_Wait_Prog;
      elsif Line (Pos .. End_Pos) = "erase" then
         FLASH_MCR := 16#0000_0004#; -- ERS;
         FLASH_LMSR := 16#0000_0003#;
         FLASH_HSR := 16#0ff_00000#;
         declare
            V : Unsigned_32;
            for V'Address use System'To_Address (0);
            pragma Import (Ada, V);
            pragma Volatile (V);
         begin
            V := 0;
         end;
         Flash_Wait_Erase;
      else
         Put_Line ("unknown sub-command");
      end if;
   end Proc_Flash;

   Commands : aliased Command_List :=
     (1,
      (1 => (new String'("flash [info|unlock|erase] - Flash programming"),
             Proc_Flash'Access)),
     null);
begin
   Register_Commands (Commands'Access);
end Flash;
