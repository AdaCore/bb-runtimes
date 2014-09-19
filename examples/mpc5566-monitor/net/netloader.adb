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

with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Nettftp;
with Elf32; use Elf32;
with Ada.Text_IO; use Ada.Text_IO;
with Netutils; use Netutils;
with Ethdrv;
with Memory_Set;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Cache; use Cache;
with Commands; use Commands;
with Netcfg;
with Term; use Term;
with Memwrite;

package body Netloader is
   procedure Read (Off : Unsigned_32;
                   Data : Address;
                   Count : Natural)
   is
      Off1 : Unsigned_32 := Off;
      Ptr : Address := Data;
      Cnt : Natural;
      Total : Natural := 0;
   begin
      while Total < Count loop
         Cnt := Count - Total;
         Nettftp.Read (Off1, Ptr, Cnt);
         if Cnt = 0 then
            Put_Line ("Read errror");
            return;
         end if;
         Off1 := Off1 + Unsigned_32 (Cnt);
         Ptr := Ptr + Storage_Count (Cnt);
         Total := Total + Cnt;
      end loop;
   end Read;

   function To_Address is new Ada.Unchecked_Conversion (Unsigned_32, Address);

   procedure Proc_Tftpd is
      Hdr : Elf32_Ehdr;
      Phdr : array (0 .. 7) of Elf32_Phdr;
   begin
      Put_Line ("ELF loader via TFTP - wait for a write request");

      Ethdrv.Eth_Init;

      Nettftp.Open;

      Read (0, Hdr'Address, Hdr'Size / 8);

      if Hdr.E_Ident_Mag0 /= Ei_Mag0
        or Hdr.E_Ident_Mag1 /= Ei_Mag1
        or Hdr.E_Ident_Mag2 /= Ei_Mag2
        or Hdr.E_Ident_Mag3 /= Ei_Mag3
      then
         Put_Line ("Not an ELF file");
         return;
      end if;

      if Hdr.E_Ident_Class /= Elfclass32
        or else Hdr.E_Ident_Version /= Ev_Current
      then
         Put_Line ("Unsupported elf file");
         return;
      end if;

      if (Default_Bit_Order = High_Order_First
            and then Hdr.E_Ident_Data /= Elfdata2msb)
        or else (Default_Bit_Order = Low_Order_First
                   and then Hdr.E_Ident_Data /= Elfdata2lsb)
      then
         Put_Line ("Bad byte order");
         return;
      end if;

      Put ("Phoff: ");
      Put_Hex (Hdr.E_Phoff);
      Put (", phentsize: ");
      Put_Hex (Hdr.E_Phentsize);
      Put (", phnum: ");
      Put_Hex (Hdr.E_Phnum);
      New_Line;

      if Hdr.E_Phoff = 0
        or else Hdr.E_Phentsize /= Elf32_Phdr'Size / 8
        or else Hdr.E_Phnum > Phdr'Length
      then
         Put_Line ("ELF file is not loadable");
         return;
      end if;

      Read (Hdr.E_Phoff, Phdr'Address,
            Natural (Hdr.E_Phnum) * Elf32_Phdr'Size / 8);

      Put_Line ("Type     Offset   Vaddr    Paddr    FileSZ   MemSZ    Flags");
      for I in 0 .. Natural (Hdr.E_Phnum) - 1 loop
         declare
            P : Elf32_Phdr renames Phdr (I);
            Sz : Unsigned_32;
            Res : Address;
            pragma Unreferenced (Res);
         begin
            Put_Hex (Phdr (I).P_Type);
            Put (' ');
            Put_Hex (Phdr (I).P_Offset);
            Put (' ');
            Put_Hex (Phdr (I).P_Vaddr);
            Put (' ');
            Put_Hex (Phdr (I).P_Paddr);
            Put (' ');
            Put_Hex (Phdr (I).P_Filesz);
            Put (' ');
            Put_Hex (Phdr (I).P_Memsz);
            Put (' ');
            Put_Hex (Phdr (I).P_Flags);

            if P.P_Type = Pt_Load then
               Put_Line (" Load");

               Read (P.P_Offset, To_Address (P.P_Paddr), Natural (P.P_Filesz));
               Sz := P.P_Memsz - P.P_Filesz;
               if Sz > 0 then
                  Res := Memory_Set.Memset
                    (To_Address (P.P_Paddr + P.P_Filesz), 0,
                     C.size_t (P.P_Memsz));
               end if;
               if (P.P_Flags and Pf_X) /= 0 then
                  Cache_Flush_Range
                    (To_Address (P.P_Paddr),
                     To_Address (P.P_Paddr + P.P_Memsz));
               end if;
            else
               Put_Line (" Skipped");
            end if;
         end;
      end loop;

      Nettftp.Close;
      Put_Line ("Done");

      Memwrite.Exec_Addr := Hdr.E_Entry;
   end Proc_Tftpd;

   procedure Parse_Ip_Addr (Str : String; Res : out In_Addr; Ok : out Boolean)
   is
      V : Unsigned_32;
      P : Natural;
   begin
      Res := 0;
      Ok := False;
      P := Str'First;
      for I in 0 .. 3 loop
         V := 0;
         while P <= Str'Last and then Str (P) in '0' .. '9' loop
            V := V * 10 + Character'Pos (Str (P)) - Character'Pos ('0');
            P := P + 1;
         end loop;
         Res := Res * 256 + V;
         if P > Str'Last and then I < 3 then
            --  Missing separator
            return;
         end if;
         if P <= Str'Last and then Str (P) /= '.' then
            --  Separator is '.'
            return;
         end if;
         --  Skip the '.'
         P := P + 1;
      end loop;
      if P <= Str'Last then
         --  There are some extra characters
         return;
      end if;
      Ok := True;
   end Parse_Ip_Addr;

   procedure Proc_Ifconfig is
      use Netcfg;
      Ok : Boolean;
      P : Natural;
      Res : In_Addr;
      Netmask : Unsigned_32;
   begin
      Next_Word;
      if End_Pos > Pos then
         P := End_Pos + 1;
         for I in Pos .. End_Pos loop
            if Line (I) = '/' then
               P := I;
               exit;
            end if;
         end loop;
         if P > End_Pos then
            Put_Line ("Missing netmask");
            return;
         end if;
         Parse_Ip_Addr (Line (Pos .. P - 1), Res, Ok);
         if not Ok then
            Put_Line ("cannot parse IP address");
            return;
         end if;

         Gw_Ip_Addr := Null_Ip_Addr;
         My_Ip_Addr := Res;

         Pos := P + 1;
         Parse_Unsigned32 (Netmask, Ok);
         if not Ok or else Netmask < 2 or else Netmask > 31 then
            Put_Line ("bad netmask");
            return;
         end if;
         My_Netmask := Shift_Left (16#ffff_ffff#, 32 - Natural (Netmask));

         Next_Word;
         if End_Pos > Pos then
            Parse_Ip_Addr (Line (Pos .. End_Pos), Res, Ok);
            if not Ok then
               Put_Line ("cannot parse IP address");
               return;
            end if;
            Gw_Ip_Addr := Res;
         end if;
      else
         Put ("eth address: ");
         Disp_Eth_Addr (My_Eth_Addr);
         New_Line;
         Put ("IP address:  ");
         Disp_Ip_Addr (My_Ip_Addr);
         Put ("/");
         Disp_Ip_Addr (My_Netmask);
         New_Line;
         Put ("Gateway: ");
         Disp_Ip_Addr (Gw_Ip_Addr);
         New_Line;
      end if;
   end Proc_Ifconfig;

   Commands : aliased Command_List :=
     (2,
      ((new String'("tftpd - TFTPD loader"),
        Proc_Tftpd'Access),
       (new String'("ifconfig - Display network configuration"),
        Proc_Ifconfig'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Netloader;
