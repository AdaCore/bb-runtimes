------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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
with System;
with Ada.Real_Time; use Ada.Real_Time;

procedure Hello is
   SCI_BASE : constant := 16#fff7_e400#;

   SCIGCR0 : Unsigned_32;
   for SCIGCR0'Address use System'To_Address (SCI_BASE + 16#00#);
   pragma Volatile (SCIGCR0);
   pragma Import (Ada, SCIGCR0);

   SCIGCR1 : Unsigned_32;
   for SCIGCR1'Address use System'To_Address (SCI_BASE + 16#04#);
   pragma Volatile (SCIGCR1);
   pragma Import (Ada, SCIGCR1);

   BRS : Unsigned_32;
   for BRS'Address use System'To_Address (SCI_BASE + 16#2c#);
   pragma Volatile (BRS);
   pragma Import (Ada, BRS);

   SCIFORMAT : Unsigned_32;
   for SCIFORMAT'Address use System'To_Address (SCI_BASE + 16#28#);
   pragma Volatile (SCIFORMAT);
   pragma Import (Ada, SCIFORMAT);

   SCIPIO0 : Unsigned_32;
   for SCIPIO0'Address use System'To_Address (SCI_BASE + 16#3c#);
   pragma Volatile (SCIPIO0);
   pragma Import (Ada, SCIPIO0);

   SCIPIO8 : Unsigned_32;
   for SCIPIO8'Address use System'To_Address (SCI_BASE + 16#5c#);
   pragma Volatile (SCIPIO8);
   pragma Import (Ada, SCIPIO8);

   SCITD : Unsigned_32;
   for SCITD'Address use System'To_Address (SCI_BASE + 16#38#);
   pragma Volatile (SCITD);
   pragma Import (Ada, SCITD);

   SCIFLR : Unsigned_32;
   for SCIFLR'Address use System'To_Address (SCI_BASE + 16#1c#);
   pragma Volatile (SCIFLR);
   pragma Import (Ada, SCIFLR);

   procedure Init is
   begin
      --  Bring out of reset
      SCIGCR0 := 1;

      --  8n1, enable RX and TX, async, idle-line mode, SWnRST, internal clk
      SCIGCR1 := 16#03_00_00_22#;

      --  Baud rate. PLLCLK=180Mhz, VCLK = PLLCLK / 2
      declare
         Baud : constant := 115200;
         VCLK : constant := 90_000_000;
         P : constant := VCLK / (16 * Baud) - 1;
         M : constant := (VCLK / Baud) rem 16;
      begin
         BRS := P + M * 2**24;
      end;

      --  8 bits
      SCIFORMAT := 7;

      --  Enable Tx and Rx pins, pull-up
      SCIPIO0 := 2#110#;
      SCIPIO8 := 2#110#;

      --  Enable SCI
      SCIGCR1 := SCIGCR1 or 16#80#;
   end Init;

   procedure Put (C : Character) is
   begin
      SCITD := Character'Pos (C);
      while (SCIFLR and 16#100#) = 0 loop
         null;
      end loop;
   end Put;

   procedure Put_Line (S : String) is
   begin
      for I in S'Range loop
         Put (S (I));
      end loop;
      Put (ASCII.CR);
      Put (ASCII.LF);
   end Put_Line;

   Timeout : Time := Clock;
   Cycle : constant Time_Span := Seconds (1);
begin
   Init;

   loop
      delay until Timeout;
      Timeout := Timeout + Cycle;
      Put_Line ("Hello world");
   end loop;
end Hello;
