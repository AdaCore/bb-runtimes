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
with System; use System;
with Ada.Interrupts.Names;
with System.Storage_Elements;

package body Sci is
   SCI_BASE : constant := 16#fff7_e400#;

   TX_READY : constant := 16#100#;
   RX_READY : constant := 16#200#;

   SET_TX_INT : constant := 16#100#;

   SCIGCR0 : Unsigned_32;
   for SCIGCR0'Address use System'To_Address (SCI_BASE + 16#00#);
   pragma Volatile (SCIGCR0);
   pragma Import (Ada, SCIGCR0);

   SCIGCR1 : Unsigned_32;
   for SCIGCR1'Address use System'To_Address (SCI_BASE + 16#04#);
   pragma Volatile (SCIGCR1);
   pragma Import (Ada, SCIGCR1);

   SCISETINT : Unsigned_32;
   for SCISETINT'Address use System'To_Address (SCI_BASE + 16#0C#);
   pragma Volatile (SCISETINT);
   pragma Import (Ada, SCISETINT);

   SCICLEARINT : Unsigned_32;
   for SCICLEARINT'Address use System'To_Address (SCI_BASE + 16#10#);
   pragma Volatile (SCICLEARINT);
   pragma Import (Ada, SCICLEARINT);

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

   protected Prot is
      pragma Interrupt_Priority;
      procedure Write (S : Address; L : Natural);

      entry Wait;
   private
      procedure Handler;
      pragma Attach_Handler
        (Handler, Ada.Interrupts.Names.LIN_Level_0_Interrupt);

      Done : Boolean := False;
      Ptr : Address;
      Len : Natural;
   end Prot;

   protected body Prot is
      procedure Write_One
      is
         use System.Storage_Elements;

         C : Character;
         for C'Address use Ptr;
         pragma Import (Ada, C);
      begin
         SCITD := Character'Pos (C);

         Len := Len - 1;
         Ptr := Ptr + 1;
      end Write_One;

      procedure Write (S : Address; L : Natural) is
      begin
         pragma Assert (not Done);

         Ptr := S;
         Len := L;

         Write_One;

         --  Enable interrupts (level is 0 at reset).
         SCISETINT := SET_TX_INT;

         Done := False;
      end Write;

      entry Wait when Done is
      begin
         null;
      end Wait;

      procedure Handler is
      begin
         pragma Assert ((SCIFLR and TX_READY) /= 0);
         if Len = 0 then
            --  Disable interrupts
            SCICLEARINT := SET_TX_INT;
            Done := True;
         else
            --  Next character
            Write_One;
         end if;
      end Handler;
   end Prot;

   procedure Put_Line (S : String)
   is
      Crlf : String := (ASCII.CR, ASCII.LF);
   begin
      if S'Length > 0 then
         Prot.Write (S'Address, S'Length);
         Prot.Wait;
      end if;
      Prot.Write (Crlf'Address, Crlf'Length);
      Prot.Wait;
   end Put_Line;

begin
   Init;
end Sci;
