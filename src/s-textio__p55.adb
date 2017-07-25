------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with System;

with Interfaces;

package body System.Text_IO is
   use Interfaces;

   ESCI_CR1 : Unsigned_32;
   for ESCI_CR1'Address use 16#FFFB_0000# + 0;
   pragma Import (Ada, ESCI_CR1);
   pragma Volatile (ESCI_CR1);

   ESCI_CR2 : Unsigned_16;
   for ESCI_CR2'Address use 16#FFFB_0000# + 4;
   pragma Import (Ada, ESCI_CR2);
   pragma Volatile (ESCI_CR2);

   ESCI_DR : Unsigned_16;
   for ESCI_DR'Address use 16#FFFB_0000# + 6;
   pragma Import (Ada, ESCI_DR);
   pragma Volatile (ESCI_DR);

   ESCI_SR : Unsigned_32;
   for ESCI_SR'Address use 16#FFFB_0000# + 8;
   pragma Import (Ada, ESCI_SR);
   pragma Volatile (ESCI_SR);

   RDRF : constant := 16#2000_0000#;
   TDRE : constant := 16#8000_0000#;
   --  ESCI_SR bits

   SIU_PCR89 : Unsigned_16;
   for SIU_PCR89'Address use 16#C3F9_00F2#;
   pragma Import (Ada, SIU_PCR89);
   pragma Volatile (SIU_PCR89);

   SIU_PCR90 : Unsigned_16;
   for SIU_PCR90'Address use 16#C3F9_00F4#;
   pragma Import (Ada, SIU_PCR90);
   pragma Volatile (SIU_PCR90);

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Clear rdrf (w1c bit)
      ESCI_SR := RDRF;

      --  Send the character

      return Character'Val (ESCI_DR and 16#FF#);
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Enable ESCI module
      ESCI_CR2 := 16#2000#;

      --  Enable Tx & Rx, 8n1, keep baud rate.
      --  Note that br = Fsys/(16 * baud)
      ESCI_CR1 := (ESCI_CR1 and 16#FFFF_0000#) or 16#000C#;

      --  Configure pads, enable txda and rxda
      SIU_PCR89 := 16#400#;
      SIU_PCR90 := 16#400#;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (ESCI_SR and RDRF) /= 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (ESCI_SR and TDRE) /= 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Clear tdre (w1c bit)
      ESCI_SR := TDRE;

      --  Send the character

      ESCI_DR := Character'Pos (C);
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
