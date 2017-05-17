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

with System.BB.Board_Parameters;

package body System.Text_IO is
   use Interfaces;

   Uthr : Character;
   for Uthr'Address use
     System.BB.Board_Parameters.CCSRBAR + 16#4500#;
   pragma Import (Ada, Uthr);
   pragma Volatile (Uthr);

   Ulsr : Unsigned_8;
   for Ulsr'Address use
     System.BB.Board_Parameters.CCSRBAR + 16#4505#;
   pragma Import (Ada, Ulsr);
   pragma Volatile (Ulsr);

   Dr   : constant Unsigned_8 := 2#0000_0001#;
   Thre : constant Unsigned_8 := 2#0010_0000#;
   --  Ulsr bits

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      return Uthr;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return (Ulsr and Dr) /= 0;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return (Ulsr and Thre) /= 0;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      --  Send the character

      Uthr := C;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
