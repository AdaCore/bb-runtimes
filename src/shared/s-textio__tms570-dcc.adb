------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

--  Minimal version of Text_IO body for use on TMS570 using the ARM Debug
--  Control Channel (DCC)

with System.Machine_Code; use System.Machine_Code;
package body System.Text_IO is

   type Word is mod 2**32;

   function Read_DCSR return Word;
   --  Read status register for debugger communication

   ---------
   -- Get --
   ---------

   function Get return Character is
      C : Character;
   begin
      Asm ("mrc p14, 0, %0, c0, c5",
           Outputs => (Character'Asm_Output ("=r", C)),
           Volatile => True);

      return C;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      ((Read_DCSR and 2**29) = 0);

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
      ((Read_DCSR and 2**30) /= 0);

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Asm ("mcr p14, 0, %0, c0, c5",
           Inputs => (Word'Asm_Input ("r", Word (Character'Pos (C)))),
           Volatile => True);
   end Put;

   ---------------
   -- Read_DCSR --
   ---------------

   function Read_DCSR return Word is
      R : Word;
   begin
      Asm ("mrc p14, 0, %0, c0, c1",
            Outputs => (Word'Asm_Output ("=r", R)),
            Volatile => True);
      return R;
   end Read_DCSR;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
