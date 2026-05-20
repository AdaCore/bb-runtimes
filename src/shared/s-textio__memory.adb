------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . T E X T _ I O                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008-2011, Free Software Foundation, Inc.       --
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

--  Write console output in a memory buffer

package body System.Text_IO is

   --  In memory output emulation

   --  Export symbols so that gdb can easily find them.

   Output : String (1 .. 2048);
   pragma Export (C, Output, "textio_output");

   Output_Len : Natural := 0;
   pragma Export (C, Output_Len, "textio_output_len");

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Will never be called

      raise Program_Error;
      return ASCII.NUL;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Output_Len := 0;
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return False;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
   begin
      return True;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      Output_Len := Output_Len + 1;
      if Output_Len <= Output'Last then
         Output (Output_Len) := C;
      end if;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return False;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
