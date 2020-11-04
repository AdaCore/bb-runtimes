------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              N S 1 6 5 5 0                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

package Interfaces.X86_64.NS16550 with Pure, No_Elaboration_Code_All is
   type Number_Of_Bits is (Five, Six, Seven, Eight);
   for Number_Of_Bits use
     (Five => 2#00#, Six => 2#01#, Seven => 2#10#, Eight => 2#11#);

   type Number_Of_Stop_Bits is (One, Two);
   for Number_Of_Stop_Bits use (One => 0, Two => 1);

   type Parity_Option is (None, Odd, Even, Mark, Space);
   for Parity_Option use
     (None => 0, Odd => 1, Even => 2, Mark => 3, Space => 4);

   type Line_Control is record
      Data_Bits            : Number_Of_Bits;
      Stop_Bits            : Number_Of_Stop_Bits;
      Parity               : Parity_Option;
      Divisor_Latch_Access : Boolean;
   end record with Size => 8;

   for Line_Control use record
      Data_Bits            at 0 range 0 .. 1;
      Stop_Bits            at 0 range 2 .. 2;
      Parity               at 0 range 3 .. 5;
      Divisor_Latch_Access at 0 range 7 .. 7;
   end record;

   type Line_Status is record
      Data_Ready        : Boolean;
      Overrun_Error     : Boolean;
      Parity_Error      : Boolean;
      Framing_Error     : Boolean;
      Break_Indicator   : Boolean;
      Transmit_Ready    : Boolean;
      Transmitter_Empty : Boolean;
      Impending_Error   : Boolean;
   end record with Size => 8;

   for Line_Status use record
      Data_Ready        at 0 range 0 .. 0;
      Overrun_Error     at 0 range 1 .. 1;
      Parity_Error      at 0 range 2 .. 2;
      Framing_Error     at 0 range 3 .. 3;
      Break_Indicator   at 0 range 4 .. 4;
      Transmit_Ready    at 0 range 5 .. 5;
      Transmitter_Empty at 0 range 6 .. 6;
      Impending_Error   at 0 range 7 .. 7;
   end record;

   procedure Set_Line_Control (Data : Line_Control; Port : IO_Port);
   function Get_Line_Status (Port : IO_Port) return Line_Status;
   function Read_Data (Port : IO_Port) return Character;
   procedure Write_Data (Data : Character; Port : IO_Port);

end Interfaces.X86_64.NS16550;
