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

pragma Warnings (Off);
with System.BB.CPU_Primitives; use System.BB.CPU_Primitives;
with System.BB.Board_Support; use System.BB.Board_Support;
with System.BB.Parameters; use System.BB.Parameters;
pragma Warnings (On);
package Testdata is

   Literal_Data       : constant := 16#Ada_2012#;

   Constant_Data      : constant Integer := Literal_Data;
   Initialized_Data   : Integer := Literal_Data;
   Uninitialized_Data : array (1 .. 1000) of Integer;

   More_Init_Data     : array (1 .. 100) of Integer
     := (others => Initialized_Data);
   for More_Init_Data'Alignment use 8;

   More_Const_Data    : constant array (1 .. 100) of Integer
     := (1 .. 100 => Initialized_Data);

   Shared_Var     : aliased Integer := 0;
   Shared_Float   : aliased Float := 0.0;

   Test_Context   : array (1 .. 2) of aliased Context_Buffer;

   First_Thread   : access Context_Buffer;
   pragma Import (Asm, First_Thread, "first_thread_table");

   Running_Thread : access Context_Buffer;
   pragma Import (Asm, Running_Thread, "__gnat_running_thread_table");

   subtype Interrupt_ID is Natural range 0 .. Number_Of_Interrupt_ID;

   Last_Alarm     : Timer_Interval := 0;
   pragma Volatile (Last_Alarm);
   subtype Alarm_Count is Natural range 0 .. 2000;
   Alarms         : Alarm_Count := 0;
   pragma Volatile (Alarms);

   Errors         : Boolean := False;

end Testdata;
