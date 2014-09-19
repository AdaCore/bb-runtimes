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
with Testdata; use Testdata;
pragma Warnings (Off);
with System.BB.Board_Support; use System.BB.Board_Support;
pragma Warnings (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Testhandler (Vector : Vector_Id) is

   No_Interrupt : constant Interrupt_ID := Interrupt_ID (0);

   procedure Check (Success : Boolean; Name : String);
   procedure Check_No_Pending;

   -----------
   -- Check --
   -----------

   procedure Check (Success : Boolean; Name : String) is
   begin
      if not Errors then
         Put ("  * check ");
         Put (Name);
         Errors := not Success;
         Put_Line (if Errors then ", ERROR" else ", OK");
      end if;
      if Errors then
         raise Program_Error;
      end if;
   end Check;

   ----------------------
   -- Check_No_Pending --
   ----------------------

   procedure Check_No_Pending is
   begin
      if Get_Interrupt_Request (Vector) /= No_Interrupt then
         Put_Line ("  . warning: cannot verify interrupt request was cleared");
      end if;
   end Check_No_Pending;

   Now : constant Timer_Interval := Read_Clock;
   Int : Interrupt_ID;

begin -- Testhandler
   if Alarms = 0 then
      Check (Last_Alarm = 0, "alarm is indeed first invocation");
      Last_Alarm := Now;
      Check (Get_Interrupt_Request (Vector) = Alarm_Interrupt_ID,
        "interrupt is alarm interrupt");
      Clear_Alarm_Interrupt;
      pragma Debug (Check_No_Pending);

   else
      Int := Get_Interrupt_Request (Vector);
      if Int = Alarm_Interrupt_ID then
         Last_Alarm := Now;
         Alarms := Alarms + 1;
         Clear_Alarm_Interrupt;
      end if;
   end if;
end Testhandler;
