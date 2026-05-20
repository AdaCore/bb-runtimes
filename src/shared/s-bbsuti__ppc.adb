------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . B B . B O A R D _ S U P P O R T . T I M E        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  For PowerPc: use time base and decrementer registers from the core

with System.Machine_Code; use System.Machine_Code;

separate (System.BB.Board_Support)

package body Time is
   -----------------------
   -- Local Definitions --
   -----------------------

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown. This type should allow atomic reads and updates.

   function Read_TBL return Unsigned_32;
   pragma Inline (Read_TBL);
   --  Read the Time Base Lower word

   function Read_TBU return Unsigned_32;
   pragma Inline (Read_TBU);
   --  Read the Time Base Upper word

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return BB.Time.Time is
      use type BB.Time.Time;
      Lo  : Unsigned_32;
      Hi  : Unsigned_32;
      Hi1 : Unsigned_32;

   begin
      --  We can't atomically read the 64-bits counter. So check that the
      --  32 MSB don't change.

      Hi := Read_TBU;
      loop
         Lo := Read_TBL;
         Hi1 := Read_TBU;
         exit when Hi = Hi1;
         Hi := Hi1;
      end loop;

      return (BB.Time.Time (Hi) * 2 ** 32) + BB.Time.Time (Lo);
   end Read_Clock;

   ---------------------------
   -- Install_Alarm_Handler --
   ---------------------------

   procedure Install_Alarm_Handler
     (Handler : BB.Interrupts.Interrupt_Handler) is
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler.all'Address, CPU_Specific.Decrementer_Excp);
   end Install_Alarm_Handler;

   --------------
   -- Read_TBL --
   --------------

   function Read_TBL return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbl %0",
        Outputs => Unsigned_32'Asm_Output ("=r", Res),
        Volatile => True);
      return Res;
   end Read_TBL;

   --------------
   -- Read_TBU --
   --------------

   function Read_TBU return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mftbu %0",
        Outputs => Unsigned_32'Asm_Output ("=r", Res),
        Volatile => True);
      return Res;
   end Read_TBU;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : BB.Time.Time)
   is
      use BB.Time;
      Now  : constant BB.Time.Time := Read_Clock;
      Diff : constant Unsigned_64 :=
        (if Ticks > Now then Unsigned_64 (Ticks - Now) else 1);
      Val  : Unsigned_32;
   begin
      if Diff >= 2 ** 31 then
         Val := 16#7FFF_FFFF#;
         --  The maximum value that can be set in the DEC register. MSB must
         --  not be set to avoid a useless interrupt (PowerPC triggers an
         --  interrupt when the MSB switches from 0 to 1).
      else
         Val := Unsigned_32 (Diff);
      end if;

      Asm ("mtdec %0",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_Alarm;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt
     renames BB.Board_Support.Clear_Alarm_Interrupt;
end Time;
