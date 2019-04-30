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
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with System.BB.Board_Parameters;
with System.BB.CPU_Specific;
with Interfaces; use Interfaces;

separate (System.BB.Board_Support)

package body Time is

   package BBBOPA renames System.BB.Board_Parameters;

   -----------------------
   -- Local Definitions --
   -----------------------

   Mtime_Lo : Unsigned_32
     with Volatile,
     Address => System'To_Address (BBBOPA.Mtime_Base_Address);
   Mtime_Hi : Unsigned_32
     with Volatile,
     Address => System'To_Address (BBBOPA.Mtime_Base_Address + 4);
   --  The mtime register has a 64-bit precision on all RV32, RV64, and RV128
   --  systems.

   Mtimecmp_Lo : Unsigned_32
     with Volatile,
     Address => System'To_Address (BBBOPA.Mtimecmp_Base_Address);
   Mtimecmp_Hi : Unsigned_32
     with Volatile,
     Address => System'To_Address (BBBOPA.Mtimecmp_Base_Address + 4);
   --  The mtimecmp register has a 64-bit precision on all RV32, RV64, and
   --  RV128 systems.

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return BB.Time.Time is
      Lo     : Unsigned_32;
      Hi     : Unsigned_32;
      Hi1    : Unsigned_32;
      Result : Unsigned_64;
   begin

      --  We don't atomically read the 64-bits counter. So check that the 32
      --  MSB don't change. Note that mtime could be read atomically on RV64
      --  and RV128, this would require a separate implementation.

      Hi := Mtime_Hi;
      loop
         Lo := Mtime_Lo;
         Hi1 := Mtime_Hi;
         exit when Hi = Hi1;
         Hi := Hi1;
      end loop;

      Result := Unsigned_64 (Hi) * 2**32 + Unsigned_64 (Lo);

      Result := Result * System.BB.Board_Parameters.Clock_Scale;

      return BB.Time.Time (Result);
   end Read_Clock;

   ---------------------------
   -- Install_Alarm_Handler --
   ---------------------------

   procedure Install_Alarm_Handler
     (Handler : BB.Interrupts.Interrupt_Handler)
   is
   begin
      CPU_Specific.Install_Trap_Handler
        (Handler, CPU_Specific.Timer_Trap);
   end Install_Alarm_Handler;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : BB.Time.Time)
   is

      U64_Ticks : constant Unsigned_64 :=
        Unsigned_64 (Ticks) / System.BB.Board_Parameters.Clock_Scale;

      Lo : constant Unsigned_32 :=
        Unsigned_32 (U64_Ticks and 16#FFFF_FFFF#);

      Hi : constant Unsigned_32 :=
        Unsigned_32 (Shift_Right (U64_Ticks, 32) and 16#FFFF_FFFF#);

   begin
      --  In RV32, memory-mapped writes to mtimecmp modify only one 32-bit
      --  part of the register. The following code sequence sets a 64-bit
      --  mtimecmp value without spuriously generating a timer interrupt due to
      --  the intermediate value of the comparand:

      Mtimecmp_Lo := Unsigned_32'Last; -- No smaller than old value
      Mtimecmp_Hi := Hi; -- No smaller than new value
      Mtimecmp_Lo := Lo; -- New value.
   end Set_Alarm;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      Mtimecmp_Lo := Unsigned_32'Last;
      Mtimecmp_Hi := Unsigned_32'Last;
   end Clear_Alarm_Interrupt;

end Time;
