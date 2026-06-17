------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                I N T E R F A C E S . L E O N 3 . T I M E R S             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Board_Support;
with System.BB.Board_Parameters;

package Interfaces.Leon3.Timers is
   pragma Preelaborate;

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of record types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Scaler_10 is mod 2 **  10;
   for Scaler_10'Size use  10;
   --  10-bit scaler

   ---------------------
   -- Timer Registers --
   ---------------------

   Timer_Base : constant := System.BB.Board_Parameters.Timer_Base;

   subtype Timer_Register is System.BB.Board_Support.Time.Timer_Interval;
   pragma Suppress_Initialization (Timer_Register);

   type Timer_Control_Register is record
      Enable            : Boolean;
      --  1  : enable counting
      --  0  : hold scaler (and counter) w

      Reload_Counter    : Boolean;
      --  1  : reload counter at zero and restart
      --  0  : stop counter at zero w

      Load_Counter      : Boolean;
      --  1  : load counter with preset value and start if enabled
      --  0  : no function w

      Interrupt_Enable  : Boolean;
      --  1  : timer underflow signals interrupt
      --  0  : interrupts disabled

      Interrupt_Pending : Boolean;
      --  0  : interrupt not pending
      --  1  : interrupt pending, remains 1 until writing 0 to this bit

      Chain             : Boolean;
      --  0  : timer functions independently
      --  1  : decrementing timer N begins when timer (N - 1) underflows

      Debug_Halt        : Boolean;
      --  State of timer when DF = 0, read only
      --  0  : active
      --  1  : frozen

      Reserved          : Reserved_25;
   end record;

   for Timer_Control_Register use record
      Reserved          at 0 range Bit31 .. Bit07;
      Debug_Halt        at 0 range Bit06 .. Bit06;
      Chain             at 0 range Bit05 .. Bit05;
      Interrupt_Pending at 0 range Bit04 .. Bit04;
      Interrupt_Enable  at 0 range Bit03 .. Bit03;
      Load_Counter      at 0 range Bit02 .. Bit02; -- Load_Timer (LD) in AUM
      Reload_Counter    at 0 range Bit01 .. Bit01; -- Restart (RS) in AUM
      Enable            at 0 range Bit00 .. Bit00;
   end record;

   for Timer_Control_Register'Size use 32;
   pragma Suppress_Initialization (Timer_Control_Register);
   pragma Volatile_Full_Access (Timer_Control_Register);

   -------------
   -- Timer 1 --
   -------------

   Timer_1_Counter : Timer_Register;
   Timer_1_Reload  : Timer_Register;
   Timer_1_Control : Timer_Control_Register;

   for Timer_1_Counter'Address use System'To_Address (Timer_Base + 16#10#);
   for Timer_1_Reload'Address  use System'To_Address (Timer_Base + 16#14#);
   for Timer_1_Control'Address use System'To_Address (Timer_Base + 16#18#);

   -------------
   -- Timer 2 --
   -------------

   Timer_2_Counter : Timer_Register;
   Timer_2_Reload  : Timer_Register;
   Timer_2_Control : Timer_Control_Register;

   for Timer_2_Counter'Address use System'To_Address (Timer_Base + 16#20#);
   for Timer_2_Reload'Address  use System'To_Address (Timer_Base + 16#24#);
   for Timer_2_Control'Address use System'To_Address (Timer_Base + 16#28#);

   -------------
   -- Timer 3 --
   -------------

   Timer_3_Counter : Timer_Register;
   Timer_3_Reload  : Timer_Register;
   Timer_3_Control : Timer_Control_Register;

   for Timer_3_Counter'Address use System'To_Address (Timer_Base + 16#30#);
   for Timer_3_Reload'Address  use System'To_Address (Timer_Base + 16#34#);
   for Timer_3_Control'Address use System'To_Address (Timer_Base + 16#38#);

   -------------
   -- Timer 4 --
   -------------

   Timer_4_Counter : Timer_Register;
   Timer_4_Reload  : Timer_Register;
   Timer_4_Control : Timer_Control_Register;

   for Timer_4_Counter'Address use System'To_Address (Timer_Base + 16#40#);
   for Timer_4_Reload'Address  use System'To_Address (Timer_Base + 16#44#);
   for Timer_4_Control'Address use System'To_Address (Timer_Base + 16#48#);

   --------------
   -- Watchdog --
   --------------

   --  Watchdog_Register_Address is not available.
   --  On LEON3, Timer_4 also drives the WDOGN watchdog signal

   Watchdog_Register : Timer_Register renames Timer_4_Counter;

   ---------------
   -- Prescaler --
   ---------------

   type Prescaler_Register is record
      Value    : Scaler_10;
      Reserved : Reserved_22;
   end record;

   for Prescaler_Register use record
      Reserved at 0 range Bit31 .. Bit10;
      Value    at 0 range Bit09 .. Bit00;
   end record;

   for Prescaler_Register'Size use 32;

   pragma Suppress_Initialization (Prescaler_Register);
   pragma Volatile_Full_Access (Prescaler_Register);

   Prescaler_Counter : Prescaler_Register;
   for Prescaler_Counter'Address use System'To_Address (Timer_Base + 16#00#);

   Prescaler_Reload : Prescaler_Register;
   for Prescaler_Reload'Address use System'To_Address (Timer_Base + 16#04#);

end Interfaces.Leon3.Timers;
