------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--         S Y S T E M . B B . B O A R D _ S U P P O R T . L E O N          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the appropriate mapping for the system registers.
--  This is the LEON version of this package.

pragma Restrictions (No_Elaboration_Code);

package System.BB.Board_Support.LEON is
   pragma Preelaborate;

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of records types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Scaler_10 is mod 2 **  10;
   for Scaler_10'Size use  10;
   --  10-bit scaler

   type Scaler_12 is mod 2 **  12;
   for Scaler_12'Size use  12;
   --  12-bit scaler

   type Timers_Counter is mod 2 ** 24;
   for Timers_Counter'Size use  24;
   --  Timer counters are 24-bit

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_16 is array (0 .. 15) of Boolean;
   for Reserved_16'Size use 16;
   pragma Pack (Reserved_16);

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_22 is array (0 .. 21) of Boolean;
   for Reserved_22'Size use 22;
   pragma Pack (Reserved_22);

   type Reserved_23 is array (0 .. 22) of Boolean;
   for Reserved_23'Size use 23;
   pragma Pack (Reserved_23);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Reserved_25 is array (0 .. 24) of Boolean;
   for Reserved_25'Size use 25;
   pragma Pack (Reserved_25);

   type Reserved_29 is array (0 .. 28) of Boolean;
   for Reserved_29'Size use 29;
   pragma Pack (Reserved_29);

   type Interrupt_Level is array (1 .. 15) of Boolean;
   for Interrupt_Level'Size use 15;
   pragma Pack (Interrupt_Level);

   type Status_2 is mod 2 ** 2;
   for Status_2'Size use  2;

   ------------------------------------------
   -- Addresses of memory mapped registers --
   ------------------------------------------

   Timer_1_Counter_Register_Address    : constant Address
                                           := System'To_Address (16#80000040#);
   Timer_1_Reload_Register_Address     : constant Address
                                           := System'To_Address (16#80000044#);
   Timer_1_Control_Register_Address    : constant Address
                                           := System'To_Address (16#80000048#);

   Watchdog_Register_Address           : constant Address
                                           := System'To_Address (16#8000004C#);

   Timer_2_Counter_Register_Address    : constant Address
                                           := System'To_Address (16#80000050#);
   Timer_2_Reload_Register_Address     : constant Address
                                           := System'To_Address (16#80000054#);
   Timer_2_Control_Register_Address    : constant Address
                                           := System'To_Address (16#80000058#);

   Prescaler_Counter_Register_Address  : constant Address
                                           := System'To_Address (16#80000060#);
   Prescaler_Reload_Register_Address   : constant Address
                                           := System'To_Address (16#80000064#);

   UART_1_Data_Register_Address        : constant Address
                                           := System'To_Address (16#80000070#);
   UART_1_Status_Register_Address      : constant Address
                                           := System'To_Address (16#80000074#);
   UART_1_Control_Register_Address     : constant Address
                                           := System'To_Address (16#80000078#);
   UART_1_Scaler_Register_Address      : constant Address
                                           := System'To_Address (16#8000007C#);

   UART_2_Data_Register_Address        : constant Address
                                           := System'To_Address (16#80000080#);
   UART_2_Status_Register_Address      : constant Address
                                           := System'To_Address (16#80000084#);
   UART_2_Control_Register_Address     : constant Address
                                           := System'To_Address (16#80000088#);
   UART_2_Scaler_Register_Address      : constant Address
                                           := System'To_Address (16#8000008C#);

   Interrupt_Mask_and_Priority_Register_Address : constant Address
                                           := System'To_Address (16#80000090#);
   Interrupt_Pending_Register_Address  : constant Address
                                           := System'To_Address (16#80000094#);
   Interrupt_Force_Register_Address    : constant Address
                                           := System'To_Address (16#80000098#);
   Interrupt_Clear_Register_Address    : constant Address
                                           := System'To_Address (16#8000009C#);

   Cache_Control_Register_Address      : constant Address
                                           := System'To_Address (16#80000014#);

   ---------------------
   -- Timer Registers --
   ---------------------

   type Timer_Register is
      record
         Timer_Value : Timers_Counter;
         Reserved    : Reserved_8;
      end record;

   for Timer_Register use
      record
         Timer_Value at 0 range 8 .. 31;
         Reserved    at 0 range 0 .. 7;
      end record;

   for Timer_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Register);

   type Timer_Control_Register is
      record
         Enable : Boolean;
         --  1  : enable counting
         --  0  : hold scaler (and counter) w

         Reload_Counter : Boolean;
         --  1  : reload counter at zero and restart
         --  0  : stop counter at zero w

         Load_Counter : Boolean;
         --  1  : load counter with preset value and start if enabled
         --  0  : no function w

         Reserved : Reserved_29;
      end record;

   for Timer_Control_Register use
      record
         Enable         at 0 range 31 .. 31;
         Reload_Counter at 0 range 30 .. 30;
         Load_Counter   at 0 range 29 .. 29;
         Reserved       at 0 range  0 .. 28;
      end record;

   for Timer_Control_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Control_Register);

   -------------
   -- Timer 1 --
   -------------

   Timer_1_Counter : Timer_Register;
   pragma Atomic (Timer_1_Counter);
   for Timer_1_Counter'Address use Timer_1_Counter_Register_Address;

   Timer_1_Reload : Timer_Register;
   pragma Atomic (Timer_1_Reload);
   for Timer_1_Reload'Address use Timer_1_Reload_Register_Address;

   Timer_1_Control : Timer_Control_Register;
   pragma Atomic (Timer_1_Control);
   for Timer_1_Control'Address use Timer_1_Control_Register_Address;

   -------------
   -- Timer 2 --
   -------------

   Timer_2_Counter : Timer_Register;
   pragma Atomic (Timer_2_Counter);
   for Timer_2_Counter'Address use Timer_2_Counter_Register_Address;

   Timer_2_Reload : Timer_Register;
   pragma Atomic (Timer_2_Reload);
   for Timer_2_Reload'Address use Timer_2_Reload_Register_Address;

   Timer_2_Control : Timer_Control_Register;
   pragma Atomic (Timer_2_Control);
   for Timer_2_Control'Address use Timer_2_Control_Register_Address;

   --------------
   -- Watchdog --
   --------------

   Watchdog_Register : Timer_Register;
   pragma Atomic (Watchdog_Register);
   for Watchdog_Register'Address use Watchdog_Register_Address;

   ---------------
   -- Prescaler --
   ---------------

   type Prescaler_Register is
      record
         Value    : Scaler_10;
         Reserved : Reserved_22;
      end record;

   for Prescaler_Register use
      record
         Value    at 0 range 22 .. 31;
         Reserved at 0 range  0 .. 21;
      end record;

   for Prescaler_Register'Size use 32;

   pragma Suppress_Initialization (Prescaler_Register);

   Prescaler_Counter : Prescaler_Register;
   pragma Atomic (Prescaler_Counter);
   for Prescaler_Counter'Address use Prescaler_Counter_Register_Address;

   Prescaler_Reload : Prescaler_Register;
   pragma Atomic (Prescaler_Reload);
   for Prescaler_Reload'Address use Prescaler_Reload_Register_Address;

   ------------------------------------------
   -- Interrupt Mask and Priority Register --
   ------------------------------------------

   type Interrupt_Mask_and_Priority_Register is
      record
         Reserved_1 : Boolean;

         AMBA       : Boolean;
         UART_2     : Boolean;
         UART_1     : Boolean;
         External_0 : Boolean;
         External_1 : Boolean;
         External_2 : Boolean;
         External_3 : Boolean;
         Timer_1    : Boolean;
         Timer_2    : Boolean;
         Unused_1   : Boolean;
         DSU        : Boolean;
         Unused_2   : Boolean;
         Unused_3   : Boolean;
         PCI        : Boolean;
         Unused_4   : Boolean;
         --  1  : interrupt X masked
         --  0  : interrupt X not masked r/w

         Reserved_2 : Boolean;

         Ilevel : Interrupt_Level;
         --  1 : interrupt belongs to priority level 1
         --  0 : interrupt belongs to priority level 0
      end record;

   for Interrupt_Mask_and_Priority_Register use
      record
         Reserved_1 at 0 range 31 .. 31;
         AMBA       at 0 range 30 .. 30;
         UART_2     at 0 range 29 .. 29;
         UART_1     at 0 range 28 .. 28;
         External_0 at 0 range 27 .. 27;
         External_1 at 0 range 26 .. 26;
         External_2 at 0 range 25 .. 25;
         External_3 at 0 range 24 .. 24;
         Timer_1    at 0 range 23 .. 23;
         Timer_2    at 0 range 22 .. 22;
         Unused_1   at 0 range 21 .. 21;
         DSU        at 0 range 20 .. 20;
         Unused_2   at 0 range 19 .. 19;
         Unused_3   at 0 range 18 .. 18;
         PCI        at 0 range 17 .. 17;
         Unused_4   at 0 range 16 .. 16;
         Reserved_2 at 0 range 15 .. 15;
         Ilevel     at 0 range  0 .. 14;
      end record;

   for Interrupt_Mask_and_Priority_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Mask_and_Priority_Register);

   Interrupt_Mask_and_Priority : Interrupt_Mask_and_Priority_Register;
   pragma Atomic (Interrupt_Mask_and_Priority);
   for Interrupt_Mask_and_Priority'Address use
     Interrupt_Mask_and_Priority_Register_Address;

   --------------------------
   --  Interrupt Registers --
   --------------------------

   type Interrupt_Register is
      record
         Reserved_1 : Boolean;
         AMBA       : Boolean;
         UART_2     : Boolean;
         UART_1     : Boolean;
         External_0 : Boolean;
         External_1 : Boolean;
         External_2 : Boolean;
         External_3 : Boolean;
         Timer_1    : Boolean;
         Timer_2    : Boolean;
         Unused_1   : Boolean;
         DSU        : Boolean;
         Unused_2   : Boolean;
         Unused_3   : Boolean;
         PCI        : Boolean;
         Unused_4   : Boolean;
         Reserved_2 : Reserved_16;
      end record;

   for Interrupt_Register use
      record
         Reserved_1 at 0 range 31 .. 31;
         AMBA       at 0 range 30 .. 30;
         UART_2     at 0 range 29 .. 29;
         UART_1     at 0 range 28 .. 28;
         External_0 at 0 range 27 .. 27;
         External_1 at 0 range 26 .. 26;
         External_2 at 0 range 25 .. 25;
         External_3 at 0 range 24 .. 24;
         Timer_1    at 0 range 23 .. 23;
         Timer_2    at 0 range 22 .. 22;
         Unused_1   at 0 range 21 .. 21;
         DSU        at 0 range 20 .. 20;
         Unused_2   at 0 range 19 .. 19;
         Unused_3   at 0 range 18 .. 18;
         PCI        at 0 range 17 .. 17;
         Unused_4   at 0 range 16 .. 16;
         Reserved_2 at 0 range  0 .. 15;
      end record;

   for Interrupt_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Register);

   --------------------------------
   -- Interrupt Pending Register --
   --------------------------------

   Interrupt_Pending : Interrupt_Register;
   pragma Atomic (Interrupt_Pending);
   for Interrupt_Pending'Address use Interrupt_Pending_Register_Address;

   ------------------------------
   -- Interrupt Force Register --
   ------------------------------

   Interrupt_Force : Interrupt_Register;
   pragma Atomic (Interrupt_Force);
   for Interrupt_Force'Address use Interrupt_Force_Register_Address;

   ------------------------------
   -- Interrupt Clear Register --
   ------------------------------

   Interrupt_Clear : Interrupt_Register;
   pragma Atomic (Interrupt_Clear);
   for Interrupt_Clear'Address use Interrupt_Clear_Register_Address;

   ---------------------
   -- UARTs Registers --
   ---------------------

   type UART_Data_Register is
      record
         RTD : Character;
         --  Rx/Tx Data r/w

         Reserved : Reserved_24;
         --  Not used r
      end record;

   for UART_Data_Register use
      record
         RTD      at 0 range 24 .. 31;
         Reserved at 0 range  0 .. 23;
      end record;

   for UART_Data_Register'Size use 32;

   pragma Suppress_Initialization (UART_Data_Register);

   type UART_Status_Register is
      record
         DR : Boolean;
         --  Data Ready

        TS  : Boolean;
         --  Transmitter shift register empty (no data to send)

         TH : Boolean;
         --  Transmitter hold register empty (ready to load data)

         BR : Boolean;
         --  Break received

         OV : Boolean;
         --  Overrun error

         PE : Boolean;
         --  Parity error

         FE : Boolean;
         --  Framing error

         Reserved : Reserved_25;
         --  Not used r
      end record;

   for UART_Status_Register use
      record
         DR       at 0 range 31 .. 31;
         TS       at 0 range 30 .. 30;
         TH       at 0 range 29 .. 29;
         BR       at 0 range 28 .. 28;
         OV       at 0 range 27 .. 27;
         PE       at 0 range 26 .. 26;
         FE       at 0 range 25 .. 25;
         Reserved at 0 range  0 .. 24;
      end record;

   for UART_Status_Register'Size use 32;

   pragma Suppress_Initialization (UART_Status_Register);

   type UART_Control_Register is
      record
         RE : Boolean; --  Receiver enable
         --  1  : enables the receiver
         --  0  : disables the receiver

         TE : Boolean; --  Transmitter enable
         --  1  : enables the transmitter
         --  0  : disables the transmitter

         RI : Boolean; --  Receiver interrupt enable
         --  1  : enables generation of receiver interrupt
         --  0  : disables generation of receiver interrupt

         TI : Boolean; --  Transmitter interrupt enable
         --  1  : enables generation of transmitter interrupt
         --  0  : disables generation of transmitter interrupt

         PS : Boolean; --  Parity
         --  1  : odd parity
         --  0  : even parity

         PE : Boolean; --  Parity enable
         --  1  : parity enabled
         --  0  : no parity

         FL : Boolean; --  Flow Control
         --  1  : flow control using CTS/RTS
         --  0  : disables

         LB : Boolean; --  Loop back
         --  1  : enables loop back mode
         --  0  : disables loop back mode

         EC : Boolean; --  External clock
         --  1  : external clock
         --  0  : system clock

         Reserved : Reserved_23;
      end record;

   for UART_Control_Register use
      record
         RE       at 0 range 31 .. 31;
         TE       at 0 range 30 .. 30;
         RI       at 0 range 29 .. 29;
         TI       at 0 range 28 .. 28;
         PS       at 0 range 27 .. 27;
         PE       at 0 range 26 .. 26;
         FL       at 0 range 25 .. 25;
         LB       at 0 range 24 .. 24;
         EC       at 0 range 23 .. 23;
         Reserved at 0 range  0 .. 22;
      end record;

   for UART_Control_Register'Size use 32;

   pragma Suppress_Initialization (UART_Control_Register);

   type UART_Scaler_Register is
      record
         UART_Scaler : Scaler_12;
         --  1 - 4095  : Divide factor
         --  0  : stops the UART clock

         Reserved    : Reserved_20;
      end record;

   for UART_Scaler_Register use
      record
         UART_Scaler at 0 range 20 .. 31;
         Reserved    at 0 range  0 .. 19;
      end record;

   for UART_Scaler_Register'Size use 32;

   pragma Suppress_Initialization (UART_Scaler_Register);

   ------------
   -- UART 1 --
   ------------

   UART_1_Data : UART_Data_Register;
   pragma Atomic (UART_1_Data);
   for UART_1_Data'Address use UART_1_Data_Register_Address;

   UART_1_Status : UART_Status_Register;
   pragma Atomic (UART_1_Status);
   for UART_1_Status'Address use UART_1_Status_Register_Address;

   UART_1_Control : UART_Control_Register;
   pragma Atomic (UART_1_Control);
   for UART_1_Control'Address use UART_1_Control_Register_Address;

   UART_1_Scaler : UART_Scaler_Register;
   pragma Atomic (UART_1_Scaler);
   for UART_1_Scaler'Address use UART_1_Scaler_Register_Address;

   ------------
   -- UART 2 --
   ------------

   UART_2_Data : UART_Data_Register;
   pragma Atomic (UART_2_Data);
   for UART_2_Data'Address use UART_2_Data_Register_Address;

   UART_2_Status : UART_Status_Register;
   pragma Atomic (UART_2_Status);
   for UART_2_Status'Address use UART_2_Status_Register_Address;

   UART_2_Control : UART_Control_Register;
   pragma Atomic (UART_2_Control);
   for UART_2_Control'Address use UART_2_Control_Register_Address;

   UART_2_Scaler : UART_Scaler_Register;
   pragma Atomic (UART_2_Scaler);
   for UART_2_Scaler'Address use UART_2_Scaler_Register_Address;

   ----------------------------
   -- Cache Control Register --
   ----------------------------

   type Cache_Control_Register is
      record
         Ics   : Status_2;
         Dcs   : Status_2;
         Icf   : Boolean;
         Dcf   : Boolean;
         Dde   : Status_2;
         Dte   : Status_2;
         Ide   : Status_2;
         Ite   : Status_2;
         Dp    : Boolean;
         Ip    : Boolean;
         Ib    : Boolean;
         Cpte  : Status_2;
         Cpc   : Status_2;
         Fi    : Boolean;
         Fd    : Boolean;
         Ds    : Boolean;
         Isets : Status_2;
         Dsets : Status_2;
         Irepl : Status_2;
         Drepl : Status_2;
      end record;

   for Cache_Control_Register use
     record
         Ics   at 0 range 30 .. 31;
         Dcs   at 0 range 28 .. 29;
         Icf   at 0 range 27 .. 27;
         Dcf   at 0 range 26 .. 26;
         Dde   at 0 range 24 .. 25;
         Dte   at 0 range 22 .. 23;
         Ide   at 0 range 20 .. 21;
         Ite   at 0 range 18 .. 19;
         Dp    at 0 range 17 .. 17;
         Ip    at 0 range 16 .. 16;
         Ib    at 0 range 15 .. 15;
         Cpte  at 0 range 13 .. 14;
         Cpc   at 0 range 11 .. 12;
         Fi    at 0 range 10 .. 10;
         Fd    at 0 range  9 ..  9;
         Ds    at 0 range  8 ..  8;
         Isets at 0 range  6 ..  7;
         Dsets at 0 range  4 ..  5;
         Irepl at 0 range  2 ..  3;
         Drepl at 0 range  0 ..  1;
     end record;

   for  Cache_Control_Register'Size use 32;

   pragma Suppress_Initialization (Cache_Control_Register);

   CCR : Cache_Control_Register;
   pragma Atomic (CCR);
   for CCR'Address use Cache_Control_Register_Address;

end System.BB.Board_Support.LEON;
