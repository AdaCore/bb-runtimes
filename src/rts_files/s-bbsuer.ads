------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--        S Y S T E M . B B . B O A R D _ S U P P O R T . E R C 3 2         --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
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
--  This is an ERC32 specific package.

pragma Restrictions (No_Elaboration_Code);

package System.BB.Board_Support.ERC32 is
   pragma Preelaborate;

   --  Warning : The bit numbering within a register is opposed to the
   --  bit numbering of MEC Specification Document (MCD/SPC/0009/SE).

   --  It seems to be a big-endian/little-endian notation problem.
   --  System.Default_Bit_Order is HIGH_ORDER_FIRST for this target.

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of records types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Scaler_8 is mod 2 **  8;
   for Scaler_8'Size use  8;
   --  8-bit scaler

   type Scaler_16 is mod 2 ** 16;
   for Scaler_16'Size use  16;
   --  16-bit scaler

   type Timers_Counter is mod 2 ** 32;
   for Timers_Counter'Size use  32;
   --  Timer counters are 32-bit registers

   type Segment_Address is mod 2 ** 23;
   for Segment_Address'Size use 23;

   type Check_Bits_Type is array (0 .. 6) of Boolean;
   for Check_Bits_Type'Size use 7;
   pragma Pack (Check_Bits_Type);

   type Reserved_4 is array (0 .. 3) of Boolean;
   for Reserved_4'Size use 4;
   pragma Pack (Reserved_4);

   type Reserved_7 is array (0 .. 6) of Boolean;
   for Reserved_7'Size use 7;
   pragma Pack (Reserved_7);

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_9 is array (0 .. 8) of Boolean;
   for Reserved_9'Size use 9;
   pragma Pack (Reserved_9);

   type Reserved_10 is array (0 .. 9) of Boolean;
   for Reserved_10'Size use 10;
   pragma Pack (Reserved_10);

   type Reserved_11 is array (0 .. 10) of Boolean;
   for Reserved_11'Size use 11;
   pragma Pack (Reserved_11);

   type Reserved_16 is array (0 .. 15) of Boolean;
   for Reserved_16'Size use 16;
   pragma Pack (Reserved_16);

   type Reserved_17 is array (0 .. 16) of Boolean;
   for Reserved_17'Size use 17;
   pragma Pack (Reserved_17);

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   ------------------------------------------
   -- Addresses of memory mapped registers --
   ------------------------------------------

   Control_Register_Address :
   constant System.Address := System'To_Address (16#1F80000#);

   Test_Control_Register_Address :
   constant System.Address := System'To_Address (16#01F800D0#);

   Real_Time_Clock_Counter_Address :
   constant System.Address := System'To_Address (16#1F80080#);

   General_Purpose_Timer_Counter_Address :
   constant System.Address := System'To_Address (16#01F80088#);

   Real_Time_Clock_Scaler_Address :
   constant System.Address := System'To_Address (16#01F80084#);

   General_Purpose_Timer_Scaler_Address :
   constant System.Address := System'To_Address (16#01F8008C#);

   Timer_Control_Register_Address :
   constant System.Address := System'To_Address (16#01F80098#);

   Watchdog_Trap_Door_Set_Register_Address :
   constant System.Address := System'To_Address (16#01F80064#);

   Access_Protection_Segment_1_Base_Register_Address :
   constant System.Address := System'To_Address (16#01F80020#);

   Access_Protection_Segment_2_Base_Register_Address :
   constant System.Address := System'To_Address (16#01F80028#);

   Access_Protection_Segment_1_End_Register_Address :
   constant System.Address := System'To_Address (16#01F80024#);

   Access_Protection_Segment_2_End_Register_Address :
   constant System.Address := System'To_Address (16#01F8002C#);

   Interrupt_Mask_Register_Address :
   constant System.Address := System'To_Address (16#01F8004C#);

   Interrupt_Force_Register_Address :
   constant System.Address := System'To_Address (16#01F80054#);

   UART_Channel_A_Rx_Tx_Register_Address :
   constant System.Address := System'To_Address (16#01F800E0#);

   UART_Channel_B_Rx_Tx_Register_Address :
   constant System.Address := System'To_Address (16#01F800E4#);

   UART_Status_Register_Address :
   constant System.Address := System'To_Address (16#01F800E8#);

   ----------------------
   -- Control Register --
   ----------------------

   type Control_Register is
      record
         PRD : Boolean;
         --  Power-down 1  : enabled (allowed) 0 : disabled r/w

         SWR : Boolean;
         --  Software reset 1  : enabled (allowed) 0 : disabled r/w

         BTO : Boolean;
         --  Bus timeout 1  : enabled 0 : disabled r/w

         BP : Boolean;
         --  Block protection instead of normal access protection
         --  1  : enabled 0 : disabled r/w

         WDCS : Boolean;
         --  Watchdog clock supply
         --  1  : external clock with prescaler (divide by 16)
         --  0  : external clock, no prescaler r/w

         IUEMMSK : Boolean;
         --  IU Error Mode Mask 1  : Error masked (= disabled)
         --  0  : Error not masked r/w

         RHIUEM : Boolean;
         --  Reset or Halt when IU error mode (ERROR*)
         --  1  : Reset 0 : Halt r/w

         IUHEMSK : Boolean;
         --  IU Hardware Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHIUHE : Boolean;
         --  Reset or Halt when IU Hardware Error (HWERR*)
         --  1  : Reset 0 : Halt r/w

         IUCMPMSK : Boolean;
         --  IU Comparison Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHIUCMP : Boolean;
         --  Reset or Halt when IU comparison error 1  : Reset 0 : Halt r/w

         FPUCMPMSK : Boolean;
         --  FPU Comparison Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHFPUCMP : Boolean;
         --  Reset or Halt when FPU comparison error
         --  1  : Reset 0 : Halt r/w

         MECHEMSK : Boolean;
         --  MEC HW Error Mask
         --  1  : Error masked (= disabled) 0 : Error not masked r/w

         RHMECHE : Boolean;
         --  Reset or Halt when MEC HW Error (MECHWERR)
         --  1  : Reset 0 : Halt r/w

         RESERVED : Boolean;
         --  Not used r

         DMAE : Boolean;
         --  1 DMA 1  : enabled 0 : disabled r/w

         DPE : Boolean;
         --  DMA Parity Enabled 1  : enabled 0 : disabled r/w

         DST : Boolean;
         --  DMA session timeout 1  : enabled 0 : disabled r/w

         UBR : Boolean;
         --  UART baud rate(1)
         --  1  : No change of UART scaler baudrate
         --  0  : Divide UART scaler baudrate by two r/w

         UPE : Boolean;
         --  UART parity enable
         --  1  : parity enabled 0 : no parity r/w

         UP : Boolean;
         --  UART parity 1  : odd parity 0 : even parity r/w

         USB : Boolean;
         --   UART stop bits 1  : two stop bits 0 : one stop bit r/w

         UCS : Boolean;
         --  UART clock supply 1  : system clock 0 : external clock r/w

         UART_Scaler : Scaler_8;
         --  1 - 255 : Divide factor (1) 0: stops the UART clock r/w
      end record;

   for Control_Register use
      record
         PRD at 0 range 31 .. 31;
         SWR at 0 range 30 .. 30;
         BTO at 0 range 29 .. 29;
         BP at 0 range 28 .. 28;
         WDCS at 0 range 27 .. 27;
         IUEMMSK at 0 range 26 .. 26;
         RHIUEM at 0 range 25 .. 25;
         IUHEMSK at 0 range 24 .. 24;
         RHIUHE at 0 range 23 .. 23;
         IUCMPMSK at 0 range 22 .. 22;
         RHIUCMP at 0 range 21 .. 21;
         FPUCMPMSK at 0 range 20 .. 20;
         RHFPUCMP at 0 range 19 .. 19;
         MECHEMSK at 0 range 18 .. 18;
         RHMECHE at 0 range 17 .. 17;
         RESERVED at 0 range 16 .. 16;
         DMAE at 0 range 15 .. 15;
         DPE at 0 range 14 .. 14;
         DST at 0 range 13 .. 13;
         UBR at 0 range 12 .. 12;
         UPE at 0 range 11 .. 11;
         UP at 0 range 10 .. 10;
         USB at 0 range 9 .. 9;
         UCS at 0 range 8 .. 8;
         UART_scaler at 0 range 0 .. 7;
      end record;

   for Control_Register'Size use 32;

   pragma Suppress_Initialization (Control_Register);

   Control : Control_Register;
   pragma Atomic (Control);
   for Control'Address use Control_Register_Address;

   ------------------
   -- Test Control --
   ------------------

   type Test_Control_Register is
      record
         Check_Bits                            : Check_Bits_Type;
         --  CB 0 Check bits r/w

         Reserved10                            : Reserved_10;
         --  0 Not used r

         EDAC_Test_Enable                      : Boolean;
         --  ET 0 EDAC test enable
         --  0: Testing disabled, 1: Memory test enabled, r/w

         Parity_Test                           : Boolean;
         --  PT 0 Parity test
         --  1 : test enabled, 0 : test disabled, r/w

         Interrupt_Force_Register_Write_Enable : Boolean;
         --  IT 0 Interrupt Force Register Write Enable
         --  1 : enabled, 0 : disabled, r/w

         Error_Write_Enable                    : Boolean;
         --  EWE 0 Error Write Enable
         --  1: Write to Error and Reset Status Register enabled
         --  0: Write to Error and Reset Status Register disabled, r/w

         Reserved11                            : Reserved_11;
         --  0 Not used r
      end record;

   for Test_Control_Register use
      record
         Check_Bits at 0 range 25 .. 31;
         Reserved10 at 0 range 15 .. 24;
         EDAC_Test_Enable at 0 range 14 .. 14;
         Parity_Test at 0 range 13 .. 13;
         Interrupt_Force_Register_Write_Enable at 0 range 12 .. 12;
         Error_Write_Enable at 0 range 11 .. 11;
         Reserved11 at 0 range 0 .. 10;
      end record;

   for Test_Control_Register'Size use 32;

   pragma Suppress_Initialization (Test_Control_Register);

   Test_Control : Test_Control_Register;
   pragma Atomic (Test_Control);
   for Test_Control'Address use Test_Control_Register_Address;

   -----------------------------
   -- Real Time Clock Counter --
   -----------------------------

   Real_Time_Clock_Counter : Timers_Counter;
   for Real_Time_Clock_Counter'Address use Real_Time_Clock_Counter_Address;

   -----------------------------------
   -- General Purpose Timer Counter --
   -----------------------------------

   General_Purpose_Timer_Counter : Timers_Counter;
   for General_Purpose_Timer_Counter'Address use
     General_Purpose_Timer_Counter_Address;

   ----------------------------
   -- Real Time Clock Scaler --
   ----------------------------

   type Real_Time_Clock_Scaler_Register is
      record
         RTCS : Scaler_8;
         Reserved : Reserved_24;
      end record;

   for Real_Time_Clock_Scaler_Register use
      record
         RTCS at 0 range 24 .. 31;
         Reserved at 0 range 0 .. 23;
      end record;

   for Real_Time_Clock_Scaler_Register'Size use 32;

   pragma Suppress_Initialization (Real_Time_Clock_Scaler_Register);

   Real_Time_Clock_Scaler : Real_Time_Clock_Scaler_Register;
   pragma Atomic (Real_Time_Clock_Scaler);
   for Real_Time_Clock_Scaler'Address use Real_Time_Clock_Scaler_Address;

   ----------------------------------
   -- General Purpose Timer Scaler --
   ----------------------------------

   type General_Purpose_Timer_Scaler_Register is
      record
         GPTS : Scaler_16;
         Reserved : Reserved_16;
      end record;

   for General_Purpose_Timer_Scaler_Register use
      record
         GPTS at 0 range 16 .. 31;
         Reserved at 0 range 0 .. 15;
      end record;

   for General_Purpose_Timer_Scaler_Register'Size use 32;

   pragma Suppress_Initialization (General_Purpose_Timer_Scaler_Register);

   General_Purpose_Timer_Scaler : General_Purpose_Timer_Scaler_Register;
   pragma Atomic (General_Purpose_Timer_Scaler);
   for General_Purpose_Timer_Scaler'Address use
     General_Purpose_Timer_Scaler_Address;

   -------------------
   -- Timer Control --
   -------------------

   type Timer_Control_Register is
      record
         GCR : Boolean; -- General Purpose Timer Counter Reload
         --  1  : reload counter at zero and restart
         --  0  : stop counter at zero w

         GCL : Boolean; -- General Purpose Timer counter load
         --  1  : load counter with preset value and start if enabled
         --  0  : no function w

         GSE : Boolean; -- General Purpose Timer enable
         --  1  : enable counting
         --  0  : hold scaler (and counter) w

         GSL : Boolean; -- General Purpose Timer Scaler load
         --  1  : load scaler with preset value and start if enabled
         --  0  : no function w

         Reserved4 : Reserved_4; -- 0 Not used r

         RTCCR : Boolean; --  Real Time Clock Counter Reload
         --  1  : reload counter at zero and restart
         --  0  : stop counter at zero w

         RTCCL : Boolean; -- Real Time Clock counter load
         --  1  : load counter with preset value and start if enabled
         --  0  : no function w

         RTCSE : Boolean; -- Real Time Clock Scaler enable
         --  1  : enable counting
         --  0  : hold scaler (and counter) w

         RTCSL : Boolean; -- Real Time Clock Scaler load
         --  1  : load scaler with preset value and start if enabled
         --  0  : no function w

         Reserved20 : Reserved_20; -- 0h Not used
      end record;

   for Timer_Control_Register use
      record
         GCR at 0 range 31 .. 31;
         GCL at 0 range 30 .. 30;
         GSE at 0 range 29 .. 29;
         GSL at 0 range 28 .. 28;
         Reserved4 at 0 range 24 .. 27;
         RTCCR at 0 range 23 .. 23;
         RTCCL at 0 range 22 .. 22;
         RTCSE at 0 range 21 .. 21;
         RTCSL at 0 range 20 .. 20;
         Reserved20 at 0 range 0 .. 19;
      end record;

   for Timer_Control_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Control_Register);

   Timer_Control : Timer_Control_Register;
   pragma Atomic (Timer_Control);
   for Timer_Control'Address use Timer_Control_Register_Address;

   ----------------------------
   -- Watchdog Trap Door Set --
   ----------------------------

   type Watchdog_Trap_Door_Set_Register is mod 2 ** 32;
   for Watchdog_Trap_Door_Set_Register'Size use 32;

   Watchdog_Trap_Door_Set : Watchdog_Trap_Door_Set_Register;
   for Watchdog_Trap_Door_Set'Address use
     Watchdog_Trap_Door_Set_Register_Address;

   ------------------------------------
   -- Access Protection Segment Base --
   ------------------------------------

   type Access_Protection_Segment_Base_Register is
      record
         SEGBASE : Segment_Address;

         UE : Boolean; -- User Enabled
         --  1 : Access protection enabled in user mode
         --  0 : Access protection disabled in user mode

         SE : Boolean; -- Supervisor Enabled
         --  1 : Access protection enabled in supervisor mode
         --  0 : Access protection disabled in supervisor mode

         Reserved : Reserved_7;
      end record;

   for Access_Protection_Segment_Base_Register use
      record
         SEGBASE at 0 range 9 .. 31;
         UE at 0 range 8 .. 8;
         SE at 0 range 7 .. 7;
         Reserved at 0 range 0 .. 6;
      end record;

   for Access_Protection_Segment_Base_Register'Size use 32;

   pragma Suppress_Initialization (Access_Protection_Segment_Base_Register);

   Protected_Segment_1_Base_Register : Access_Protection_Segment_Base_Register;
   pragma Atomic (Protected_Segment_1_Base_Register);
   for Protected_Segment_1_Base_Register'Address use
     Access_Protection_Segment_1_Base_Register_Address;

   Protected_Segment_2_Base_Register : Access_Protection_Segment_Base_Register;
   pragma Atomic (Protected_Segment_2_Base_Register);
   for Protected_Segment_2_Base_Register'Address use
     Access_Protection_Segment_2_Base_Register_Address;

   -----------------------------------
   -- Access Protection Segment End --
   -----------------------------------

   type Access_Protection_Segment_End_Register is
      record
         SEGEND : Segment_Address;
         Reserved : Reserved_9;
      end record;

   for Access_Protection_Segment_End_Register use
      record
         SEGEND at 0 range 9 .. 31;
         Reserved at 0 range 0 .. 8;
      end record;

   for Access_Protection_Segment_End_Register'Size use 32;

   pragma Suppress_Initialization (Access_Protection_Segment_End_Register);

   Protected_Segment_1_End_Register : Access_Protection_Segment_End_Register;
   pragma Atomic (Protected_Segment_1_End_Register);
   for Protected_Segment_1_End_Register'Address use
     Access_Protection_Segment_1_End_Register_Address;

   Protected_Segment_2_End_Register : Access_Protection_Segment_End_Register;
   pragma Atomic (Protected_Segment_2_End_Register);
   for Protected_Segment_2_End_Register'Address use
     Access_Protection_Segment_2_End_Register_Address;

   --------------------
   -- Interrupt Mask --
   --------------------

   type Interrupt_Mask_Register is
      record
         Reserved1 : Boolean;
         --  1  : interrupt X masked
         --  0  : interrupt X not masked r/w

         Masked_Hardware_Errors : Boolean;
         External_Interrupt_0 : Boolean;
         External_Interrupt_1 : Boolean;
         UART_A_Ready : Boolean;
         UART_B_Ready : Boolean;
         Correctable_Error_In_Memory : Boolean;
         UART_Error : Boolean;
         DMA_Access_Error : Boolean;
         DMA_Time_Out : Boolean;
         External_Interrupt_2 : Boolean;
         External_Interrupt_3 : Boolean;
         General_Purpose_Timer : Boolean;
         Real_Time_Clock : Boolean;
         External_Interrupt_4 : Boolean;
         Reserved17 : Reserved_17;
      end record;

   for Interrupt_Mask_Register use
      record
         Reserved1 at 0 range 31 .. 31;
         Masked_Hardware_Errors at 0 range 30 .. 30;
         External_Interrupt_0 at 0 range 29 .. 29;
         External_Interrupt_1 at 0 range 28 .. 28;
         UART_A_Ready at 0 range 27 .. 27;
         UART_B_Ready at 0 range 26 .. 26;
         Correctable_Error_In_Memory at 0 range 25 .. 25;
         UART_Error at 0 range 24 .. 24;
         DMA_Access_Error at 0 range 23 .. 23;
         DMA_Time_Out at 0 range 22 .. 22;
         External_Interrupt_2 at 0 range 21 .. 21;
         External_Interrupt_3 at 0 range 20 .. 20;
         General_Purpose_Timer at 0 range 19 .. 19;
         Real_Time_Clock at 0 range 18 .. 18;
         External_Interrupt_4 at 0 range 17 .. 17;
         Reserved17 at 0 range 0 .. 16;
      end record;

   for Interrupt_Mask_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Mask_Register);

   Interrupt_Mask : Interrupt_Mask_Register;
   pragma Atomic (Interrupt_Mask);
   for Interrupt_Mask'Address use Interrupt_Mask_Register_Address;

   ---------------------
   -- Interrupt Force --
   ---------------------

   type Interrupt_Force_Register is
      record
         Reserved1 : Boolean;
         --  1  : interrupt X forced
         --  0  : interrupt X not forced r/w

         Masked_Hardware_Errors : Boolean;
         External_Interrupt_0 : Boolean;
         External_Interrupt_1 : Boolean;
         UART_A_Ready : Boolean;
         UART_B_Ready : Boolean;
         Correctable_Error_In_Memory : Boolean;
         UART_Error : Boolean;
         DMA_Access_Error : Boolean;
         DMA_Time_Out : Boolean;
         External_Interrupt_2 : Boolean;
         External_Interrupt_3 : Boolean;
         General_Purpose_Timer : Boolean;
         Real_Time_Clock : Boolean;
         External_Interrupt_4 : Boolean;
         Watch_Dog_Time_Out : Boolean;
         Reserved16 : Reserved_16;
      end record;

   for Interrupt_Force_Register use
      record
         Reserved1 at 0 range 31 .. 31;
         Masked_Hardware_Errors at 0 range 30 .. 30;
         External_Interrupt_0 at 0 range 29 .. 29;
         External_Interrupt_1 at 0 range 28 .. 28;
         UART_A_Ready at 0 range 27 .. 27;
         UART_B_Ready at 0 range 26 .. 26;
         Correctable_Error_In_Memory at 0 range 25 .. 25;
         UART_Error at 0 range 24 .. 24;
         DMA_Access_Error at 0 range 23 .. 23;
         DMA_Time_Out at 0 range 22 .. 22;
         External_Interrupt_2 at 0 range 21 .. 21;
         External_Interrupt_3 at 0 range 20 .. 20;
         General_Purpose_Timer at 0 range 19 .. 19;
         Real_Time_Clock at 0 range 18 .. 18;
         External_Interrupt_4 at 0 range 17 .. 17;
         Watch_Dog_Time_Out at 0 range 16 .. 16;
         Reserved16 at 0 range 0 .. 15;
      end record;

   for Interrupt_Force_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Force_Register);

   Interrupt_Force : Interrupt_Force_Register;
   pragma Atomic (Interrupt_Force);
   for Interrupt_Force'Address use Interrupt_Force_Register_Address;

   -----------------
   -- UART Status --
   -----------------

   type UART_Status_Register is
      record
         DRA : Boolean;
         --  Data Ready in channel A r

         TSEA : Boolean;
         --  Transmitter A Send register Empty (no data to send) r

         THEA : Boolean;
         --  Transmitter A Holding register Empty (ready to load data) r

         Reserved1A : Boolean;
         --  Not used r

         FEA : Boolean;
         --  Framing Error in receiver A r

         PEA : Boolean;
         --  Parity Error in receiver A r

         OEA : Boolean;
         --  Overrun Error in receiver A r

         CUA : Boolean;
         --  Clear UART A (bit read as zero) r/w

         Reserved8A : Reserved_8;
         --  Not used r

         DRB : Boolean;
         --  Data Ready in channel B r

         TSEB : Boolean;
         --  Transmitter B Send register Empty (no data to send) r

         THEB : Boolean;
         --  Transmitter B Holding register Empty (ready to load data) r

         Reserved1B : Boolean;
         --  Not used r

         FEB : Boolean;
         --  Framing Error in receiver B r

         PEB : Boolean;
         --  Parity Error in receiver B r

         OEB : Boolean;
         --  Overrun Error in receiver B r

         CUB : Boolean;
         --  Clear UART B (bit read as zero) r/w

         Reserved8B : Reserved_8;
         --  Not used r
      end record;

   for UART_Status_Register use
      record
         DRA at 0 range 31 .. 31;
         TSEA at 0 range 30 .. 30;
         THEA at 0 range 29 .. 29;
         Reserved1A at 0 range 28 .. 28;
         FEA at 0 range 27 .. 27;
         PEA at 0 range 26 .. 26;
         OEA at 0 range 25 .. 25;
         CUA at 0 range 24 .. 24;
         Reserved8A at 0 range 16 .. 23;
         DRB at 0 range 15 .. 15;
         TSEB at 0 range 14 .. 14;
         THEB at 0 range 13 .. 13;
         Reserved1B at 0 range 12 .. 12;
         FEB at 0 range 11 .. 11;
         PEB at 0 range 10 .. 10;
         OEB at 0 range 9 .. 9;
         CUB at 0 range 8 .. 8;
         Reserved8B at 0 range 0 .. 7;
      end record;

   for UART_Status_Register'Size use 32;

   pragma Suppress_Initialization (UART_Status_Register);

   UART_Status : UART_Status_Register;
   pragma Atomic (UART_Status);
   for UART_Status'Address use UART_Status_Register_Address;

   ------------------------
   -- UART Channel Rx Tx --
   ------------------------

   type UART_Channel_Rx_Tx_Register is
      record
         RTD : Character;
         --  Rx/Tx Data r/w

         Reserved24 : Reserved_24;
         --  Not used r
      end record;

   for UART_Channel_Rx_Tx_Register use
      record
         RTD        at 0 range 24 .. 31;
         Reserved24 at 0 range 0  .. 23;
      end record;

   for UART_Channel_Rx_Tx_Register'Size use 32;

   pragma Suppress_Initialization (UART_Channel_Rx_Tx_Register);

   UART_Channel_A : UART_Channel_Rx_Tx_Register;
   pragma Atomic (UART_Channel_A);
   for UART_Channel_A'Address use UART_Channel_A_Rx_Tx_Register_Address;

   UART_Channel_B : UART_Channel_Rx_Tx_Register;
   pragma Atomic (UART_Channel_B);
   for UART_Channel_B'Address use UART_Channel_B_Rx_Tx_Register_Address;

end System.BB.Board_Support.ERC32;
