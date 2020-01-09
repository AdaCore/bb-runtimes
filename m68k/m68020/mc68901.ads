------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                              M C 6 8 9 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2019-2020, Free Software Foundation, Inc.          --
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

with System;

package MC68901 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ------------------------------
   -- MC68901 Memory Addresses --
   ------------------------------

   MC68901_Base_Address : constant := 16#FFF8_0000#;
   GPDR_Offset_Address  : constant := 16#01#;
   AER_Offset_Address   : constant := 16#03#;
   DDR_Offset_Address   : constant := 16#05#;
   IERA_Offset_Address  : constant := 16#07#;
   IERB_Offset_Address  : constant := 16#09#;
   IPRA_Offset_Address  : constant := 16#0B#;
   IPRB_Offset_Address  : constant := 16#0D#;
   ISRA_Offset_Address  : constant := 16#0F#;
   ISRB_Offset_Address  : constant := 16#11#;
   IMRA_Offset_Address  : constant := 16#13#;
   IMRB_Offset_Address  : constant := 16#15#;
   VR_Offset_Address    : constant := 16#17#;
   TACR_Offset_Address  : constant := 16#19#;
   TBCR_Offset_Address  : constant := 16#1B#;
   TCDCR_Offset_Address : constant := 16#1D#;
   TADR_Offset_Address  : constant := 16#1F#;
   TBDR_Offset_Address  : constant := 16#21#;
   TCDR_Offset_Address  : constant := 16#23#;
   TDCR_Offset_Address  : constant := 16#25#;
   SCR_Offset_Address   : constant := 16#27#;
   UCR_Offset_Address   : constant := 16#29#;
   RSR_Offset_Address   : constant := 16#2B#;
   TSR_Offset_Address   : constant := 16#2D#;
   UDR_Offset_Address   : constant := 16#2F#;

   -----------------------
   -- Hardware Features --
   -----------------------

   Register_Size : constant := 8;

   type Clock_Modes is (Normal, Fractional);
   type Character_Lengths is (Bits_8, Bits_7, Bits_6, Bits_5);
   type Format_Controls is
     (Sychronous_No_Start_No_Stop_Bits,
      Asynchronous_Start_Stop_Bits,
      Asynchronous_Start_1_5_Stop_Bits,
      Asynchronous_Start_2_Stop_Bits);

   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);
   --  Enables or disables the desired operation

   type Parity_Selection is (Odd_Parity, Even_Parity);
   for Parity_Selection use (Odd_Parity => 0, Even_Parity => 1);

   type Output_State is (High_Impedance, Low, High, Loopback_Mode);

   type Timer_Prescaler is (Stopped, Prescaler_4, Prescaler_10, Prescaler_16,
                            Prescaler_50, Prescaler_64, Prescaler_100,
                            Prescaler_200);
   for Timer_Prescaler use (Stopped       => 0,
                            Prescaler_4   => 1,
                            Prescaler_10  => 2,
                            Prescaler_16  => 3,
                            Prescaler_50  => 4,
                            Prescaler_64  => 5,
                            Prescaler_100 => 6,
                            Prescaler_200 => 7);

   type Timer_Data is mod 2**8 with Size => 8;

   --------------------
   -- Register Types --
   --------------------

   type USART_Control is record
      Clock_Mode       : Clock_Modes;
      Character_Length : Character_Lengths;
      Format_Control   : Format_Controls;
      Parity           : Enable_Type;
      Parity_Kind      : Parity_Selection;
   end record with Size => Register_Size;

   type Receiver_Status is record
      Buffer_Full                  : Boolean;
      Overrun_Error                : Boolean;
      Parity_Error                 : Boolean;
      Frame_Error                  : Boolean;
      Found_Search_Or_Break_Detect : Boolean;
      Match_Char_In_Progress       : Boolean;
      Synchronous_Strip            : Enable_Type;
      Receiver                     : Enable_Type;
   end record with Size => Register_Size;

   type Transmitter_Status is record
      Buffer_Empty        : Boolean;
      Underrun_Error      : Boolean;
      Auto_Turnaround     : Enable_Type;
      End_Of_Transmission : Boolean;
      Break               : Enable_Type;
      Hi_And_Low          : Output_State;
      Transmitter         : Enable_Type;
   end record with Size => Register_Size;

   type TimerAB_Control is record
      Reset_Output   : Boolean;
      Operation_Mode : Boolean;
      Prescaler      : Timer_Prescaler;
   end record with Size => Register_Size;

   type TimerAB_Data is record
      Data : Timer_Data;
   end record with Size => Register_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for USART_Control use record
      Clock_Mode       at 0 range 0 .. 0;
      Character_Length at 0 range 1 .. 2;
      Format_Control   at 0 range 3 .. 4;
      Parity           at 0 range 5 .. 5;
      Parity_Kind      at 0 range 6 .. 6;
   end record;

   for Receiver_Status use record
      Buffer_Full                  at 0 range 0 .. 0;
      Overrun_Error                at 0 range 1 .. 1;
      Parity_Error                 at 0 range 2 .. 2;
      Frame_Error                  at 0 range 3 .. 3;
      Found_Search_Or_Break_Detect at 0 range 4 .. 4;
      Match_Char_In_Progress       at 0 range 5 .. 5;
      Synchronous_Strip            at 0 range 6 .. 6;
      Receiver                     at 0 range 7 .. 7;
   end record;

   for Transmitter_Status use record
      Buffer_Empty        at 0 range 0 .. 0;
      Underrun_Error      at 0 range 1 .. 1;
      Auto_Turnaround     at 0 range 2 .. 2;
      End_Of_Transmission at 0 range 3 .. 3;
      Break               at 0 range 4 .. 4;
      Hi_And_Low          at 0 range 5 .. 6;
      Transmitter         at 0 range 7 .. 7;
   end record;

   for TimerAB_Control use record
      Reset_Output   at 0 range 3 .. 3;
      Operation_Mode at 0 range 4 .. 4;
      Prescaler      at 0 range 5 .. 7;
   end record;

   for TimerAB_Data use record
      Data at 0 range 0 .. 7;
   end record;

   --------------------
   -- GPIO Registers --
   --------------------

   USART_Control_Register : USART_Control
     with Volatile, Address =>
       System'To_Address (MC68901_Base_Address + UCR_Offset_Address);

   Receiver_Status_Register : Receiver_Status
     with Volatile, Address =>
       System'To_Address (MC68901_Base_Address + RSR_Offset_Address);

   Transmitter_Status_Register : Transmitter_Status
     with Volatile, Address =>
       System'To_Address (MC68901_Base_Address + TSR_Offset_Address);

   USART_Data_Register : Character
     with Volatile, Size => Register_Size,
          Address =>
            System'To_Address (MC68901_Base_Address + UDR_Offset_Address);

   TimerB_Control_Register : TimerAB_Control
     with Volatile, Address =>
       System'To_Address (MC68901_Base_Address + TBCR_Offset_Address);

   TimerB_Data_Register : TimerAB_Data
     with Volatile, Address =>
       System'To_Address (MC68901_Base_Address + TBDR_Offset_Address);

end MC68901;
