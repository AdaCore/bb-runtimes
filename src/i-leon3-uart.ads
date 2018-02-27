------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  I N T E R F A C E S . L E O N 3 . U A R T               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with System.BB.Board_Parameters;

package Interfaces.Leon3.Uart is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   --------------------
   -- UART Registers --
   --------------------

   type Scaler_12 is mod 2 **  12;
   for Scaler_12'Size use  12;
   --  12-bit scaler

   type FIFO_Count is mod 64;
   for FIFO_Count'Size use 6;

   type Parity_Kind is (Even, Odd);

   type UART_Data_Register is record
      FIFO : Character;
      --  Reading and writing accesses receiver resp. transmitter FIFOs

      Reserved : Reserved_24;
      --  Not used r
   end record;

   for UART_Data_Register use record
      Reserved at 0 range Bit31 .. Bit08;
      FIFO     at 0 range Bit07 .. Bit00;
   end record;

   for UART_Data_Register'Size use 32;
   pragma Suppress_Initialization (UART_Data_Register);

   type UART_Status_Register is record
      Data_Ready                       : Boolean;
      Transmitter_Shift_Register_Empty : Boolean;
      Transmitter_FIFO_Empty           : Boolean;
      Break_Received                   : Boolean;
      Overrun                          : Boolean;
      Parity_Error                     : Boolean;
      Framing_Error                    : Boolean;
      Transmitter_FIFO_Half_Full       : Boolean;
      Receiver_FIFO_Half_Full          : Boolean;
      Transmitter_FIFO_Full            : Boolean;
      Receiver_FIFO_Full               : Boolean;
      Reserved                         : Reserved_9;
      Transmitter_FIFO_Count           : FIFO_Count;
      Receiver_FIFO_Count              : FIFO_Count;
   end record;

   for UART_Status_Register use record
      Receiver_FIFO_Count              at 0 range Bit31 .. Bit26;
      Transmitter_FIFO_Count           at 0 range Bit25 .. Bit20;
      Reserved                         at 0 range Bit19 .. Bit11;
      Receiver_FIFO_Full               at 0 range Bit10 .. Bit10;
      Transmitter_FIFO_Full            at 0 range Bit09 .. Bit09;
      Receiver_FIFO_Half_Full          at 0 range Bit08 .. Bit08;
      Transmitter_FIFO_Half_Full       at 0 range Bit07 .. Bit07;
      Framing_Error                    at 0 range Bit06 .. Bit06;
      Parity_Error                     at 0 range Bit05 .. Bit05;
      Overrun                          at 0 range Bit04 .. Bit04;
      Break_Received                   at 0 range Bit03 .. Bit03;
      Transmitter_FIFO_Empty           at 0 range Bit02 .. Bit02;
      Transmitter_Shift_Register_Empty at 0 range Bit01 .. Bit01;
      Data_Ready                       at 0 range Bit00 .. Bit00;
   end record;

   for UART_Status_Register'Size use 32;
   pragma Suppress_Initialization (UART_Status_Register);
   pragma Volatile_Full_Access (UART_Status_Register);

   type UART_Control_Register is record
      Receiver_Enable                   : Boolean;
      Transmitter_Enable                : Boolean; --  Transmitter enable
      Receiver_Interrupt_Enable         : Boolean;
      Transmitter_Interrupt_Enable      : Boolean;
      Parity_Select                     : Parity_Kind;
      Parity_Enable                     : Boolean;
      Reserved_1                        : Boolean;
      Loop_Back                         : Boolean;
      Reserved_2                        : Boolean;
      Receiver_FIFO_Interrupt_Enable    : Boolean;
      Transmitter_FIFO_Interrupt_Enable : Boolean;
      Reserved_3                        : Reserved_20;
      FIFO_Available                    : Boolean;
   end record;

   for UART_Control_Register use record
      FIFO_Available                    at 0 range Bit31 .. Bit31;
      Reserved_3                        at 0 range Bit30 .. Bit11;
      Receiver_FIFO_Interrupt_Enable    at 0 range Bit10 .. Bit10;
      Transmitter_FIFO_Interrupt_Enable at 0 range Bit09 .. Bit09;
      Reserved_2                        at 0 range Bit08 .. Bit08;
      Loop_Back                         at 0 range Bit07 .. Bit07;
      Reserved_1                        at 0 range Bit06 .. Bit06;
      Parity_Enable                     at 0 range Bit05 .. Bit05;
      Parity_Select                     at 0 range Bit04 .. Bit04;
      Transmitter_Interrupt_Enable      at 0 range Bit03 .. Bit03;
      Receiver_Interrupt_Enable         at 0 range Bit02 .. Bit02;
      Transmitter_Enable                at 0 range Bit01 .. Bit01;
      Receiver_Enable                   at 0 range Bit00 .. Bit00;
   end record;

   for UART_Control_Register'Size use 32;
   pragma Suppress_Initialization (UART_Control_Register);
   pragma Volatile_Full_Access (UART_Control_Register);

   type UART_Scaler_Register is record
      UART_Scaler : Scaler_12;
      --  1 - 4095  : Divide factor
      --  0  : stops the UART clock

      Reserved : Reserved_20;
   end record;

   for UART_Scaler_Register use record
      Reserved    at 0 range Bit31 .. Bit12;
      UART_Scaler at 0 range Bit11 .. Bit00;
   end record;

   for UART_Scaler_Register'Size use 32;
   pragma Suppress_Initialization (UART_Scaler_Register);
   pragma Volatile_Full_Access (UART_Scaler_Register);

   ----------
   -- UART --
   ----------

   UART_Base : constant := System.BB.Board_Parameters.UART_Base;

   UART_Data : UART_Data_Register;
   for UART_Data'Address use System'To_Address (UART_Base + 16#00#);

   UART_Status : UART_Status_Register;
   for UART_Status'Address use System'To_Address (UART_Base + 16#04#);

   UART_Control : UART_Control_Register;
   for UART_Control'Address use System'To_Address (UART_Base + 16#08#);

   UART_Scaler : UART_Scaler_Register;
   for UART_Scaler'Address use System'To_Address (UART_Base + 16#0C#);

end Interfaces.Leon3.Uart;
