------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with System.BB.Board_Parameters;

package body System.Text_IO is

   -----------
   -- Local --
   -----------

   type Scaler_12 is mod 2 **  12;
   for Scaler_12'Size use  12;

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_23 is array (0 .. 22) of Boolean;
   for Reserved_23'Size use 23;
   pragma Pack (Reserved_23);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Reserved_25 is array (0 .. 24) of Boolean;
   for Reserved_25'Size use 25;
   pragma Pack (Reserved_25);

   UART_1_Data_Register_Address :
     constant System.Address := 16#80000070#;

   UART_1_Status_Register_Address :
     constant System.Address := 16#80000074#;

   UART_1_Control_Register_Address :
     constant System.Address := 16#80000078#;

   UART_1_Scaler_Register_Address :
     constant System.Address := 16#8000007C#;

   type UART_Data_Register is
      record
         RTD      : Character;
         Reserved : Reserved_24;
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
         DR       : Boolean;
         TS       : Boolean;
         TH       : Boolean;
         BR       : Boolean;
         OV       : Boolean;
         PE       : Boolean;
         FE       : Boolean;
         Reserved : Reserved_25;
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
         RE       : Boolean;
         TE       : Boolean;
         RI       : Boolean;
         TI       : Boolean;
         PS       : Boolean;
         PE       : Boolean;
         FL       : Boolean;
         LB       : Boolean;
         EC       : Boolean;
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
         Reserved    : Reserved_20;
      end record;

   for UART_Scaler_Register use
      record
         UART_Scaler at 0 range 20 .. 31;
         Reserved    at 0 range  0 .. 19;
      end record;

   for UART_Scaler_Register'Size use 32;

   pragma Suppress_Initialization (UART_Scaler_Register);

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

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Will never be called

      raise Program_Error;
      return ASCII.NUL;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Control_Aux : UART_Control_Register;
      Scaler_Aux  : UART_Scaler_Register;

   begin
      Control_Aux := UART_1_Control;

      Scaler_Aux :=
        (UART_Scaler =>
           Scaler_12 ((System.BB.Board_Parameters.Clock_Frequency * 10
                       / (115200 * 8) - 5) / 10),
         Reserved => (others => False));

      Control_Aux.RE := True;
      Control_Aux.TE := True;
      Control_Aux.RI := False;
      Control_Aux.PE := False;
      Control_Aux.PS := False;
      Control_Aux.FL := False;
      Control_Aux.LB := False;
      Control_Aux.EC := False;
      Control_Aux.Reserved := (others => False);

      UART_1_Control := Control_Aux;
      UART_1_Scaler  := Scaler_Aux;

      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return False;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      UART_Status_Aux : constant UART_Status_Register := UART_1_Status;
   begin
      return UART_Status_Aux.TH;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
      UART_Tx : constant UART_Data_Register :=
                  (RTD => C, Reserved => (others => False));
   begin
      UART_1_Data := UART_Tx;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;

end System.Text_IO;
