--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from PolarFire.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  serial communication controller with a flexible serial data
--        interface
--
package Interfaces.Microsemi.CoreUARTapb is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype Tx_Data_Value_Field is Interfaces.Microsemi.Byte;

   --  Transmit Data register
   type Tx_Data_Register is record
      Value         : Tx_Data_Value_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.Microsemi.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Tx_Data_Register use record
      Value         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype Rx_Data_Value_Field is Interfaces.Microsemi.Byte;

   --  Receive Data register
   type Rx_Data_Register is record
      Value         : Rx_Data_Value_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.Microsemi.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Rx_Data_Register use record
      Value         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype Control_1_Baud_Value_Field is Interfaces.Microsemi.Byte;

   --  Control register 1
   type Control_1_Register is record
      Baud_Value    : Control_1_Baud_Value_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.Microsemi.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Control_1_Register use record
      Baud_Value    at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   type Control_2_Odd_N_Even_Field is
     (Even,
      Odd)
     with Size => 1;
   for Control_2_Odd_N_Even_Field use
     (Even => 0,
      Odd => 1);

   subtype Control_2_Baud_Value_Field is Interfaces.Microsemi.UInt5;

   --  Control register 1
   type Control_2_Register is record
      Bit8          : Boolean := False;
      Parity_En     : Boolean := False;
      Odd_N_Even    : Control_2_Odd_N_Even_Field :=
                       Interfaces.Microsemi.CoreUARTapb.Even;
      Baud_Value    : Control_2_Baud_Value_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : Interfaces.Microsemi.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Control_2_Register use record
      Bit8          at 0 range 0 .. 0;
      Parity_En     at 0 range 1 .. 1;
      Odd_N_Even    at 0 range 2 .. 2;
      Baud_Value    at 0 range 3 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Control register 1
   type Status_Register is record
      TX_Rdy        : Boolean := False;
      RX_Rdy        : Boolean := False;
      Parity_Err    : Boolean := False;
      Overflow      : Boolean := False;
      Framing_Err   : Boolean := False;
      --  unspecified
      Reserved_5_31 : Interfaces.Microsemi.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Status_Register use record
      TX_Rdy        at 0 range 0 .. 0;
      RX_Rdy        at 0 range 1 .. 1;
      Parity_Err    at 0 range 2 .. 2;
      Overflow      at 0 range 3 .. 3;
      Framing_Err   at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype Control_3_Baud_Val_Fraction_Field is Interfaces.Microsemi.UInt3;

   --  Control register 1
   type Control_3_Register is record
      Baud_Val_Fraction : Control_3_Baud_Val_Fraction_Field := 16#0#;
      --  unspecified
      Reserved_3_31     : Interfaces.Microsemi.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for Control_3_Register use record
      Baud_Val_Fraction at 0 range 0 .. 2;
      Reserved_3_31     at 0 range 3 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  serial communication controller with a flexible serial data interface
   type CoreUARTapb_Peripheral is record
      --  Transmit Data register
      Tx_Data   : aliased Tx_Data_Register;
      --  Receive Data register
      Rx_Data   : aliased Rx_Data_Register;
      --  Control register 1
      Control_1 : aliased Control_1_Register;
      --  Control register 1
      Control_2 : aliased Control_2_Register;
      --  Control register 1
      Status    : aliased Status_Register;
      --  Control register 1
      Control_3 : aliased Control_3_Register;
   end record
     with Volatile;

   for CoreUARTapb_Peripheral use record
      Tx_Data   at 16#0# range 0 .. 31;
      Rx_Data   at 16#4# range 0 .. 31;
      Control_1 at 16#8# range 0 .. 31;
      Control_2 at 16#C# range 0 .. 31;
      Status    at 16#10# range 0 .. 31;
      Control_3 at 16#14# range 0 .. 31;
   end record;

   --  serial communication controller with a flexible serial data interface
   CoreUARTapb_Periph : aliased CoreUARTapb_Peripheral
     with Import, Address => CoreUARTapb_Base;

end Interfaces.Microsemi.CoreUARTapb;
