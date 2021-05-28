--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

--  Single-cycle IO block\n
--          Provides core-local and inter-core hardware for the two processors,
--  with single-cycle access.
package Interfaces.RP2040.SIO is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype GPIO_IN_GPIO_IN_Field is Interfaces.RP2040.UInt30;

   --  Input value for GPIO pins
   type GPIO_IN_Register is record
      --  Read-only. Input value for GPIO0...29
      GPIO_IN        : GPIO_IN_GPIO_IN_Field;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_IN_Register use record
      GPIO_IN        at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_HI_IN_GPIO_HI_IN_Field is Interfaces.RP2040.UInt6;

   --  Input value for QSPI pins
   type GPIO_HI_IN_Register is record
      --  Read-only. Input value on QSPI IO in order 0..5: SCLK, SSn, SD0, SD1,
      --  SD2, SD3
      GPIO_HI_IN    : GPIO_HI_IN_GPIO_HI_IN_Field;
      --  unspecified
      Reserved_6_31 : Interfaces.RP2040.UInt26;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_IN_Register use record
      GPIO_HI_IN    at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype GPIO_OUT_GPIO_OUT_Field is Interfaces.RP2040.UInt30;

   --  GPIO output value
   type GPIO_OUT_Register is record
      --  Set output level (1/0 -> high/low) for GPIO0...29.\n Reading back
      --  gives the last value written, NOT the input value from the pins.\n If
      --  core 0 and core 1 both write to GPIO_OUT simultaneously (or to a
      --  SET/CLR/XOR alias),\n the result is as though the write from core 0
      --  took place first,\n and the write from core 1 was then applied to
      --  that intermediate result.
      GPIO_OUT       : GPIO_OUT_GPIO_OUT_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OUT_Register use record
      GPIO_OUT       at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OUT_SET_GPIO_OUT_SET_Field is Interfaces.RP2040.UInt30;

   --  GPIO output value set
   type GPIO_OUT_SET_Register is record
      --  Perform an atomic bit-set on GPIO_OUT, i.e. `GPIO_OUT |= wdata`
      GPIO_OUT_SET   : GPIO_OUT_SET_GPIO_OUT_SET_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OUT_SET_Register use record
      GPIO_OUT_SET   at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OUT_CLR_GPIO_OUT_CLR_Field is Interfaces.RP2040.UInt30;

   --  GPIO output value clear
   type GPIO_OUT_CLR_Register is record
      --  Perform an atomic bit-clear on GPIO_OUT, i.e. `GPIO_OUT &= ~wdata`
      GPIO_OUT_CLR   : GPIO_OUT_CLR_GPIO_OUT_CLR_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OUT_CLR_Register use record
      GPIO_OUT_CLR   at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OUT_XOR_GPIO_OUT_XOR_Field is Interfaces.RP2040.UInt30;

   --  GPIO output value XOR
   type GPIO_OUT_XOR_Register is record
      --  Perform an atomic bitwise XOR on GPIO_OUT, i.e. `GPIO_OUT ^= wdata`
      GPIO_OUT_XOR   : GPIO_OUT_XOR_GPIO_OUT_XOR_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OUT_XOR_Register use record
      GPIO_OUT_XOR   at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OE_GPIO_OE_Field is Interfaces.RP2040.UInt30;

   --  GPIO output enable
   type GPIO_OE_Register is record
      --  Set output enable (1/0 -> output/input) for GPIO0...29.\n Reading
      --  back gives the last value written.\n If core 0 and core 1 both write
      --  to GPIO_OE simultaneously (or to a SET/CLR/XOR alias),\n the result
      --  is as though the write from core 0 took place first,\n and the write
      --  from core 1 was then applied to that intermediate result.
      GPIO_OE        : GPIO_OE_GPIO_OE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OE_Register use record
      GPIO_OE        at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OE_SET_GPIO_OE_SET_Field is Interfaces.RP2040.UInt30;

   --  GPIO output enable set
   type GPIO_OE_SET_Register is record
      --  Perform an atomic bit-set on GPIO_OE, i.e. `GPIO_OE |= wdata`
      GPIO_OE_SET    : GPIO_OE_SET_GPIO_OE_SET_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OE_SET_Register use record
      GPIO_OE_SET    at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OE_CLR_GPIO_OE_CLR_Field is Interfaces.RP2040.UInt30;

   --  GPIO output enable clear
   type GPIO_OE_CLR_Register is record
      --  Perform an atomic bit-clear on GPIO_OE, i.e. `GPIO_OE &= ~wdata`
      GPIO_OE_CLR    : GPIO_OE_CLR_GPIO_OE_CLR_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OE_CLR_Register use record
      GPIO_OE_CLR    at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_OE_XOR_GPIO_OE_XOR_Field is Interfaces.RP2040.UInt30;

   --  GPIO output enable XOR
   type GPIO_OE_XOR_Register is record
      --  Perform an atomic bitwise XOR on GPIO_OE, i.e. `GPIO_OE ^= wdata`
      GPIO_OE_XOR    : GPIO_OE_XOR_GPIO_OE_XOR_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : Interfaces.RP2040.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_OE_XOR_Register use record
      GPIO_OE_XOR    at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype GPIO_HI_OUT_GPIO_HI_OUT_Field is Interfaces.RP2040.UInt6;

   --  QSPI output value
   type GPIO_HI_OUT_Register is record
      --  Set output level (1/0 -> high/low) for QSPI IO0...5.\n Reading back
      --  gives the last value written, NOT the input value from the pins.\n If
      --  core 0 and core 1 both write to GPIO_HI_OUT simultaneously (or to a
      --  SET/CLR/XOR alias),\n the result is as though the write from core 0
      --  took place first,\n and the write from core 1 was then applied to
      --  that intermediate result.
      GPIO_HI_OUT   : GPIO_HI_OUT_GPIO_HI_OUT_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OUT_Register use record
      GPIO_HI_OUT   at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OUT_SET_GPIO_HI_OUT_SET_Field is Interfaces.RP2040.UInt6;

   --  QSPI output value set
   type GPIO_HI_OUT_SET_Register is record
      --  Perform an atomic bit-set on GPIO_HI_OUT, i.e. `GPIO_HI_OUT |= wdata`
      GPIO_HI_OUT_SET : GPIO_HI_OUT_SET_GPIO_HI_OUT_SET_Field := 16#0#;
      --  unspecified
      Reserved_6_31   : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OUT_SET_Register use record
      GPIO_HI_OUT_SET at 0 range 0 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OUT_CLR_GPIO_HI_OUT_CLR_Field is Interfaces.RP2040.UInt6;

   --  QSPI output value clear
   type GPIO_HI_OUT_CLR_Register is record
      --  Perform an atomic bit-clear on GPIO_HI_OUT, i.e. `GPIO_HI_OUT &=
      --  ~wdata`
      GPIO_HI_OUT_CLR : GPIO_HI_OUT_CLR_GPIO_HI_OUT_CLR_Field := 16#0#;
      --  unspecified
      Reserved_6_31   : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OUT_CLR_Register use record
      GPIO_HI_OUT_CLR at 0 range 0 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OUT_XOR_GPIO_HI_OUT_XOR_Field is Interfaces.RP2040.UInt6;

   --  QSPI output value XOR
   type GPIO_HI_OUT_XOR_Register is record
      --  Perform an atomic bitwise XOR on GPIO_HI_OUT, i.e. `GPIO_HI_OUT ^=
      --  wdata`
      GPIO_HI_OUT_XOR : GPIO_HI_OUT_XOR_GPIO_HI_OUT_XOR_Field := 16#0#;
      --  unspecified
      Reserved_6_31   : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OUT_XOR_Register use record
      GPIO_HI_OUT_XOR at 0 range 0 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OE_GPIO_HI_OE_Field is Interfaces.RP2040.UInt6;

   --  QSPI output enable
   type GPIO_HI_OE_Register is record
      --  Set output enable (1/0 -> output/input) for QSPI IO0...5.\n Reading
      --  back gives the last value written.\n If core 0 and core 1 both write
      --  to GPIO_HI_OE simultaneously (or to a SET/CLR/XOR alias),\n the
      --  result is as though the write from core 0 took place first,\n and the
      --  write from core 1 was then applied to that intermediate result.
      GPIO_HI_OE    : GPIO_HI_OE_GPIO_HI_OE_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OE_Register use record
      GPIO_HI_OE    at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OE_SET_GPIO_HI_OE_SET_Field is Interfaces.RP2040.UInt6;

   --  QSPI output enable set
   type GPIO_HI_OE_SET_Register is record
      --  Perform an atomic bit-set on GPIO_HI_OE, i.e. `GPIO_HI_OE |= wdata`
      GPIO_HI_OE_SET : GPIO_HI_OE_SET_GPIO_HI_OE_SET_Field := 16#0#;
      --  unspecified
      Reserved_6_31  : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OE_SET_Register use record
      GPIO_HI_OE_SET at 0 range 0 .. 5;
      Reserved_6_31  at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OE_CLR_GPIO_HI_OE_CLR_Field is Interfaces.RP2040.UInt6;

   --  QSPI output enable clear
   type GPIO_HI_OE_CLR_Register is record
      --  Perform an atomic bit-clear on GPIO_HI_OE, i.e. `GPIO_HI_OE &=
      --  ~wdata`
      GPIO_HI_OE_CLR : GPIO_HI_OE_CLR_GPIO_HI_OE_CLR_Field := 16#0#;
      --  unspecified
      Reserved_6_31  : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OE_CLR_Register use record
      GPIO_HI_OE_CLR at 0 range 0 .. 5;
      Reserved_6_31  at 0 range 6 .. 31;
   end record;

   subtype GPIO_HI_OE_XOR_GPIO_HI_OE_XOR_Field is Interfaces.RP2040.UInt6;

   --  QSPI output enable XOR
   type GPIO_HI_OE_XOR_Register is record
      --  Perform an atomic bitwise XOR on GPIO_HI_OE, i.e. `GPIO_HI_OE ^=
      --  wdata`
      GPIO_HI_OE_XOR : GPIO_HI_OE_XOR_GPIO_HI_OE_XOR_Field := 16#0#;
      --  unspecified
      Reserved_6_31  : Interfaces.RP2040.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIO_HI_OE_XOR_Register use record
      GPIO_HI_OE_XOR at 0 range 0 .. 5;
      Reserved_6_31  at 0 range 6 .. 31;
   end record;

   subtype FIFO_ST_VLD_Field is Interfaces.RP2040.Bit;
   subtype FIFO_ST_RDY_Field is Interfaces.RP2040.Bit;
   subtype FIFO_ST_WOF_Field is Interfaces.RP2040.Bit;
   subtype FIFO_ST_ROE_Field is Interfaces.RP2040.Bit;

   --  Status register for inter-core FIFOs (mailboxes).\n There is one FIFO in
   --  the core 0 -> core 1 direction, and one core 1 -> core 0. Both are 32
   --  bits wide and 8 words deep.\n Core 0 can see the read side of the 1->0
   --  FIFO (RX), and the write side of 0->1 FIFO (TX).\n Core 1 can see the
   --  read side of the 0->1 FIFO (RX), and the write side of 1->0 FIFO (TX).\n
   --  The SIO IRQ for each core is the logical OR of the VLD, WOF and ROE
   --  fields of its FIFO_ST register.
   type FIFO_ST_Register is record
      --  Read-only. Value is 1 if this core's RX FIFO is not empty (i.e. if
      --  FIFO_RD is valid)
      VLD           : FIFO_ST_VLD_Field := 16#0#;
      --  Read-only. Value is 1 if this core's TX FIFO is not full (i.e. if
      --  FIFO_WR is ready for more data)
      RDY           : FIFO_ST_RDY_Field := 16#1#;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Sticky flag indicating the TX FIFO was written when
      --  full. This write was ignored by the FIFO.
      WOF           : FIFO_ST_WOF_Field := 16#0#;
      --  Write data bit of one shall clear (set to zero) the corresponding bit
      --  in the field. Sticky flag indicating the RX FIFO was read when empty.
      --  This read was ignored by the FIFO.
      ROE           : FIFO_ST_ROE_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : Interfaces.RP2040.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFO_ST_Register use record
      VLD           at 0 range 0 .. 0;
      RDY           at 0 range 1 .. 1;
      WOF           at 0 range 2 .. 2;
      ROE           at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype DIV_CSR_READY_Field is Interfaces.RP2040.Bit;
   subtype DIV_CSR_DIRTY_Field is Interfaces.RP2040.Bit;

   --  Control and status register for divider.
   type DIV_CSR_Register is record
      --  Read-only. Reads as 0 when a calculation is in progress, 1
      --  otherwise.\n Writing an operand (xDIVIDEND, xDIVISOR) will
      --  immediately start a new calculation, no\n matter if one is already in
      --  progress.\n Writing to a result register will immediately terminate
      --  any in-progress calculation\n and set the READY and DIRTY flags.
      READY         : DIV_CSR_READY_Field;
      --  Read-only. Changes to 1 when any register is written, and back to 0
      --  when QUOTIENT is read.\n Software can use this flag to make
      --  save/restore more efficient (skip if not DIRTY).\n If the flag is
      --  used in this way, it's recommended to either read QUOTIENT only,\n or
      --  REMAINDER and then QUOTIENT, to prevent data loss on context switch.
      DIRTY         : DIV_CSR_DIRTY_Field;
      --  unspecified
      Reserved_2_31 : Interfaces.RP2040.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIV_CSR_Register use record
      READY         at 0 range 0 .. 0;
      DIRTY         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype INTERP0_CTRL_LANE0_SHIFT_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE0_MASK_LSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE0_MASK_MSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE0_SIGNED_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE0_CROSS_INPUT_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE0_CROSS_RESULT_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE0_ADD_RAW_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE0_FORCE_MSB_Field is Interfaces.RP2040.UInt2;
   subtype INTERP0_CTRL_LANE0_BLEND_Field is Interfaces.RP2040.Bit;
   --  INTERP0_CTRL_LANE0_OVERF array element
   subtype INTERP0_CTRL_LANE0_OVERF_Element is Interfaces.RP2040.Bit;

   --  INTERP0_CTRL_LANE0_OVERF array
   type INTERP0_CTRL_LANE0_OVERF_Field_Array is array (0 .. 2)
     of INTERP0_CTRL_LANE0_OVERF_Element
     with Component_Size => 1, Size => 3;

   --  Type definition for INTERP0_CTRL_LANE0_OVERF
   type INTERP0_CTRL_LANE0_OVERF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OVERF as a value
            Val : Interfaces.RP2040.UInt3;
         when True =>
            --  OVERF as an array
            Arr : INTERP0_CTRL_LANE0_OVERF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for INTERP0_CTRL_LANE0_OVERF_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Control register for lane 0
   type INTERP0_CTRL_LANE0_Register is record
      --  Logical right-shift applied to accumulator before masking
      SHIFT          : INTERP0_CTRL_LANE0_SHIFT_Field := 16#0#;
      --  The least-significant bit allowed to pass by the mask (inclusive)
      MASK_LSB       : INTERP0_CTRL_LANE0_MASK_LSB_Field := 16#0#;
      --  The most-significant bit allowed to pass by the mask (inclusive)\n
      --  Setting MSB < LSB may cause chip to turn inside-out
      MASK_MSB       : INTERP0_CTRL_LANE0_MASK_MSB_Field := 16#0#;
      --  If SIGNED is set, the shifted and masked accumulator value is
      --  sign-extended to 32 bits\n before adding to BASE0, and LANE0 PEEK/POP
      --  appear extended to 32 bits when read by processor.
      SIGNED         : INTERP0_CTRL_LANE0_SIGNED_Field := 16#0#;
      --  If 1, feed the opposite lane's accumulator into this lane's shift +
      --  mask hardware.\n Takes effect even if ADD_RAW is set (the CROSS_INPUT
      --  mux is before the shift+mask bypass)
      CROSS_INPUT    : INTERP0_CTRL_LANE0_CROSS_INPUT_Field := 16#0#;
      --  If 1, feed the opposite lane's result into this lane's accumulator on
      --  POP.
      CROSS_RESULT   : INTERP0_CTRL_LANE0_CROSS_RESULT_Field := 16#0#;
      --  If 1, mask + shift is bypassed for LANE0 result. This does not affect
      --  FULL result.
      ADD_RAW        : INTERP0_CTRL_LANE0_ADD_RAW_Field := 16#0#;
      --  ORed into bits 29:28 of the lane result presented to the processor on
      --  the bus.\n No effect on the internal 32-bit datapath. Handy for using
      --  a lane to generate sequence\n of pointers into flash or SRAM.
      FORCE_MSB      : INTERP0_CTRL_LANE0_FORCE_MSB_Field := 16#0#;
      --  Only present on INTERP0 on each core. If BLEND mode is enabled:\n -
      --  LANE1 result is a linear interpolation between BASE0 and BASE1,
      --  controlled\n by the 8 LSBs of lane 1 shift and mask value (a
      --  fractional number between\n 0 and 255/256ths)\n - LANE0 result does
      --  not have BASE0 added (yields only the 8 LSBs of lane 1 shift+mask
      --  value)\n - FULL result does not have lane 1 shift+mask value added
      --  (BASE2 + lane 0 shift+mask)\n LANE1 SIGNED flag controls whether the
      --  interpolation is signed or unsigned.
      BLEND          : INTERP0_CTRL_LANE0_BLEND_Field := 16#0#;
      --  unspecified
      Reserved_22_22 : Interfaces.RP2040.Bit := 16#0#;
      --  Read-only. Indicates if any masked-off MSBs in ACCUM0 are set.
      OVERF          : INTERP0_CTRL_LANE0_OVERF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_26_31 : Interfaces.RP2040.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP0_CTRL_LANE0_Register use record
      SHIFT          at 0 range 0 .. 4;
      MASK_LSB       at 0 range 5 .. 9;
      MASK_MSB       at 0 range 10 .. 14;
      SIGNED         at 0 range 15 .. 15;
      CROSS_INPUT    at 0 range 16 .. 16;
      CROSS_RESULT   at 0 range 17 .. 17;
      ADD_RAW        at 0 range 18 .. 18;
      FORCE_MSB      at 0 range 19 .. 20;
      BLEND          at 0 range 21 .. 21;
      Reserved_22_22 at 0 range 22 .. 22;
      OVERF          at 0 range 23 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype INTERP0_CTRL_LANE1_SHIFT_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE1_MASK_LSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE1_MASK_MSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP0_CTRL_LANE1_SIGNED_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE1_CROSS_INPUT_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE1_CROSS_RESULT_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE1_ADD_RAW_Field is Interfaces.RP2040.Bit;
   subtype INTERP0_CTRL_LANE1_FORCE_MSB_Field is Interfaces.RP2040.UInt2;

   --  Control register for lane 1
   type INTERP0_CTRL_LANE1_Register is record
      --  Logical right-shift applied to accumulator before masking
      SHIFT          : INTERP0_CTRL_LANE1_SHIFT_Field := 16#0#;
      --  The least-significant bit allowed to pass by the mask (inclusive)
      MASK_LSB       : INTERP0_CTRL_LANE1_MASK_LSB_Field := 16#0#;
      --  The most-significant bit allowed to pass by the mask (inclusive)\n
      --  Setting MSB < LSB may cause chip to turn inside-out
      MASK_MSB       : INTERP0_CTRL_LANE1_MASK_MSB_Field := 16#0#;
      --  If SIGNED is set, the shifted and masked accumulator value is
      --  sign-extended to 32 bits\n before adding to BASE1, and LANE1 PEEK/POP
      --  appear extended to 32 bits when read by processor.
      SIGNED         : INTERP0_CTRL_LANE1_SIGNED_Field := 16#0#;
      --  If 1, feed the opposite lane's accumulator into this lane's shift +
      --  mask hardware.\n Takes effect even if ADD_RAW is set (the CROSS_INPUT
      --  mux is before the shift+mask bypass)
      CROSS_INPUT    : INTERP0_CTRL_LANE1_CROSS_INPUT_Field := 16#0#;
      --  If 1, feed the opposite lane's result into this lane's accumulator on
      --  POP.
      CROSS_RESULT   : INTERP0_CTRL_LANE1_CROSS_RESULT_Field := 16#0#;
      --  If 1, mask + shift is bypassed for LANE1 result. This does not affect
      --  FULL result.
      ADD_RAW        : INTERP0_CTRL_LANE1_ADD_RAW_Field := 16#0#;
      --  ORed into bits 29:28 of the lane result presented to the processor on
      --  the bus.\n No effect on the internal 32-bit datapath. Handy for using
      --  a lane to generate sequence\n of pointers into flash or SRAM.
      FORCE_MSB      : INTERP0_CTRL_LANE1_FORCE_MSB_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP0_CTRL_LANE1_Register use record
      SHIFT          at 0 range 0 .. 4;
      MASK_LSB       at 0 range 5 .. 9;
      MASK_MSB       at 0 range 10 .. 14;
      SIGNED         at 0 range 15 .. 15;
      CROSS_INPUT    at 0 range 16 .. 16;
      CROSS_RESULT   at 0 range 17 .. 17;
      ADD_RAW        at 0 range 18 .. 18;
      FORCE_MSB      at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype INTERP0_ACCUM0_ADD_INTERP0_ACCUM0_ADD_Field is
     Interfaces.RP2040.UInt24;

   --  Values written here are atomically added to ACCUM0\n Reading yields lane
   --  0's raw shift and mask value (BASE0 not added).
   type INTERP0_ACCUM0_ADD_Register is record
      INTERP0_ACCUM0_ADD : INTERP0_ACCUM0_ADD_INTERP0_ACCUM0_ADD_Field :=
                            16#0#;
      --  unspecified
      Reserved_24_31     : Interfaces.RP2040.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP0_ACCUM0_ADD_Register use record
      INTERP0_ACCUM0_ADD at 0 range 0 .. 23;
      Reserved_24_31     at 0 range 24 .. 31;
   end record;

   subtype INTERP0_ACCUM1_ADD_INTERP0_ACCUM1_ADD_Field is
     Interfaces.RP2040.UInt24;

   --  Values written here are atomically added to ACCUM1\n Reading yields lane
   --  1's raw shift and mask value (BASE1 not added).
   type INTERP0_ACCUM1_ADD_Register is record
      INTERP0_ACCUM1_ADD : INTERP0_ACCUM1_ADD_INTERP0_ACCUM1_ADD_Field :=
                            16#0#;
      --  unspecified
      Reserved_24_31     : Interfaces.RP2040.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP0_ACCUM1_ADD_Register use record
      INTERP0_ACCUM1_ADD at 0 range 0 .. 23;
      Reserved_24_31     at 0 range 24 .. 31;
   end record;

   subtype INTERP1_CTRL_LANE0_SHIFT_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE0_MASK_LSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE0_MASK_MSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE0_SIGNED_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE0_CROSS_INPUT_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE0_CROSS_RESULT_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE0_ADD_RAW_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE0_FORCE_MSB_Field is Interfaces.RP2040.UInt2;
   subtype INTERP1_CTRL_LANE0_CLAMP_Field is Interfaces.RP2040.Bit;
   --  INTERP1_CTRL_LANE0_OVERF array element
   subtype INTERP1_CTRL_LANE0_OVERF_Element is Interfaces.RP2040.Bit;

   --  INTERP1_CTRL_LANE0_OVERF array
   type INTERP1_CTRL_LANE0_OVERF_Field_Array is array (0 .. 2)
     of INTERP1_CTRL_LANE0_OVERF_Element
     with Component_Size => 1, Size => 3;

   --  Type definition for INTERP1_CTRL_LANE0_OVERF
   type INTERP1_CTRL_LANE0_OVERF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OVERF as a value
            Val : Interfaces.RP2040.UInt3;
         when True =>
            --  OVERF as an array
            Arr : INTERP1_CTRL_LANE0_OVERF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for INTERP1_CTRL_LANE0_OVERF_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Control register for lane 0
   type INTERP1_CTRL_LANE0_Register is record
      --  Logical right-shift applied to accumulator before masking
      SHIFT          : INTERP1_CTRL_LANE0_SHIFT_Field := 16#0#;
      --  The least-significant bit allowed to pass by the mask (inclusive)
      MASK_LSB       : INTERP1_CTRL_LANE0_MASK_LSB_Field := 16#0#;
      --  The most-significant bit allowed to pass by the mask (inclusive)\n
      --  Setting MSB < LSB may cause chip to turn inside-out
      MASK_MSB       : INTERP1_CTRL_LANE0_MASK_MSB_Field := 16#0#;
      --  If SIGNED is set, the shifted and masked accumulator value is
      --  sign-extended to 32 bits\n before adding to BASE0, and LANE0 PEEK/POP
      --  appear extended to 32 bits when read by processor.
      SIGNED         : INTERP1_CTRL_LANE0_SIGNED_Field := 16#0#;
      --  If 1, feed the opposite lane's accumulator into this lane's shift +
      --  mask hardware.\n Takes effect even if ADD_RAW is set (the CROSS_INPUT
      --  mux is before the shift+mask bypass)
      CROSS_INPUT    : INTERP1_CTRL_LANE0_CROSS_INPUT_Field := 16#0#;
      --  If 1, feed the opposite lane's result into this lane's accumulator on
      --  POP.
      CROSS_RESULT   : INTERP1_CTRL_LANE0_CROSS_RESULT_Field := 16#0#;
      --  If 1, mask + shift is bypassed for LANE0 result. This does not affect
      --  FULL result.
      ADD_RAW        : INTERP1_CTRL_LANE0_ADD_RAW_Field := 16#0#;
      --  ORed into bits 29:28 of the lane result presented to the processor on
      --  the bus.\n No effect on the internal 32-bit datapath. Handy for using
      --  a lane to generate sequence\n of pointers into flash or SRAM.
      FORCE_MSB      : INTERP1_CTRL_LANE0_FORCE_MSB_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : Interfaces.RP2040.Bit := 16#0#;
      --  Only present on INTERP1 on each core. If CLAMP mode is enabled:\n -
      --  LANE0 result is shifted and masked ACCUM0, clamped by a lower bound
      --  of\n BASE0 and an upper bound of BASE1.\n - Signedness of these
      --  comparisons is determined by LANE0_CTRL_SIGNED
      CLAMP          : INTERP1_CTRL_LANE0_CLAMP_Field := 16#0#;
      --  Read-only. Indicates if any masked-off MSBs in ACCUM0 are set.
      OVERF          : INTERP1_CTRL_LANE0_OVERF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_26_31 : Interfaces.RP2040.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP1_CTRL_LANE0_Register use record
      SHIFT          at 0 range 0 .. 4;
      MASK_LSB       at 0 range 5 .. 9;
      MASK_MSB       at 0 range 10 .. 14;
      SIGNED         at 0 range 15 .. 15;
      CROSS_INPUT    at 0 range 16 .. 16;
      CROSS_RESULT   at 0 range 17 .. 17;
      ADD_RAW        at 0 range 18 .. 18;
      FORCE_MSB      at 0 range 19 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      CLAMP          at 0 range 22 .. 22;
      OVERF          at 0 range 23 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype INTERP1_CTRL_LANE1_SHIFT_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE1_MASK_LSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE1_MASK_MSB_Field is Interfaces.RP2040.UInt5;
   subtype INTERP1_CTRL_LANE1_SIGNED_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE1_CROSS_INPUT_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE1_CROSS_RESULT_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE1_ADD_RAW_Field is Interfaces.RP2040.Bit;
   subtype INTERP1_CTRL_LANE1_FORCE_MSB_Field is Interfaces.RP2040.UInt2;

   --  Control register for lane 1
   type INTERP1_CTRL_LANE1_Register is record
      --  Logical right-shift applied to accumulator before masking
      SHIFT          : INTERP1_CTRL_LANE1_SHIFT_Field := 16#0#;
      --  The least-significant bit allowed to pass by the mask (inclusive)
      MASK_LSB       : INTERP1_CTRL_LANE1_MASK_LSB_Field := 16#0#;
      --  The most-significant bit allowed to pass by the mask (inclusive)\n
      --  Setting MSB < LSB may cause chip to turn inside-out
      MASK_MSB       : INTERP1_CTRL_LANE1_MASK_MSB_Field := 16#0#;
      --  If SIGNED is set, the shifted and masked accumulator value is
      --  sign-extended to 32 bits\n before adding to BASE1, and LANE1 PEEK/POP
      --  appear extended to 32 bits when read by processor.
      SIGNED         : INTERP1_CTRL_LANE1_SIGNED_Field := 16#0#;
      --  If 1, feed the opposite lane's accumulator into this lane's shift +
      --  mask hardware.\n Takes effect even if ADD_RAW is set (the CROSS_INPUT
      --  mux is before the shift+mask bypass)
      CROSS_INPUT    : INTERP1_CTRL_LANE1_CROSS_INPUT_Field := 16#0#;
      --  If 1, feed the opposite lane's result into this lane's accumulator on
      --  POP.
      CROSS_RESULT   : INTERP1_CTRL_LANE1_CROSS_RESULT_Field := 16#0#;
      --  If 1, mask + shift is bypassed for LANE1 result. This does not affect
      --  FULL result.
      ADD_RAW        : INTERP1_CTRL_LANE1_ADD_RAW_Field := 16#0#;
      --  ORed into bits 29:28 of the lane result presented to the processor on
      --  the bus.\n No effect on the internal 32-bit datapath. Handy for using
      --  a lane to generate sequence\n of pointers into flash or SRAM.
      FORCE_MSB      : INTERP1_CTRL_LANE1_FORCE_MSB_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : Interfaces.RP2040.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP1_CTRL_LANE1_Register use record
      SHIFT          at 0 range 0 .. 4;
      MASK_LSB       at 0 range 5 .. 9;
      MASK_MSB       at 0 range 10 .. 14;
      SIGNED         at 0 range 15 .. 15;
      CROSS_INPUT    at 0 range 16 .. 16;
      CROSS_RESULT   at 0 range 17 .. 17;
      ADD_RAW        at 0 range 18 .. 18;
      FORCE_MSB      at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype INTERP1_ACCUM0_ADD_INTERP1_ACCUM0_ADD_Field is
     Interfaces.RP2040.UInt24;

   --  Values written here are atomically added to ACCUM0\n Reading yields lane
   --  0's raw shift and mask value (BASE0 not added).
   type INTERP1_ACCUM0_ADD_Register is record
      INTERP1_ACCUM0_ADD : INTERP1_ACCUM0_ADD_INTERP1_ACCUM0_ADD_Field :=
                            16#0#;
      --  unspecified
      Reserved_24_31     : Interfaces.RP2040.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP1_ACCUM0_ADD_Register use record
      INTERP1_ACCUM0_ADD at 0 range 0 .. 23;
      Reserved_24_31     at 0 range 24 .. 31;
   end record;

   subtype INTERP1_ACCUM1_ADD_INTERP1_ACCUM1_ADD_Field is
     Interfaces.RP2040.UInt24;

   --  Values written here are atomically added to ACCUM1\n Reading yields lane
   --  1's raw shift and mask value (BASE1 not added).
   type INTERP1_ACCUM1_ADD_Register is record
      INTERP1_ACCUM1_ADD : INTERP1_ACCUM1_ADD_INTERP1_ACCUM1_ADD_Field :=
                            16#0#;
      --  unspecified
      Reserved_24_31     : Interfaces.RP2040.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTERP1_ACCUM1_ADD_Register use record
      INTERP1_ACCUM1_ADD at 0 range 0 .. 23;
      Reserved_24_31     at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Single-cycle IO block\n Provides core-local and inter-core hardware for
   --  the two processors, with single-cycle access.
   type SIO_Peripheral is record
      --  Processor core identifier\n Value is 0 when read from processor core
      --  0, and 1 when read from processor core 1.
      CPUID              : aliased Interfaces.RP2040.UInt32;
      --  Input value for GPIO pins
      GPIO_IN            : aliased GPIO_IN_Register;
      --  Input value for QSPI pins
      GPIO_HI_IN         : aliased GPIO_HI_IN_Register;
      --  GPIO output value
      GPIO_OUT           : aliased GPIO_OUT_Register;
      --  GPIO output value set
      GPIO_OUT_SET       : aliased GPIO_OUT_SET_Register;
      --  GPIO output value clear
      GPIO_OUT_CLR       : aliased GPIO_OUT_CLR_Register;
      --  GPIO output value XOR
      GPIO_OUT_XOR       : aliased GPIO_OUT_XOR_Register;
      --  GPIO output enable
      GPIO_OE            : aliased GPIO_OE_Register;
      --  GPIO output enable set
      GPIO_OE_SET        : aliased GPIO_OE_SET_Register;
      --  GPIO output enable clear
      GPIO_OE_CLR        : aliased GPIO_OE_CLR_Register;
      --  GPIO output enable XOR
      GPIO_OE_XOR        : aliased GPIO_OE_XOR_Register;
      --  QSPI output value
      GPIO_HI_OUT        : aliased GPIO_HI_OUT_Register;
      --  QSPI output value set
      GPIO_HI_OUT_SET    : aliased GPIO_HI_OUT_SET_Register;
      --  QSPI output value clear
      GPIO_HI_OUT_CLR    : aliased GPIO_HI_OUT_CLR_Register;
      --  QSPI output value XOR
      GPIO_HI_OUT_XOR    : aliased GPIO_HI_OUT_XOR_Register;
      --  QSPI output enable
      GPIO_HI_OE         : aliased GPIO_HI_OE_Register;
      --  QSPI output enable set
      GPIO_HI_OE_SET     : aliased GPIO_HI_OE_SET_Register;
      --  QSPI output enable clear
      GPIO_HI_OE_CLR     : aliased GPIO_HI_OE_CLR_Register;
      --  QSPI output enable XOR
      GPIO_HI_OE_XOR     : aliased GPIO_HI_OE_XOR_Register;
      --  Status register for inter-core FIFOs (mailboxes).\n There is one FIFO
      --  in the core 0 -> core 1 direction, and one core 1 -> core 0. Both are
      --  32 bits wide and 8 words deep.\n Core 0 can see the read side of the
      --  1->0 FIFO (RX), and the write side of 0->1 FIFO (TX).\n Core 1 can
      --  see the read side of the 0->1 FIFO (RX), and the write side of 1->0
      --  FIFO (TX).\n The SIO IRQ for each core is the logical OR of the VLD,
      --  WOF and ROE fields of its FIFO_ST register.
      FIFO_ST            : aliased FIFO_ST_Register;
      --  Write access to this core's TX FIFO
      FIFO_WR            : aliased Interfaces.RP2040.UInt32;
      --  Read access to this core's RX FIFO
      FIFO_RD            : aliased Interfaces.RP2040.UInt32;
      --  Spinlock state\n A bitmap containing the state of all 32 spinlocks
      --  (1=locked).\n Mainly intended for debugging.
      SPINLOCK_ST        : aliased Interfaces.RP2040.UInt32;
      --  Divider unsigned dividend\n Write to the DIVIDEND operand of the
      --  divider, i.e. the p in `p / q`.\n Any operand write starts a new
      --  calculation. The results appear in QUOTIENT, REMAINDER.\n
      --  UDIVIDEND/SDIVIDEND are aliases of the same internal register. The U
      --  alias starts an\n unsigned calculation, and the S alias starts a
      --  signed calculation.
      DIV_UDIVIDEND      : aliased Interfaces.RP2040.UInt32;
      --  Divider unsigned divisor\n Write to the DIVISOR operand of the
      --  divider, i.e. the q in `p / q`.\n Any operand write starts a new
      --  calculation. The results appear in QUOTIENT, REMAINDER.\n
      --  UDIVIDEND/SDIVIDEND are aliases of the same internal register. The U
      --  alias starts an\n unsigned calculation, and the S alias starts a
      --  signed calculation.
      DIV_UDIVISOR       : aliased Interfaces.RP2040.UInt32;
      --  Divider signed dividend\n The same as UDIVIDEND, but starts a signed
      --  calculation, rather than unsigned.
      DIV_SDIVIDEND      : aliased Interfaces.RP2040.UInt32;
      --  Divider signed divisor\n The same as UDIVISOR, but starts a signed
      --  calculation, rather than unsigned.
      DIV_SDIVISOR       : aliased Interfaces.RP2040.UInt32;
      --  Divider result quotient\n The result of `DIVIDEND / DIVISOR`
      --  (division). Contents undefined while CSR_READY is low.\n For signed
      --  calculations, QUOTIENT is negative when the signs of DIVIDEND and
      --  DIVISOR differ.\n This register can be written to directly, for
      --  context save/restore purposes. This halts any\n in-progress
      --  calculation and sets the CSR_READY and CSR_DIRTY flags.\n Reading
      --  from QUOTIENT clears the CSR_DIRTY flag, so should read results in
      --  the order\n REMAINDER, QUOTIENT if CSR_DIRTY is used.
      DIV_QUOTIENT       : aliased Interfaces.RP2040.UInt32;
      --  Divider result remainder\n The result of `DIVIDEND % DIVISOR`
      --  (modulo). Contents undefined while CSR_READY is low.\n For signed
      --  calculations, REMAINDER is negative only when DIVIDEND is negative.\n
      --  This register can be written to directly, for context save/restore
      --  purposes. This halts any\n in-progress calculation and sets the
      --  CSR_READY and CSR_DIRTY flags.
      DIV_REMAINDER      : aliased Interfaces.RP2040.UInt32;
      --  Control and status register for divider.
      DIV_CSR            : aliased DIV_CSR_Register;
      --  Read/write access to accumulator 0
      INTERP0_ACCUM0     : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to accumulator 1
      INTERP0_ACCUM1     : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE0 register.
      INTERP0_BASE0      : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE1 register.
      INTERP0_BASE1      : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE2 register.
      INTERP0_BASE2      : aliased Interfaces.RP2040.UInt32;
      --  Read LANE0 result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP0_POP_LANE0  : aliased Interfaces.RP2040.UInt32;
      --  Read LANE1 result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP0_POP_LANE1  : aliased Interfaces.RP2040.UInt32;
      --  Read FULL result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP0_POP_FULL   : aliased Interfaces.RP2040.UInt32;
      --  Read LANE0 result, without altering any internal state (PEEK).
      INTERP0_PEEK_LANE0 : aliased Interfaces.RP2040.UInt32;
      --  Read LANE1 result, without altering any internal state (PEEK).
      INTERP0_PEEK_LANE1 : aliased Interfaces.RP2040.UInt32;
      --  Read FULL result, without altering any internal state (PEEK).
      INTERP0_PEEK_FULL  : aliased Interfaces.RP2040.UInt32;
      --  Control register for lane 0
      INTERP0_CTRL_LANE0 : aliased INTERP0_CTRL_LANE0_Register;
      --  Control register for lane 1
      INTERP0_CTRL_LANE1 : aliased INTERP0_CTRL_LANE1_Register;
      --  Values written here are atomically added to ACCUM0\n Reading yields
      --  lane 0's raw shift and mask value (BASE0 not added).
      INTERP0_ACCUM0_ADD : aliased INTERP0_ACCUM0_ADD_Register;
      --  Values written here are atomically added to ACCUM1\n Reading yields
      --  lane 1's raw shift and mask value (BASE1 not added).
      INTERP0_ACCUM1_ADD : aliased INTERP0_ACCUM1_ADD_Register;
      --  On write, the lower 16 bits go to BASE0, upper bits to BASE1
      --  simultaneously.\n Each half is sign-extended to 32 bits if that
      --  lane's SIGNED flag is set.
      INTERP0_BASE_1AND0 : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to accumulator 0
      INTERP1_ACCUM0     : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to accumulator 1
      INTERP1_ACCUM1     : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE0 register.
      INTERP1_BASE0      : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE1 register.
      INTERP1_BASE1      : aliased Interfaces.RP2040.UInt32;
      --  Read/write access to BASE2 register.
      INTERP1_BASE2      : aliased Interfaces.RP2040.UInt32;
      --  Read LANE0 result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP1_POP_LANE0  : aliased Interfaces.RP2040.UInt32;
      --  Read LANE1 result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP1_POP_LANE1  : aliased Interfaces.RP2040.UInt32;
      --  Read FULL result, and simultaneously write lane results to both
      --  accumulators (POP).
      INTERP1_POP_FULL   : aliased Interfaces.RP2040.UInt32;
      --  Read LANE0 result, without altering any internal state (PEEK).
      INTERP1_PEEK_LANE0 : aliased Interfaces.RP2040.UInt32;
      --  Read LANE1 result, without altering any internal state (PEEK).
      INTERP1_PEEK_LANE1 : aliased Interfaces.RP2040.UInt32;
      --  Read FULL result, without altering any internal state (PEEK).
      INTERP1_PEEK_FULL  : aliased Interfaces.RP2040.UInt32;
      --  Control register for lane 0
      INTERP1_CTRL_LANE0 : aliased INTERP1_CTRL_LANE0_Register;
      --  Control register for lane 1
      INTERP1_CTRL_LANE1 : aliased INTERP1_CTRL_LANE1_Register;
      --  Values written here are atomically added to ACCUM0\n Reading yields
      --  lane 0's raw shift and mask value (BASE0 not added).
      INTERP1_ACCUM0_ADD : aliased INTERP1_ACCUM0_ADD_Register;
      --  Values written here are atomically added to ACCUM1\n Reading yields
      --  lane 1's raw shift and mask value (BASE1 not added).
      INTERP1_ACCUM1_ADD : aliased INTERP1_ACCUM1_ADD_Register;
      --  On write, the lower 16 bits go to BASE0, upper bits to BASE1
      --  simultaneously.\n Each half is sign-extended to 32 bits if that
      --  lane's SIGNED flag is set.
      INTERP1_BASE_1AND0 : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK0          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK1          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK2          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK3          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK4          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK5          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK6          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK7          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK8          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK9          : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK10         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK11         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK12         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK13         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK14         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK15         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK16         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK17         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK18         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK19         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK20         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK21         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK22         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK23         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK24         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK25         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK26         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK27         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK28         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK29         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK30         : aliased Interfaces.RP2040.UInt32;
      --  Reading from a spinlock address will:\n - Return 0 if lock is already
      --  locked\n - Otherwise return nonzero, and simultaneously claim the
      --  lock\n\n Writing (any value) releases the lock.\n If core 0 and core
      --  1 attempt to claim the same lock simultaneously, core 0 wins.\n The
      --  value returned on success is 0x1 << lock number.
      SPINLOCK31         : aliased Interfaces.RP2040.UInt32;
   end record
     with Volatile;

   for SIO_Peripheral use record
      CPUID              at 16#0# range 0 .. 31;
      GPIO_IN            at 16#4# range 0 .. 31;
      GPIO_HI_IN         at 16#8# range 0 .. 31;
      GPIO_OUT           at 16#10# range 0 .. 31;
      GPIO_OUT_SET       at 16#14# range 0 .. 31;
      GPIO_OUT_CLR       at 16#18# range 0 .. 31;
      GPIO_OUT_XOR       at 16#1C# range 0 .. 31;
      GPIO_OE            at 16#20# range 0 .. 31;
      GPIO_OE_SET        at 16#24# range 0 .. 31;
      GPIO_OE_CLR        at 16#28# range 0 .. 31;
      GPIO_OE_XOR        at 16#2C# range 0 .. 31;
      GPIO_HI_OUT        at 16#30# range 0 .. 31;
      GPIO_HI_OUT_SET    at 16#34# range 0 .. 31;
      GPIO_HI_OUT_CLR    at 16#38# range 0 .. 31;
      GPIO_HI_OUT_XOR    at 16#3C# range 0 .. 31;
      GPIO_HI_OE         at 16#40# range 0 .. 31;
      GPIO_HI_OE_SET     at 16#44# range 0 .. 31;
      GPIO_HI_OE_CLR     at 16#48# range 0 .. 31;
      GPIO_HI_OE_XOR     at 16#4C# range 0 .. 31;
      FIFO_ST            at 16#50# range 0 .. 31;
      FIFO_WR            at 16#54# range 0 .. 31;
      FIFO_RD            at 16#58# range 0 .. 31;
      SPINLOCK_ST        at 16#5C# range 0 .. 31;
      DIV_UDIVIDEND      at 16#60# range 0 .. 31;
      DIV_UDIVISOR       at 16#64# range 0 .. 31;
      DIV_SDIVIDEND      at 16#68# range 0 .. 31;
      DIV_SDIVISOR       at 16#6C# range 0 .. 31;
      DIV_QUOTIENT       at 16#70# range 0 .. 31;
      DIV_REMAINDER      at 16#74# range 0 .. 31;
      DIV_CSR            at 16#78# range 0 .. 31;
      INTERP0_ACCUM0     at 16#80# range 0 .. 31;
      INTERP0_ACCUM1     at 16#84# range 0 .. 31;
      INTERP0_BASE0      at 16#88# range 0 .. 31;
      INTERP0_BASE1      at 16#8C# range 0 .. 31;
      INTERP0_BASE2      at 16#90# range 0 .. 31;
      INTERP0_POP_LANE0  at 16#94# range 0 .. 31;
      INTERP0_POP_LANE1  at 16#98# range 0 .. 31;
      INTERP0_POP_FULL   at 16#9C# range 0 .. 31;
      INTERP0_PEEK_LANE0 at 16#A0# range 0 .. 31;
      INTERP0_PEEK_LANE1 at 16#A4# range 0 .. 31;
      INTERP0_PEEK_FULL  at 16#A8# range 0 .. 31;
      INTERP0_CTRL_LANE0 at 16#AC# range 0 .. 31;
      INTERP0_CTRL_LANE1 at 16#B0# range 0 .. 31;
      INTERP0_ACCUM0_ADD at 16#B4# range 0 .. 31;
      INTERP0_ACCUM1_ADD at 16#B8# range 0 .. 31;
      INTERP0_BASE_1AND0 at 16#BC# range 0 .. 31;
      INTERP1_ACCUM0     at 16#C0# range 0 .. 31;
      INTERP1_ACCUM1     at 16#C4# range 0 .. 31;
      INTERP1_BASE0      at 16#C8# range 0 .. 31;
      INTERP1_BASE1      at 16#CC# range 0 .. 31;
      INTERP1_BASE2      at 16#D0# range 0 .. 31;
      INTERP1_POP_LANE0  at 16#D4# range 0 .. 31;
      INTERP1_POP_LANE1  at 16#D8# range 0 .. 31;
      INTERP1_POP_FULL   at 16#DC# range 0 .. 31;
      INTERP1_PEEK_LANE0 at 16#E0# range 0 .. 31;
      INTERP1_PEEK_LANE1 at 16#E4# range 0 .. 31;
      INTERP1_PEEK_FULL  at 16#E8# range 0 .. 31;
      INTERP1_CTRL_LANE0 at 16#EC# range 0 .. 31;
      INTERP1_CTRL_LANE1 at 16#F0# range 0 .. 31;
      INTERP1_ACCUM0_ADD at 16#F4# range 0 .. 31;
      INTERP1_ACCUM1_ADD at 16#F8# range 0 .. 31;
      INTERP1_BASE_1AND0 at 16#FC# range 0 .. 31;
      SPINLOCK0          at 16#100# range 0 .. 31;
      SPINLOCK1          at 16#104# range 0 .. 31;
      SPINLOCK2          at 16#108# range 0 .. 31;
      SPINLOCK3          at 16#10C# range 0 .. 31;
      SPINLOCK4          at 16#110# range 0 .. 31;
      SPINLOCK5          at 16#114# range 0 .. 31;
      SPINLOCK6          at 16#118# range 0 .. 31;
      SPINLOCK7          at 16#11C# range 0 .. 31;
      SPINLOCK8          at 16#120# range 0 .. 31;
      SPINLOCK9          at 16#124# range 0 .. 31;
      SPINLOCK10         at 16#128# range 0 .. 31;
      SPINLOCK11         at 16#12C# range 0 .. 31;
      SPINLOCK12         at 16#130# range 0 .. 31;
      SPINLOCK13         at 16#134# range 0 .. 31;
      SPINLOCK14         at 16#138# range 0 .. 31;
      SPINLOCK15         at 16#13C# range 0 .. 31;
      SPINLOCK16         at 16#140# range 0 .. 31;
      SPINLOCK17         at 16#144# range 0 .. 31;
      SPINLOCK18         at 16#148# range 0 .. 31;
      SPINLOCK19         at 16#14C# range 0 .. 31;
      SPINLOCK20         at 16#150# range 0 .. 31;
      SPINLOCK21         at 16#154# range 0 .. 31;
      SPINLOCK22         at 16#158# range 0 .. 31;
      SPINLOCK23         at 16#15C# range 0 .. 31;
      SPINLOCK24         at 16#160# range 0 .. 31;
      SPINLOCK25         at 16#164# range 0 .. 31;
      SPINLOCK26         at 16#168# range 0 .. 31;
      SPINLOCK27         at 16#16C# range 0 .. 31;
      SPINLOCK28         at 16#170# range 0 .. 31;
      SPINLOCK29         at 16#174# range 0 .. 31;
      SPINLOCK30         at 16#178# range 0 .. 31;
      SPINLOCK31         at 16#17C# range 0 .. 31;
   end record;

   --  Single-cycle IO block\n Provides core-local and inter-core hardware for
   --  the two processors, with single-cycle access.
   SIO_Periph : aliased SIO_Peripheral
     with Import, Address => SIO_Base;

end Interfaces.RP2040.SIO;
