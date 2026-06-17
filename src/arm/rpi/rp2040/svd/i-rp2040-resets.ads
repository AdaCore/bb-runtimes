--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd


with System;

package Interfaces.RP2040.RESETS is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype RESET_adc_Field is Interfaces.RP2040.Bit;
   subtype RESET_busctrl_Field is Interfaces.RP2040.Bit;
   subtype RESET_dma_Field is Interfaces.RP2040.Bit;
   --  RESET_i2c array element
   subtype RESET_i2c_Element is Interfaces.RP2040.Bit;

   --  RESET_i2c array
   type RESET_i2c_Field_Array is array (0 .. 1) of RESET_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_i2c
   type RESET_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  i2c as an array
            Arr : RESET_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_io_bank0_Field is Interfaces.RP2040.Bit;
   subtype RESET_io_qspi_Field is Interfaces.RP2040.Bit;
   subtype RESET_jtag_Field is Interfaces.RP2040.Bit;
   subtype RESET_pads_bank0_Field is Interfaces.RP2040.Bit;
   subtype RESET_pads_qspi_Field is Interfaces.RP2040.Bit;
   --  RESET_pio array element
   subtype RESET_pio_Element is Interfaces.RP2040.Bit;

   --  RESET_pio array
   type RESET_pio_Field_Array is array (0 .. 1) of RESET_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_pio
   type RESET_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  pio as an array
            Arr : RESET_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype RESET_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype RESET_pwm_Field is Interfaces.RP2040.Bit;
   subtype RESET_rtc_Field is Interfaces.RP2040.Bit;
   --  RESET_spi array element
   subtype RESET_spi_Element is Interfaces.RP2040.Bit;

   --  RESET_spi array
   type RESET_spi_Field_Array is array (0 .. 1) of RESET_spi_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_spi
   type RESET_spi_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  spi as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  spi as an array
            Arr : RESET_spi_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_spi_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_syscfg_Field is Interfaces.RP2040.Bit;
   subtype RESET_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype RESET_tbman_Field is Interfaces.RP2040.Bit;
   subtype RESET_timer_Field is Interfaces.RP2040.Bit;
   --  RESET_uart array element
   subtype RESET_uart_Element is Interfaces.RP2040.Bit;

   --  RESET_uart array
   type RESET_uart_Field_Array is array (0 .. 1) of RESET_uart_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_uart
   type RESET_uart_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  uart as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  uart as an array
            Arr : RESET_uart_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_uart_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_usbctrl_Field is Interfaces.RP2040.Bit;

   --  Reset control. If a bit is set it means the peripheral is in reset. 0
   --  means the peripheral's reset is deasserted.
   type RESET_Register is record
      adc            : RESET_adc_Field := 16#1#;
      busctrl        : RESET_busctrl_Field := 16#1#;
      dma            : RESET_dma_Field := 16#1#;
      i2c            : RESET_i2c_Field := (As_Array => False, Val => 16#1#);
      io_bank0       : RESET_io_bank0_Field := 16#1#;
      io_qspi        : RESET_io_qspi_Field := 16#1#;
      jtag           : RESET_jtag_Field := 16#1#;
      pads_bank0     : RESET_pads_bank0_Field := 16#1#;
      pads_qspi      : RESET_pads_qspi_Field := 16#1#;
      pio            : RESET_pio_Field := (As_Array => False, Val => 16#1#);
      pll_sys        : RESET_pll_sys_Field := 16#1#;
      pll_usb        : RESET_pll_usb_Field := 16#1#;
      pwm            : RESET_pwm_Field := 16#1#;
      rtc            : RESET_rtc_Field := 16#1#;
      spi            : RESET_spi_Field := (As_Array => False, Val => 16#1#);
      syscfg         : RESET_syscfg_Field := 16#1#;
      sysinfo        : RESET_sysinfo_Field := 16#1#;
      tbman          : RESET_tbman_Field := 16#1#;
      timer          : RESET_timer_Field := 16#1#;
      uart           : RESET_uart_Field := (As_Array => False, Val => 16#1#);
      usbctrl        : RESET_usbctrl_Field := 16#1#;
      --  unspecified
      Reserved_25_31 : Interfaces.RP2040.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESET_Register use record
      adc            at 0 range 0 .. 0;
      busctrl        at 0 range 1 .. 1;
      dma            at 0 range 2 .. 2;
      i2c            at 0 range 3 .. 4;
      io_bank0       at 0 range 5 .. 5;
      io_qspi        at 0 range 6 .. 6;
      jtag           at 0 range 7 .. 7;
      pads_bank0     at 0 range 8 .. 8;
      pads_qspi      at 0 range 9 .. 9;
      pio            at 0 range 10 .. 11;
      pll_sys        at 0 range 12 .. 12;
      pll_usb        at 0 range 13 .. 13;
      pwm            at 0 range 14 .. 14;
      rtc            at 0 range 15 .. 15;
      spi            at 0 range 16 .. 17;
      syscfg         at 0 range 18 .. 18;
      sysinfo        at 0 range 19 .. 19;
      tbman          at 0 range 20 .. 20;
      timer          at 0 range 21 .. 21;
      uart           at 0 range 22 .. 23;
      usbctrl        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype WDSEL_adc_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_busctrl_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_dma_Field is Interfaces.RP2040.Bit;
   --  WDSEL_i2c array element
   subtype WDSEL_i2c_Element is Interfaces.RP2040.Bit;

   --  WDSEL_i2c array
   type WDSEL_i2c_Field_Array is array (0 .. 1) of WDSEL_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WDSEL_i2c
   type WDSEL_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  i2c as an array
            Arr : WDSEL_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WDSEL_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WDSEL_io_bank0_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_io_qspi_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_jtag_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_pads_bank0_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_pads_qspi_Field is Interfaces.RP2040.Bit;
   --  WDSEL_pio array element
   subtype WDSEL_pio_Element is Interfaces.RP2040.Bit;

   --  WDSEL_pio array
   type WDSEL_pio_Field_Array is array (0 .. 1) of WDSEL_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WDSEL_pio
   type WDSEL_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  pio as an array
            Arr : WDSEL_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WDSEL_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WDSEL_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_pwm_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_rtc_Field is Interfaces.RP2040.Bit;
   --  WDSEL_spi array element
   subtype WDSEL_spi_Element is Interfaces.RP2040.Bit;

   --  WDSEL_spi array
   type WDSEL_spi_Field_Array is array (0 .. 1) of WDSEL_spi_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WDSEL_spi
   type WDSEL_spi_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  spi as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  spi as an array
            Arr : WDSEL_spi_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WDSEL_spi_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WDSEL_syscfg_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_tbman_Field is Interfaces.RP2040.Bit;
   subtype WDSEL_timer_Field is Interfaces.RP2040.Bit;
   --  WDSEL_uart array element
   subtype WDSEL_uart_Element is Interfaces.RP2040.Bit;

   --  WDSEL_uart array
   type WDSEL_uart_Field_Array is array (0 .. 1) of WDSEL_uart_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for WDSEL_uart
   type WDSEL_uart_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  uart as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  uart as an array
            Arr : WDSEL_uart_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for WDSEL_uart_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype WDSEL_usbctrl_Field is Interfaces.RP2040.Bit;

   --  Watchdog select. If a bit is set then the watchdog will reset this
   --  peripheral when the watchdog fires.
   type WDSEL_Register is record
      adc            : WDSEL_adc_Field := 16#0#;
      busctrl        : WDSEL_busctrl_Field := 16#0#;
      dma            : WDSEL_dma_Field := 16#0#;
      i2c            : WDSEL_i2c_Field := (As_Array => False, Val => 16#0#);
      io_bank0       : WDSEL_io_bank0_Field := 16#0#;
      io_qspi        : WDSEL_io_qspi_Field := 16#0#;
      jtag           : WDSEL_jtag_Field := 16#0#;
      pads_bank0     : WDSEL_pads_bank0_Field := 16#0#;
      pads_qspi      : WDSEL_pads_qspi_Field := 16#0#;
      pio            : WDSEL_pio_Field := (As_Array => False, Val => 16#0#);
      pll_sys        : WDSEL_pll_sys_Field := 16#0#;
      pll_usb        : WDSEL_pll_usb_Field := 16#0#;
      pwm            : WDSEL_pwm_Field := 16#0#;
      rtc            : WDSEL_rtc_Field := 16#0#;
      spi            : WDSEL_spi_Field := (As_Array => False, Val => 16#0#);
      syscfg         : WDSEL_syscfg_Field := 16#0#;
      sysinfo        : WDSEL_sysinfo_Field := 16#0#;
      tbman          : WDSEL_tbman_Field := 16#0#;
      timer          : WDSEL_timer_Field := 16#0#;
      uart           : WDSEL_uart_Field := (As_Array => False, Val => 16#0#);
      usbctrl        : WDSEL_usbctrl_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : Interfaces.RP2040.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDSEL_Register use record
      adc            at 0 range 0 .. 0;
      busctrl        at 0 range 1 .. 1;
      dma            at 0 range 2 .. 2;
      i2c            at 0 range 3 .. 4;
      io_bank0       at 0 range 5 .. 5;
      io_qspi        at 0 range 6 .. 6;
      jtag           at 0 range 7 .. 7;
      pads_bank0     at 0 range 8 .. 8;
      pads_qspi      at 0 range 9 .. 9;
      pio            at 0 range 10 .. 11;
      pll_sys        at 0 range 12 .. 12;
      pll_usb        at 0 range 13 .. 13;
      pwm            at 0 range 14 .. 14;
      rtc            at 0 range 15 .. 15;
      spi            at 0 range 16 .. 17;
      syscfg         at 0 range 18 .. 18;
      sysinfo        at 0 range 19 .. 19;
      tbman          at 0 range 20 .. 20;
      timer          at 0 range 21 .. 21;
      uart           at 0 range 22 .. 23;
      usbctrl        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype RESET_DONE_adc_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_busctrl_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_dma_Field is Interfaces.RP2040.Bit;
   --  RESET_DONE_i2c array element
   subtype RESET_DONE_i2c_Element is Interfaces.RP2040.Bit;

   --  RESET_DONE_i2c array
   type RESET_DONE_i2c_Field_Array is array (0 .. 1)
     of RESET_DONE_i2c_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_DONE_i2c
   type RESET_DONE_i2c_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  i2c as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  i2c as an array
            Arr : RESET_DONE_i2c_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_DONE_i2c_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_DONE_io_bank0_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_io_qspi_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_jtag_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_pads_bank0_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_pads_qspi_Field is Interfaces.RP2040.Bit;
   --  RESET_DONE_pio array element
   subtype RESET_DONE_pio_Element is Interfaces.RP2040.Bit;

   --  RESET_DONE_pio array
   type RESET_DONE_pio_Field_Array is array (0 .. 1)
     of RESET_DONE_pio_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_DONE_pio
   type RESET_DONE_pio_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  pio as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  pio as an array
            Arr : RESET_DONE_pio_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_DONE_pio_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_DONE_pll_sys_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_pll_usb_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_pwm_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_rtc_Field is Interfaces.RP2040.Bit;
   --  RESET_DONE_spi array element
   subtype RESET_DONE_spi_Element is Interfaces.RP2040.Bit;

   --  RESET_DONE_spi array
   type RESET_DONE_spi_Field_Array is array (0 .. 1)
     of RESET_DONE_spi_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_DONE_spi
   type RESET_DONE_spi_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  spi as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  spi as an array
            Arr : RESET_DONE_spi_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_DONE_spi_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_DONE_syscfg_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_sysinfo_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_tbman_Field is Interfaces.RP2040.Bit;
   subtype RESET_DONE_timer_Field is Interfaces.RP2040.Bit;
   --  RESET_DONE_uart array element
   subtype RESET_DONE_uart_Element is Interfaces.RP2040.Bit;

   --  RESET_DONE_uart array
   type RESET_DONE_uart_Field_Array is array (0 .. 1)
     of RESET_DONE_uart_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for RESET_DONE_uart
   type RESET_DONE_uart_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  uart as a value
            Val : Interfaces.RP2040.UInt2;
         when True =>
            --  uart as an array
            Arr : RESET_DONE_uart_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for RESET_DONE_uart_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype RESET_DONE_usbctrl_Field is Interfaces.RP2040.Bit;

   --  Reset done. If a bit is set then a reset done signal has been returned
   --  by the peripheral. This indicates that the peripheral's registers are
   --  ready to be accessed.
   type RESET_DONE_Register is record
      --  Read-only.
      adc            : RESET_DONE_adc_Field;
      --  Read-only.
      busctrl        : RESET_DONE_busctrl_Field;
      --  Read-only.
      dma            : RESET_DONE_dma_Field;
      --  Read-only.
      i2c            : RESET_DONE_i2c_Field;
      --  Read-only.
      io_bank0       : RESET_DONE_io_bank0_Field;
      --  Read-only.
      io_qspi        : RESET_DONE_io_qspi_Field;
      --  Read-only.
      jtag           : RESET_DONE_jtag_Field;
      --  Read-only.
      pads_bank0     : RESET_DONE_pads_bank0_Field;
      --  Read-only.
      pads_qspi      : RESET_DONE_pads_qspi_Field;
      --  Read-only.
      pio            : RESET_DONE_pio_Field;
      --  Read-only.
      pll_sys        : RESET_DONE_pll_sys_Field;
      --  Read-only.
      pll_usb        : RESET_DONE_pll_usb_Field;
      --  Read-only.
      pwm            : RESET_DONE_pwm_Field;
      --  Read-only.
      rtc            : RESET_DONE_rtc_Field;
      --  Read-only.
      spi            : RESET_DONE_spi_Field;
      --  Read-only.
      syscfg         : RESET_DONE_syscfg_Field;
      --  Read-only.
      sysinfo        : RESET_DONE_sysinfo_Field;
      --  Read-only.
      tbman          : RESET_DONE_tbman_Field;
      --  Read-only.
      timer          : RESET_DONE_timer_Field;
      --  Read-only.
      uart           : RESET_DONE_uart_Field;
      --  Read-only.
      usbctrl        : RESET_DONE_usbctrl_Field;
      --  unspecified
      Reserved_25_31 : Interfaces.RP2040.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESET_DONE_Register use record
      adc            at 0 range 0 .. 0;
      busctrl        at 0 range 1 .. 1;
      dma            at 0 range 2 .. 2;
      i2c            at 0 range 3 .. 4;
      io_bank0       at 0 range 5 .. 5;
      io_qspi        at 0 range 6 .. 6;
      jtag           at 0 range 7 .. 7;
      pads_bank0     at 0 range 8 .. 8;
      pads_qspi      at 0 range 9 .. 9;
      pio            at 0 range 10 .. 11;
      pll_sys        at 0 range 12 .. 12;
      pll_usb        at 0 range 13 .. 13;
      pwm            at 0 range 14 .. 14;
      rtc            at 0 range 15 .. 15;
      spi            at 0 range 16 .. 17;
      syscfg         at 0 range 18 .. 18;
      sysinfo        at 0 range 19 .. 19;
      tbman          at 0 range 20 .. 20;
      timer          at 0 range 21 .. 21;
      uart           at 0 range 22 .. 23;
      usbctrl        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type RESETS_Peripheral is record
      --  Reset control. If a bit is set it means the peripheral is in reset. 0
      --  means the peripheral's reset is deasserted.
      RESET      : aliased RESET_Register;
      --  Watchdog select. If a bit is set then the watchdog will reset this
      --  peripheral when the watchdog fires.
      WDSEL      : aliased WDSEL_Register;
      --  Reset done. If a bit is set then a reset done signal has been
      --  returned by the peripheral. This indicates that the peripheral's
      --  registers are ready to be accessed.
      RESET_DONE : aliased RESET_DONE_Register;
   end record
     with Volatile;

   for RESETS_Peripheral use record
      RESET      at 16#0# range 0 .. 31;
      WDSEL      at 16#4# range 0 .. 31;
      RESET_DONE at 16#8# range 0 .. 31;
   end record;

   RESETS_Periph : aliased RESETS_Peripheral
     with Import, Address => RESETS_Base;

end Interfaces.RP2040.RESETS;
