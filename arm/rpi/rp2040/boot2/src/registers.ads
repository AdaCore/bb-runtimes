------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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
with Interfaces;
with System;

--  This package defines a subset of the RP2040 registers that are used
--  by the stage 2 bootloader.
--
--  The SVD-generated definitions are not used, since the use of records
--  and aggregates leads to a) the compiler requiring elaboration to
--  initialise constants from aggregates; and b) direct assignment of a
--  record-based register (instead of using a constant variable) leads to
--  fairly large code.
--
--  Since the boot2 cannot use elaboration, *and* we're very constrained by
--  size (boot2 must fit in 252 bytes!), we instead represent registers as
--  plain Unsigned_32 values. This represenation allows GNAT to generate
--  reasonably small code that requires no elaboration.
--
--  Only the registers needed by boot2 are defined here.

package Registers with
  No_Elaboration_Code_All
is

   -------------
   -- XIP_SSI --
   -------------

   package XIP_SSI is

      type Peripheral is record
         CTRLR0         : aliased Interfaces.Unsigned_32;
         CTRLR1         : aliased Interfaces.Unsigned_32;
         SSIENR         : aliased Interfaces.Unsigned_32;
         MWCR           : aliased Interfaces.Unsigned_32;
         SER            : aliased Interfaces.Unsigned_32;
         BAUDR          : aliased Interfaces.Unsigned_32;
         TXFTLR         : aliased Interfaces.Unsigned_32;
         RXFTLR         : aliased Interfaces.Unsigned_32;
         TXFLR          : aliased Interfaces.Unsigned_32;
         RXFLR          : aliased Interfaces.Unsigned_32;
         SR             : aliased Interfaces.Unsigned_32;
         IMR            : aliased Interfaces.Unsigned_32;
         ISR            : aliased Interfaces.Unsigned_32;
         RISR           : aliased Interfaces.Unsigned_32;
         TXOICR         : aliased Interfaces.Unsigned_32;
         RXOICR         : aliased Interfaces.Unsigned_32;
         RXUICR         : aliased Interfaces.Unsigned_32;
         MSTICR         : aliased Interfaces.Unsigned_32;
         ICR            : aliased Interfaces.Unsigned_32;
         DMACR          : aliased Interfaces.Unsigned_32;
         DMATDLR        : aliased Interfaces.Unsigned_32;
         DMARDLR        : aliased Interfaces.Unsigned_32;
         IDR            : aliased Interfaces.Unsigned_32;
         SSI_VERSION_ID : aliased Interfaces.Unsigned_32;
         DR0            : aliased Interfaces.Unsigned_32;
         RX_SAMPLE_DLY  : aliased Interfaces.Unsigned_32;
         SPI_CTRLR0     : aliased Interfaces.Unsigned_32;
         TXD_DRIVE_EDGE : aliased Interfaces.Unsigned_32;
      end record
        with Volatile;

      for Peripheral use record
         CTRLR0         at 16#0# range 0 .. 31;
         CTRLR1         at 16#4# range 0 .. 31;
         SSIENR         at 16#8# range 0 .. 31;
         MWCR           at 16#C# range 0 .. 31;
         SER            at 16#10# range 0 .. 31;
         BAUDR          at 16#14# range 0 .. 31;
         TXFTLR         at 16#18# range 0 .. 31;
         RXFTLR         at 16#1C# range 0 .. 31;
         TXFLR          at 16#20# range 0 .. 31;
         RXFLR          at 16#24# range 0 .. 31;
         SR             at 16#28# range 0 .. 31;
         IMR            at 16#2C# range 0 .. 31;
         ISR            at 16#30# range 0 .. 31;
         RISR           at 16#34# range 0 .. 31;
         TXOICR         at 16#38# range 0 .. 31;
         RXOICR         at 16#3C# range 0 .. 31;
         RXUICR         at 16#40# range 0 .. 31;
         MSTICR         at 16#44# range 0 .. 31;
         ICR            at 16#48# range 0 .. 31;
         DMACR          at 16#4C# range 0 .. 31;
         DMATDLR        at 16#50# range 0 .. 31;
         DMARDLR        at 16#54# range 0 .. 31;
         IDR            at 16#58# range 0 .. 31;
         SSI_VERSION_ID at 16#5C# range 0 .. 31;
         DR0            at 16#60# range 0 .. 31;
         RX_SAMPLE_DLY  at 16#F0# range 0 .. 31;
         SPI_CTRLR0     at 16#F4# range 0 .. 31;
         TXD_DRIVE_EDGE at 16#F8# range 0 .. 31;
      end record;

      Periph_Base : constant := 16#1800_0000#;

      Periph : Peripheral with
        Import,
        Address => System'To_Address (Periph_Base);

      --  Some values for the SR register
      SR_BUSY_Mask : constant Interfaces.Unsigned_32 := 16#0000_0001#;
      SR_TFE_Mask  : constant Interfaces.Unsigned_32 := 16#0000_0004#;

   end XIP_SSI;

   ---------------
   -- PADS_QSPI --
   ---------------

   package PADS_QSPI is

      type Peripheral is record
         VOLTAGE_SELECT : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SCLK : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SD0  : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SD1  : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SD2  : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SD3  : aliased Interfaces.Unsigned_32;
         GPIO_QSPI_SS   : aliased Interfaces.Unsigned_32;
      end record
        with Volatile;

      Periph_Base : constant := 16#4002_0000#;

      Periph : Peripheral with
        Import,
        Address => System'To_Address (Periph_Base);

   end PADS_QSPI;

end Registers;
