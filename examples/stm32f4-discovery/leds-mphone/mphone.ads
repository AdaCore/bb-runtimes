------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2013, Free Software Foundation, Inc.           --
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

pragma Warnings (Off);
with System.STM32F4; use System.STM32F4;
pragma Warnings (On);

package Mphone is
   pragma Elaborate_Body;

   --  Bit definitions for RCC AHB1ENR register
   RCC_AHB1ENR_GPIOC    : constant Word := 16#04#;

   --  Bit definitions for RCC APB1ENR register
   RCC_APB1ENR_SPI2      : constant Word := 16#0000_4000#;

   GPIOC_Base           : constant := AHB1_Peripheral_Base + 16#0800#;
   SPI2_Base            : constant := APB1_Peripheral_Base + 16#3800#;

   GPIOC : GPIO_Registers with Volatile,
                             Import,
                             Address => System'To_Address (GPIOC_Base);

   type SPI_Registers is record
      CR1     : Word;
      CR2     : Word;
      SR      : Word;
      DR      : Word;
      CRCPR   : Word;
      RXCRCR  : Word;
      TXCRCR  : Word;
      I2SCFGR : Word;
      I2SPR   : Word;
   end record;

   SPI2 : SPI_Registers with Volatile,
                             Import,
                             Address => System'To_Address (SPI2_Base);

   package SPI_I2SCFGR is
      I2SMOD          : constant Word := 16#0800#;
      I2SE            : constant Word := 16#0400#;
      I2SCFG_SlaveTx  : constant Word := 16#0000#;
      I2SCFG_SlaveRx  : constant Word := 16#0100#;
      I2SCFG_MasterTx : constant Word := 16#0200#;
      I2SCFG_MasterRx : constant Word := 16#0300#;
      PCMSYNC         : constant Word := 16#0080#;
      I2SSTD_Philips  : constant Word := 16#0000#;
      I2SSTD_MSB      : constant Word := 16#0010#;
      I2SSTD_LSB      : constant Word := 16#0020#;
      I2SSTD_PCM      : constant Word := 16#0030#;
      CKPOL           : constant Word := 16#0008#;
      DATLEN_16       : constant Word := 16#0000#;
      DATLEN_24       : constant Word := 16#0002#;
      DATLEN_32       : constant Word := 16#0004#;
      CHLEN_16        : constant Word := 16#0000#;
      CHLEN_32        : constant Word := 16#0001#;
   end SPI_I2SCFGR;

   package SPI_I2SPR is
      MCKOE : constant Word := 16#200#;
      ODD   : constant Word := 16#100#;
   end SPI_I2SPR;

   package SPI_CR2 is
      TXEIE   : constant Word := 16#80#;
      RXNEIE  : constant Word := 16#40#;
      ERRIE   : constant Word := 16#20#;
      FRF     : constant Word := 16#10#;
      SSOE    : constant Word := 16#04#;
      TXDMAEN : constant Word := 16#02#;
      RXDMAEN : constant Word := 16#01#;
   end SPI_CR2;

   package SPI_SR is
      FRE    : constant Word := 16#100#;
      BSY    : constant Word := 16#080#;
      OVR    : constant Word := 16#040#;
      MODF   : constant Word := 16#020#;
      CRCERR : constant Word := 16#010#;
      UDR    : constant Word := 16#008#;
      CHSIDE : constant Word := 16#004#;
      TXE    : constant Word := 16#002#;
      RXNE   : constant Word := 16#001#;
   end SPI_SR;

   subtype Sample_Type is Natural range 0 .. 32;
   function Get_Spd_Sample return Natural;
end Mphone;
