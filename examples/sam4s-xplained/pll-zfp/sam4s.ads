------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
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
with Interfaces; use Interfaces;
with System;

package Sam4s is
   pragma Suppress (Alignment_Check);

   Peripheral_Base : constant := 16#4000_0000#;

   SPI_Base : constant := Peripheral_Base + 16#8000#;

   System_Controller_Base : constant := Peripheral_Base + 16#e_0000#;
   PMC_Base  : constant := System_Controller_Base + 16#0400#;
   EFC0_Base : constant := System_Controller_Base + 16#0A00#;
   EFC1_Base : constant := System_Controller_Base + 16#0C00#;

   PIOA_Base : constant := System_Controller_Base + 16#0e00#;
   PIOC_Base : constant := System_Controller_Base + 16#1200#;

   type PMC_Registers is record
      PMC_SCER   : Unsigned_32;
      PMC_SCDR   : Unsigned_32;
      PMC_SCSR   : Unsigned_32;
      Pad0       : Unsigned_32;

      PMC_PCER0  : Unsigned_32;
      PMC_PCDR0  : Unsigned_32;
      PMC_PCSR0  : Unsigned_32;
      Pad1       : Unsigned_32;

      CKGR_MOR   : Unsigned_32;
      CKGR_MCFR  : Unsigned_32;
      CKGR_PLLAR : Unsigned_32;
      CKGR_PLLBR : Unsigned_32;

      PMC_MCKR   : Unsigned_32;
      Pad3_4     : Unsigned_32;
      PMC_USB    : Unsigned_32;
      Pad3_C     : Unsigned_32;

      PMC_PCK0   : Unsigned_32;
      PMC_PCK1   : Unsigned_32;
      PMC_PCK2   : Unsigned_32;
      Pad4_C     : Unsigned_32;

      Pad5_0 : Unsigned_32;
      Pad5_4 : Unsigned_32;
      Pad5_8 : Unsigned_32;
      Pad5_C : Unsigned_32;

      PMC_IER    : Unsigned_32;
      PMC_IDR    : Unsigned_32;
      PMC_SR     : Unsigned_32;
      PMC_IMR    : Unsigned_32;

      PMC_FSMR   : Unsigned_32;
      PMC_FSPR   : Unsigned_32;
      PMC_FOCR   : Unsigned_32;
      Pad7_C : Unsigned_32;

      --  Not complete
   end record;

   PMC : PMC_Registers with Volatile, Import,
                            Address => System'To_Address (PMC_Base);

   package CKGR_MOR is
      CFDEN    : constant := 2 ** 25;
      MOSCSEL  : constant := 2 ** 24;
      KEY      : constant := 16#37# * 2 ** 16;

      MOSCXTST : constant := 2 ** 8;

      MOSCRCEN : constant := 2 ** 3;
      WAITMODE : constant := 2 ** 2;
      MOSCXTBY : constant := 2 ** 1;
      MOSCXTEN : constant := 2 ** 0;
   end CKGR_MOR;

   package PMC_SR is
      MCKRDY   : constant := 2 ** 3;
      LOCKB    : constant := 2 ** 2;
      LOCKA    : constant := 2 ** 1;
      MOSCXTS  : constant := 2 ** 0;
   end PMC_SR;

   package CKGR_PLLxR is
      DIV      : constant := 2 ** 0;
      PLLCOUNT : constant := 2 ** 8;
      MUL      : constant := 2 ** 16;
      ONE      : constant := 2 ** 29;
   end CKGR_PLLxR;

   package PMC_MCKR is
      PLLBDIV2 : constant := 2 ** 13;
      PLLADIV2 : constant := 2 ** 12;
      PRES_Mask : constant := 2#111# * 2 ** 4;
      CLK_1  : constant := 0 * 2 ** 4;
      CLK_2  : constant := 1 * 2 ** 4;
      CLK_4  : constant := 2 * 2 ** 4;
      CLK_8  : constant := 3 * 2 ** 4;
      CLK_16 : constant := 4 * 2 ** 4;
      CLK_32 : constant := 5 * 2 ** 4;
      CLK_64 : constant := 6 * 2 ** 4;
      CLK_3  : constant := 7 * 2 ** 4;
      CSS_Mask : constant := 2#11# * 2 ** 0;
      SLOW_CLK : constant := 0 * 2 ** 0;
      MAIN_CLK : constant := 1 * 2 ** 0;
      PLLA_CLK : constant := 2 * 2 ** 0;
      PLLB_CLK : constant := 3 * 2 ** 0;
   end PMC_MCKR;

   type EEFC_Registers is record
      EEFC_FMR : Unsigned_32;
      EEFC_FCR : Unsigned_32;
      EEFC_FSR : Unsigned_32;
      EEFC_FFR : Unsigned_32;
   end record;

   package EEFC_FMR is
      FRDY : constant := 2 ** 0;
      FWS  : constant := 2 ** 8;
      SCOD : constant := 2 ** 16;
      FAM  : constant := 2 ** 24;
      CLOE : constant := 2 ** 26;
   end EEFC_FMR;

   EFC0 : EEFC_Registers with Volatile, Import,
                            Address => System'To_Address (EFC0_Base);

   type PIO_Registers is record
      PER : Unsigned_32;
      PDR : Unsigned_32;
      PSR : Unsigned_32;
      Pad0 : Unsigned_32;

      OER : Unsigned_32;
      ODR : Unsigned_32;
      OSR : Unsigned_32;
      Pad1 : Unsigned_32;

      IFER : Unsigned_32;
      IFDR : Unsigned_32;
      IFSR : Unsigned_32;
      Pad2 : Unsigned_32;

      SODR : Unsigned_32;
      CODR : Unsigned_32;
      ODSR : Unsigned_32;
      PDSR : Unsigned_32;

      IER : Unsigned_32;
      IDR : Unsigned_32;
      IMR : Unsigned_32;
      ISR : Unsigned_32;

      MDER : Unsigned_32;
      MDDR : Unsigned_32;
      MDSR : Unsigned_32;
      Pad5 : Unsigned_32;

      PUDR : Unsigned_32;
      PUER : Unsigned_32;
      PUSR : Unsigned_32;
      Pad6 : Unsigned_32;

      ABCDSR1 : Unsigned_32;
      ABCDSR2 : Unsigned_32;
      Pad7_8 : Unsigned_32;
      Pad7_C : Unsigned_32;

      IFSCDR : Unsigned_32;
      IFSCER : Unsigned_32;
      IFSCSR : Unsigned_32;
      SCDR : Unsigned_32;

      PPDDR : Unsigned_32;
      PPDER : Unsigned_32;
      PPDSR : Unsigned_32;
      Pad9 : Unsigned_32;

      OWER : Unsigned_32;
      OWDR : Unsigned_32;
      OWSR : Unsigned_32;
      Pada : Unsigned_32;

      AIMER : Unsigned_32;
      AIMDR : Unsigned_32;
      AIMMR : Unsigned_32;
      Padb  : Unsigned_32;

      ESR : Unsigned_32;
      LSR : Unsigned_32;
      ELSR : Unsigned_32;
      Padc : Unsigned_32;

      FELLSR : Unsigned_32;
      REHLSR : Unsigned_32;
      FRLHSR : Unsigned_32;
      Padd   : Unsigned_32;

      LOCKSR : Unsigned_32;
      WPMR   : Unsigned_32;
      WPSR   : Unsigned_32;
      PadE_C : Unsigned_32;
   end record;

   PIOA : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOA_Base);
   PIOC : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOC_Base);

   PIOA_ID : constant := 11;
   PIOC_ID : constant := 13;

   type SPI_Registers is record
      SPI_CR  : Unsigned_32;
      SPI_MR  : Unsigned_32;
      SPI_RDR : Unsigned_32;
      SPI_TDR : Unsigned_32;

      SPI_SR : Unsigned_32;
      SPI_IER : Unsigned_32;
      SPI_IDR : Unsigned_32;
      SPI_IMR : Unsigned_32;

      Pad_20 : Unsigned_32;
      Pad_24 : Unsigned_32;
      Pad_28 : Unsigned_32;
      Pad_2c : Unsigned_32;

      SPI_CSR0 : Unsigned_32;
      SPI_CSR1 : Unsigned_32;
      SPI_CSR2 : Unsigned_32;
      SPI_CSR3 : Unsigned_32;

      --  ...
   end record;

   package SPI_CR is
      SPIEN  : constant := 2 ** 0;
      SPIDIS : constant := 2 ** 1;
      SWRST  : constant := 2 ** 7;

      LASTXFER : constant := 2 ** 24;
   end SPI_CR;

   package SPI_MR is
      MSTR    : constant := 2 ** 0;
      PS      : constant := 2 ** 1;
      PCSDEC  : constant := 2 ** 2;
      MODFDIS : constant := 2 ** 4;
      WDRBT   : constant := 2 ** 5;
      LLB     : constant := 2 ** 7;

      PCS         : constant := 2 ** 16;
      PCS_Mask    : constant := 2#1111# * PCS;
      DLYBCS      : constant := 2 ** 16;
      DLYBCS_Mask : constant := 16#ff# * DLYBCS;
   end SPI_MR;

   package SPI_TDR is
      TD       : constant := 2 ** 0;
      PCS      : constant := 2 ** 16;
      LASTXFER : constant := 2 ** 24;
   end SPI_TDR;

   package SPI_SR is
      RDRF    : constant := 2 ** 0;
      TDRE    : constant := 2 ** 1;
      MODF    : constant := 2 ** 2;
      OVRES   : constant := 2 ** 3;
      ENDRX   : constant := 2 ** 4;
      ENDTX   : constant := 2 ** 5;
      RXBUFF  : constant := 2 ** 6;
      TXBUFE  : constant := 2 ** 7;
      NSSR    : constant := 2 ** 8;
      TXEMPTY : constant := 2 ** 9;
      UNDES   : constant := 2 ** 10;
      SPIENS  : constant := 2 ** 16;
   end SPI_SR;

   package SPI_CSR is
      CPOL   : constant := 2 ** 0;
      NCPHA  : constant := 2 ** 1;
      CSNAAT : constant := 2 ** 2;
      CSAAT  : constant := 2 ** 3;
      BITS   : constant := 2 ** 4;
      SCBR   : constant := 2 ** 8;
      DLYBS  : constant := 2 ** 16;
      DLYBCT : constant := 2 ** 24;
   end SPI_CSR;

   SPI : SPI_Registers with Volatile, Import,
                             Address => System'To_Address (SPI_Base);

   SPI_ID : constant := 21;
end Sam4s;
