------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . S A M 4 S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

--  This file provides register definitions for the SAM4S (ARM Cortex M4)
--  microcontrollers from Atmel. Definitions are taken from 'SAM4S Series'
--  datasheet (document 11100E-ATARM-24-Jul-13).

package System.SAM4S is
   pragma No_Elaboration_Code_All;
   --  Allow user code with pragma No_Elaboration_Code_All to use this package

   pragma Preelaborate (System.SAM4S);

   pragma Suppress (Alignment_Check);
   --  Avoid any warnings for address clauses on variables of type record.

   type Word is mod 2**32;

   --  Define address bases for various peripherals

   Peripheral_Base : constant := 16#4000_0000#;

   SPI_Base : constant := Peripheral_Base + 16#8000#;

   System_Controller_Base : constant := Peripheral_Base + 16#e_0000#;
   PMC_Base   : constant := System_Controller_Base + 16#0400#;
   UART0_Base : constant := System_Controller_Base + 16#0600#;
   UART1_Base : constant := System_Controller_Base + 16#0800#;
   EFC0_Base  : constant := System_Controller_Base + 16#0A00#;
   EFC1_Base  : constant := System_Controller_Base + 16#0C00#;

   PIOA_Base  : constant := System_Controller_Base + 16#0e00#;
   PIOB_Base  : constant := System_Controller_Base + 16#1000#;
   PIOC_Base  : constant := System_Controller_Base + 16#1200#;
   WDT_Base   : constant := System_Controller_Base + 16#1450#;

   ---------------------------------
   -- Power Management Controller --
   ---------------------------------

   type PMC_Registers is record
      PMC_SCER   : Word;
      PMC_SCDR   : Word;
      PMC_SCSR   : Word;
      Pad0       : Word;

      PMC_PCER0  : Word;
      PMC_PCDR0  : Word;
      PMC_PCSR0  : Word;
      Pad1       : Word;

      CKGR_MOR   : Word;
      CKGR_MCFR  : Word;
      CKGR_PLLAR : Word;
      CKGR_PLLBR : Word;

      PMC_MCKR   : Word;
      Pad3_4     : Word;
      PMC_USB    : Word;
      Pad3_C     : Word;

      PMC_PCK0   : Word;
      PMC_PCK1   : Word;
      PMC_PCK2   : Word;
      Pad4_C     : Word;

      Pad5_0     : Word;
      Pad5_4     : Word;
      Pad5_8     : Word;
      Pad5_C     : Word;

      PMC_IER    : Word;
      PMC_IDR    : Word;
      PMC_SR     : Word;
      PMC_IMR    : Word;

      PMC_FSMR   : Word;
      PMC_FSPR   : Word;
      PMC_FOCR   : Word;
      Pad7_C     : Word;

      --  Not complete
   end record;

   PMC : PMC_Registers with Volatile, Import,
                            Address => System'To_Address (PMC_Base);

   --  Constants for the CKGR MOR register

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

   --  Constants for the PMC SR register

   package PMC_SR is
      MCKRDY   : constant := 2 ** 3;
      LOCKB    : constant := 2 ** 2;
      LOCKA    : constant := 2 ** 1;
      MOSCXTS  : constant := 2 ** 0;
   end PMC_SR;

   --  Constants for the CKGR PLLAR and PLLBR registers

   package CKGR_PLLxR is
      DIV      : constant := 2 ** 0;
      PLLCOUNT : constant := 2 ** 8;
      MUL      : constant := 2 ** 16;
      ONE      : constant := 2 ** 29;
   end CKGR_PLLxR;

   --  Constants for the PMC MCKR register

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

   ----------------------------------------
   -- Enhanced Embedded Flash Controller --
   ----------------------------------------

   type EEFC_Registers is record
      EEFC_FMR : Word;
      EEFC_FCR : Word;
      EEFC_FSR : Word;
      EEFC_FFR : Word;
   end record;

   --  Constants for the EEFC FMR register

   package EEFC_FMR is
      FRDY : constant := 2 ** 0;
      FWS  : constant := 2 ** 8;
      SCOD : constant := 2 ** 16;
      FAM  : constant := 2 ** 24;
      CLOE : constant := 2 ** 26;
   end EEFC_FMR;

   EFC0 : EEFC_Registers with Volatile, Import,
                            Address => System'To_Address (EFC0_Base);

   --------------------------------------
   -- Parallel Input/Output Controller --
   --------------------------------------

   type PIO_Registers is record
      PER     : Word;
      PDR     : Word;
      PSR     : Word;
      Pad0    : Word;

      OER     : Word;
      ODR     : Word;
      OSR     : Word;
      Pad1    : Word;

      IFER    : Word;
      IFDR    : Word;
      IFSR    : Word;
      Pad2    : Word;

      SODR    : Word;
      CODR    : Word;
      ODSR    : Word;
      PDSR    : Word;

      IER     : Word;
      IDR     : Word;
      IMR     : Word;
      ISR     : Word;

      MDER    : Word;
      MDDR    : Word;
      MDSR    : Word;
      Pad5    : Word;

      PUDR    : Word;
      PUER    : Word;
      PUSR    : Word;
      Pad6    : Word;

      ABCDSR1 : Word;
      ABCDSR2 : Word;
      Pad7_8  : Word;
      Pad7_C  : Word;

      IFSCDR  : Word;
      IFSCER  : Word;
      IFSCSR  : Word;
      SCDR    : Word;

      PPDDR   : Word;
      PPDER   : Word;
      PPDSR   : Word;
      Pad9    : Word;

      OWER    : Word;
      OWDR    : Word;
      OWSR    : Word;
      Pada    : Word;

      AIMER   : Word;
      AIMDR   : Word;
      AIMMR   : Word;
      Padb    : Word;

      ESR     : Word;
      LSR     : Word;
      ELSR    : Word;
      Padc    : Word;

      FELLSR  : Word;
      REHLSR  : Word;
      FRLHSR  : Word;
      Padd    : Word;

      LOCKSR  : Word;
      WPMR    : Word;
      WPSR    : Word;
      PadE_C  : Word;
   end record;

   PIOA : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOA_Base);
   PIOB : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOB_Base);
   PIOC : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOC_Base);

   PIOA_ID : constant := 11;
   PIOB_ID : constant := 12;
   PIOC_ID : constant := 13;

   ----------------------------------
   --  Serial Peripheral Interface --
   ----------------------------------

   type SPI_Registers is record
      SPI_CR   : Word;
      SPI_MR   : Word;
      SPI_RDR  : Word;
      SPI_TDR  : Word;

      SPI_SR   : Word;
      SPI_IER  : Word;
      SPI_IDR  : Word;
      SPI_IMR  : Word;

      Pad_20   : Word;
      Pad_24   : Word;
      Pad_28   : Word;
      Pad_2c   : Word;

      SPI_CSR0 : Word;
      SPI_CSR1 : Word;
      SPI_CSR2 : Word;
      SPI_CSR3 : Word;

      --  ...
   end record;

   --  Constants for the SPI CR register

   package SPI_CR is
      SPIEN  : constant := 2 ** 0;
      SPIDIS : constant := 2 ** 1;
      SWRST  : constant := 2 ** 7;

      LASTXFER : constant := 2 ** 24;
   end SPI_CR;

   --  Constants for the SPI MR register

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

   --  Constants for the SPI TDR register

   package SPI_TDR is
      TD       : constant := 2 ** 0;
      PCS      : constant := 2 ** 16;
      LASTXFER : constant := 2 ** 24;
   end SPI_TDR;

   --  Constants for the SPI SR register; also used by the SPI IER, IDR and
   --  IMR registers.

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

   --  Constants for the SPI CSR register

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

   --------------------
   -- Watchdog Timer --
   --------------------

   type WDT_Registers is record
      WDT_CR : Word;
      WDT_MR : Word;
      WDT_SR : Word;
   end record;

   --  Constants for the WDT CR register

   package WDT_CR is
      KEY    : constant := 16#a5_00_00_00#;
      WDRSTT : constant := 2 ** 0;
   end WDT_CR;

   --  Constants for the WDT MR register

   package WDT_MR is
      WDV       : constant := 2 ** 0;
      WDFIEN    : constant := 2 ** 2;
      WDRSTEN   : constant := 2 ** 13;
      WDDPROC   : constant := 2 ** 14;
      WDDIS     : constant := 2 ** 15;
      WDD       : constant := 2 ** 16;
      WDDBGHLT  : constant := 2 ** 13;
      WDIDLEHLT : constant := 2 ** 13;
   end WDT_MR;

   WDT : WDT_Registers with Volatile, Import,
                             Address => System'To_Address (WDT_Base);

   -------------------------------------------------
   -- Universal Asynchronous Receiver Transmitter --
   -------------------------------------------------

   type UART_Registers is record
      UART_CR   : Word;
      UART_MR   : Word;
      UART_IER  : Word;
      UART_IDR  : Word;

      UART_IMR  : Word;
      UART_SR   : Word;
      UART_RHR  : Word;
      UART_THR  : Word;

      UART_BRGR : Word;
   end record;

   --  Constants for the UART CR register

   package UART_CR is
      RSTRX  : constant := 2 ** 2;
      RSTTX  : constant := 2 ** 3;
      RXEN   : constant := 2 ** 4;
      RXDIS  : constant := 2 ** 5;
      TXEN   : constant := 2 ** 6;
      TXDIS  : constant := 2 ** 7;
      RSTSTA : constant := 2 ** 8;
   end UART_CR;

   --  Constants for the UART MR register

   package UART_MR is
      CHMODE_NORMAL          : constant := 0 * 2 ** 14;
      CHMODE_AUTOMATIC       : constant := 1 * 2 ** 14;
      CHMODE_LOCAL_LOOPBACK  : constant := 2 * 2 ** 14;
      CHMODE_REMOTE_LOOPBACK : constant := 3 * 2 ** 14;

      PAR_EVEN  : constant := 0 * 2 ** 9;
      PAR_ODD   : constant := 1 * 2 ** 9;
      PAR_SPACE : constant := 2 * 2 ** 9;
      PAR_MARK  : constant := 3 * 2 ** 9;
      PAR_NO    : constant := 4 * 2 ** 9;
   end UART_MR;

   --  Constants for the UART SR register

   package UART_SR is
      RXRDY   : constant := 2 ** 0;
      TXRDY   : constant := 2 ** 1;
      ENDRX   : constant := 2 ** 3;
      ENDTX   : constant := 2 ** 4;
      OVRE    : constant := 2 ** 5;
      FRAME   : constant := 2 ** 6;
      PARE    : constant := 2 ** 7;
      TXEMPTY : constant := 2 ** 9;
      TXBUFE  : constant := 2 ** 11;
      RXBUFF  : constant := 2 ** 12;
   end UART_SR;

   UART0 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (UART0_Base);
   UART1 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (UART1_Base);

   UART0_ID : constant := 8;
   UART1_ID : constant := 9;
end System.SAM4S;
