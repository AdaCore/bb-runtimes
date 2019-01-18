------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . S A M 3 X 8 S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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

--  This file provides register definitions for the Sam3x8e (ARM Cortex M3)
--  microcontrollers from Atmel. Definitions are taken from 'Sam3x8e Series'
--  datasheet (document Atmel-11057C-ATARM-SAM3X-SAM3A-Datasheet_23-Mar-15).

--
--  This package should be called "System.Sam3x8e", but after lots of testing,
--  it seems that for some reason it just wouldn't find the package.  Calling
--  the package "System.Sam3x8" does work.
--
package System.Sam3x8 is
   pragma No_Elaboration_Code_All;
   --  Allow user code with pragma No_Elaboration_Code_All to use this package

   pragma Preelaborate (System.Sam3x8);

   pragma Suppress (Alignment_Check);
   --  Avoid any warnings for address clauses on variables of type record.

   type Word is mod 2**32;

   --  Define address bases for various peripherals

   Peripheral_Base : constant := 16#4000_0000#;

   SPI0_Base : constant := Peripheral_Base + 16#8000#;
   SPI1_Base : constant := Peripheral_Base + 16#c000#;
   USART0_Base : constant := Peripheral_Base + 16#9_8000#;
   USART1_Base : constant := Peripheral_Base + 16#9_c000#;
   USART2_Base : constant := Peripheral_Base + 16#a_0000#;
   USART3_Base : constant := Peripheral_Base + 16#a_4000#;

   System_Controller_Base : constant := Peripheral_Base + 16#e_0000#;
   PMC_Base   : constant := System_Controller_Base + 16#0600#;
   UART_Base  : constant := System_Controller_Base + 16#0800#;
   EFC0_Base  : constant := System_Controller_Base + 16#0A00#;
   EFC1_Base  : constant := System_Controller_Base + 16#0C00#;

   PIOA_Base  : constant := System_Controller_Base + 16#0e00#;
   PIOB_Base  : constant := System_Controller_Base + 16#1000#;
   PIOC_Base  : constant := System_Controller_Base + 16#1200#;
   PIOD_Base  : constant := System_Controller_Base + 16#1400#;
   PIOE_Base  : constant := System_Controller_Base + 16#1600#;
   PIOF_Base  : constant := System_Controller_Base + 16#1800#;
   WDT_Base   : constant := System_Controller_Base + 16#1a50#;

   ---------------------------------
   -- Power Management Controller --
   ---------------------------------

   type PMC_Registers is record
      PMC_SCER   : Word;
      PMC_SCDR   : Word;
      PMC_SCSR   : Word;
      Pad0c      : Word;

      PMC_PCER0  : Word;
      PMC_PCDR0  : Word;
      PMC_PCSR0  : Word;
      CKGR_UCKR  : Word;

      CKGR_MOR   : Word;
      CKGR_MCFR  : Word;
      CKGR_PLLAR : Word;
      Pad2c      : Word;

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
--      LOCKB    : constant := 2 ** 2; -- Not in sam3x8e
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
--      PLLBDIV2 : constant := 2 ** 13;
      UPLLDIV2 : constant := 2 ** 13;
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
      UPLL_CLK : constant := 3 * 2 ** 0;
--      PLLB_CLK : constant := 3 * 2 ** 0;
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
--      SCOD : constant := 2 ** 16;
      FAM  : constant := 2 ** 24;
--      CLOE : constant := 2 ** 26;
   end EEFC_FMR;

   EFC0 : EEFC_Registers with Volatile, Import,
                            Address => System'To_Address (EFC0_Base);

   --------------------------------------
   -- Parallel Input/Output Controller --
   --------------------------------------

--  <<<working/checking>>>  --
   type PIO_Registers is record
      PER     : Word;
      PDR     : Word;
      PSR     : Word;
      Pad0C   : Word;

      OER     : Word;
      ODR     : Word;
      OSR     : Word;
      Pad1C   : Word;

      IFER    : Word;
      IFDR    : Word;
      IFSR    : Word;
      Pad2C   : Word;

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
      Pad5C   : Word;

      PUDR    : Word;
      PUER    : Word;
      PUSR    : Word;
      Pad6C   : Word;

      ABSR    : Word;
      Pad74   : Word;
      Pad78   : Word;
      Pad7C   : Word;

      SCIFSR  : Word;
      DIFSR  : Word;
      IFSCSR  : Word;
      SCDR    : Word;

      Pad90   : Word;
      Pad94   : Word;
      Pad98   : Word;
      Pad9C   : Word;

      OWER    : Word;
      OWDR    : Word;
      OWSR    : Word;
      PadAC   : Word;

      AIMER   : Word;
      AIMDR   : Word;
      AIMMR   : Word;
      PadBC   : Word;

      ESR     : Word;
      LSR     : Word;
      ELSR    : Word;
      PadCC   : Word;

      FELLSR  : Word;
      REHLSR  : Word;
      FRLHSR  : Word;
      PadDC   : Word;

      LOCKSR  : Word;
      WPMR    : Word;
      WPSR    : Word;
--      PadEC   : Word;
   end record;

   PIOA : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOA_Base);
   PIOB : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOB_Base);
   PIOC : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOC_Base);
   PIOD : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOD_Base);
   PIOE : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOE_Base);
   PIOF : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOF_Base);

   PIOA_ID : constant := 11;
   PIOB_ID : constant := 12;
   PIOC_ID : constant := 13;
   PIOD_ID : constant := 14;
   PIOE_ID : constant := 15;
   PIOF_ID : constant := 16;

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

   --  Constants for the SPI RDR register

   package SPI_DDR is
      RD       : constant := 2 ** 0;
      PCS      : constant := 2 ** 16;
   end SPI_DDR;

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
--      ENDRX   : constant := 2 ** 4;
--      ENDTX   : constant := 2 ** 5;
--      RXBUFF  : constant := 2 ** 6;
--      TXBUFE  : constant := 2 ** 7;
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

   SPI0 : SPI_Registers with Volatile, Import,
                            Address => System'To_Address (SPI0_Base);

   SPI1 : SPI_Registers with Volatile, Import,
                            Address => System'To_Address (SPI1_Base);

   SPI0_ID : constant := 24;
   SPI1_ID : constant := 25;

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
      WDFIEN    : constant := 2 ** 12;
      WDRSTEN   : constant := 2 ** 13;
      WDDPROC   : constant := 2 ** 14;
      WDDIS     : constant := 2 ** 15;
      WDD       : constant := 2 ** 16;
      WDDBGHLT  : constant := 2 ** 28;
      WDIDLEHLT : constant := 2 ** 29;
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

   UART : UART_Registers with Volatile, Import,
                               Address => System'To_Address (UART_Base);

   UART_ID : constant := 8;

   -------------------------------------------------------------
   -- Universal Synchronous Asynchronous Receiver Transmitter --
   -------------------------------------------------------------

   type USART_Registers is record
      US_CR    : Word;
      US_MR    : Word;
      US_IER   : Word;
      US_IDR   : Word;

      US_IMR   : Word;
      US_SR    : Word;
      US_RHR   : Word;
      US_THR   : Word;

      US_BRGR  : Word;
      US_RTOR  : Word;
      US_TTGR  : Word;
      Pad2_c   : Word;

      Pad3_0   : Word;
      Pad3_4   : Word;
      Pad3_8   : Word;
      Pad3_c   : Word;

      US_FIDI  : Word;
      US_NER   : Word;
      Pad4_8   : Word;
      US_IF    : Word;

      US_MAN   : Word;
      US_LINMR : Word;
      US_LINIR : Word;
      US_WPMR : Word;

      US_WPSR : Word;
   end record;

   --  Constants for the USART CR register

   package US_CR is
      RSTRX  : constant := 2 ** 2;
      RSTTX  : constant := 2 ** 3;
      RXEN   : constant := 2 ** 4;
      RXDIS  : constant := 2 ** 5;
      TXEN   : constant := 2 ** 6;
      TXDIS  : constant := 2 ** 7;
      RSTSTA : constant := 2 ** 8;
      STTBRK : constant := 2 ** 9;
      STPBRK : constant := 2 ** 10;
      STTTO  : constant := 2 ** 11;
      SENDA  : constant := 2 ** 12;
      RSTIT  : constant := 2 ** 13;
      RSTNAK : constant := 2 ** 14;
      RETTO  : constant := 2 ** 15;
      RSTEN_FCS : constant := 2 ** 18;
      RSTDIS_RCS : constant := 2 ** 19;
      LINABT : constant := 2 ** 20;
      LINWKUP : constant := 2 ** 21;
   end US_CR;

   --  Constants for the UART MR register

   package US_MR is
      USART_MODE_NORMAL         : constant := 16#0#;
      USART_MODE_RS485          : constant := 16#1#;
      USART_MODE_HW_HANDSHAKING : constant := 16#2#;
      USART_MODE_ISO7816_T_0    : constant := 16#4#;
      USART_MODE_ISO7816_T_1    : constant := 16#6#;
      USART_MODE_IRDA           : constant := 16#8#;
      USART_MODE_LIN_MASTER     : constant := 16#A#;
      USART_MODE_LIN_SLAVE      : constant := 16#B#;
      USART_MODE_SPI_MASTER     : constant := 16#E#;
      USART_MODE_SPI_SLAVE      : constant := 16#F#;

      USCLKS_MCK  : constant := 0 * 2 ** 4;
      USCLKS_DIV  : constant := 1 * 2 ** 4;
      USCLKS_SCK  : constant := 3 * 2 ** 4;

      CHRL_5_BIT : constant := 0 * 2 ** 6;
      CHRL_6_BIT : constant := 1 * 2 ** 6;
      CHRL_7_BIT : constant := 2 * 2 ** 6;
      CHRL_8_BIT : constant := 3 * 2 ** 6;

      SYNC_CPHA : constant := 2 ** 8;

      PAR_EVEN  : constant := 0 * 2 ** 9;
      PAR_ODD   : constant := 1 * 2 ** 9;
      PAR_SPACE : constant := 2 * 2 ** 9;
      PAR_MARK  : constant := 3 * 2 ** 9;
      PAR_NO    : constant := 4 * 2 ** 9;
      PAR_MULTIDROP : constant := 6 * 2 ** 9;

      NBSTOP_1_BIT   : constant := 0 * 2 ** 12;
      NBSTOP_1_5_BIT : constant := 1 * 2 ** 12;
      NBSTOP_2_BIT   : constant := 2 * 2 ** 12;

      CHMODE_NORMAL          : constant := 0 * 2 ** 14;
      CHMODE_AUTOMATIC       : constant := 1 * 2 ** 14;
      CHMODE_LOCAL_LOOPBACK  : constant := 2 * 2 ** 14;
      CHMODE_REMOTE_LOOPBACK : constant := 3 * 2 ** 14;

      MSBF_CPOL : constant := 2 ** 16;
      MODE9     : constant := 2 ** 17;
      CLKO      : constant := 2 ** 18;
      OVER      : constant := 2 ** 19;

      INACK     : constant := 2 ** 20;
      DSNACK    : constant := 2 ** 21;
      VAR_SYNC  : constant := 2 ** 22;
      INVDATA   : constant := 2 ** 23;

      MAX_ITERATION : constant := 2 ** 24;  --  3 bit field

      FILTER    : constant := 2 ** 28;
      MAN       : constant := 2 ** 29;
      MODSYNC   : constant := 2 ** 30;
      ONEBIT    : constant := 2 ** 31;
   end US_MR;

   --  Constants for the USART IER, IDR, and IMR
   --  Interrupt enable, disable, and mask registers)

   package US_IER_IDR_IMR is
      RSRDY   : constant := 2 ** 0;
      TXRDY   : constant := 2 ** 1;
      RXBRK   : constant := 2 ** 2;
      ENDRX   : constant := 2 ** 3;

      ENDTX   : constant := 2 ** 4;
      OVRE    : constant := 2 ** 5;
      FRAME   : constant := 2 ** 6;
      PARE    : constant := 2 ** 7;

      TIMEOUT : constant := 2 ** 8;
      TXEMPTY : constant := 2 ** 9;
      ITER_UNRE : constant := 2 ** 10;
      TXBUFE  : constant := 2 ** 11;

      RXBUFF  : constant := 2 ** 12;
      NACK_LINBK : constant := 2 ** 13;
      LINID   : constant := 2 ** 14;
      LINTC   : constant := 2 ** 15;

      CTSIC   : constant := 2 ** 19;
      MANE    : constant := 2 ** 24;
      LINBE   : constant := 2 ** 25;
      LINISFE : constant := 2 ** 26;

      LINIPE  : constant := 2 ** 27;
      LINCE   : constant := 2 ** 28;
      LINSNRE : constant := 2 ** 29;
   end US_IER_IDR_IMR;

   --  Constants for the USART SR status register

   package US_SR is
      RXRDY   : constant := 2 ** 0;
      TXRDY   : constant := 2 ** 1;
      ENDRX   : constant := 2 ** 3;
      ENDTX   : constant := 2 ** 4;
      OVRE    : constant := 2 ** 5;
      FRAME   : constant := 2 ** 6;
      PARE    : constant := 2 ** 7;

      TIMEOUT : constant := 2 ** 8;
      TXEMPTY : constant := 2 ** 9;
      ITER_UNRE : constant := 2 ** 10;
      TXBUFE  : constant := 2 ** 11;
      RXBUFF  : constant := 2 ** 12;
      NACK_LINBK : constant := 2 ** 13;
      LINID   : constant := 2 ** 14;
      LINTC   : constant := 2 ** 15;

      CTSIC   : constant := 2 ** 19;
      CTS_LINBLS : constant := 2 ** 23;
      MANERR  : constant := 2 ** 24;
      LINBE   : constant := 2 ** 25;
      LINISFE : constant := 2 ** 26;
      LINIPE  : constant := 2 ** 27;
      LINCE   : constant := 2 ** 28;
      LINSNRE : constant := 2 ** 29;
   end US_SR;

   --  Constants for USART RHR register

   package US_RHR is
      RXCHR  : constant := 16#1FF#;
      RXSYNH : constant := 2 ** 15;
   end US_RHR;

   --  Constants for USART THR register

   package US_THR is
      TXCHR  : constant := 16#1FF#;
      TXSYNH : constant := 2 ** 15;
   end US_THR;

   --  Constants for USART BRGR register

   package US_BRGR is
      CD  : constant := 2 ** 0;
      FP  : constant := 2 ** 16;
   end US_BRGR;

   --  Constants for USART Manchester Configuration Register

   package US_MAN is
      TX_PL   : constant := 2 ** 0;
      TX_PP   : constant := 2 ** 8;
      TX_MPOL : constant := 2 ** 12;
      RX_PL   : constant := 2 ** 16;
      RX_PP   : constant := 2 ** 24;
      RX_MPOL : constant := 2 ** 28;
      DRIFT   : constant := 2 ** 30;
   end US_MAN;

   --  Constants for USART LIN Mode Register

   package US_LINMR is
      PUBLISH   : constant := 0;
      SUBSCRIBE : constant := 1;
      IGNORE    : constant := 2;

      PARDIS    : constant := 2 ** 2;
      CHKDIS    : constant := 2 ** 3;
      CHKTYP    : constant := 2 ** 4;
      DLM       : constant := 2 ** 5;
      FSDIS     : constant := 2 ** 6;
      WKUPTYP   : constant := 2 ** 7;

      DLC       : constant := 2 ** 8;
      PDCM      : constant := 2 ** 16;
   end US_LINMR;

   --  Constants for USART Write Protect Mode Register

   package US_WPMR is
      WPEN   : constant := 2 ** 0;
      WPKEY  : constant := 16#555341# * 2 ** 8;
   end US_WPMR;

   --  Constants for USART Write Protect Status Register

   package US_WPSR is
      WPVS   : constant := 2 ** 0;
      WPVSRC : constant := 2 ** 8;
   end US_WPSR;

   USART0 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (USART0_Base);
   USART1 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (USART1_Base);
   USART2 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (USART2_Base);
   USART3 : UART_Registers with Volatile, Import,
                               Address => System'To_Address (USART3_Base);

   USART0_ID : constant := 17;
   USART1_ID : constant := 18;
   USART2_ID : constant := 19;
   USART3_ID : constant := 20;

end System.Sam3x8;
