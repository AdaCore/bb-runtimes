------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with Interfaces; use Interfaces;
with System;

procedure Sys_Startup is
   --  SYS

   CSDISCLR : Unsigned_32;
   for CSDISCLR'Address use System'To_Address (16#ffff_ff00# + 16#38#);
   pragma Volatile (CSDISCLR);
   pragma Import (Ada, CSDISCLR);

   SYS1_GHVSRC : Unsigned_32;
   for SYS1_GHVSRC'Address use System'To_Address (16#ffff_ff00# + 16#48#);
   pragma Volatile (SYS1_GHVSRC);
   pragma Import (Ada, SYS1_GHVSRC);

   SYS1_VCLKASRC : Unsigned_32;
   for SYS1_VCLKASRC'Address use System'To_Address (16#ffff_ff00# + 16#4c#);
   pragma Volatile (SYS1_VCLKASRC);
   pragma Import (Ada, SYS1_VCLKASRC);

   SYS1_RCLKSRC : Unsigned_32;
   for SYS1_RCLKSRC'Address use System'To_Address (16#ffff_ff00# + 16#50#);
   pragma Volatile (SYS1_RCLKSRC);
   pragma Import (Ada, SYS1_RCLKSRC);

   SYS1_CSVSTAT : Unsigned_32;
   for SYS1_CSVSTAT'Address use System'To_Address (16#ffff_ff00# + 16#54#);
   pragma Volatile (SYS1_CSVSTAT);
   pragma Import (Ada, SYS1_CSVSTAT);

   SYS1_MSTGCR : Unsigned_32;
   for SYS1_MSTGCR'Address use System'To_Address (16#ffff_ff00# + 16#58#);
   pragma Volatile (SYS1_MSTGCR);
   pragma Import (Ada, SYS1_MSTGCR);

   SYS1_MINITGCR : Unsigned_32;
   for SYS1_MINITGCR'Address use System'To_Address (16#ffff_ff00# + 16#5c#);
   pragma Volatile (SYS1_MINITGCR);
   pragma Import (Ada, SYS1_MINITGCR);

   SYS1_MSINENA : Unsigned_32;
   for SYS1_MSINENA'Address use System'To_Address (16#ffff_ff00# + 16#60#);
   pragma Volatile (SYS1_MSINENA);
   pragma Import (Ada, SYS1_MSINENA);

   SYS1_MSTCGSTAT : Unsigned_32;
   for SYS1_MSTCGSTAT'Address use System'To_Address (16#ffff_ff00# + 16#68#);
   pragma Volatile (SYS1_MSTCGSTAT);
   pragma Import (Ada, SYS1_MSTCGSTAT);

   SYS1_MINISTAT : Unsigned_32;
   for SYS1_MINISTAT'Address use System'To_Address (16#ffff_ff00# + 16#6c#);
   pragma Volatile (SYS1_MINISTAT);
   pragma Import (Ada, SYS1_MINISTAT);

   SYS1_PLLCTL1 : Unsigned_32;
   for SYS1_PLLCTL1'Address use System'To_Address (16#ffff_ff00# + 16#70#);
   pragma Volatile (SYS1_PLLCTL1);
   pragma Import (Ada, SYS1_PLLCTL1);

   PLLCTL2 : Unsigned_32;
   for PLLCTL2'Address use System'To_Address (16#ffff_ff00# + 16#74#);
   pragma Volatile (PLLCTL2);
   pragma Import (Ada, PLLCTL2);

   SYS1_LPOMONCTL : Unsigned_32;
   for SYS1_LPOMONCTL'Address use System'To_Address (16#ffff_ff00# + 16#88#);
   pragma Volatile (SYS1_LPOMONCTL);
   pragma Import (Ada, SYS1_LPOMONCTL);

   SYS1_CLKCNTL : Unsigned_32;
   for SYS1_CLKCNTL'Address use System'To_Address (16#ffff_ff00# + 16#d0#);
   pragma Volatile (SYS1_CLKCNTL);
   pragma Import (Ada, SYS1_CLKCNTL);

   SYS1_SYSESR : Unsigned_32;
   for SYS1_SYSESR'Address use System'To_Address (16#ffff_ff00# + 16#e4#);
   pragma Volatile (SYS1_SYSESR);
   pragma Import (Ada, SYS1_SYSESR);

   --  SYS2

   SYS2_PLLCTL3 : Unsigned_32;
   for SYS2_PLLCTL3'Address use System'To_Address (16#ffff_e100# + 16#00#);
   pragma Volatile (SYS2_PLLCTL3);
   pragma Import (Ada, SYS2_PLLCTL3);

   SYS2_STCLKDIV : Unsigned_32;
   for SYS2_STCLKDIV'Address use System'To_Address (16#ffff_e100# + 16#08#);
   pragma Volatile (SYS2_STCLKDIV);
   pragma Import (Ada, SYS2_STCLKDIV);

   SYS2_CLK2CNTRL : Unsigned_32;
   for SYS2_CLK2CNTRL'Address use System'To_Address (16#ffff_e100# + 16#3c#);
   pragma Volatile (SYS2_CLK2CNTRL);
   pragma Import (Ada, SYS2_CLK2CNTRL);

   SYS2_VCLKACON1 : Unsigned_32;
   for SYS2_VCLKACON1'Address use System'To_Address (16#ffff_e100# + 16#40#);
   pragma Volatile (SYS2_VCLKACON1);
   pragma Import (Ada, SYS2_VCLKACON1);

   --  EFC

   EFCERRSTAT : Unsigned_32;
   for EFCERRSTAT'Address use System'To_Address (16#fff8_c000# + 16#3c#);
   pragma Volatile (EFCERRSTAT);
   pragma Import (Ada, EFCERRSTAT);

   --  PCR

   PSPWRDWNCLR0 : Unsigned_32;
   for PSPWRDWNCLR0'Address use System'To_Address (16#ffff_e000# + 16#a0#);
   pragma Volatile (PSPWRDWNCLR0);
   pragma Import (Ada, PSPWRDWNCLR0);

   PSPWRDWNCLR1 : Unsigned_32;
   for PSPWRDWNCLR1'Address use System'To_Address (16#ffff_e000# + 16#a4#);
   pragma Volatile (PSPWRDWNCLR1);
   pragma Import (Ada, PSPWRDWNCLR1);

   PSPWRDWNCLR2 : Unsigned_32;
   for PSPWRDWNCLR2'Address use System'To_Address (16#ffff_e000# + 16#a8#);
   pragma Volatile (PSPWRDWNCLR2);
   pragma Import (Ada, PSPWRDWNCLR2);

   PSPWRDWNCLR3 : Unsigned_32;
   for PSPWRDWNCLR3'Address use System'To_Address (16#ffff_e000# + 16#ac#);
   pragma Volatile (PSPWRDWNCLR3);
   pragma Import (Ada, PSPWRDWNCLR3);

   --  FLASH

   FLASH_FRDCNTL : Unsigned_32;
   for FLASH_FRDCNTL'Address use System'To_Address (16#fff8_7000#);
   pragma Volatile (FLASH_FRDCNTL);
   pragma Import (Ada, FLASH_FRDCNTL);

   FLASH_FEDACCTRL1 : Unsigned_32;
   for FLASH_FEDACCTRL1'Address use System'To_Address (16#fff8_7008#);
   pragma Volatile (FLASH_FEDACCTRL1);
   pragma Import (Ada, FLASH_FEDACCTRL1);

   FLASH_FEDACSTATUS : Unsigned_32;
   for FLASH_FEDACSTATUS'Address use System'To_Address (16#fff8_701c#);
   pragma Volatile (FLASH_FEDACSTATUS);
   pragma Import (Ada, FLASH_FEDACSTATUS);

   FLASH_FUNC_ERR_ADD : Unsigned_32;
   for FLASH_FUNC_ERR_ADD'Address use System'To_Address (16#fff8_7020#);
   pragma Volatile (FLASH_FUNC_ERR_ADD);
   pragma Import (Ada, FLASH_FUNC_ERR_ADD);

   FLASH_FDIAGCTRL : Unsigned_32;
   for FLASH_FDIAGCTRL'Address use System'To_Address (16#fff8_706c#);
   pragma Volatile (FLASH_FDIAGCTRL);
   pragma Import (Ada, FLASH_FDIAGCTRL);

   FLASH_FPAROVR : Unsigned_32;
   for FLASH_FPAROVR'Address use System'To_Address (16#fff8_707c#);
   pragma Volatile (FLASH_FPAROVR);
   pragma Import (Ada, FLASH_FPAROVR);

   FLASH_FSM_WR_ENA : Unsigned_32;
   for FLASH_FSM_WR_ENA'Address use System'To_Address (16#fff8_7288#);
   pragma Volatile (FLASH_FSM_WR_ENA);
   pragma Import (Ada, FLASH_FSM_WR_ENA);

   FLASH_EEPROM_CONFIG : Unsigned_32;
   for FLASH_EEPROM_CONFIG'Address use System'To_Address (16#fff8_72b8#);
   pragma Volatile (FLASH_EEPROM_CONFIG);
   pragma Import (Ada, FLASH_EEPROM_CONFIG);

   --  TCRAM

   TCRAM1_RAMCTRL : Unsigned_32;
   for TCRAM1_RAMCTRL'Address use System'To_Address (16#ffff_f800#);
   pragma Volatile (TCRAM1_RAMCTRL);
   pragma Import (Ada, TCRAM1_RAMCTRL);

   TCRAM1_RAMTHRESHOLD : Unsigned_32;
   for TCRAM1_RAMTHRESHOLD'Address use System'To_Address (16#ffff_f804#);
   pragma Volatile (TCRAM1_RAMTHRESHOLD);
   pragma Import (Ada, TCRAM1_RAMTHRESHOLD);

   TCRAM1_RAMINTCTRL : Unsigned_32;
   for TCRAM1_RAMINTCTRL'Address use System'To_Address (16#ffff_f80c#);
   pragma Volatile (TCRAM1_RAMINTCTRL);
   pragma Import (Ada, TCRAM1_RAMINTCTRL);

   TCRAM1_RAMERRSTATUS : Unsigned_32;
   for TCRAM1_RAMERRSTATUS'Address use System'To_Address (16#ffff_f810#);
   pragma Volatile (TCRAM1_RAMERRSTATUS);
   pragma Import (Ada, TCRAM1_RAMERRSTATUS);

   TCRAM2_RAMCTRL : Unsigned_32;
   for TCRAM2_RAMCTRL'Address use System'To_Address (16#ffff_f900#);
   pragma Volatile (TCRAM2_RAMCTRL);
   pragma Import (Ada, TCRAM2_RAMCTRL);

   TCRAM2_RAMTHRESHOLD : Unsigned_32;
   for TCRAM2_RAMTHRESHOLD'Address use System'To_Address (16#ffff_f904#);
   pragma Volatile (TCRAM2_RAMTHRESHOLD);
   pragma Import (Ada, TCRAM2_RAMTHRESHOLD);

   TCRAM2_RAMINTCTRL : Unsigned_32;
   for TCRAM2_RAMINTCTRL'Address use System'To_Address (16#ffff_f90c#);
   pragma Volatile (TCRAM2_RAMINTCTRL);
   pragma Import (Ada, TCRAM2_RAMINTCTRL);

   TCRAM2_RAMERRSTATUS : Unsigned_32;
   for TCRAM2_RAMERRSTATUS'Address use System'To_Address (16#ffff_f910#);
   pragma Volatile (TCRAM2_RAMERRSTATUS);
   pragma Import (Ada, TCRAM2_RAMERRSTATUS);

   --  PBIST

   PBIST_DLR : Unsigned_32;
   for PBIST_DLR'Address use System'To_Address (16#ffff_e400# + 16#164#);
   pragma Volatile (PBIST_DLR);
   pragma Import (Ada, PBIST_DLR);

   PBIST_PACT : Unsigned_32;
   for PBIST_PACT'Address use System'To_Address (16#ffff_e400# + 16#180#);
   pragma Volatile (PBIST_PACT);
   pragma Import (Ada, PBIST_PACT);

   PBIST_OVER : Unsigned_32;
   for PBIST_OVER'Address use System'To_Address (16#ffff_e400# + 16#188#);
   pragma Volatile (PBIST_OVER);
   pragma Import (Ada, PBIST_OVER);

   PBIST_FSRF0 : Unsigned_32;
   for PBIST_FSRF0'Address use System'To_Address (16#ffff_e400# + 16#190#);
   pragma Volatile (PBIST_FSRF0);
   pragma Import (Ada, PBIST_FSRF0);

   PBIST_FSRF1 : Unsigned_32;
   for PBIST_FSRF1'Address use System'To_Address (16#ffff_e400# + 16#194#);
   pragma Volatile (PBIST_FSRF1);
   pragma Import (Ada, PBIST_FSRF1);

   PBIST_ROM : Unsigned_32;
   for PBIST_ROM'Address use System'To_Address (16#ffff_e400# + 16#1c0#);
   pragma Volatile (PBIST_ROM);
   pragma Import (Ada, PBIST_ROM);

   PBIST_ALGO : Unsigned_32;
   for PBIST_ALGO'Address use System'To_Address (16#ffff_e400# + 16#1c4#);
   pragma Volatile (PBIST_ALGO);
   pragma Import (Ada, PBIST_ALGO);

   PBIST_RINFOL : Unsigned_32;
   for PBIST_RINFOL'Address use System'To_Address (16#ffff_e400# + 16#1c8#);
   pragma Volatile (PBIST_RINFOL);
   pragma Import (Ada, PBIST_RINFOL);

   PBIST_RINFOU : Unsigned_32;
   for PBIST_RINFOU'Address use System'To_Address (16#ffff_e400# + 16#1cc#);
   pragma Volatile (PBIST_RINFOU);
   pragma Import (Ada, PBIST_RINFOU);

   --  STC

   STC_STCGCR0 : Unsigned_32;
   for STC_STCGCR0'Address use System'To_Address (16#ffff_e600# + 16#00#);
   pragma Volatile (STC_STCGCR0);
   pragma Import (Ada, STC_STCGCR0);

   STC_STCGCR1 : Unsigned_32;
   for STC_STCGCR1'Address use System'To_Address (16#ffff_e600# + 16#04#);
   pragma Volatile (STC_STCGCR1);
   pragma Import (Ada, STC_STCGCR1);

   STC_STCTPR : Unsigned_32;
   for STC_STCTPR'Address use System'To_Address (16#ffff_e600# + 16#08#);
   pragma Volatile (STC_STCTPR);
   pragma Import (Ada, STC_STCTPR);

   STC_STCGSTAT : Unsigned_32;
   for STC_STCGSTAT'Address use System'To_Address (16#ffff_e600# + 16#14#);
   pragma Volatile (STC_STCGSTAT);
   pragma Import (Ada, STC_STCGSTAT);

   STC_STCSCSCR : Unsigned_32;
   for STC_STCSCSCR'Address use System'To_Address (16#ffff_e600# + 16#3c#);
   pragma Volatile (STC_STCSCSCR);
   pragma Import (Ada, STC_STCSCSCR);

   --  CCM

   CCM_CCMSR : Unsigned_32;
   for CCM_CCMSR'Address use System'To_Address (16#ffff_f600# + 16#00#);
   pragma Volatile (CCM_CCMSR);
   pragma Import (Ada, CCM_CCMSR);

   CCM_CCMKEYR : Unsigned_32;
   for CCM_CCMKEYR'Address use System'To_Address (16#ffff_f600# + 16#04#);
   pragma Volatile (CCM_CCMKEYR);
   pragma Import (Ada, CCM_CCMKEYR);

   --  ESM

   ESM_ESMSR1 : Unsigned_32;
   for ESM_ESMSR1'Address use System'To_Address (16#ffff_f500# + 16#18#);
   pragma Volatile (ESM_ESMSR1);
   pragma Import (Ada, ESM_ESMSR1);

   ESM_ESMSR2 : Unsigned_32;
   for ESM_ESMSR2'Address use System'To_Address (16#ffff_f500# + 16#1c#);
   pragma Volatile (ESM_ESMSR2);
   pragma Import (Ada, ESM_ESMSR2);

   ESM_ESMSR3 : Unsigned_32;
   for ESM_ESMSR3'Address use System'To_Address (16#ffff_f500# + 16#20#);
   pragma Volatile (ESM_ESMSR3);
   pragma Import (Ada, ESM_ESMSR3);

   ESM_ESMEKR : Unsigned_32;
   for ESM_ESMEKR'Address use System'To_Address (16#ffff_f500# + 16#38#);
   pragma Volatile (ESM_ESMEKR);
   pragma Import (Ada, ESM_ESMEKR);

   ESM_ESMSSR2 : Unsigned_32;
   for ESM_ESMSSR2'Address use System'To_Address (16#ffff_f500# + 16#3c#);
   pragma Volatile (ESM_ESMSSR2);
   pragma Import (Ada, ESM_ESMSSR2);

   procedure Memory_Init (Mask : Unsigned_32);

   -----------------
   -- Memory_Init --
   -----------------

   procedure Memory_Init (Mask : Unsigned_32) is
   begin
      --  See sequence in TRM 2.3.4.2

      --  Enable global memory initialization key by programming 0xa to
      --  MINITGCR
      SYS1_MINITGCR := 16#a#;

      --  Select the module by programming MSINENA
      SYS1_MSINENA := Mask;

      --  When the memory initialization is complete, the module will set
      --  MIDONE in MINISTAT
      while (SYS1_MSTCGSTAT and 16#100#) = 0 loop
         null;
      end loop;

      SYS1_MINITGCR := 16#5#;
   end Memory_Init;

   --------------
   -- Power_Up --
   --------------

   procedure Power_Up is
   begin
      --  2.7 Configure PLLs

      --  fpll = foscin*NF/(NR*OD*R) = 16*90/(4*2*16) = 11.25 Mhz
      --  This can be increased to 180Mhz in steps once the PLL has acquired
      --  the lock, which takes (127 + 1024 * NR) ocillator cycles.

      SYS1_PLLCTL1 := 16#2f03_5900#;
      --  ROS=0, MASK_SLIP=1, PLLDIV=0xf (R=16), ROF=0, REFCLKDIV=3 (NR=4)
      --  PLLMUL=0x5900 (NF=0x5a=90)

      PLLCTL2 := 16#04c5_c249#;
      --  FMENA=0, SPREADINGRATE=0x13 (NS=20), MULMOD=0x5c, ODPLL=1 (OD=2)
      --  SPR_AMOUNT=0x49 */

      --  Initially PLL2 output is configured to be 2.5Mhz.
      --  fpll = 16*20/(4*2*16)

      SYS2_PLLCTL3 := 16#2f03_1300#;
      --  ODPLL2=1 (OD2=2), PLLDIV2=0xf (R2=16), REFCLKDIV2=0x3 (NR2=4),
      --  PLLMUL2=0x1300 (NF2=20)

      --  2.8 Enable clock sources

      CSDISCLR := 16#42#;
      --  Sources 1 (PLL) and 6 (PLL2)

      --  2.10 Release reset and clocks to peripheral

      --  Power-up all peripherals
      PSPWRDWNCLR0 := 16#ffff_ffff#;
      PSPWRDWNCLR1 := 16#ffff_ffff#;
      PSPWRDWNCLR2 := 16#ffff_ffff#;
      PSPWRDWNCLR3 := 16#ffff_ffff#;

      --  Enable peripherals
      SYS1_CLKCNTL := SYS1_CLKCNTL or 16#100#;  --  PENA = 1

      --  ??? Pin Mux

      --  ??? Wait for eFuse controller self-test to complete

      --  2.11 Configure flash access

      --  Enable pipeline mode, address setup wait state amd data read
      --  wait state
      FLASH_FRDCNTL := 16#311#;

      --  Setup flash access wait states for bank 7
      FLASH_FSM_WR_ENA := 5;
      FLASH_EEPROM_CONFIG := 16#00030002#;

      --  Disable write access to flash state machine
      FLASH_FSM_WR_ENA := 16#a#;

      --  2.12 Configure flash bank and pump power modes

      --  We're leaving the flash banks and pump fall-back mode as active

      --  2.13 Configure oscillator monitor

      SYS1_LPOMONCTL := 16#01_00_10_08#;

      --  2.15 Clock domains

      --  Setup VCLK1, VCLK2, VCLK3
      SYS1_CLKCNTL := (SYS1_CLKCNTL and 16#ffff#) or 16#0101_0000#;
      SYS2_CLK2CNTRL := 16#01#;

      --  Wait for PLL to acquire lock and become available
      while (SYS1_CSVSTAT and 16#42#) /= 16#42# loop
         null;
      end loop;

      --  Setup GCLK, HCLK and VCLK clock source (use PLL1)
      SYS1_GHVSRC := 16#01_01_00_01#;

      --  Setup VCLK1, VCLK2, VCLK3
      SYS1_CLKCNTL := (SYS1_CLKCNTL and 16#ffff#) or 16#0101_0000#;
      SYS2_CLK2CNTRL := 16#01#;

      --  Setup RTICLK
      --  RTICLK1 divider value is 2, RTI clock1 source is VCLK
      SYS1_RCLKSRC := 16#01_09_01_09#;

      --  Setup asynchronous peripheral clock sources for AVCLK1 and AVCLK2
      SYS1_VCLKASRC := 16#06_00#;
      SYS2_VCLKACON1 := 16#01_19_00_19#;

      --  Now the PLLs are locked, the PLL outputs can be speed up.
      --  The R divider is changed to 0
      SYS1_PLLCTL1 := SYS1_PLLCTL1 and 16#e0_ffffff#;
      SYS2_PLLCTL3 := SYS2_PLLCTL3 and 16#e0_ffffff#;
   end Power_Up;

begin
   --  Power-On, External or Software reset.
   Power_Up;

   --  2.21 Initialize CPU RAM

   Memory_Init (1);
end Sys_Startup;
