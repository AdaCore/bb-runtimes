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
with System.Machine_Code; use System.Machine_Code;

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

   PORST : constant := 2**15;
   OSCRST : constant := 2**14;
   WDRST : constant := 2**13;
   CPURST : constant := 2**5;
   SWRST : constant := 2**4;
   EXTRST : constant := 2**3;

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

   CCM_CMPE : constant := 2**16;
   CCM_STC : constant := 2**8;
   CCM_STE : constant := 2**0;

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

   procedure Dmb;
   pragma Inline (Dmb);
   --  Data memory barrier

   procedure Isb;
   pragma Inline (Isb);
   --  Instruction synchronization barrier

   procedure Pbist_Run (Algo : Unsigned_32; Raminfol : Unsigned_32);
   procedure Memory_Init (Mask : Unsigned_32);

   procedure Start_Failure (Val : Unsigned_32);
   pragma No_Return (Start_Failure);
   pragma Inline (Start_Failure);
   --  This procedure is called in case of failure and loop forever

   ---------
   -- Dmb --
   ---------

   procedure Dmb is
   begin
      Asm ("dmb", Volatile => True);
   end Dmb;

   ---------
   -- Isb --
   ---------

   procedure Isb is
   begin
      Asm ("isb", Volatile => True, Clobber => "memory");
   end Isb;

   ---------------
   -- Get_ACTLR --
   ---------------

   function Get_ACTLR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c1, c0, 1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ACTLR;

   ---------------
   -- Set_ACTLR --
   ---------------

   procedure Set_ACTLR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c1, c0, 1",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_ACTLR;

   -------------------
   -- Start_Failure --
   -------------------

   procedure Start_Failure (Val : Unsigned_32) is
   begin
      --  Add a comment in the assembly file to keep the failure number. Note
      --  that this procedure must be inlined.
      Asm ("@ Failure %0",
           Inputs => Unsigned_32'Asm_Input ("i", Val), Volatile => True);

      --  Infinite loop
      loop
         null;
      end loop;
   end Start_Failure;

   ---------------
   -- Pbist_Run --
   ---------------

   procedure Pbist_Run (Algo : Unsigned_32; Raminfol : Unsigned_32) is
   begin
      --  First set up the PBIST ROM clock as this clock frequency is limited
      --  to 90 Mhz
      SYS1_MSTGCR := SYS1_MSTGCR or 16#0000_0100#;

      --  Enable PBIST controller
      SYS1_MSINENA := 1;

      --  Enable PBIST self-test
      SYS1_MSTGCR := (SYS1_MSTGCR and 16#ffff_fff0#) or 16#a#;

      --  Wait at least 32 VCLK cycles
      for I in 1 .. 32 loop
         null;
      end loop;

      --  Enable PBIST clocks and ROM clock
      PBIST_PACT := 3;

      --  Select algo and RAM groups
      PBIST_ALGO := Algo;
      PBIST_RINFOL := Raminfol;
      PBIST_RINFOU := 0;

      --  ROM content will not override ALGO and RINFOx settings
      PBIST_OVER := 0;

      --  Algorithm code is loaded from ROM
      PBIST_ROM := 3;

      --  Start PBIST
      PBIST_DLR := 16#14#;

      --  Wait until memory self-test done
      while (SYS1_MSTCGSTAT and 1) /= 1 loop
         null;
      end loop;

      --  Check failure
      if (PBIST_FSRF0 and 1) = 1 or else (PBIST_FSRF1 and 1) = 1 then
         Start_Failure (1);
      end if;

      --  Disable PBIST
      PBIST_PACT := 0;
      SYS1_MSTGCR := (SYS1_MSTGCR and 16#ffff_fff0#) or 16#5#;
   end Pbist_Run;

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

   -------------------
   -- Stc_Self_Test --
   -------------------

   procedure Stc_Self_Test is
   begin
      --  Note that the CPU is reset after the test.
      --  90Mhz
      SYS2_STCLKDIV := 2**24;

      --  Restart self-test, select one interval
      STC_STCGCR0 := 16#0001_0001#;

      --  Enable self-check compare logic. Insert stuck-at-fault inside CPU
      STC_STCSCSCR := 16#1_a#;

      --  Time-out
      STC_STCTPR := 16#000f_ffff#;

      --  Wait for 16 VBUS cycles
      for I in 1 .. 16 loop
         null;
      end loop;

      --  Enable self-test
      STC_STCGCR1 := 16#a#;

      --  Wait for 16 VBUS cycles
      for I in 1 .. 16 loop
         null;
      end loop;

      Asm ("wfi", Volatile => True);
      loop
         Asm ("nop", Volatile => True);
      end loop;
   end Stc_Self_Test;

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

      --  2.9 Run self-test on the eFuse controller SECDED logic

      declare
         Efc_Status : constant Unsigned_32 := EFCERRSTAT and 16#1f#;
      begin
         if Efc_Status = 0 then
            --  No error
            null;  --  ??? Run stuck at zero test.  See efcCheck
         elsif Efc_Status = 16#15# then
            --  At least one single bit error detected and corrected
            null; --  ???  Run self-test
         else
            --  Some other EFC error detected
            Start_Failure (10);
         end if;
      end;

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

      --  2.14 Run self-test on the flash module

      --  Enable ECC logic
      FLASH_FEDACCTRL1 := 16#000a_060a#;

      --  Clear possible 1-bit error dectected during flash OTP
      if (ESM_ESMSR1 and 16#40#) /= 0 then
         ESM_ESMSR1 := 16#40#;
      end if;

      --  Check ECC: read location with deliberate 1-bit error
      declare
         FLASH_1BITERROR : Unsigned_32;
         for FLASH_1BITERROR'Address use System'To_Address (16#f00803f0#);
         pragma Volatile (FLASH_1BITERROR);
         pragma Import (Ada, FLASH_1BITERROR);

         V : Unsigned_32 := FLASH_1BITERROR;
         pragma Unreferenced (V);
      begin
         V := FLASH_1BITERROR;
      end;

      if (FLASH_FEDACSTATUS and 16#06#) = 0 then
         --  Error not detected
         Start_Failure (30);
      end if;

      --  Clear status
      FLASH_FEDACSTATUS := 16#0001_0006#;
      ESM_ESMSR1 := 16#40#;

      --  Check ECC: read location with deliberate 2-bit error
      declare
         FLASH_2BITERROR : Unsigned_32;
         for FLASH_2BITERROR'Address use System'To_Address (16#f00803f8#);
         pragma Volatile (FLASH_2BITERROR);
         pragma Import (Ada, FLASH_2BITERROR);

         V : Unsigned_32 := FLASH_2BITERROR;
         pragma Unreferenced (V);
      begin
         V := FLASH_2BITERROR;
      end;

      if (ESM_ESMSR3 and 16#80#) = 0 then
         --  Error not detected
         Start_Failure (31);
      end if;

      --  Clear status
      declare
         Tmp : Unsigned_32;
      begin
         Tmp := FLASH_FUNC_ERR_ADD;
      end;
      FLASH_FEDACSTATUS := 16#0002_0100#;
      ESM_ESMSR3 := 16#80#;

      --  The nERROR pin will become inactive once the LTC counter expires
      ESM_ESMEKR := 5;

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

   ---------------------
   -- Enable_Btcm_Ecc --
   ---------------------

   procedure Enable_Btcm_Ecc is
      Val : Unsigned_32;
   begin
      Val := Get_ACTLR;
      Val := Val or 16#0C00_0000#;
      Dmb;
      Set_ACTLR (Val);
      Isb;
   end Enable_Btcm_Ecc;

   ----------------------
   -- Disable_Btcm_Ecc --
   ----------------------

   procedure Disable_Btcm_Ecc is
      Val : Unsigned_32;
   begin
      Val := Get_ACTLR;
      Val := Val and not 16#0C00_0000#;
      Dmb;
      Set_ACTLR (Val);
      Isb;
   end Disable_Btcm_Ecc;

   --------------------
   -- Check_Btcm_Ecc --
   --------------------

   procedure Check_Btcm_Ecc is
   begin
      --  Single error, B0TCM

      --  Enable write to ECC RAM, enable ECC error response
      TCRAM1_RAMCTRL := 16#005_010a#;

      --  The first 1-bit error will cause an error respond
      TCRAM1_RAMTHRESHOLD := 1;

      --  Allow SERR to be reported to ESM
      TCRAM1_RAMINTCTRL := 1;

      --  Cause a 1-bit ECC error
      Disable_Btcm_Ecc;

      declare
         Tcram_A_1bit_Error : Unsigned_32;
         for Tcram_A_1bit_Error'Address use System'To_Address (16#0840_0000#);
         pragma Import (Ada, Tcram_A_1bit_Error);
      begin
         Tcram_A_1bit_Error := Tcram_A_1bit_Error xor 1;
      end;

      Enable_Btcm_Ecc;

      --  Disable write to ECC RAM.
      TCRAM1_RAMCTRL := 16#005_000a#;

      --  Read location with 1-bit ECC error
      declare
         Tcram_A_1bit : Unsigned_32;
         for Tcram_A_1bit'Address use System'To_Address (16#0800_0000#);
         pragma Volatile (Tcram_A_1bit);
         pragma Import (Ada, Tcram_A_1bit);

         V : Unsigned_32;
         pragma Unreferenced (V);
      begin
         V := Tcram_A_1bit;
      end;

      --  Check SERR
      if (TCRAM1_RAMERRSTATUS and 1) = 0
        or else (ESM_ESMSR1 and 16#0400_0000#) = 0
      then
         Start_Failure (40);
      end if;

      --  Clear SERR
      TCRAM2_RAMERRSTATUS := 1;

      --  Clear status flag for ESM
      ESM_ESMSR1 := 16#0400_0000#;

      --  Single error, B1TCM

      --  Enable write to ECC RAM, enable ECC error response
      TCRAM2_RAMCTRL := 16#005_010a#;

      --  The first 1-bit error will cause an error respond
      TCRAM2_RAMTHRESHOLD := 1;

      --  Allow SERR to be reported to ESM
      TCRAM2_RAMINTCTRL := 1;

      --  Cause a 1-bit ECC error
      Disable_Btcm_Ecc;

      declare
         Tcram_A_1bit_Error : Unsigned_32;
         for Tcram_A_1bit_Error'Address use System'To_Address (16#0840_0008#);
         pragma Import (Ada, Tcram_A_1bit_Error);
      begin
         Tcram_A_1bit_Error := Tcram_A_1bit_Error xor 1;
      end;

      Enable_Btcm_Ecc;

      --  Disable write to ECC RAM.
      TCRAM2_RAMCTRL := 16#005_000a#;

      --  Read location with 1-bit ECC error
      declare
         Tcram_A_1bit : Unsigned_32;
         for Tcram_A_1bit'Address use System'To_Address (16#0800_0008#);
         pragma Volatile (Tcram_A_1bit);
         pragma Import (Ada, Tcram_A_1bit);

         V : Unsigned_32;
         pragma Unreferenced (V);
      begin
         V := Tcram_A_1bit;
      end;

      --  Check SERR
      if (TCRAM2_RAMERRSTATUS and 1) = 0
        or else (ESM_ESMSR1 and 16#1000_0000#) = 0
      then
         Start_Failure (41);
      end if;

      --  Clear SERR
      TCRAM2_RAMERRSTATUS := 1;

      --  Clear status flag for ESM
      ESM_ESMSR1 := 16#1000_0000#;

      --  Uncorrectable error, B0TCM

      --  Enable write to ECC RAM, enable ECC error response
      TCRAM1_RAMCTRL := 16#005_010a#;

      --  Cause a 2-bit ECC error
      Disable_Btcm_Ecc;

      declare
         Tcram_A_2bit_Error : Unsigned_32;
         for Tcram_A_2bit_Error'Address use System'To_Address (16#0840_0010#);
         pragma Import (Ada, Tcram_A_2bit_Error);
      begin
         Tcram_A_2bit_Error := Tcram_A_2bit_Error xor 3;
      end;

      Enable_Btcm_Ecc;

      --  Read location with 2-bit ECC error
      declare
         Tcram_A_2bit : Unsigned_32;
         for Tcram_A_2bit'Address use System'To_Address (16#0800_0010#);
         pragma Volatile (Tcram_A_2bit);
         pragma Import (Ada, Tcram_A_2bit);

         V : Unsigned_32;
         pragma Unreferenced (V);
      begin
         --  Will generate a data abort, the handler will skip the faulting
         --  instruction
         V := Tcram_A_2bit;
      end;

      Dmb;

      --  Check SERR
      if (TCRAM1_RAMERRSTATUS and 16#20#) = 0
        or else (ESM_ESMSR3 and 16#8#) = 0
      then
         Start_Failure (42);
      end if;

      --  Clear SERR
      TCRAM1_RAMERRSTATUS := 16#20#;

      --  Clear status flag for ESM
      ESM_ESMSR3 := 16#8#;

      --  The nERROR pin will become inactive once the LTC counter expires
      ESM_ESMEKR := 5;

      --  Uncorrectable error, B1TCM

      --  Enable write to ECC RAM, enable ECC error response
      --  (On both port, as the data abort handler only check tcram1)
      TCRAM1_RAMCTRL := 16#005_010a#;
      TCRAM2_RAMCTRL := 16#005_010a#;

      --  Cause a 2-bit ECC error
      Disable_Btcm_Ecc;

      declare
         Tcram_A_2bit_Error : Unsigned_32;
         for Tcram_A_2bit_Error'Address use System'To_Address (16#0840_0018#);
         pragma Import (Ada, Tcram_A_2bit_Error);
      begin
         Tcram_A_2bit_Error := Tcram_A_2bit_Error xor 3;
      end;

      Enable_Btcm_Ecc;

      --  Read location with 2-bit ECC error
      declare
         Tcram_A_2bit : Unsigned_32;
         for Tcram_A_2bit'Address use System'To_Address (16#0800_0018#);
         pragma Volatile (Tcram_A_2bit);
         pragma Import (Ada, Tcram_A_2bit);

         V : Unsigned_32;
         pragma Unreferenced (V);
      begin
         --  Will generate a data abort, the handler will skip the faulting
         --  instruction
         V := Tcram_A_2bit;
      end;

      Dmb;

      --  Check SERR
      if (TCRAM2_RAMERRSTATUS and 16#20#) = 0
        or else (ESM_ESMSR3 and 16#20#) = 0
      then
         Start_Failure (43);
      end if;

      --  Clear SERR
      TCRAM2_RAMERRSTATUS := 16#20#;

      --  Clear status flag for ESM
      ESM_ESMSR3 := 16#20#;

      --  The nERROR pin will become inactive once the LTC counter expires
      ESM_ESMEKR := 5;

      --  Disable write to ecc
      TCRAM1_RAMCTRL := 16#005_000a#;
      TCRAM2_RAMCTRL := 16#005_000a#;
   end Check_Btcm_Ecc;

   ---------------------
   -- Check_Flash_Ecc --
   ---------------------

   procedure Check_Flash_Ecc is
      Flash_Bad_ECC : Unsigned_32;
      for Flash_Bad_ECC'Address use System'To_Address (16#2008_0000#);
      pragma Volatile (Flash_Bad_ECC);
      pragma Import (Ada, Flash_Bad_ECC);

      Val : Unsigned_32;
   begin
      --  Enable ECC checking
      FLASH_FEDACCTRL1 := 16#000a_060a#;

      --  Diagnostic mode enabled, ECC data correction diagnostic
      FLASH_FDIAGCTRL := 16#0005_0007#;

      FLASH_FPAROVR := 16#0000_5401#;

      --  Trigger
      FLASH_FDIAGCTRL := FLASH_FDIAGCTRL or 16#0100_0000#;

      --  Read a flash location
      Val := Flash_Bad_ECC;

      --  Disable diagnostic
      FLASH_FDIAGCTRL := 16#000a_0007#;

      if (FLASH_FEDACSTATUS and 2) = 0 then
         Start_Failure (50);
      end if;

      --  Clear bit
      FLASH_FEDACSTATUS := 2;

      ESM_ESMSR1 := 16#40#;

      --  Diagnostic mode enabled, ECC data correction diagnostic
      FLASH_FDIAGCTRL := 16#0005_0007#;

      FLASH_FPAROVR := 16#0000_5403#;

      FLASH_FDIAGCTRL := FLASH_FDIAGCTRL or 16#0100_0000#;

      --  Read a flash location
      Val := Flash_Bad_Ecc;

      --  Disable diagnostic
      FLASH_FDIAGCTRL := 16#000a_0007#;

      if (FLASH_FEDACSTATUS and 16#100#) = 0 then
         Start_Failure (51);
      end if;

      --  Clear bit
      FLASH_FEDACSTATUS := 16#100#;

      ESM_ESMSR3 := 16#80#;

      --  The nERROR pin will become inactive once the LTC counter expires
      ESM_ESMEKR := 5;
   end Check_Flash_Ecc;

begin
   --  2.3 Enable response to ECC errors indicated by CPU for accesses to flash
   FLASH_FEDACCTRL1 := 16#000a_060a#;

   --  2.4 Enable CPU Event Export. This allows the CPU to signal any
   --  single-bit or double-bit errors detected by its ECC logic for accesses
   --  to program flash or data RAM.
   declare
      Pmcr : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c9, c12, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Pmcr),
           Volatile => True);
      Pmcr := Pmcr or 16#10#;
      Asm ("mcr p15, 0, %0, c9, c12, 0",
           Inputs => Unsigned_32'Asm_Input ("r", Pmcr),
           Volatile => True);
   end;

   --  2.5 Enable CPU ECC checking for ATCM (flash accesses)
   declare
      Actlr : Unsigned_32;
   begin
      Actlr := Get_ACTLR;
      Actlr := Actlr or 2**25;
      Dmb;
      Set_ACTLR (Actlr);
      Isb;
   end;

   --  2.6 Handle the cause of Reset
   --  Check SYSESR and STCSCSCR if necessary
   if (SYS1_SYSESR and (PORST or EXTRST or SWRST)) /= 0 then
      --  Power-On, External or Software reset.
      Power_Up;

      if (SYS1_SYSESR and PORST) /= 0 then
         --  Clear all reset status flags.
         SYS1_SYSESR := 16#ffff#;

         --  Check if there were ESM group3 errors during power-up
         if ESM_ESMSR3 /= 0 then
            Start_Failure (2);
         end if;

         --  2.16 Run a diagnostic check on CPU self-test controller
         Stc_Self_Test;
      else
         SYS1_SYSESR := 16#ffff# and not (EXTRST or SWRST);
      end if;
   elsif (SYS1_SYSESR and OSCRST) /= 0 then
      --  Oscillator failure
      --  ??? Add user to handle it
      null;
   elsif (SYS1_SYSESR and WDRST) /= 0 then
      --  Watchdog violation
      --  ??? Add user to handle it
      null;
   elsif (SYS1_SYSESR and CPURST) /= 0 then
      --  CPU reset
      --  Can be caused by CPU self-test completion or by togglon the
      --  "CPU RESET" of the CPU reset control register

      if (STC_STCSCSCR and 16#0f#) = 16#0a# then
         --  Check if the stc self check was run

         if (STC_STCGSTAT and 3) /= 3 then
            --  Failure is expected
            Start_Failure (3);
         else
            --  2.17 Run CPU self-test
            --  Note that the CPU is reset after the test

            --  STC self test is ok; start CPU self test

            --  Clear self-check mode
            STC_STCSCSCR := 5;

            --  Clear global STC status flags
            STC_STCGSTAT := 3;

            --  Clear ESM group1 channel 27 status flag
            ESM_ESMSR1 := 2**27;

            --  Run all 24 test intervals starting from interval 0
            STC_STCGCR0 := 16#0018_0001#;

            --  Wait for 16 VBUS cycles
            for I in 1 .. 16 loop
               null;
            end loop;

            --  Enable self-test
            STC_STCGCR1 := 16#a#;

            --  Wait for 16 VBUS cycles
            for I in 1 .. 16 loop
               null;
            end loop;

            Asm ("wfi", Volatile => True);
            loop
               Asm ("nop", Volatile => True);
            end loop;
         end if;
      elsif (STC_STCGSTAT and 1) = 1 then
         --  CPU self-test completion
         if (STC_STCGSTAT and 2) = 2 then
            --  Self test failure
            Start_Failure (4);
         else
            --  Self test ok
            null;
         end if;
      else
         --  CPU reset caued by software
         null;
      end if;
   else
      --  Should never happen
      Start_Failure (5);
   end if;

   --  2.18 Run a diagnostic check on the CPU compare module

   --  Set self-test mode
   CCM_CCMKEYR := 6;

   --  Wait until complete
   while (CCM_CCMSR and CCM_STC) = 0 loop
      null;
   end loop;

   --  Check result
   if (CCM_CCMSR and CCM_STE) /= 0 then
      Start_Failure (20);
   end if;

   --  Set self-test error forcing mode
   CCM_CCMKEYR := 16#f#;

   --  Check result
   if (ESM_ESMSR1 and 16#8000_0000#) = 0 then
      Start_Failure (21);
   end if;

   --  Clear flag
   ESM_ESMSR1 := 16#8000_0000#;

   --  Set error forcing mode
   CCM_CCMKEYR := 16#9#;

   if (ESM_ESMSR2 and 4) = 0 then
      Start_Failure (22);
   end if;

   --  Clear flag
   ESM_ESMSR2 := 4;
   ESM_ESMSSR2 := 4;
   ESM_ESMSR1 := 16#8000_0000#;

   --  Clear CMPE flag
   CCM_CCMSR := CCM_CMPE;

   --  Return to lock-step mode
   CCM_CCMKEYR := 5;

   --  The nERROR pin will become inactive once the LTC counter expires
   ESM_ESMEKR := 5;

   --  2.19 Run a diagnostic check on the PBIST controller

   --  Select algo 3, march13n to be run
   --  RAM group 1 (in fact PBIST ROM)
   Pbist_Run (3, 1);

   --  2.20 Start a self-test on the CPU RAM using the PBIST controller

   --  The CPU RAM is single-port memory. The first 64kB of CPU RAM is RAM
   --  group #6, which 0x20 corresponds to. See datasheet table 4-26.
   --  Other groups will be tested later.
   Pbist_Run (8, 16#20#);

   --  2.21 Initialize CPU RAM

   Memory_Init (1);

   --  2.22 Enable the Cortex-r4f cpu's ECC checking for BxTCM interface

   Enable_Btcm_Ecc;

   --  2.23 Start a self-test on all duart-port memories using the PBIST

   --  See datasheet table 4-26
   Pbist_Run (4, 16#ce_ffdc#);

   --  2.24 Run a self-test on CPU's ECC logic for accesses to TCRAM
   --  ??? Check redundant decode logic, check address parity
   Check_Btcm_Ecc;

   --  2.25 Run a self-test on CPU's ECC logic for accesses to Flash
   Check_Flash_Ecc;

   --  2.26 Start a self-test on all single port memories using the PBIST

   --  ESRAM1 is excluded as it was already tested
   --  ??? What about ethernet 25 ?
   Pbist_Run (8, 16#08310020# and not 16#20#);

   --  ??? Release MibSPIx modules from local reset

   --  2.27 On-chip SRAM auto initialization

   --  See TRM table 2-16. MIBSPI are excluded
   Memory_Init (2#1_1110_0101_0111_1111#); -- 16#1_e57f#

   --  2.28 Run a self test on all peripheral RAM's parity protection mechanism
   --  ??? TODO

   --  2.29 Enable the cortex-R4F CPU's VIC port
   --  Done in the ravenscar runtime

   --  2.30 Vectored Interrupt Manager (VIM) configuration
   --  Done by ravenscar

   --  2.31 Enable interrupts in the cortex-R4F CPU
   --  Done by ravenscar

   --  2.32 Setup the Error Signaling Modle (ESM) responses to group1 errors
   --  ??? TODO

   --  2.33 Additional initializations required by compiler
   --  Done by the assembly file
end Sys_Startup;
