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
with Sam4s; use Sam4s;
with Oled; use Oled;

procedure Leds is
   Led_Pin  : constant := 2 ** 23; -- PC23
   Led1_Pin : constant := 2 ** 20; -- PC20
   Led2_Pin : constant := 2 ** 16; -- PA16
   Led3_Pin : constant := 2 ** 22; -- PC22;

   But_Pin  : constant := 2 ** 2;  -- PA2
   But1_Pin : constant := 2 ** 0;  -- PA0
   But2_Pin : constant := 2 ** 29; -- PC29
   But3_Pin : constant := 2 ** 30; -- PC30

   procedure Init_Pll is
   begin
      --  5 wait states for the flash
      EFC0.EEFC_FMR := 5 * EEFC_FMR.FWS;

      --  28.2.13 Programming Sequence

      --  1. Enabling the Main Oscillator:

      --  The main oscillator is enabled by setting the MOSCXTEN field in
      --  the Main Oscillator Register (CKGR_MOR). The user can define a
      --  start-up time. This can be achieved by writing a value in the
      --  MOSCXTST field in CKGR_MOR. Once this register has been
      --  correctly configured, the user must wait for MOSCXTS field in
      --  the PMC_SR register to be set. This can be done either by
      --  polling the status register, or by waiting the interrupt line
      --  to be raised if the associated inter- rupt to MOSCXTS has been
      --  enabled in the PMC_IER register.

      PMC.CKGR_MOR := (PMC.CKGR_MOR and not CKGR_MOR.MOSCXTBY)
        or CKGR_MOR.KEY or CKGR_MOR.MOSCXTEN or 62 * CKGR_MOR.MOSCXTST;

      --  Wait until the xtal stabilize
      while (PMC.PMC_SR and PMC_SR.MOSCXTS) = 0 loop
         null;
      end loop;

      --  Select the xtal (must already be true, but doesn't harm)
      PMC.CKGR_MOR := PMC.CKGR_MOR or CKGR_MOR.KEY or CKGR_MOR.MOSCSEL;

      --  2. Checking the Main Oscillator Frequency (Optional):

      --  In some situations the user may need an accurate measure of the
      --  main clock frequency. This measure can be accomplished via the
      --  Main Clock Frequency Register (CKGR_MCFR).  Once the MAINFRDY
      --  field is set in CKGR_MCFR, the user may read the MAINF field in
      --  CKGR_MCFR by performing another CKGR_MCFR read access. This
      --  provides the number of main clock cycles within sixteen slow
      --  clock cycles.

      null;

      --  3. Setting PLL and Divider:

      --  All parameters needed to configure PLLA and the divider are
      --  located in CKGR_PLLAxR.

      --  The DIV field is used to control the divider itself. It must be
      --  set to 1 when PLL is used. By default, DIV parameter is set to
      --  0 which means that the divider is turned off.

      --  The MUL field is the PLL multiplier factor. This parameter can
      --  be programmed between 0 and 80. If MUL is set to 0, PLL will be
      --  turned off, otherwise the PLL output frequency is PLL input
      --  frequency multiplied by (MUL + 1).

      --  The PLLCOUNT field specifies the number of slow clock cycles
      --  before the LOCK bit is set in PMC_SR, after CKGR_PLLA(B)R has
      --  been written.

      --  First disable
      PMC.CKGR_PLLAR := CKGR_PLLxR.ONE;

      PMC.CKGR_PLLAR := CKGR_PLLxR.ONE
        or 1 * CKGR_PLLxR.DIV
        + (10 - 1) * CKGR_PLLxR.MUL
        + 16#3f# * CKGR_PLLxR.PLLCOUNT;

      --  Once the CKGR_PLL register has been written, the user must wait
      --  for the LOCK bit to be set in the PMC_SR. This can be done
      --  either by polling the status register or by waiting the
      --  interrupt line to be raised if the associated interrupt to LOCK
      --  has been enabled in PMC_IER. All parameters in CKGR_PLLA(B)R
      --  can be programmed in a single write operation. If at some stage
      --  one of the following parameters, MUL or DIV is modified, the
      --  LOCK bit will go low to indicate that PLL is not ready
      --  yet. When PLL is locked, LOCK will be set again. The user is
      --  constrained to wait for LOCK bit to be set before using the PLL
      --  output clock.

      while (PMC.PMC_SR and PMC_SR.LOCKA) = 0 loop
         null;
      end loop;

      --  4. Selection of Master Clock and Processor Clock

      --  The Master Clock and the Processor Clock are configurable via
      --  the Master Clock Register (PMC_MCKR).

      --  The CSS field is used to select the Master Clock divider
      --  source. By default, the selected clock source is main clock.

      --  The PRES field is used to control the Master Clock
      --  prescaler. The user can choose between different values (1, 2,
      --  3, 4, 8, 16, 32, 64). Master Clock output is prescaler input
      --  divided by PRES parameter. By default, PRES parameter is set to
      --  1 which means that master clock is equal to main clock.

      --  Once PMC_MCKR has been written, the user must wait for the
      --  MCKRDY bit to be set in PMC_SR. This can be done either by
      --  polling the status register or by waiting for the interrupt
      --  line to be raised if the associated interrupt to MCKRDY has
      --  been enabled in the PMC_IER register.

      --  The PMC_MCKR must not be programmed in a single write
      --  operation. The preferred programming sequence for PMC_MCKR is
      --  as follows:

      --  * If a new value for CSS field corresponds to PLL Clock,
      --    * Program the PRES field in PMC_MCKR.
      --    * Wait for the MCKRDY bit to be set in PMC_SR.
      --    * Program the CSS field in PMC_MCKR.
      --    * Wait for the MCKRDY bit to be set in PMC_SR.
      --  * If a new value for CSS field corresponds to Main Clock
      --    or Slow Clock,
      --    * Program the CSS field in PMC_MCKR.
      --    * Wait for the MCKRDY bit to be set in the PMC_SR.
      --    * Program the PRES field in PMC_MCKR.
      --    * Wait for the MCKRDY bit to be set in PMC_SR.

      PMC.PMC_MCKR := (PMC.PMC_MCKR and not PMC_MCKR.PRES_Mask)
        or PMC_MCKR.CLK_1;

      while (PMC.PMC_SR and PMC_SR.MCKRDY) = 0 loop
         null;
      end loop;

      PMC.PMC_MCKR := (PMC.PMC_MCKR and not PMC_MCKR.CSS_Mask)
        or PMC_MCKR.PLLA_CLK;

      while (PMC.PMC_SR and PMC_SR.MCKRDY) = 0 loop
         null;
      end loop;

      --  If at some stage one of the following parameters, CSS or PRES
      --  is modified, the MCKRDY bit will go low to indicate that the
      --  Master Clock and the Processor Clock are not ready yet. The
      --  user must wait for MCKRDY bit to be set again before using the
      --  Master and Processor Clocks.

      --  Note: IF PLLx clock was selected as the Master Clock and the
      --  user decides to modify it by writing in CKGR_PLLR, the MCKRDY
      --  flag will go low while PLL is unlocked. Once PLL is locked
      --  again, LOCK goes high and MCKRDY is set.

      --  While PLL is unlocked, the Master Clock selection is
      --  automatically changed to Slow Clock. For further information,
      --  see Section 28.2.14.2 “Clock Switching Waveforms” on page 467.

      --  Code Example:
      --        write_register(PMC_MCKR,0x00000001)
      --        wait (MCKRDY=1)
      --        write_register(PMC_MCKR,0x00000011)
      --        wait (MCKRDY=1)

      --  The Master Clock is main clock divided by 2.
      --  The Processor Clock is the Master Clock.

      --  5. Selection of Programmable Clocks

      --  Programmable clocks are controlled via registers, PMC_SCER,
      --  PMC_SCDR and PMC_SCSR.

      --  Programmable clocks can be enabled and/or disabled via PMC_SCER
      --  and PMC_SCDR. 3 Programmable clocks can be enabled or
      --  disabled. The PMC_SCSR provides a clear indication as to which
      --  Programmable clock is enabled. By default all Programmable
      --  clocks are disabled.

      --  Programmable Clock Registers (PMC_PCKx) are used to configure
      --  Programmable clocks.

      --  The CSS field is used to select the Programmable clock divider
      --  source. Four clock options are available: main clock, slow
      --  clock, PLLACK, PLLBCK. By default, the clock source selected is
      --  slow clock.

      --  The PRES field is used to control the Programmable clock
      --  prescaler. It is possible to choose between different values
      --  (1, 2, 4, 8, 16, 32, 64). Programmable clock output is
      --  prescaler input divided by PRES parameter. By default, the PRES
      --  parameter is set to 0 which means that master clock is equal to
      --  slow clock.

      --  Once PMC_PCKx has been programmed, The corresponding
      --  Programmable clock must be enabled and the user is constrained
      --  to wait for the PCKRDYx bit to be set in PMC_SR. This can be
      --  done either by polling the status register or by waiting the
      --  interrupt line to be raised, if the associated interrupt to
      --  PCKRDYx has been enabled in the PMC_IER register. All parame-
      --  ters in PMC_PCKx can be programmed in a single write operation.

      --  If the CSS and PRES parameters are to be modified, the
      --  corresponding Programmable clock must be disabled first. The
      --  parameters can then be modified. Once this has been done, the
      --  user must re-enable the Programmable clock and wait for the
      --  PCKRDYx bit to be set.

      null;

      --  6. Enabling Peripheral Clocks

      --  Once all of the previous steps have been completed, the
      --  peripheral clocks can be enabled and/or disabled via registers
      --  PMC_PCER0, PMC_PCER, PMC_PCDR0 and PMC_PCDR.

      null;
   end Init_Pll;

   procedure Wait is
   begin
      for I in 1 .. 16#fffff# loop
         null;
      end loop;
   end Wait;
begin
   Init_Pll;

   --  Enable clock for GPIO-A and GPIO-C

   PMC.PMC_PCER0 := 2 ** PIOA_ID + 2 ** PIOC_ID;

   --  Configure Led

   PIOC.PER := Led_Pin + Led1_Pin + Led3_Pin + But2_Pin + But3_Pin;
   PIOC.OER := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.CODR := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.MDDR := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.PUER := But2_Pin + But3_Pin;

   PIOA.PER := Led2_Pin + But_Pin + But1_Pin;
   PIOA.OER := Led2_Pin;
   PIOA.CODR := Led2_Pin;
   PIOA.MDDR := Led2_Pin;
   PIOA.PUER := But_Pin + But1_Pin;

   Oled_Init;

   loop
      --  Off

      if (PIOA.PDSR and But_Pin) /= 0 then
         PIOC.SODR := Led_Pin;
      end if;
      PIOA.SODR := Led2_Pin;
      PIOC.CODR := Led1_Pin + Led3_Pin;
      Wait;

      --  On
      if (PIOA.PDSR and But_Pin) /= 0 then
         PIOC.CODR := Led_Pin;
      end if;
      PIOA.CODR := Led2_Pin;
      PIOC.SODR := Led1_Pin + Led3_Pin;
      Wait;
   end loop;
end Leds;
