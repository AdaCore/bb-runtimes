------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2016, AdaCore                      --
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
pragma Restrictions (No_Elaboration_Code);
--  This subprogram is called before elaboration

pragma Warnings (Off);
with System.SAM4S; use System.SAM4S;
pragma Warnings (On);

with Interfaces.SAM;        use Interfaces.SAM;
with Interfaces.SAM.EFC;    use Interfaces.SAM.EFC;
with Interfaces.SAM.PMC;    use Interfaces.SAM.PMC;
with Interfaces.SAM.SYSC;   use Interfaces.SAM.SYSC;

procedure Setup_Pll is
   CKGR_MOR : CKGR_MOR_Register;
begin
   --  Main crystal is 12 Mhz and PLLA is set to x10. So main clock is 120 Mhz.

   --  5 wait states for the flash
   EFC0_Periph.FMR.FWS := 5;

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

   CKGR_MOR := PMC_Periph.CKGR_MOR;
   --  The Key field needs to be set to 0x37 to enable write to the
   --  CKGR_MOR register
   CKGR_MOR.KEY := Passwd;
   CKGR_MOR.MOSCXTEN := True;
   CKGR_MOR.MOSCXTST := 62;
   PMC_Periph.CKGR_MOR := CKGR_MOR;

   --  Wait until the xtal stabilize
   while not PMC_Periph.PMC_SR.MOSCXTS loop
      null;
   end loop;

   --  Select the xtal (must already be true, but doesn't harm)
   CKGR_MOR := PMC_Periph.CKGR_MOR;
   --  The Key field needs to be set to 0x37 to enable write to the
   --  CKGR_MOR register
   CKGR_MOR.KEY := Passwd;
   CKGR_MOR.MOSCSEL := True;
   PMC_Periph.CKGR_MOR := CKGR_MOR;

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

   --  First disable: MULA set to 0, DIVA set to 0
   PMC_Periph.CKGR_PLLAR :=
     (ONE    => True,
      MULA   => 0,
      DIVA   => 0,
      others => <>);

   PMC_Periph.CKGR_PLLAR :=
     (ONE  => True,
      DIVA => 1,
      MULA => 10 - 1,
      others => <>);

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

   while not PMC_Periph.PMC_SR.LOCKA loop
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

   PMC_Periph.PMC_MCKR.PRES := Clk_1;

   while not PMC_Periph.PMC_SR.MCKRDY loop
      null;
   end loop;

   PMC_Periph.PMC_MCKR.CSS := Plla_Clk;

   while not PMC_Periph.PMC_SR.MCKRDY loop
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
   --  PCKRDYx has been enabled in the PMC_IER register. All parameters in
   --  PMC_PCKx can be programmed in a single write operation.

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

   --  Disable watchdog.  The register can be written once, so this file has
   --  to be modified to enable watchdog.
   WDT_Periph.MR.WDDIS := True;

end Setup_Pll;
