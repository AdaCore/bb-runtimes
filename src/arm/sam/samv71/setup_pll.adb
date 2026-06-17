------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    Copyright (C) 2013-2019, AdaCore                      --
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
with System.SAMV71; use System.SAMV71;
pragma Warnings (On);

with Interfaces.SAM;        use Interfaces.SAM;
with Interfaces.SAM.EFC;    use Interfaces.SAM.EFC;
with Interfaces.SAM.PMC;    use Interfaces.SAM.PMC;

with System.BB.Board_Parameters;

procedure Setup_Pll
is
   Master_Clock : Natural;
   FWS : EFC_EEFC_FMR_FWS_Field;
begin
   --  Set FWS to max to handle clock changes below

   EFC_Periph.EEFC_FMR := (FWS => 6,
                           CLOE => 1,
                           others => <>);

   --  31.17 Recommended Programming Sequence
   --  1. If the Main crystal oscillator is not required, the PLL and divider
   --    can be directly configured (Step 6.) else this oscillator must be
   --    started (Step 2.).

   null;

   --  2. Enable the Main crystal oscillator by setting CKGR_MOR.MOSCXTEN.
   --    The user can define a startup time. This can be done by configuring
   --    the appropriate value in CKGR_MOR.MOSCXTST. Once this register has
   --    been correctly configured, the user must wait for PMC_SR.MOSCXTS to
   --    be set. This can be done either by polling PMC_SR.MOSCXTS, or by
   --    waiting for the interrupt line to be raised if the associated
   --    interrupt source (MOSCXTS) has been enabled in PMC_IER.

   PMC_Periph.CKGR_MOR := (MOSCXTEN => 1,
                           MOSCRCEN => 1,
                           MOSCXTST => 8,
                           KEY => Passwd,
                           others => <>);

   while PMC_Periph.PMC_SR.MOSCXTS = 0 loop
      null;
   end loop;

   --  3. Switch MAINCK to the Main crystal oscillator by setting
   --    CKGR_MOR.MOSCSEL.

   PMC_Periph.CKGR_MOR := (MOSCXTEN => 1,
                           MOSCRCEN => 1,
                           MOSCXTST => 8,
                           KEY => Passwd,
                           MOSCSEL => 1,
                           others => <>);

   --  4. Wait for PMC_SR.MOSCSELS to be set to ensure the switch is complete.

   while PMC_Periph.PMC_SR.MOSCSELS = 0 loop
      null;
   end loop;

   --  5. Check MAINCK frequency:
   --    This Frequency Can Be Measured Via CKGR_MCFR.
   --    Read CKGR_MCFR until The MAINFRDY Field is Set, After Which The
   --      User Can Read CKGR_MCFR.MAINF By Performing An Additional Read.
   --      This Provides The Number of Main Clock Cycles That Have Been
   --      Counted During A Period of 16 SLCK Cycles.
   --    If MAINF = 0, switch MAINCK to the Main RC Oscillator by clearing
   --    CKGR_MOR.MOSCSEL. If MAINF /= 0, proceed to Step 6

   while PMC_Periph.CKGR_MCFR.MAINFRDY = 0 loop
      null;
   end loop;

   if PMC_Periph.CKGR_MCFR.MAINF = 0 then
      PMC_Periph.CKGR_MOR.MOSCSEL := 0;
   end if;

   --  6. Set PLLA and Divider (if not required, proceed to Step 7.):
   --    All parameters needed to configure PLLA and the divider are located
   --      in CKGR_PLLAR.
   --  CKGR_PLLAR.DIVA is used to control the divider. This parameter can be
   --     Programmed Between 0 and 127. Divider Output is Divider Input Divided
   --     By DIVA Parameter. By Default, DIVA Field is Cleared Which Means That
   --     The Divider and PLLA Are Turned Off.
   --  CKGR_PLLAR.MULA is The PLLA Multiplier Factor. This Parameter Can Be
   --    Programmed Between 0 and 62. if MULA is Cleared, PLLA Will Be Turned
   --    Off, Otherwise The PLLA Output Frequency is PLLA Input Frequency
   --    Multiplied By (MULA + 1).
   --  CKGR_PLLAR.PLLACOUNT specifies the number of SLCK cycles before
   --    PMC_SR.LOCKA is set after CKGR_PLLAR has been written.
   --  Once CKGR_PLLAR Has Been Written, The User Must Wait for PMC_SR.LOCKA
   --    To Be Set. This Can Be Done Either By Polling PMC_SR.LOCKA or By
   --    Waiting for The Interrupt Line To Be Raised if The Associated
   --    Interrupt Source (LOCKA) Has Been Enabled in PMC_IER. all Fields in
   --    CKGR_PLLAR Can Be Programmed in A Single Write Operation. if MULA or
   --    DIVA is Modified, The LOCKA Bit Goes Low To Indicate That PLLA is not
   --    Yet Ready. when PLLA is Locked, LOCKA is Set Again. The User Must Wait
   --    for The LOCKA Bit To Be Set Before Using The PLLA Output Clock.

   PMC_Periph.CKGR_PLLAR := (ONE => 1,
                             MULA => System.BB.Board_Parameters.PLL_MULA - 1,
                             DIVA => System.BB.Board_Parameters.PLL_DIVA,
                             PLLACOUNT => 16#3F#,
                             others => <>);

   while PMC_Periph.PMC_SR.LOCKA = 0 loop
      null;
   end loop;

   --  7. Select MCK and HCLK:
   --  MCK and HCLK are configurable via PMC_MCKR.
   --  CSS is Used To select The Clock Source of MCK and HCLK. By Default, The
   --    Selected Clock Source is MAINCK.
   --  PRES is used to define the HCLK and MCK prescaler.s The user can choose
   --    between different values (1, 2, 3, 4, 8, 16, 32, 64). Prescaler output
   --    is the selected clock source frequency divided by the PRES value.
   --  MDIV is used to define the MCK divider. It is possible to choose between
   --    different values (0, 1, 2, 3). MCK output is the HCLK frequency
   --    divided by 1, 2, 3 or 4, depending on the value programmed in MDIV.
   --  By default, MDIV is cleared, which indicates that the HCLK is equal
   --    to MCK.
   --  Once the PMC_MCKR has been written, the user must wait for PMC_SR.MCKRDY
   --    to be set. This can be done either by polling PMC_SR.MCKRDY or by
   --    waiting for the interrupt line to be raised if the associated
   --    interrupt source (MCKRDY) has been enabled in PMC_IER. PMC_MCKR
   --    must not be programmed in a single write operation. The programming
   --    sequence for PMC_MCKR is as follows :
   --      If a new value for PMC_MCKR.CSS corresponds to any of the available
   --        PLL clocks :
   --      a. Program PMC_MCKR.PRES.
   --      b. Wait for PMC_SR.MCKRDY to be set.
   --      c. Program PMC_MCKR.MDIV.
   --      d. Wait for PMC_SR.MCKRDY to be set.
   --      e. Program PMC_MCKR.CSS.
   --      f. Wait for PMC_SR.MCKRDY to be set.
   --      If a new value for PMC_MCKR.CSS corresponds to MAINCK or SLCK:
   --      a. Program PMC_MCKR.CSS.
   --      b. Wait for PMC_SR.MCKRDY to be set.
   --      c. Program PMC_MCKR.PRES.
   --      d. Wait for PMC_SR.MCKRDY to be set.
   --      If CSS, MDIV or PRES are modified at any stage, the MCKRDY bit goes
   --        low to indicate that MCK and HCLK are not yet ready. The user
   --        must wait for MCKRDY bit to be set again before using MCK and
   --        HCLK.
   --  MCK is MAINCK divided by 2.

   PMC_Periph.PMC_MCKR.PRES := Clk_1;
   while PMC_Periph.PMC_SR.MCKRDY = 0 loop
      null;
   end loop;

   pragma Warnings (Off);
   PMC_Periph.PMC_MCKR.MDIV :=
   (if System.BB.Board_Parameters.Clock_Frequency > 150_000_000 then
      PMC.Pck_Div2 else PMC.Eq_Pck);
   pragma Warnings (On);

   while PMC_Periph.PMC_SR.MCKRDY = 0 loop
      null;
   end loop;

   PMC_Periph.PMC_MCKR.CSS := PMC.Plla_Clk;
   while PMC_Periph.PMC_SR.MCKRDY = 0 loop
      null;
   end loop;

   --  8. Select the Programmable clocks (PCKx):
   --  PCKx are controlled via registers PMC_SCER, PMC_SCDR and PMC_SCSR.
   --  PCKx Can Be Enabled and / or Disabled Via PMC_SCER and PMC_SCDR.
   --    Three PCKx Can Be Used. PMC_SCSR Indicates Which PCKx is Enabled.
   --    By Default all PCKx Are Disabled.
   --  PMC_PCKx Registers Are Used To Configure PCKx.
   --  PMC_PCKx.CSS is used to select the PCKx divider source. Several clock
   --    Options are available :
   --    MAINCK, SLCK, MCK, PLLACK, UPLLCKDIV
   --  SLCK is The Default Clock Source.
   --  PMC_PCKx.PRES is Used To Control The PCKx Prescaler. It is Possible To
   --    Choose Between Different Values (1 To 256). PCKx Output is Prescaler
   --    Input Divided By PRES. By Default, The PRES Value is Cleared Which
   --    Means That PCKx is Equal To Slow Clock.
   --  Once PMC_PCKx Has Been Configured, The Corresponding PCKx Must Be
   --    Enabled and The User Must Wait for PMC_SR.PCKRDYx To Be Set. This
   --    Can Be Done Either By Polling PMC_SR.PCKRDYx or By Waiting for The
   --    Interrupt Line To Be Raised if The Associated Interrupt Source
   --    (PCKRDYx) Has Been Enabled in PMC_IER. all Parameters in PMC_PCKx
   --    Can Be Programmed in A Single Write Operation.
   --  If The PMC_PCKx.CSS and PMC_PCKx.PRES Parameters Are To Be Modified,
   --    The Corresponding PCKx Must Be Disabled First. The Parameters Can
   --    then Be Modified. Once This Has Been Done, The User Must Re - Enable
   --    PCKx and Wait for The PCKRDYx Bit To Be Set.

   null;

   --  9. Enable the peripheral clocks
   --    Once all of The Previous Steps Have Been Completed, The Peripheral
   --  Clocks Can Be Enabled and / or Disabled Via Registers PMC_PCERx and
   --    PMC_PCDRx.

   null;

   case PMC_Periph.PMC_MCKR.MDIV is
      when Eq_Pck =>
         Master_Clock := System.BB.Board_Parameters.Clock_Frequency;
      when Pck_Div2 =>
         Master_Clock := System.BB.Board_Parameters.Clock_Frequency / 2;
      when Pck_Div4 =>
         Master_Clock := System.BB.Board_Parameters.Clock_Frequency / 4;
      when Pck_Div3 =>
         Master_Clock := System.BB.Board_Parameters.Clock_Frequency / 3;
   end case;

   if Master_Clock < 23_000_000 then
      FWS := 0;
   elsif Master_Clock < 46_000_000 then
      FWS := 1;
   elsif Master_Clock < 69_000_000 then
      FWS := 2;
   elsif Master_Clock < 92_000_000 then
      FWS := 3;
   elsif Master_Clock < 115_000_000 then
      FWS := 4;
   elsif Master_Clock < 138_000_000 then
      FWS := 5;
   else
      FWS := 6;
   end if;

   EFC_Periph.EEFC_FMR := (FWS => FWS,
                           CLOE => 1,
                           others => <>);

end Setup_Pll;
