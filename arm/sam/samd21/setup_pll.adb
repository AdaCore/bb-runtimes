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

with Interfaces.SAM;         use Interfaces.SAM;
with Interfaces.SAM.NVMCTRL; use Interfaces.SAM.NVMCTRL;
with Interfaces.SAM.SYSCTRL; use Interfaces.SAM.SYSCTRL;
with Interfaces.SAM.GCLK;    use Interfaces.SAM.GCLK;
with Interfaces.SAM.PORT;    use Interfaces.SAM.PORT;
with Interfaces.SAM.PM;      use Interfaces.SAM.PM;

with System;
with Ada.Unchecked_Conversion;

--  with System.BB.Board_Parameters;

--  following Atmel's SAM D21 DFLL48M 48 MHz Initialization Example
procedure Setup_Pll
is
   type Addr_Record is record
      Extra : Interfaces.SAM.UInt26;
      Data  : Interfaces.SAM.UInt6;
   end record;
   for Addr_Record use record
      Extra at 0 range 0 .. 25;
      Data  at 0 range 26 .. 31;
   end record;
   function Addr_To_Rec is new Ada.Unchecked_Conversion (
      Source => System.Address,
      Target => Addr_Record
   );

   --  Factory calibrated value for "DFLL48M Coarse Cal" from NVM Cal Area
   Coarse_Cal_Addr : constant System.Address :=
      System'To_Address (16#00806024#);
   R : constant Addr_Record := Addr_To_Rec (Coarse_Cal_Addr);
   Coarse_Cal_Value : constant UInt6 := R.Data;
begin
   --  1) Set Flash wait states for 48 MHz (Table 37-40 in sparkfun data sheet)
   --  1 wait state required @ 3.3V & 48MHz
   NVMCTRL_Periph.CTRLB.RWS := Half;

   --  2) Enable XOSC32K clock (External on-board 32.768kHz oscillator),
   --  will be used as DFLL48M reference.

   --  Configure SYSCTRL->XOSC32K settings
   SYSCTRL_Periph.XOSC32K := (
                     WRTLOCK  => 0,
                     STARTUP  => 2,
                     ONDEMAND => 0,
                     RUNSTDBY => 0,
                     AAMPEN   => 0,
                     EN1K     => SYSCTRL_Periph.XOSC32K.EN1K,
                     EN32K    => 1,
                     ENABLE   => SYSCTRL_Periph.XOSC32K.ENABLE,
                     XTALEN   => 1
    );

   --  Enable the Oscillator - Separate step per data sheet (sec 17.6.3)
   SYSCTRL_Periph.XOSC32K.ENABLE := 1;

   --  Wait for XOSC32K to stabilize
   while SYSCTRL_Periph.PCLKSR.XOSC32KRDY = Bit (0) loop
      null;
   end loop;

   --  3. Put XOSC32K as a source of Generic Clock Generator 1

   --  Set the Generic Clock Generator 1 output divider to 1

   GCLK_Periph.GENDIV := (
      ID  => 1,
      DIV => 1
   );

   GCLK_Periph.GENCTRL := (
      ID => 1,
      SRC => Interfaces.SAM.GCLK.Xosc32K,
      GENEN => 1,
      IDC => 1,
      OOV => 0,
      OE => 1,
      DIVSEL => 0,
      RUNSTDBY => 0
   );

   --  GENCTRL is Write-Synchronized...so wait for write to complete
   while GCLK_Periph.STATUS.SYNCBUSY = Bit (1) loop
      null;
   end loop;

   --  4. Put Generic Clock Generator 1 as a source for
   --  Generic Clock Multiplexer 0 (DFLL48M reference).

   GCLK_Periph.CLKCTRL := (
      ID       => Interfaces.SAM.GCLK.Dfll48,
      GEN      => Interfaces.SAM.GCLK.Gclk1,
      CLKEN    => 1,
      WRTLOCK  => 0
   );

   --  5. Enable DFLL48M clock.

   --  DFLL Configuration in Closed Loop mode, cf product data sheet chapter
   --  17.6.7.1 - Closed-Loop Operation
   --  Enable the DFLL48M in open loop mode. Without this step, attempts to go
   --  into closed loop mode at 48 MHz will
   --  result in Processor Reset
   --  PCLKSR.DFLLRDY must be one before writing to the DFLL Control register
   --  Note that the DFLLRDY bit represents status of register synchronization
   --  - NOT clock stability
   --  (see Data Sheet 17.6.14 Synchronization for detail)
   while SYSCTRL_Periph.PCLKSR.DFLLRDY = Bit (0) loop
      null;
   end loop;
   --  SAMD21 Errata says ONDEMAND has to be 0 before configuring DFLL
   SYSCTRL_Periph.DFLLCTRL.ONDEMAND := Bit (0);
   while SYSCTRL_Periph.PCLKSR.DFLLRDY = Bit (0) loop
      null;
   end loop;

   --  Set up the Multiplier, Coarse and Fine steps
   --  Coarse step - use half of the max value (63)
   --  Fine step - use half of the max value (1023)
   --  Multiplier = MAIN_CLK_FREQ (48MHz) / EXT_32K_CLK_FREQ (32768 Hz)
   SYSCTRL_Periph.DFLLMUL := (
      CSTEP => 31,
      FSTEP => 511,
      MUL   => 1465
   );

   --  To reduce lock time, load factory calibrated values into DFLLVAL
   --  Location of value is defined in Data Sheet Table 10-5.
   --  NVM Software Calibration Area Mapping

   --  write the coarse calibration value
   SYSCTRL_Periph.DFLLVAL.COARSE := Coarse_Cal_Value;
   --  Switch DFLL48M to Closed Loop mode and enable WAITLOCK

   SYSCTRL_Periph.DFLLCTRL.ENABLE := Bit (1);
   SYSCTRL_Periph.DFLLCTRL.MODE := Bit (1);
   SYSCTRL_Periph.DFLLCTRL.WAITLOCK := Bit (1);

   while SYSCTRL_Periph.PCLKSR.DFLLRDY = Bit (0) loop
      null;
   end loop;
   --  6. Switch Generic Clock Generator 0 to DFLL48M. CPU will run at 48 MHz.

   --  Now that DFLL48M is running, switch CLKGEN0 source to it
   --  Enable output of GCLK_MAIN to the GCLK_IO[0] GPIO pin
   GCLK_Periph.GENCTRL := (
      ID => 0,
      SRC => Interfaces.SAM.GCLK.Dfll48M,
      GENEN => 1,
      IDC => 1,
      OOV => 0,
      OE => 1,
      DIVSEL => 0,
      RUNSTDBY => 0
   );
   --  GENCTRL is Write-Synchronized...so wait for write to complete
   while GCLK_Periph.STATUS.SYNCBUSY = Bit (1) loop
      null;
   end loop;
   --  Direct the GCLK_IO[0] output to PA28
   PORT_Periph.WRCONFIG0.HWSEL := Bit (1);
   PORT_Periph.WRCONFIG0.WRPINCFG := Bit (1);
   PORT_Periph.WRCONFIG0.WRPMUX := Bit (1);
   PORT_Periph.WRCONFIG0.PMUX := UInt4 (7);
   PORT_Periph.WRCONFIG0.PMUXEN := Bit (1);
   PORT_Periph.WRCONFIG0.PINMASK := UInt16 (2#0001_0000_0000_0000#);

   --  7) Modify prescaler value of OSC8M to produce 8MHz output

   --  Prescale by 1
   SYSCTRL_Periph.OSC8M.PRESC := Interfaces.SAM.SYSCTRL.Val_0;
   --  Oscillator is always on if enabled
   SYSCTRL_Periph.OSC8M.ONDEMAND := Bit (0);

   --  8) Put OSC8M as source for Generic Clock Generator 3

   --  Set the Generic Clock Generator 3 output divider to 1
   GCLK_Periph.GENDIV := (
      ID  => 3,
      DIV => 1
   );

   --  Configure Generic Clock Generator 3 with OSC8M as source
   GCLK_Periph.GENCTRL := (
      ID => 3,
      SRC => Interfaces.SAM.GCLK.Osc8M,
      GENEN => 1,
      IDC => 1,
      OOV => 0,
      OE => 0,
      DIVSEL => 0,
      RUNSTDBY => 0
   );
   --  GENCTRL is Write-Synchronized...so wait for write to complete
   while GCLK_Periph.STATUS.SYNCBUSY = Bit (1) loop
      null;
   end loop;

   --  9) Set CPU and APBx BUS Clocks to 48MHz
   PM_Periph.CPUSEL.CPUDIV := Interfaces.SAM.PM.Div1;
   PM_Periph.APBASEL.APBADIV := Interfaces.SAM.PM.Div1;
   PM_Periph.APBBSEL.APBBDIV := Interfaces.SAM.PM.Div1;
   PM_Periph.APBCSEL.APBCDIV := Interfaces.SAM.PM.Div1;

end Setup_Pll;
