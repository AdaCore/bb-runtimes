------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

pragma Ada_2012;
pragma Suppress (All_Checks);

with Interfaces; use Interfaces;

with Interfaces.RP2040;          use Interfaces.RP2040;
with Interfaces.RP2040.CLOCKS;   use Interfaces.RP2040.CLOCKS;
with Interfaces.RP2040.PLL_SYS;  use Interfaces.RP2040.PLL_SYS;
with Interfaces.RP2040.RESETS;   use Interfaces.RP2040.RESETS;
with Interfaces.RP2040.ROSC;     use Interfaces.RP2040.ROSC;
with Interfaces.RP2040.WATCHDOG; use Interfaces.RP2040.WATCHDOG;
with Interfaces.RP2040.XOSC;     use Interfaces.RP2040.XOSC;

with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.BB.MCU_Parameters;   use System.BB.MCU_Parameters;

--  This initialization procedure mainly initializes the PLLs for clk_sys
--  and clk_ref.

procedure Setup_Clocks is

   function SYS_CLK_SEL_Mask (SRC : CLK_SYS_CTRL_SRC_Field) return UInt32
   is (Shift_Left (1, Natural (CLK_SYS_CTRL_SRC_Field'Pos (SRC))));

   function REF_CLK_SEL_Mask (SRC : CLK_REF_CTRL_SRC_Field) return UInt32
   is (Shift_Left (1, Natural (CLK_REF_CTRL_SRC_Field'Pos (SRC))));

   procedure Enable_XOSC;
   procedure Enable_ROSC;

   procedure Enable_PLL
      (PLL_Periph    : not null access PLL_Peripheral;
       Reference     : Hertz;
       Reference_Div : Natural;
       VCO_Multiple  : Positive;
       Post_Div_1    : Natural;
       Post_Div_2    : Natural);

   procedure Enable_XOSC is
      Startup_Delay : constant STARTUP_DELAY_Field :=
        STARTUP_DELAY_Field ((XOSC_Frequency / 1000) / 256);

      pragma Compile_Time_Error
        (XOSC_Startup_Delay_Mult = 0,
         "XOSC_Startup_Delay_Mult cannot be zero");

      pragma Compile_Time_Error
        (XOSC_Startup_Delay_Mult > (STARTUP_DELAY_Field'Last / Startup_Delay),
         "XOSC_Startup_Delay_Mult is too large");
   begin
      XOSC_Periph.CTRL.FREQ_RANGE := Val_1_15MHZ;

      --  1 millisecond startup delay
      XOSC_Periph.STARTUP.DELAY_k := Startup_Delay * XOSC_Startup_Delay_Mult;

      XOSC_Periph.CTRL.ENABLE := ENABLE;

      while XOSC_Periph.STATUS.STABLE = 0 loop
         null;
      end loop;
   end Enable_XOSC;

   procedure Enable_ROSC is
   begin
      --  Just ensure that ROSC is enabled, don't reset it or change the
      --  frequency
      ROSC_Periph.CTRL.ENABLE := ENABLE;
      while ROSC_Periph.STATUS.STABLE = 0 loop
         null;
      end loop;
   end Enable_ROSC;

   procedure Enable_PLL
      (PLL_Periph    : not null access PLL_Peripheral;
       Reference     : Hertz;
       Reference_Div : Natural;
       VCO_Multiple  : Positive;
       Post_Div_1    : Natural;
       Post_Div_2    : Natural)
   is
      VCO     : constant Hertz := (Reference / Reference_Div) * VCO_Multiple;
      --  ref_mhz : constant Natural := Reference / 1_000_000 / Reference_Div;
      FBDIV   : constant FBDIV_INT_FBDIV_INT_Field := FBDIV_INT_FBDIV_INT_Field
         (VCO / (Reference / Reference_Div));
   begin
      --  Assert (FBDIV in 16 .. 320);
      --  Assert (Post_Div_1 in 1 .. 7);
      --  Assert (Post_Div_2 in 1 .. 7);
      --  Assert (Post_Div_1 >= Post_Div_2);
      --  Assert (ref_mhz <= (VCO / 16));

      --  Ensure PLL is stopped before configuring
      PLL_Periph.PWR := (others => <>);
      PLL_Periph.FBDIV_INT := (others => <>);

      PLL_Periph.CS :=
         (REFDIV => CS_REFDIV_Field (Reference_Div),
          others => <>);

      PLL_Periph.FBDIV_INT.FBDIV_INT := FBDIV;

      --  Turn on PLL
      PLL_Periph.PWR.PD := False;
      PLL_Periph.PWR.VCOPD := False;

      --  Wait for lock
      while not PLL_Periph.CS.LOCK loop
         null;
      end loop;

      --  Setup post dividers
      PLL_Periph.PRIM :=
         (POSTDIV1 => PRIM_POSTDIV1_Field (Post_Div_1),
          POSTDIV2 => PRIM_POSTDIV2_Field (Post_Div_2),
          others   => <>);

      --  Turn on post dividers
      PLL_Periph.PWR.POSTDIVPD := False;
   end Enable_PLL;

   --  PLL input frequency

   PLL_Sys_Input : constant Hertz := Reference / PLL_Sys_Reference_Div;
   PLL_USB_Input : constant Hertz := Reference / PLL_USB_Reference_Div;

   --  VCO frequency

   PLL_Sys_FOUTVCO : constant FOUTVCO_Frequency_Range :=
     PLL_Sys_Input * PLL_Sys_VCO_Multiple;

   PLL_USB_FOUTVCO : constant FOUTVCO_Frequency_Range :=
     PLL_USB_Input * PLL_USB_VCO_Multiple;

   --  Check FBDIV range.

   pragma Compile_Time_Error
     (PLL_Sys_FOUTVCO / PLL_Sys_Input not in FBDIV_Range,
      "Invalid pll_sys configuration (FBDIV range exceeded)");

   pragma Compile_Time_Error
     (PLL_USB_FOUTVCO / PLL_USB_Input not in FBDIV_Range,
      "Invalid pll_usb configuration (FBDIV range exceeded)");

   --  Check final output frequency

   pragma Compile_Time_Error
     (PLL_Sys_FOUTVCO / (PLL_Sys_Post_Div_1 * PLL_Sys_Post_Div_2)
      not in Clk_Sys_Frequency_Range,
      "Invalid pll_sys configuration (excessive PLL output frequency)");

   pragma Compile_Time_Error
     (PLL_USB_FOUTVCO / (PLL_USB_Post_Div_1 * PLL_USB_Post_Div_2)
      not in Clk_USB_Frequency_Range,
      "Invalid pll_usb configuration (does not generate 48 MHz clk_usb)");

   --  Check that the POSTDIV1 is higher than POSTDIV2 when two different
   --  values are required for lower power consumption

   pragma Compile_Time_Error
     (PLL_Sys_Post_Div_1 < PLL_Sys_Post_Div_2,
      "Invalid pll_sys configuration (POSTDIV1 smaller than POSTDIV2)");

   pragma Compile_Time_Error
     (PLL_USB_Post_Div_1 < PLL_USB_Post_Div_2,
      "Invalid pll_usb configuration (POSTDIV1 smaller than POSTDIV2)");

begin
   --  Enable RESUS in case things go badly
   CLOCKS_Periph.CLK_SYS_RESUS_CTRL.ENABLE := 1;

   --  Enable watchdog and maybe XOSC
   if Has_XOSC then
      Enable_XOSC;
   else
      Enable_ROSC;
   end if;

   --  Configure 1 MHz watchdog tick.
   WATCHDOG_Periph.TICK :=
      (CYCLES => TICK_CYCLES_Field (Reference / 1_000_000),
       ENABLE => 1,
       others => <>);

   CLOCKS_Periph.FC0_REF_KHZ.FC0_REF_KHZ :=
     FC0_REF_KHZ_FC0_REF_KHZ_Field (Reference / 1_000);

   --  Ensure that clk_sys = clk_ref before we reset the PLLs
   CLOCKS_Periph.CLK_SYS_CTRL.SRC := clk_ref;
   while CLOCKS_Periph.CLK_SYS_SELECTED /= SYS_CLK_SEL_Mask (clk_ref) loop
      null;
   end loop;

   --  Switch clk_ref to XOSC if available
   if Has_XOSC then
      CLOCKS_Periph.CLK_REF_CTRL.SRC := xosc_clksrc;
      while CLOCKS_Periph.CLK_REF_SELECTED
        /= REF_CLK_SEL_Mask (xosc_clksrc)
      loop
         null;
      end loop;
   else
      CLOCKS_Periph.CLK_REF_CTRL.SRC := rosc_clksrc_ph;
      while CLOCKS_Periph.CLK_REF_SELECTED
        /= REF_CLK_SEL_Mask (rosc_clksrc_ph)
      loop
         null;
      end loop;
   end if;
   CLOCKS_Periph.CLK_REF_DIV := (INT => 1, others => <>);

   --  Wait for PLLs to come out of reset
   RESETS_Periph.RESET.pll_sys := 0;
   RESETS_Periph.RESET.pll_usb := 0;
   while RESETS_Periph.RESET_DONE.pll_sys = 0
         or else RESETS_Periph.RESET_DONE.pll_usb = 0
   loop
      null;
   end loop;

   Enable_PLL (System.BB.MCU_Parameters.PLL_SYS_Periph'Access,
               Reference     => Reference,
               Reference_Div => PLL_Sys_Reference_Div,
               VCO_Multiple  => PLL_Sys_VCO_Multiple,
               Post_Div_1    => PLL_Sys_Post_Div_1,
               Post_Div_2    => PLL_Sys_Post_Div_2);
   Enable_PLL (System.BB.MCU_Parameters.PLL_USB_Periph'Access,
               Reference     => Reference,
               Reference_Div => PLL_USB_Reference_Div,
               VCO_Multiple  => PLL_USB_VCO_Multiple,
               Post_Div_1    => PLL_USB_Post_Div_1,
               Post_Div_2    => PLL_USB_Post_Div_2);

   --  Switch clk_sys to pll_sys
   CLOCKS_Periph.CLK_SYS_DIV         := (INT => 1, FRAC => 0);
   CLOCKS_Periph.CLK_SYS_CTRL.AUXSRC := clksrc_pll_sys;
   CLOCKS_Periph.CLK_SYS_CTRL.SRC    := clksrc_clk_sys_aux;
   while CLOCKS_Periph.CLK_SYS_SELECTED
     /= SYS_CLK_SEL_Mask (clksrc_clk_sys_aux)
   loop
      null;
   end loop;

   --  Switch clk_usb to pll_usb
   CLOCKS_Periph.CLK_USB_DIV.INT := 1;
   CLOCKS_Periph.CLK_USB_CTRL.AUXSRC := clksrc_pll_usb;

   --  Switch clk_adc to pll_usb
   CLOCKS_Periph.CLK_ADC_DIV.INT := 1;
   CLOCKS_Periph.CLK_ADC_CTRL.AUXSRC := clksrc_pll_usb;

   --  Switch clk_rtc to pll_usb / 1024 = 46_875 Hz
   CLOCKS_Periph.CLK_RTC_DIV := (INT => 1024, FRAC => 0);
   CLOCKS_Periph.CLK_RTC_CTRL.AUXSRC := clksrc_pll_usb;
end Setup_Clocks;