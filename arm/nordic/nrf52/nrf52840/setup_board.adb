------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          Copyright (C) 2012-2019, Free Software Foundation, Inc.         --
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

--  This file is based on the startup code from the following file in Nordic's
--  nRF5 SDK (version 15.1.0): modules/nrfx/mdk/system_nrf52840.c for errata
--  workarounds and configuration of the SWD pins and reset pin.
--
--  This Errata_X functions detect if certain errata are applicable for the
--  MCU. If they are applicable, then a workaround is applied for the errata.
--  Some of these errata workarounds rely on reading and/or writing registers
--  that are not documented in the datasheet. As mentioned above, these
--  register addresses and values are copied from Nordic's nRF5 SDK.

pragma Ada_2012; -- To work around pre-commit check?
pragma Suppress (All_Checks);

with System;
with System.Machine_Code;    use System.Machine_Code;

with Interfaces.NRF52;       use Interfaces.NRF52;
with Interfaces.NRF52.CCM;   use Interfaces.NRF52.CCM;
with Interfaces.NRF52.CLOCK; use Interfaces.NRF52.CLOCK;
with Interfaces.NRF52.FICR;  use Interfaces.NRF52.FICR;
with Interfaces.NRF52.GPIO;  use Interfaces.NRF52.GPIO;
with Interfaces.NRF52.NVMC;  use Interfaces.NRF52.NVMC;
with Interfaces.NRF52.UICR;  use Interfaces.NRF52.UICR;
with Interfaces.NRF52.TEMP;  use Interfaces.NRF52.TEMP;

procedure Setup_Board is

   ---------------------------
   --  Board Configuration  --
   ---------------------------

   Use_HFXO : constant Boolean := False;
   --  Set to True to use the high-frequency external oscillator (HFXO).
   --  When False, the on-chip oscillator is used.
   --  The HFXO can also be turned on and off later by the main program.

   LFCLK_Source : constant LFCLKSRC_SRC_Field := Xtal;
   --  Selects the source for the LFCLK.
   --  Xtal selects the external 32.768 kHz crystal (LFXO).
   --  Rc selects the internal 32.768 kHz RC oscillator.
   --  Synth selects the LFCLK synthesized from the 16 MHz HFCLK.

   Use_SWO_Trace : constant Boolean := True;
   --  Set to True to enable the SWO trace pins.

   Use_Reset_Pin : constant Boolean := True;
   --  When True, P0.18 will be configured as the reset pin.

   LFRC_Used : constant Boolean := LFCLK_Source = Rc;
   --  When the LFRC oscillator is not used it is put into ultra-low power mode

   --------------------------
   --  Errata Workarounds  --
   --------------------------

   --  Some of these registers are not documented in the Objective Product Spec
   --  but they are used in the nRF5 SDK startup code to detect when
   --  certain errata are applicable.

   Undocumented_Reg_1 : UInt32
      with Address => System'To_Address (16#1000_0130#);

   Undocumented_Reg_2 : UInt32
      with Address => System'To_Address (16#1000_0134#);

   --  Undocumented Registers used for the workaround of Erratas 98, 115, 120.
   Errata_98_Reg : UInt32
      with Volatile, Address => System'To_Address (16#4000_568C#);
   Errata_115_Reg_1 : UInt32
      with Volatile, Address => System'To_Address (16#4000_0EE4#);
   Errata_115_Reg_2 : UInt32
      with Volatile, Address => System'To_Address (16#1000_0258#);
   Errata_120_Reg : UInt32
      with Volatile, Address => System'To_Address (16#4002_9640#);

   --  The POWER.RESETREAS register.
   --  We define this here instead of including Interfaces.POWER as a
   --  workaround for a warning about unused bits in the definition
   --  of RAM_Cluster in the auto-generated code.
   POWER_RESETREAS : UInt32
      with Volatile, Address => System'To_Address (16#4000_0400#);

   --  The following functions detect if different errata are applicable on
   --  the specific MCU revision. For example, Errata_36 checks if a errata #36
   --  is applicable ("CLOCK: Some registers are not reset when expected").

   function Errata_36 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_66 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_98 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_103 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_115 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_120 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   function Errata_136 return Boolean is
      (Undocumented_Reg_1 = 8 and Undocumented_Reg_2 = 0)
     with Inline_Always;

   procedure NVIC_SystemReset;

   procedure NVIC_SystemReset is
      SCB_AIRCR : UInt32
         with Volatile, Address => System'To_Address (16#E000_ED0C#);

      VECTKEY : constant UInt32 := 16#05FA_0000#;

      PRIGROUP_Mask : constant UInt32 := 16#0000_0700#;

      SYSRESETREQ : constant UInt32 := 16#0000_0004#;

   begin
      --  Ensure all outstanding memory accesses including buffered write
      --  are completed before reset
      Asm ("dsb", Volatile => True);

      SCB_AIRCR := VECTKEY or (SCB_AIRCR and PRIGROUP_Mask) or SYSRESETREQ;

      Asm ("dsb", Volatile => True);

      loop
         null;
      end loop;
   end NVIC_SystemReset;

begin

   --  Enable SWO trace pins
   if Use_SWO_Trace then
      CLOCK_Periph.TRACECONFIG.TRACEMUX := Serial;
      P1_Periph.PIN_CNF (0) := PIN_CNF_Register'
         (DIR            => Output,
          INPUT          => Connect,
          PULL           => Disabled,
          Reserved_4_7   => 0,
          DRIVE          => H0H1,
          Reserved_11_15 => 0,
          SENSE          => Disabled,
          Reserved_18_31 => 0);
   end if;

   --  Workaround for Errata 36 "CLOCK: Some registers are not reset when
   --  expected".
   if Errata_36 then
      CLOCK_Periph.EVENTS_DONE := (EVENTS_DONE => 0, others => <>);
      CLOCK_Periph.EVENTS_CTTO := (EVENTS_CTTO => 0, others => <>);
      CLOCK_Periph.CTIV.CTIV   := 0;
   end if;

   --  Workaround for Errata 66 "TEMP: Linearity specification not met with
   --  default settings"
   if Errata_66 then
      TEMP_Periph.A0.A0 := FICR_Periph.TEMP.A0.A;
      TEMP_Periph.A1.A1 := FICR_Periph.TEMP.A1.A;
      TEMP_Periph.A2.A2 := FICR_Periph.TEMP.A2.A;
      TEMP_Periph.A3.A3 := FICR_Periph.TEMP.A3.A;
      TEMP_Periph.A4.A4 := FICR_Periph.TEMP.A4.A;
      TEMP_Periph.A5.A5 := FICR_Periph.TEMP.A5.A;
      TEMP_Periph.B0.B0 := FICR_Periph.TEMP.B0.B;
      TEMP_Periph.B1.B1 := FICR_Periph.TEMP.B1.B;
      TEMP_Periph.B2.B2 := FICR_Periph.TEMP.B2.B;
      TEMP_Periph.B3.B3 := FICR_Periph.TEMP.B3.B;
      TEMP_Periph.B4.B4 := FICR_Periph.TEMP.B4.B;
      TEMP_Periph.B5.B5 := FICR_Periph.TEMP.B5.B;
      TEMP_Periph.T0.T0 := FICR_Periph.TEMP.T0.T;
      TEMP_Periph.T1.T1 := FICR_Periph.TEMP.T1.T;
      TEMP_Periph.T2.T2 := FICR_Periph.TEMP.T2.T;
      TEMP_Periph.T3.T3 := FICR_Periph.TEMP.T3.T;
      TEMP_Periph.T4.T4 := FICR_Periph.TEMP.T4.T;
   end if;

   --  Workaround for Errata 98 "NFCT: Not able to communicate with the peer"
   if Errata_98 then
      Errata_98_Reg := 16#0003_8148#;
   end if;

   --  Workaround for Errata 103 "CCM: Wrong reset value of CCM MAXPACKETSIZE"
   if Errata_103 then
      CCM_Periph.MAXPACKETSIZE.MAXPACKETSIZE := 16#FB#;
   end if;

   --  Workaround for Errata 115 "RAM: RAM content cannot be trusted upon
   --  waking up from System ON Idle or System OFF mode"
   if Errata_115 then
      Errata_115_Reg_1 := ((Errata_115_Reg_1 and 16#FFFF_FFF0#)
                           or (Errata_115_Reg_2 and 16#0000_000F#));
   end if;

   --  Workaround for Errata 120 "QSPI: Data read or written is corrupted"
   if Errata_120 then
      Errata_120_Reg := 16#200#;
   end if;

   --  Workaround for Errata 136 "System: Bits in RESETREAS are set when they
   --  should not be"
   if Errata_136 then
      --  Clear all flags except RESETPIN if RESETPIN is the reset reason
      if (POWER_RESETREAS and 16#0000_0001#) /= 0 then
         POWER_RESETREAS := 16#FFFF_FFFE#;
      end if;
   end if;

   if Use_Reset_Pin then
      --  Enable nRESET pin on P0.18
      if UICR_Periph.PSELRESET (0).CONNECT = Disconnected or
         UICR_Periph.PSELRESET (1).CONNECT = Disconnected
      then
         NVMC_Periph.CONFIG := CONFIG_Register'
            (WEN           => Wen,
             Reserved_2_31 => 0);
         loop
            exit when NVMC_Periph.READY.READY = Ready;
         end loop;

         UICR_Periph.PSELRESET (0) := PSELRESET_Register'
            (PIN           => 18,
             PORT          => 0,
             Reserved_6_30 => 0,
             CONNECT       => Connected);
         loop
            exit when NVMC_Periph.READY.READY = Ready;
         end loop;

         UICR_Periph.PSELRESET (1) := PSELRESET_Register'
            (PIN           => 18,
             PORT          => 0,
             Reserved_6_30 => 0,
             CONNECT       => Connected);
         loop
            exit when NVMC_Periph.READY.READY = Ready;
         end loop;

         NVMC_Periph.CONFIG := CONFIG_Register'
            (WEN           => Ren,
             Reserved_2_31 => 0);
         loop
            exit when NVMC_Periph.READY.READY = Ready;
         end loop;

         NVIC_SystemReset;
      end if;
   end if;

   --  Configure the 32.768 kHz external crystal.
   --  The LFCLK will be started later, if required by the runtime.
   --
   --  The Ravenscar runtime uses LFCLK as its timing source for task delays,
   --  so LFCLK will be started by System.BB.Board_Support.Initialize_Board.
   --
   --  The ZFP runtime does not use LFCLK, so it is not started in ZFP.
   CLOCK_Periph.LFCLKSRC := LFCLKSRC_Register'
     (SRC            => LFCLK_Source,
      Reserved_2_15  => 0,
      BYPASS         => Disabled,
      EXTERNAL       => Disabled,
      Reserved_18_31 => 0);

   --  If the internal RC oscillator is not used as the LFCLK source, then
   --  put it into ultra-low power (ULP) mode to save some power.
   if not LFRC_Used then
      CLOCK_Periph.LFRCMODE.MODE := Ulp;
   end if;

   --  Optionally enable the external HFXO.
   --  If HFXO is disabled, then the HFCLK will use the internal HF oscillator
   if Use_HFXO then
      CLOCK_Periph.TASKS_HFCLKSTART := (TASKS_HFCLKSTART => 1,
                                        others           => <>);
   end if;
end Setup_Board;
