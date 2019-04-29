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
--  nRF5 SDK (version 15.1.0): modules/nrfx/mdk/system_nrf52.c for errata
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

with Interfaces;             use Interfaces;
with Interfaces.NRF52;       use Interfaces.NRF52;
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

   --------------------------
   --  Errata Workarounds  --
   --------------------------

   --  Some of these registers are not documented in the Objective Product Spec
   --  but they are used in the nRF5 SDK startup code to detect when
   --  certain errata are applicable.

   Undocumented_Reg_1 : UInt32
      with Address => System'To_Address (16#F000_0FE0#);

   Undocumented_Reg_2 : UInt32
      with Address => System'To_Address (16#F000_0FE4#);

   Undocumented_Reg_3 : UInt32
      with Address => System'To_Address (16#F000_0FE8#);

   Undocumented_Reg_4 : UInt32
      with Address => System'To_Address (16#1000_0130#);

   Undocumented_Reg_5 : UInt32
      with Address => System'To_Address (16#1000_0134#);

   --  Undocumented registers used for the workaround of erratas 12, 16
   Errata_12_Reg_1 : UInt32
      with Volatile, Address => System'To_Address (16#4001_3540#);
   Errata_12_Reg_2 : UInt32
      with Volatile, Address => System'To_Address (16#1000_0324#);
   Errata_16_Reg : UInt32
      with Volatile, Address => System'To_Address (16#4007_C074#);
   Errata_31_Reg_1 : UInt32
      with Volatile, Address => System'To_Address (16#4000_053C#);
   Errata_31_Reg_2 : UInt32
      with Volatile, Address => System'To_Address (16#1000_0244#);
   Errata_37_Reg : UInt32
      with Volatile, Address => System'To_Address (16#4000_05A0#);
   Errata_57_Reg_1 : UInt32
      with Volatile, Address => System'To_Address (16#4000_5610#);
   Errata_57_Reg_2 : UInt32
      with Volatile, Address => System'To_Address (16#4000_5688#);
   Errata_57_Reg_3 : UInt32
      with Volatile, Address => System'To_Address (16#4000_5618#);
   Errata_57_Reg_4 : UInt32
      with Volatile, Address => System'To_Address (16#4000_5614#);
   Errata_108_Reg_1 : UInt32
      with Volatile, Address => System'To_Address (16#4000_0EE4#);
   Errata_108_Reg_2 : UInt32
      with Volatile, Address => System'To_Address (16#1000_0258#);
   Errata_182_Reg : UInt32
      with Volatile, Address => System'To_Address (16#4000_173C#);

   CoreDebug_DEMCR : UInt32
      with Volatile, Address => System'To_Address (16#E000_EDFC#);

   DEMCR_TRCENA_Mask : constant UInt32 := 16#0100_0000#;

   --  The POWER.RESETREAS register.
   --  We define this here instead of including Interfaces.POWER as a
   --  workaround for a warning about unused bits in the definition
   --  of RAM_Cluster in the auto-generated code.
   POWER_RESETREAS : UInt32
      with Volatile, Address => System'To_Address (16#4000_0400#);

   --  The following functions detect if different errata are applicable on
   --  the specific MCU revision. For example, Errata_12 checks if a errata #12
   --  is applicable ("COMP: Reference ladder not correctly calibrated").

   function Errata_12 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) in 16#30# | 16#40# | 16#50#))
     with Inline_Always;

   function Errata_16 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) = 16#30#))
     with Inline_Always;

   function Errata_31 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) in 16#30# | 16#40# | 16#50#))
     with Inline_Always;

   function Errata_32 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) = 16#30#))
     with Inline_Always;

   function Errata_36 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) in 16#30# | 16#40# | 16#50#))
     with Inline_Always;

   function Errata_37 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) = 16#30#))
     with Inline_Always;

   function Errata_57 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) = 16#30#))
     with Inline_Always;

   function Errata_66 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) = 16#50#))
     with Inline_Always;

   function Errata_108 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) in 16#30# | 16#40# | 16#50#))
     with Inline_Always;

   function Errata_136 return Boolean is
      (((Undocumented_Reg_1 and 16#FF#) = 6)
       and ((Undocumented_Reg_2 and 16#F#) = 0)
       and ((Undocumented_Reg_3 and 16#F0#) in 16#30# | 16#40# | 16#50#))
     with Inline_Always;

   function Errata_182 return Boolean is
      (Undocumented_Reg_4 = 6 and Undocumented_Reg_5 = 6)
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
      CoreDebug_DEMCR := CoreDebug_DEMCR or DEMCR_TRCENA_Mask;
      CLOCK_Periph.TRACECONFIG.TRACEMUX := Serial;
      P0_Periph.PIN_CNF (18) := PIN_CNF_Register'
         (DIR            => Output,
          INPUT          => Connect,
          PULL           => Disabled,
          Reserved_4_7   => 0,
          DRIVE          => H0H1,
          Reserved_11_15 => 0,
          SENSE          => Disabled,
          Reserved_18_31 => 0);
   end if;

   --  Workaround for Errata 12 "COMP: Reference ladder not correctly
   --  calibrated"
   if Errata_12 then
      Errata_12_Reg_1 := Shift_Right (Errata_12_Reg_2 and 16#1F00#, 8);
   end if;

   --  Workaround for Errata 16 "System: RAM may be corrupt on wakeup from
   --  CPU IDLE"
   if Errata_16 then
      Errata_16_Reg := 3131961357;
   end if;

   --  Workaround for Errata 31 "CLOCK: Calibration values are not correctly
   --  loaded from FICR at reset"
   if Errata_31 then
      Errata_31_Reg_1 := Shift_Right (Errata_31_Reg_2 and 16#E000#, 13);
   end if;

   --  Workaround for Errata 32 "DIF: Debug session automatically enables
   --  TracePort pins"
   if Errata_32 then
      CoreDebug_DEMCR := CoreDebug_DEMCR and (not DEMCR_TRCENA_Mask);
   end if;

   --  Workaround for Errata 36 "CLOCK: Some registers are not reset when
   --  expected"
   if Errata_36 then
      CLOCK_Periph.EVENTS_DONE := (EVENTS_DONE => 0, others => <>);
      CLOCK_Periph.EVENTS_CTTO := (EVENTS_CTTO => 0, others => <>);
      CLOCK_Periph.CTIV        := (CTIV => 0, Reserved_7_31 => 0);
   end if;

   --  Workaround for Errata 37 "RADIO: Encryption engine is slow by default"
   if Errata_37 then
      Errata_37_Reg := 3;
   end if;

   --  Workaround for Errata 57 "NFCT: NFC Modulation amplitude"
   if Errata_57 then
      Errata_57_Reg_1 := 16#0000_0005#;
      Errata_57_Reg_2 := 16#0000_0001#;
      Errata_57_Reg_3 := 16#0000_0000#;
      Errata_57_Reg_4 := 16#0000_003F#;
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

   --  Workaround for Errata 108 "RAM: RAM content cannot be trusted upon
   --  waking up from System ON Idle or System OFF mode"
   if Errata_108 then
      Errata_108_Reg_1 := Errata_108_Reg_2 and 16#0000_004F#;
   end if;

   --  Workaround for Errata 136 "System: Bits in RESETREAS are set when they
   --  should not be"
   if Errata_136 then
      --  Clear all flags except RESETPIN if RESETPIN is the reset reason
      if (POWER_RESETREAS and 16#0000_0001#) /= 0 then
         POWER_RESETREAS := 16#FFFF_FFFE#;
      end if;
   end if;

   --  Workaround for Errata 182 "RADIO: Fixes for anomalies #102, #106, and
   --  #107 do not take effect"
   if Errata_182 then
      Errata_182_Reg := Errata_182_Reg or 16#200#;
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
            (PIN           => 21,
             Reserved_6_30 => 0,
             CONNECT       => Connected);
         loop
            exit when NVMC_Periph.READY.READY = Ready;
         end loop;

         UICR_Periph.PSELRESET (1) := PSELRESET_Register'
            (PIN           => 21,
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

   --  Optionally enable the external HFXO.
   --  If HFXO is disabled, then the HFCLK will use the internal HF oscillator
   if Use_HFXO then
      CLOCK_Periph.TASKS_HFCLKSTART := (TASKS_HFCLKSTART => 1, others => <>);
   end if;
end Setup_Board;
