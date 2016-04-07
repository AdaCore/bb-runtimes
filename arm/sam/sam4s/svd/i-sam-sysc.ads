--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAM4SD32C.svd

pragma Ada_2012;

with Interfaces.Bit_Types;
with System;

package Interfaces.SAM.SYSC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   ----------------------
   -- RSTC_CR_Register --
   ----------------------

   subtype CR_KEY_Field is Interfaces.Bit_Types.Byte;

   --  Control Register
   type RSTC_CR_Register is record
      --  Write-only. Processor Reset
      PROCRST       : Boolean := False;
      --  unspecified
      Reserved_1_1  : Interfaces.Bit_Types.Bit := 16#0#;
      --  Write-only. Peripheral Reset
      PERRST        : Boolean := False;
      --  Write-only. External Reset
      EXTRST        : Boolean := False;
      --  unspecified
      Reserved_4_23 : Interfaces.Bit_Types.UInt20 := 16#0#;
      --  Write-only. System Reset Key
      KEY           : CR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_CR_Register use record
      PROCRST       at 0 range 0 .. 0;
      Reserved_1_1  at 0 range 1 .. 1;
      PERRST        at 0 range 2 .. 2;
      EXTRST        at 0 range 3 .. 3;
      Reserved_4_23 at 0 range 4 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   ----------------------
   -- RSTC_SR_Register --
   ----------------------

   subtype SR_RSTTYP_Field is Interfaces.Bit_Types.UInt3;

   --  Status Register
   type RSTC_SR_Register is record
      --  Read-only. User Reset Status
      URSTS          : Boolean := False;
      --  unspecified
      Reserved_1_7   : Interfaces.Bit_Types.UInt7;
      --  Read-only. Reset Type
      RSTTYP         : SR_RSTTYP_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : Interfaces.Bit_Types.UInt5;
      --  Read-only. NRST Pin Level
      NRSTL          : Boolean := False;
      --  Read-only. Software Reset Command in Progress
      SRCMP          : Boolean := False;
      --  unspecified
      Reserved_18_31 : Interfaces.Bit_Types.UInt14;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_SR_Register use record
      URSTS          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      RSTTYP         at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NRSTL          at 0 range 16 .. 16;
      SRCMP          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   ----------------------
   -- RSTC_MR_Register --
   ----------------------

   subtype MR_ERSTL_Field is Interfaces.Bit_Types.UInt4;
   subtype MR_KEY_Field is Interfaces.Bit_Types.Byte;

   --  Mode Register
   type RSTC_MR_Register is record
      --  User Reset Enable
      URSTEN         : Boolean := True;
      --  unspecified
      Reserved_1_3   : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  User Reset Interrupt Enable
      URSTIEN        : Boolean := False;
      --  unspecified
      Reserved_5_7   : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  External Reset Length
      ERSTL          : MR_ERSTL_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : Interfaces.Bit_Types.UInt12 := 16#0#;
      --  Password
      KEY            : MR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_MR_Register use record
      URSTEN         at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      URSTIEN        at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      ERSTL          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      KEY            at 0 range 24 .. 31;
   end record;

   ----------------------
   -- SUPC_CR_Register --
   ----------------------

   --  Voltage Regulator Off
   type VROFF_Field is
     (
      --  no effect.
      No_Effect,
      --  if KEY is correct, asserts vddcore_nreset and stops the voltage
      --  regulator.
      Stop_Vreg)
     with Size => 1;
   for VROFF_Field use
     (No_Effect => 0,
      Stop_Vreg => 1);

   --  Crystal Oscillator Select
   type XTALSEL_Field is
     (
      --  no effect.
      No_Effect,
      --  if KEY is correct, switches the slow clock on the crystal oscillator
      --  output.
      Crystal_Sel)
     with Size => 1;
   for XTALSEL_Field use
     (No_Effect => 0,
      Crystal_Sel => 1);

   --  Supply Controller Control Register
   type SUPC_CR_Register is record
      --  unspecified
      Reserved_0_1  : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  Write-only. Voltage Regulator Off
      VROFF         : VROFF_Field := No_Effect;
      --  Write-only. Crystal Oscillator Select
      XTALSEL       : XTALSEL_Field := No_Effect;
      --  unspecified
      Reserved_4_23 : Interfaces.Bit_Types.UInt20 := 16#0#;
      --  Write-only. Password
      KEY           : CR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_CR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      VROFF         at 0 range 2 .. 2;
      XTALSEL       at 0 range 3 .. 3;
      Reserved_4_23 at 0 range 4 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   ------------------------
   -- SUPC_SMMR_Register --
   ------------------------

   subtype SMMR_SMTH_Field is Interfaces.Bit_Types.UInt4;

   --  Supply Monitor Sampling Period
   type SMSMPL_Field is
     (
      --  Supply Monitor disabled
      Smd,
      --  Continuous Supply Monitor
      Csm,
      --  Supply Monitor enabled one SLCK period every 32 SLCK periods
      SMSMPL_Field_32Slck,
      --  Supply Monitor enabled one SLCK period every 256 SLCK periods
      SMSMPL_Field_256Slck,
      --  Supply Monitor enabled one SLCK period every 2,048 SLCK periods
      SMSMPL_Field_2048Slck)
     with Size => 3;
   for SMSMPL_Field use
     (Smd => 0,
      Csm => 1,
      SMSMPL_Field_32Slck => 2,
      SMSMPL_Field_256Slck => 3,
      SMSMPL_Field_2048Slck => 4);

   --  Supply Monitor Reset Enable
   type SMRSTEN_Field is
     (
      --  the core reset signal "vddcore_nreset" is not affected when a supply
      --  monitor detection occurs.
      Not_Enable,
      --  the core reset signal, vddcore_nreset is asserted when a supply
      --  monitor detection occurs.
      Enable)
     with Size => 1;
   for SMRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Supply Monitor Interrupt Enable
   type SMIEN_Field is
     (
      --  the SUPC interrupt signal is not affected when a supply monitor
      --  detection occurs.
      Not_Enable,
      --  the SUPC interrupt signal is asserted when a supply monitor detection
      --  occurs.
      Enable)
     with Size => 1;
   for SMIEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Supply Controller Supply Monitor Mode Register
   type SUPC_SMMR_Register is record
      --  Supply Monitor Threshold
      SMTH           : SMMR_SMTH_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : Interfaces.Bit_Types.UInt4 := 16#0#;
      --  Supply Monitor Sampling Period
      SMSMPL         : SMSMPL_Field := Smd;
      --  unspecified
      Reserved_11_11 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Supply Monitor Reset Enable
      SMRSTEN        : SMRSTEN_Field := Not_Enable;
      --  Supply Monitor Interrupt Enable
      SMIEN          : SMIEN_Field := Not_Enable;
      --  unspecified
      Reserved_14_31 : Interfaces.Bit_Types.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SMMR_Register use record
      SMTH           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      SMSMPL         at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SMRSTEN        at 0 range 12 .. 12;
      SMIEN          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ----------------------
   -- SUPC_MR_Register --
   ----------------------

   --  Brownout Detector Reset Enable
   type BODRSTEN_Field is
     (
      --  the core reset signal "vddcore_nreset" is not affected when a
      --  brownout detection occurs.
      Not_Enable,
      --  the core reset signal, vddcore_nreset is asserted when a brownout
      --  detection occurs.
      Enable)
     with Size => 1;
   for BODRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Brownout Detector Disable
   type BODDIS_Field is
     (
      --  the core brownout detector is enabled.
      Enable,
      --  the core brownout detector is disabled.
      Disable)
     with Size => 1;
   for BODDIS_Field use
     (Enable => 0,
      Disable => 1);

   --  Voltage Regulator enable
   type ONREG_Field is
     (
      --  Internal voltage regulator is not used (external power supply is
      --  used)
      Onreg_Unused,
      --  internal voltage regulator is used
      Onreg_Used)
     with Size => 1;
   for ONREG_Field use
     (Onreg_Unused => 0,
      Onreg_Used => 1);

   --  Oscillator Bypass
   type OSCBYPASS_Field is
     (
      --  no effect. Clock selection depends on XTALSEL value.
      No_Effect,
      --  the 32-KHz XTAL oscillator is selected and is put in bypass mode.
      Bypass)
     with Size => 1;
   for OSCBYPASS_Field use
     (No_Effect => 0,
      Bypass => 1);

   --  Supply Controller Mode Register
   type SUPC_MR_Register is record
      --  unspecified
      Reserved_0_11  : Interfaces.Bit_Types.UInt12 := 16#A00#;
      --  Brownout Detector Reset Enable
      BODRSTEN       : BODRSTEN_Field := Enable;
      --  Brownout Detector Disable
      BODDIS         : BODDIS_Field := Enable;
      --  Voltage Regulator enable
      ONREG          : ONREG_Field := Onreg_Used;
      --  unspecified
      Reserved_15_19 : Interfaces.Bit_Types.UInt5 := 16#0#;
      --  Oscillator Bypass
      OSCBYPASS      : OSCBYPASS_Field := No_Effect;
      --  unspecified
      Reserved_21_23 : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  Password Key
      KEY            : MR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_MR_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      BODRSTEN       at 0 range 12 .. 12;
      BODDIS         at 0 range 13 .. 13;
      ONREG          at 0 range 14 .. 14;
      Reserved_15_19 at 0 range 15 .. 19;
      OSCBYPASS      at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      KEY            at 0 range 24 .. 31;
   end record;

   ------------------------
   -- SUPC_WUMR_Register --
   ------------------------

   --  Supply Monitor Wake Up Enable
   type SMEN_Field is
     (
      --  the supply monitor detection has no wake up effect.
      Not_Enable,
      --  the supply monitor detection forces the wake up of the core power
      --  supply.
      Enable)
     with Size => 1;
   for SMEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Real Time Timer Wake Up Enable
   type RTTEN_Field is
     (
      --  the RTT alarm signal has no wake up effect.
      Not_Enable,
      --  the RTT alarm signal forces the wake up of the core power supply.
      Enable)
     with Size => 1;
   for RTTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Real Time Clock Wake Up Enable
   type RTCEN_Field is
     (
      --  the RTC alarm signal has no wake up effect.
      Not_Enable,
      --  the RTC alarm signal forces the wake up of the core power supply.
      Enable)
     with Size => 1;
   for RTCEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low power Debouncer ENable WKUP0
   type LPDBCEN0_Field is
     (
      --  the WKUP0 input pin is not connected with low power debouncer.
      Not_Enable,
      --  the WKUP0 input pin is connected with low power debouncer and can
      --  force a core wake up.
      Enable)
     with Size => 1;
   for LPDBCEN0_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low power Debouncer ENable WKUP1
   type LPDBCEN1_Field is
     (
      --  the WKUP1input pin is not connected with low power debouncer.
      Not_Enable,
      --  the WKUP1 input pin is connected with low power debouncer and can
      --  force a core wake up.
      Enable)
     with Size => 1;
   for LPDBCEN1_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low power Debouncer Clear
   type LPDBCCLR_Field is
     (
      --  a low power debounce event does not create an immediate clear on
      --  first half GPBR registers.
      Not_Enable,
      --  a low power debounce event on WKUP0 or WKUP1 generates an immediate
      --  clear on first half GPBR registers.
      Enable)
     with Size => 1;
   for LPDBCCLR_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Wake Up Inputs Debouncer Period
   type WKUPDBC_Field is
     (
      --  Immediate, no debouncing, detected active at least on one Slow Clock
      --  edge.
      Immediate,
      --  WKUPx shall be in its active state for at least 3 SLCK periods
      WKUPDBC_Field_3_Sclk,
      --  WKUPx shall be in its active state for at least 32 SLCK periods
      WKUPDBC_Field_32_Sclk,
      --  WKUPx shall be in its active state for at least 512 SLCK periods
      WKUPDBC_Field_512_Sclk,
      --  WKUPx shall be in its active state for at least 4,096 SLCK periods
      WKUPDBC_Field_4096_Sclk,
      --  WKUPx shall be in its active state for at least 32,768 SLCK periods
      WKUPDBC_Field_32768_Sclk)
     with Size => 3;
   for WKUPDBC_Field use
     (Immediate => 0,
      WKUPDBC_Field_3_Sclk => 1,
      WKUPDBC_Field_32_Sclk => 2,
      WKUPDBC_Field_512_Sclk => 3,
      WKUPDBC_Field_4096_Sclk => 4,
      WKUPDBC_Field_32768_Sclk => 5);

   --  Low Power DeBounCer Period
   type LPDBC_Field is
     (
      --  Disable the low power debouncer.
      Disable,
      --  WKUP0/1 in its active state for at least 2 RTCOUT0 periods
      LPDBC_Field_2_Rtcout0,
      --  WKUP0/1 in its active state for at least 3 RTCOUT0 periods
      LPDBC_Field_3_Rtcout0,
      --  WKUP0/1 in its active state for at least 4 RTCOUT0 periods
      LPDBC_Field_4_Rtcout0,
      --  WKUP0/1 in its active state for at least 5 RTCOUT0 periods
      LPDBC_Field_5_Rtcout0,
      --  WKUP0/1 in its active state for at least 6 RTCOUT0 periods
      LPDBC_Field_6_Rtcout0,
      --  WKUP0/1 in its active state for at least 7 RTCOUT0 periods
      LPDBC_Field_7_Rtcout0,
      --  WKUP0/1 in its active state for at least 8 RTCOUT0 periods
      LPDBC_Field_8_Rtcout0)
     with Size => 3;
   for LPDBC_Field use
     (Disable => 0,
      LPDBC_Field_2_Rtcout0 => 1,
      LPDBC_Field_3_Rtcout0 => 2,
      LPDBC_Field_4_Rtcout0 => 3,
      LPDBC_Field_5_Rtcout0 => 4,
      LPDBC_Field_6_Rtcout0 => 5,
      LPDBC_Field_7_Rtcout0 => 6,
      LPDBC_Field_8_Rtcout0 => 7);

   --  Supply Controller Wake Up Mode Register
   type SUPC_WUMR_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Supply Monitor Wake Up Enable
      SMEN           : SMEN_Field := Not_Enable;
      --  Real Time Timer Wake Up Enable
      RTTEN          : RTTEN_Field := Not_Enable;
      --  Real Time Clock Wake Up Enable
      RTCEN          : RTCEN_Field := Not_Enable;
      --  unspecified
      Reserved_4_4   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Low power Debouncer ENable WKUP0
      LPDBCEN0       : LPDBCEN0_Field := Not_Enable;
      --  Low power Debouncer ENable WKUP1
      LPDBCEN1       : LPDBCEN1_Field := Not_Enable;
      --  Low power Debouncer Clear
      LPDBCCLR       : LPDBCCLR_Field := Not_Enable;
      --  unspecified
      Reserved_8_11  : Interfaces.Bit_Types.UInt4 := 16#0#;
      --  Wake Up Inputs Debouncer Period
      WKUPDBC        : WKUPDBC_Field := Immediate;
      --  unspecified
      Reserved_15_15 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Low Power DeBounCer Period
      LPDBC          : LPDBC_Field := Disable;
      --  unspecified
      Reserved_19_31 : Interfaces.Bit_Types.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_WUMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      SMEN           at 0 range 1 .. 1;
      RTTEN          at 0 range 2 .. 2;
      RTCEN          at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      LPDBCEN0       at 0 range 5 .. 5;
      LPDBCEN1       at 0 range 6 .. 6;
      LPDBCCLR       at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      WKUPDBC        at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LPDBC          at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ------------------------
   -- SUPC_WUIR_Register --
   ------------------------

   --  Wake Up Input Enable 0
   type WKUPEN0_Field is
     (
      --  the corresponding wake-up input has no wake up effect.
      Disable,
      --  the corresponding wake-up input forces the wake up of the core power
      --  supply.
      Enable)
     with Size => 1;
   for WKUPEN0_Field use
     (Disable => 0,
      Enable => 1);

   -----------------
   -- WUIR.WKUPEN --
   -----------------

   --  WUIR_WKUPEN array
   type WUIR_WKUPEN_Field_Array is array (0 .. 15) of WKUPEN0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for WUIR_WKUPEN
   type WUIR_WKUPEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPEN as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  WKUPEN as an array
            Arr : WUIR_WKUPEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for WUIR_WKUPEN_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Wake Up Input Type 0
   type WKUPT0_Field is
     (
      --  a low level for a period defined by WKUPDBC on the corresponding
      --  wake-up input forces the wake up of the core power supply.
      Low,
      --  a high level for a period defined by WKUPDBC on the correspond-ing
      --  wake-up input forces the wake up of the core power supply.
      High)
     with Size => 1;
   for WKUPT0_Field use
     (Low => 0,
      High => 1);

   ----------------
   -- WUIR.WKUPT --
   ----------------

   --  WUIR_WKUPT array
   type WUIR_WKUPT_Field_Array is array (0 .. 15) of WKUPT0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for WUIR_WKUPT
   type WUIR_WKUPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPT as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  WKUPT as an array
            Arr : WUIR_WKUPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for WUIR_WKUPT_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Supply Controller Wake Up Inputs Register
   type SUPC_WUIR_Register is record
      --  Wake Up Input Enable 0
      WKUPEN : WUIR_WKUPEN_Field := (As_Array => False, Val => 16#0#);
      --  Wake Up Input Type 0
      WKUPT  : WUIR_WKUPT_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_WUIR_Register use record
      WKUPEN at 0 range 0 .. 15;
      WKUPT  at 0 range 16 .. 31;
   end record;

   ----------------------
   -- SUPC_SR_Register --
   ----------------------

   --  WKUP Wake Up Status
   type WKUPS_Field is
     (
      --  no wake up due to the assertion of the WKUP pins has occurred since
      --  the last read of SUPC_SR.
      No,
      --  at least one wake up due to the assertion of the WKUP pins has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for WKUPS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Detection Wake Up Status
   type SMWS_Field is
     (
      --  no wake up due to a supply monitor detection has occurred since the
      --  last read of SUPC_SR.
      No,
      --  at least one wake up due to a supply monitor detection has occurred
      --  since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SMWS_Field use
     (No => 0,
      Present => 1);

   --  Brownout Detector Reset Status
   type BODRSTS_Field is
     (
      --  no core brownout rising edge event has been detected since the last
      --  read of the SUPC_SR.
      No,
      --  at least one brownout output rising edge event has been detected
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for BODRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Reset Status
   type SMRSTS_Field is
     (
      --  no supply monitor detection has generated a core reset since the last
      --  read of the SUPC_SR.
      No,
      --  at least one supply monitor detection has generated a core reset
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for SMRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Status
   type SMS_Field is
     (
      --  no supply monitor detection since the last read of SUPC_SR.
      No,
      --  at least one supply monitor detection since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SMS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Output Status
   type SMOS_Field is
     (
      --  the supply monitor detected VDDIO higher than its threshold at its
      --  last measurement.
      High,
      --  the supply monitor detected VDDIO lower than its threshold at its
      --  last measurement.
      Low)
     with Size => 1;
   for SMOS_Field use
     (High => 0,
      Low => 1);

   --  32-kHz Oscillator Selection Status
   type OSCSEL_Field is
     (
      --  the slow clock, SLCK is generated by the embedded 32-kHz RC
      --  oscillator.
      Rc,
      --  the slow clock, SLCK is generated by the 32-kHz crystal oscillator.
      Cryst)
     with Size => 1;
   for OSCSEL_Field use
     (Rc => 0,
      Cryst => 1);

   --  Low Power Debouncer Wake Up Status on WKUP0
   type LPDBCS0_Field is
     (
      --  no wake up due to the assertion of the WKUP0 pin has occurred since
      --  the last read of SUPC_SR.
      No,
      --  at least one wake up due to the assertion of the WKUP0 pin has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for LPDBCS0_Field use
     (No => 0,
      Present => 1);

   --  Low Power Debouncer Wake Up Status on WKUP1
   type LPDBCS1_Field is
     (
      --  no wake up due to the assertion of the WKUP1 pin has occurred since
      --  the last read of SUPC_SR.
      No,
      --  at least one wake up due to the assertion of the WKUP1 pin has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for LPDBCS1_Field use
     (No => 0,
      Present => 1);

   --  WKUP Input Status 0
   type WKUPIS0_Field is
     (
      --  the corresponding wake-up input is disabled, or was inactive at the
      --  time the debouncer triggered a wake up event.
      Dis,
      --  the corresponding wake-up input was active at the time the debouncer
      --  triggered a wake up event.
      En)
     with Size => 1;
   for WKUPIS0_Field use
     (Dis => 0,
      En => 1);

   ---------------
   -- SR.WKUPIS --
   ---------------

   --  SR_WKUPIS array
   type SR_WKUPIS_Field_Array is array (0 .. 15) of WKUPIS0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for SR_WKUPIS
   type SR_WKUPIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPIS as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  WKUPIS as an array
            Arr : SR_WKUPIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for SR_WKUPIS_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Supply Controller Status Register
   type SUPC_SR_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.Bit_Types.Bit;
      --  Read-only. WKUP Wake Up Status
      WKUPS          : WKUPS_Field := No;
      --  Read-only. Supply Monitor Detection Wake Up Status
      SMWS           : SMWS_Field := No;
      --  Read-only. Brownout Detector Reset Status
      BODRSTS        : BODRSTS_Field := No;
      --  Read-only. Supply Monitor Reset Status
      SMRSTS         : SMRSTS_Field := No;
      --  Read-only. Supply Monitor Status
      SMS            : SMS_Field := No;
      --  Read-only. Supply Monitor Output Status
      SMOS           : SMOS_Field := High;
      --  Read-only. 32-kHz Oscillator Selection Status
      OSCSEL         : OSCSEL_Field := Rc;
      --  unspecified
      Reserved_8_12  : Interfaces.Bit_Types.UInt5;
      --  Read-only. Low Power Debouncer Wake Up Status on WKUP0
      LPDBCS0        : LPDBCS0_Field := No;
      --  Read-only. Low Power Debouncer Wake Up Status on WKUP1
      LPDBCS1        : LPDBCS1_Field := No;
      --  unspecified
      Reserved_15_15 : Interfaces.Bit_Types.Bit;
      --  Read-only. WKUP Input Status 0
      WKUPIS         : SR_WKUPIS_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      WKUPS          at 0 range 1 .. 1;
      SMWS           at 0 range 2 .. 2;
      BODRSTS        at 0 range 3 .. 3;
      SMRSTS         at 0 range 4 .. 4;
      SMS            at 0 range 5 .. 5;
      SMOS           at 0 range 6 .. 6;
      OSCSEL         at 0 range 7 .. 7;
      Reserved_8_12  at 0 range 8 .. 12;
      LPDBCS0        at 0 range 13 .. 13;
      LPDBCS1        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      WKUPIS         at 0 range 16 .. 31;
   end record;

   ---------------------
   -- RTT_MR_Register --
   ---------------------

   subtype MR_RTPRES_Field is Interfaces.Bit_Types.Short;

   --  Mode Register
   type RTT_MR_Register is record
      --  Real-time Timer Prescaler Value
      RTPRES         : MR_RTPRES_Field := 16#8000#;
      --  Alarm Interrupt Enable
      ALMIEN         : Boolean := False;
      --  Real-time Timer Increment Interrupt Enable
      RTTINCIEN      : Boolean := False;
      --  Real-time Timer Restart
      RTTRST         : Boolean := False;
      --  unspecified
      Reserved_19_19 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Real-time Timer Disable
      RTTDIS         : Boolean := False;
      --  unspecified
      Reserved_21_23 : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  Real-Time Clock 1Hz Clock Selection
      RTC1HZ         : Boolean := False;
      --  unspecified
      Reserved_25_31 : Interfaces.Bit_Types.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTT_MR_Register use record
      RTPRES         at 0 range 0 .. 15;
      ALMIEN         at 0 range 16 .. 16;
      RTTINCIEN      at 0 range 17 .. 17;
      RTTRST         at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      RTTDIS         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      RTC1HZ         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  Status Register
   type SR_Register is record
      --  Read-only. Real-time Alarm Status
      ALMS          : Boolean := False;
      --  Read-only. Real-time Timer Increment
      RTTINC        : Boolean := False;
      --  unspecified
      Reserved_2_31 : Interfaces.Bit_Types.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      ALMS          at 0 range 0 .. 0;
      RTTINC        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ---------------------
   -- WDT_CR_Register --
   ---------------------

   --  Control Register
   type WDT_CR_Register is record
      --  Write-only. Watchdog Restart
      WDRSTT        : Boolean := False;
      --  unspecified
      Reserved_1_23 : Interfaces.Bit_Types.UInt23 := 16#0#;
      --  Write-only. Password
      KEY           : CR_KEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_CR_Register use record
      WDRSTT        at 0 range 0 .. 0;
      Reserved_1_23 at 0 range 1 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   ---------------------
   -- WDT_MR_Register --
   ---------------------

   subtype MR_WDV_Field is Interfaces.Bit_Types.UInt12;
   subtype MR_WDD_Field is Interfaces.Bit_Types.UInt12;

   --  Mode Register
   type WDT_MR_Register is record
      --  Watchdog Counter Value
      WDV            : MR_WDV_Field := 16#FFF#;
      --  Watchdog Fault Interrupt Enable
      WDFIEN         : Boolean := False;
      --  Watchdog Reset Enable
      WDRSTEN        : Boolean := True;
      --  Watchdog Reset Processor
      WDRPROC        : Boolean := False;
      --  Watchdog Disable
      WDDIS          : Boolean := False;
      --  Watchdog Delta Value
      WDD            : MR_WDD_Field := 16#FFF#;
      --  Watchdog Debug Halt
      WDDBGHLT       : Boolean := True;
      --  Watchdog Idle Halt
      WDIDLEHLT      : Boolean := True;
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_MR_Register use record
      WDV            at 0 range 0 .. 11;
      WDFIEN         at 0 range 12 .. 12;
      WDRSTEN        at 0 range 13 .. 13;
      WDRPROC        at 0 range 14 .. 14;
      WDDIS          at 0 range 15 .. 15;
      WDD            at 0 range 16 .. 27;
      WDDBGHLT       at 0 range 28 .. 28;
      WDIDLEHLT      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ---------------------
   -- RTC_CR_Register --
   ---------------------

   --  Time Event Selection
   type TIMEVSEL_Field is
     (
      --  Minute change
      Minute,
      --  Hour change
      Hour,
      --  Every day at midnight
      Midnight,
      --  Every day at noon
      Noon)
     with Size => 2;
   for TIMEVSEL_Field use
     (Minute => 0,
      Hour => 1,
      Midnight => 2,
      Noon => 3);

   --  Calendar Event Selection
   type CALEVSEL_Field is
     (
      --  Week change (every Monday at time 00:00:00)
      Week,
      --  Month change (every 01 of each month at time 00:00:00)
      Month,
      --  Year change (every January 1 at time 00:00:00)
      Year)
     with Size => 2;
   for CALEVSEL_Field use
     (Week => 0,
      Month => 1,
      Year => 2);

   --  Control Register
   type RTC_CR_Register is record
      --  Update Request Time Register
      UPDTIM         : Boolean := False;
      --  Update Request Calendar Register
      UPDCAL         : Boolean := False;
      --  unspecified
      Reserved_2_7   : Interfaces.Bit_Types.UInt6 := 16#0#;
      --  Time Event Selection
      TIMEVSEL       : TIMEVSEL_Field := Minute;
      --  unspecified
      Reserved_10_15 : Interfaces.Bit_Types.UInt6 := 16#0#;
      --  Calendar Event Selection
      CALEVSEL       : CALEVSEL_Field := Week;
      --  unspecified
      Reserved_18_31 : Interfaces.Bit_Types.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CR_Register use record
      UPDTIM         at 0 range 0 .. 0;
      UPDCAL         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      TIMEVSEL       at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      CALEVSEL       at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   ---------------------
   -- RTC_MR_Register --
   ---------------------

   subtype MR_CORRECTION_Field is Interfaces.Bit_Types.UInt7;

   --  RTCOUT0 Output Source Selection
   type OUT0_Field is
     (
      --  no waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  output toggles when alarm flag rises
      Alarm_Toggle,
      --  output is a copy of the alarm flag
      Alarm_Flag,
      --  duty cycle programmable pulse
      Prog_Pulse)
     with Size => 3;
   for OUT0_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Toggle => 5,
      Alarm_Flag => 6,
      Prog_Pulse => 7);

   --  RTCOUT1 Output Source Selection
   type OUT1_Field is
     (
      --  no waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  output toggles when alarm flag rises
      Alarm_Toggle,
      --  output is a copy of the alarm flag
      Alarm_Flag,
      --  duty cycle programmable pulse
      Prog_Pulse)
     with Size => 3;
   for OUT1_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Toggle => 5,
      Alarm_Flag => 6,
      Prog_Pulse => 7);

   --  High Duration of the Output Pulse
   type THIGH_Field is
     (
      --  31.2 ms
      H_31Ms,
      --  15.6 ms
      H_16Ms,
      --  3.91 Lms
      H_4Ms,
      --  976 us
      H_976Us,
      --  488 us
      H_488Us,
      --  122 us
      H_122Us,
      --  30.5 us
      H_30Us,
      --  15.2 us
      H_15Us)
     with Size => 3;
   for THIGH_Field use
     (H_31Ms => 0,
      H_16Ms => 1,
      H_4Ms => 2,
      H_976Us => 3,
      H_488Us => 4,
      H_122Us => 5,
      H_30Us => 6,
      H_15Us => 7);

   --  Period of the Output Pulse
   type TPERIOD_Field is
     (
      --  1 second
      P_1S,
      --  500 ms
      P_500Ms,
      --  250 ms
      P_250Ms,
      --  125 ms
      P_125Ms)
     with Size => 2;
   for TPERIOD_Field use
     (P_1S => 0,
      P_500Ms => 1,
      P_250Ms => 2,
      P_125Ms => 3);

   --  Mode Register
   type RTC_MR_Register is record
      --  12-/24-hour Mode
      HRMOD          : Boolean := False;
      --  PERSIAN Calendar
      PERSIAN        : Boolean := False;
      --  unspecified
      Reserved_2_3   : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  NEGative PPM Correction
      NEGPPM         : Boolean := False;
      --  unspecified
      Reserved_5_7   : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  Slow Clock Correction
      CORRECTION     : MR_CORRECTION_Field := 16#0#;
      --  HIGH PPM Correction
      HIGHPPM        : Boolean := False;
      --  RTCOUT0 Output Source Selection
      OUT0           : OUT0_Field := No_Wave;
      --  unspecified
      Reserved_19_19 : Interfaces.Bit_Types.Bit := 16#0#;
      --  RTCOUT1 Output Source Selection
      OUT1           : OUT1_Field := No_Wave;
      --  unspecified
      Reserved_23_23 : Interfaces.Bit_Types.Bit := 16#0#;
      --  High Duration of the Output Pulse
      THIGH          : THIGH_Field := H_31Ms;
      --  unspecified
      Reserved_27_27 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Period of the Output Pulse
      TPERIOD        : TPERIOD_Field := P_1S;
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_MR_Register use record
      HRMOD          at 0 range 0 .. 0;
      PERSIAN        at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      NEGPPM         at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      CORRECTION     at 0 range 8 .. 14;
      HIGHPPM        at 0 range 15 .. 15;
      OUT0           at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      OUT1           at 0 range 20 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      THIGH          at 0 range 24 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      TPERIOD        at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------------
   -- RTC_TIMR_Register --
   -----------------------

   subtype TIMR_SEC_Field is Interfaces.Bit_Types.UInt7;
   subtype TIMR_MIN_Field is Interfaces.Bit_Types.UInt7;
   subtype TIMR_HOUR_Field is Interfaces.Bit_Types.UInt6;

   --  Time Register
   type RTC_TIMR_Register is record
      --  Current Second
      SEC            : TIMR_SEC_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Current Minute
      MIN            : TIMR_MIN_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Current Hour
      HOUR           : TIMR_HOUR_Field := 16#0#;
      --  Ante Meridiem Post Meridiem Indicator
      AMPM           : Boolean := False;
      --  unspecified
      Reserved_23_31 : Interfaces.Bit_Types.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TIMR_Register use record
      SEC            at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MIN            at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HOUR           at 0 range 16 .. 21;
      AMPM           at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------------
   -- RTC_CALR_Register --
   -----------------------

   subtype CALR_CENT_Field is Interfaces.Bit_Types.UInt7;
   subtype CALR_YEAR_Field is Interfaces.Bit_Types.Byte;
   subtype CALR_MONTH_Field is Interfaces.Bit_Types.UInt5;
   subtype CALR_DAY_Field is Interfaces.Bit_Types.UInt3;
   subtype CALR_DATE_Field is Interfaces.Bit_Types.UInt6;

   --  Calendar Register
   type RTC_CALR_Register is record
      --  Current Century
      CENT           : CALR_CENT_Field := 16#20#;
      --  unspecified
      Reserved_7_7   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Current Year
      YEAR           : CALR_YEAR_Field := 16#10#;
      --  Current Month
      MONTH          : CALR_MONTH_Field := 16#1#;
      --  Current Day in Current Week
      DAY            : CALR_DAY_Field := 16#5#;
      --  Current Day in Current Month
      DATE           : CALR_DATE_Field := 16#1#;
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CALR_Register use record
      CENT           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      YEAR           at 0 range 8 .. 15;
      MONTH          at 0 range 16 .. 20;
      DAY            at 0 range 21 .. 23;
      DATE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -------------------------
   -- RTC_TIMALR_Register --
   -------------------------

   subtype TIMALR_SEC_Field is Interfaces.Bit_Types.UInt7;
   subtype TIMALR_MIN_Field is Interfaces.Bit_Types.UInt7;
   subtype TIMALR_HOUR_Field is Interfaces.Bit_Types.UInt6;

   --  Time Alarm Register
   type RTC_TIMALR_Register is record
      --  Second Alarm
      SEC            : TIMALR_SEC_Field := 16#0#;
      --  Second Alarm Enable
      SECEN          : Boolean := False;
      --  Minute Alarm
      MIN            : TIMALR_MIN_Field := 16#0#;
      --  Minute Alarm Enable
      MINEN          : Boolean := False;
      --  Hour Alarm
      HOUR           : TIMALR_HOUR_Field := 16#0#;
      --  AM/PM Indicator
      AMPM           : Boolean := False;
      --  Hour Alarm Enable
      HOUREN         : Boolean := False;
      --  unspecified
      Reserved_24_31 : Interfaces.Bit_Types.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TIMALR_Register use record
      SEC            at 0 range 0 .. 6;
      SECEN          at 0 range 7 .. 7;
      MIN            at 0 range 8 .. 14;
      MINEN          at 0 range 15 .. 15;
      HOUR           at 0 range 16 .. 21;
      AMPM           at 0 range 22 .. 22;
      HOUREN         at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -------------------------
   -- RTC_CALALR_Register --
   -------------------------

   subtype CALALR_MONTH_Field is Interfaces.Bit_Types.UInt5;
   subtype CALALR_DATE_Field is Interfaces.Bit_Types.UInt6;

   --  Calendar Alarm Register
   type RTC_CALALR_Register is record
      --  unspecified
      Reserved_0_15  : Interfaces.Bit_Types.Short := 16#0#;
      --  Month Alarm
      MONTH          : CALALR_MONTH_Field := 16#1#;
      --  unspecified
      Reserved_21_22 : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  Month Alarm Enable
      MTHEN          : Boolean := False;
      --  Date Alarm
      DATE           : CALALR_DATE_Field := 16#1#;
      --  unspecified
      Reserved_30_30 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Date Alarm Enable
      DATEEN         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CALALR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      MONTH          at 0 range 16 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      MTHEN          at 0 range 23 .. 23;
      DATE           at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      DATEEN         at 0 range 31 .. 31;
   end record;

   ---------------------
   -- RTC_SR_Register --
   ---------------------

   --  Acknowledge for Update
   type ACKUPD_Field is
     (
      --  Time and calendar registers cannot be updated.
      Freerun,
      --  Time and calendar registers can be updated.
      Update)
     with Size => 1;
   for ACKUPD_Field use
     (Freerun => 0,
      Update => 1);

   --  Alarm Flag
   type ALARM_Field is
     (
      --  No alarm matching condition occurred.
      No_Alarmevent,
      --  An alarm matching condition has occurred.
      Alarmevent)
     with Size => 1;
   for ALARM_Field use
     (No_Alarmevent => 0,
      Alarmevent => 1);

   --  Second Event
   type SEC_Field is
     (
      --  No second event has occurred since the last clear.
      No_Secevent,
      --  At least one second event has occurred since the last clear.
      Secevent)
     with Size => 1;
   for SEC_Field use
     (No_Secevent => 0,
      Secevent => 1);

   --  Time Event
   type TIMEV_Field is
     (
      --  No time event has occurred since the last clear.
      No_Timevent,
      --  At least one time event has occurred since the last clear.
      Timevent)
     with Size => 1;
   for TIMEV_Field use
     (No_Timevent => 0,
      Timevent => 1);

   --  Calendar Event
   type CALEV_Field is
     (
      --  No calendar event has occurred since the last clear.
      No_Calevent,
      --  At least one calendar event has occurred since the last clear.
      Calevent)
     with Size => 1;
   for CALEV_Field use
     (No_Calevent => 0,
      Calevent => 1);

   --  Time and/or Date Free Running Error
   type TDERR_Field is
     (
      --  The internal free running counters are carrying valid values since
      --  the last read of RTC_SR.
      Correct,
      --  The internal free running counters have been corrupted (invalid date
      --  or time, non-BCD values) since the last read and/or they are still
      --  invalid.
      Err_Timedate)
     with Size => 1;
   for TDERR_Field use
     (Correct => 0,
      Err_Timedate => 1);

   --  Status Register
   type RTC_SR_Register is record
      --  Read-only. Acknowledge for Update
      ACKUPD        : ACKUPD_Field := Freerun;
      --  Read-only. Alarm Flag
      ALARM         : ALARM_Field := No_Alarmevent;
      --  Read-only. Second Event
      SEC           : SEC_Field := No_Secevent;
      --  Read-only. Time Event
      TIMEV         : TIMEV_Field := No_Timevent;
      --  Read-only. Calendar Event
      CALEV         : CALEV_Field := No_Calevent;
      --  Read-only. Time and/or Date Free Running Error
      TDERR         : TDERR_Field := Correct;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SR_Register use record
      ACKUPD        at 0 range 0 .. 0;
      ALARM         at 0 range 1 .. 1;
      SEC           at 0 range 2 .. 2;
      TIMEV         at 0 range 3 .. 3;
      CALEV         at 0 range 4 .. 4;
      TDERR         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------------
   -- RTC_SCCR_Register --
   -----------------------

   --  Status Clear Command Register
   type RTC_SCCR_Register is record
      --  Write-only. Acknowledge Clear
      ACKCLR        : Boolean := False;
      --  Write-only. Alarm Clear
      ALRCLR        : Boolean := False;
      --  Write-only. Second Clear
      SECCLR        : Boolean := False;
      --  Write-only. Time Clear
      TIMCLR        : Boolean := False;
      --  Write-only. Calendar Clear
      CALCLR        : Boolean := False;
      --  Write-only. Time and/or Date Free Running Error Clear
      TDERRCLR      : Boolean := False;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SCCR_Register use record
      ACKCLR        at 0 range 0 .. 0;
      ALRCLR        at 0 range 1 .. 1;
      SECCLR        at 0 range 2 .. 2;
      TIMCLR        at 0 range 3 .. 3;
      CALCLR        at 0 range 4 .. 4;
      TDERRCLR      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   ----------------------
   -- RTC_IER_Register --
   ----------------------

   --  Interrupt Enable Register
   type RTC_IER_Register is record
      --  Write-only. Acknowledge Update Interrupt Enable
      ACKEN         : Boolean := False;
      --  Write-only. Alarm Interrupt Enable
      ALREN         : Boolean := False;
      --  Write-only. Second Event Interrupt Enable
      SECEN         : Boolean := False;
      --  Write-only. Time Event Interrupt Enable
      TIMEN         : Boolean := False;
      --  Write-only. Calendar Event Interrupt Enable
      CALEN         : Boolean := False;
      --  Write-only. Time and/or Date Error Interrupt Enable
      TDERREN       : Boolean := False;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_IER_Register use record
      ACKEN         at 0 range 0 .. 0;
      ALREN         at 0 range 1 .. 1;
      SECEN         at 0 range 2 .. 2;
      TIMEN         at 0 range 3 .. 3;
      CALEN         at 0 range 4 .. 4;
      TDERREN       at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   ----------------------
   -- RTC_IDR_Register --
   ----------------------

   --  Interrupt Disable Register
   type RTC_IDR_Register is record
      --  Write-only. Acknowledge Update Interrupt Disable
      ACKDIS        : Boolean := False;
      --  Write-only. Alarm Interrupt Disable
      ALRDIS        : Boolean := False;
      --  Write-only. Second Event Interrupt Disable
      SECDIS        : Boolean := False;
      --  Write-only. Time Event Interrupt Disable
      TIMDIS        : Boolean := False;
      --  Write-only. Calendar Event Interrupt Disable
      CALDIS        : Boolean := False;
      --  Write-only. Time and/or Date Error Interrupt Disable
      TDERRDIS      : Boolean := False;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_IDR_Register use record
      ACKDIS        at 0 range 0 .. 0;
      ALRDIS        at 0 range 1 .. 1;
      SECDIS        at 0 range 2 .. 2;
      TIMDIS        at 0 range 3 .. 3;
      CALDIS        at 0 range 4 .. 4;
      TDERRDIS      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   ----------------------
   -- RTC_IMR_Register --
   ----------------------

   --  Interrupt Mask Register
   type RTC_IMR_Register is record
      --  Read-only. Acknowledge Update Interrupt Mask
      ACK           : Boolean := False;
      --  Read-only. Alarm Interrupt Mask
      ALR           : Boolean := False;
      --  Read-only. Second Event Interrupt Mask
      SEC           : Boolean := False;
      --  Read-only. Time Event Interrupt Mask
      TIM           : Boolean := False;
      --  Read-only. Calendar Event Interrupt Mask
      CAL           : Boolean := False;
      --  unspecified
      Reserved_5_31 : Interfaces.Bit_Types.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_IMR_Register use record
      ACK           at 0 range 0 .. 0;
      ALR           at 0 range 1 .. 1;
      SEC           at 0 range 2 .. 2;
      TIM           at 0 range 3 .. 3;
      CAL           at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   ----------------------
   -- RTC_VER_Register --
   ----------------------

   --  Valid Entry Register
   type RTC_VER_Register is record
      --  Read-only. Non-valid Time
      NVTIM         : Boolean := False;
      --  Read-only. Non-valid Calendar
      NVCAL         : Boolean := False;
      --  Read-only. Non-valid Time Alarm
      NVTIMALR      : Boolean := False;
      --  Read-only. Non-valid Calendar Alarm
      NVCALALR      : Boolean := False;
      --  unspecified
      Reserved_4_31 : Interfaces.Bit_Types.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_VER_Register use record
      NVTIM         at 0 range 0 .. 0;
      NVCAL         at 0 range 1 .. 1;
      NVTIMALR      at 0 range 2 .. 2;
      NVCALALR      at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  General Purpose Backup Register

   --  General Purpose Backup Register
   type GPBR_GPBR_Registers is array (0 .. 7) of Interfaces.Bit_Types.Word;

   -----------------
   -- Peripherals --
   -----------------

   --  Reset Controller
   type RSTC_Peripheral is record
      --  Control Register
      CR : RSTC_CR_Register;
      --  Status Register
      SR : RSTC_SR_Register;
      --  Mode Register
      MR : RSTC_MR_Register;
   end record
     with Volatile;

   for RSTC_Peripheral use record
      CR at 0 range 0 .. 31;
      SR at 4 range 0 .. 31;
      MR at 8 range 0 .. 31;
   end record;

   --  Reset Controller
   RSTC_Periph : aliased RSTC_Peripheral
     with Import, Address => RSTC_Base;

   --  Supply Controller
   type SUPC_Peripheral is record
      --  Supply Controller Control Register
      CR   : SUPC_CR_Register;
      --  Supply Controller Supply Monitor Mode Register
      SMMR : SUPC_SMMR_Register;
      --  Supply Controller Mode Register
      MR   : SUPC_MR_Register;
      --  Supply Controller Wake Up Mode Register
      WUMR : SUPC_WUMR_Register;
      --  Supply Controller Wake Up Inputs Register
      WUIR : SUPC_WUIR_Register;
      --  Supply Controller Status Register
      SR   : SUPC_SR_Register;
   end record
     with Volatile;

   for SUPC_Peripheral use record
      CR   at 0 range 0 .. 31;
      SMMR at 4 range 0 .. 31;
      MR   at 8 range 0 .. 31;
      WUMR at 12 range 0 .. 31;
      WUIR at 16 range 0 .. 31;
      SR   at 20 range 0 .. 31;
   end record;

   --  Supply Controller
   SUPC_Periph : aliased SUPC_Peripheral
     with Import, Address => SUPC_Base;

   --  Real-time Timer
   type RTT_Peripheral is record
      --  Mode Register
      MR : RTT_MR_Register;
      --  Alarm Register
      AR : Interfaces.Bit_Types.Word;
      --  Value Register
      VR : Interfaces.Bit_Types.Word;
      --  Status Register
      SR : SR_Register;
   end record
     with Volatile;

   for RTT_Peripheral use record
      MR at 0 range 0 .. 31;
      AR at 4 range 0 .. 31;
      VR at 8 range 0 .. 31;
      SR at 12 range 0 .. 31;
   end record;

   --  Real-time Timer
   RTT_Periph : aliased RTT_Peripheral
     with Import, Address => RTT_Base;

   --  Watchdog Timer
   type WDT_Peripheral is record
      --  Control Register
      CR : WDT_CR_Register;
      --  Mode Register
      MR : WDT_MR_Register;
      --  Status Register
      SR : SR_Register;
   end record
     with Volatile;

   for WDT_Peripheral use record
      CR at 0 range 0 .. 31;
      MR at 4 range 0 .. 31;
      SR at 8 range 0 .. 31;
   end record;

   --  Watchdog Timer
   WDT_Periph : aliased WDT_Peripheral
     with Import, Address => WDT_Base;

   --  Real-time Clock
   type RTC_Peripheral is record
      --  Control Register
      CR     : RTC_CR_Register;
      --  Mode Register
      MR     : RTC_MR_Register;
      --  Time Register
      TIMR   : RTC_TIMR_Register;
      --  Calendar Register
      CALR   : RTC_CALR_Register;
      --  Time Alarm Register
      TIMALR : RTC_TIMALR_Register;
      --  Calendar Alarm Register
      CALALR : RTC_CALALR_Register;
      --  Status Register
      SR     : RTC_SR_Register;
      --  Status Clear Command Register
      SCCR   : RTC_SCCR_Register;
      --  Interrupt Enable Register
      IER    : RTC_IER_Register;
      --  Interrupt Disable Register
      IDR    : RTC_IDR_Register;
      --  Interrupt Mask Register
      IMR    : RTC_IMR_Register;
      --  Valid Entry Register
      VER    : RTC_VER_Register;
   end record
     with Volatile;

   for RTC_Peripheral use record
      CR     at 0 range 0 .. 31;
      MR     at 4 range 0 .. 31;
      TIMR   at 8 range 0 .. 31;
      CALR   at 12 range 0 .. 31;
      TIMALR at 16 range 0 .. 31;
      CALALR at 20 range 0 .. 31;
      SR     at 24 range 0 .. 31;
      SCCR   at 28 range 0 .. 31;
      IER    at 32 range 0 .. 31;
      IDR    at 36 range 0 .. 31;
      IMR    at 40 range 0 .. 31;
      VER    at 44 range 0 .. 31;
   end record;

   --  Real-time Clock
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => RTC_Base;

   --  General Purpose Backup Register
   type GPBR_Peripheral is record
      --  General Purpose Backup Register
      GPBR : GPBR_GPBR_Registers;
   end record
     with Volatile;

   for GPBR_Peripheral use record
      GPBR at 0 range 0 .. 255;
   end record;

   --  General Purpose Backup Register
   GPBR_Periph : aliased GPBR_Peripheral
     with Import, Address => GPBR_Base;

end Interfaces.SAM.SYSC;
