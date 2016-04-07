--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

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

   --  System Reset Key
   type KEY_Field is
     (
      --  Reset value for the field
      Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for KEY_Field use
     (Key_Field_Reset => 5,
      Passwd => 165);

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
      Reserved_4_23 : Interfaces.Bit_Types.UInt20 := 16#FFB3E#;
      --  Write-only. System Reset Key
      KEY           : KEY_Field := Key_Field_Reset;
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

   --  Reset Type
   type RSTTYP_Field is
     (
      --  First power-up reset
      General_Rst,
      --  Return from Backup Mode
      Backup_Rst,
      --  Watchdog fault occurred
      Wdt_Rst,
      --  Processor reset required by the software
      Soft_Rst,
      --  NRST pin detected low
      User_Rst,
      --  Slow Crystal Failure Detection fault occured
      Slck_Xtal_Rst)
     with Size => 3;
   for RSTTYP_Field use
     (General_Rst => 0,
      Backup_Rst => 1,
      Wdt_Rst => 2,
      Soft_Rst => 3,
      User_Rst => 4,
      Slck_Xtal_Rst => 7);

   --  Status Register
   type RSTC_SR_Register is record
      --  Read-only. User Reset Status
      URSTS          : Boolean := False;
      --  unspecified
      Reserved_1_7   : Interfaces.Bit_Types.UInt7;
      --  Read-only. Reset Type
      RSTTYP         : RSTTYP_Field := General_Rst;
      --  unspecified
      Reserved_11_15 : Interfaces.Bit_Types.UInt5;
      --  Read-only. NRST Pin Level
      NRSTL          : Boolean := True;
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

   --  Write Access Password
   type KEY_Field_1 is
     (
      --  Reset value for the field
      Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for KEY_Field_1 use
     (Key_Field_Reset => 0,
      Passwd => 165);

   --  Mode Register
   type RSTC_MR_Register is record
      --  User Reset Enable
      URSTEN         : Boolean := True;
      --  Slow Clock Switching
      SCKSW          : Boolean := False;
      --  unspecified
      Reserved_2_3   : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  User Reset Interrupt Enable
      URSTIEN        : Boolean := False;
      --  unspecified
      Reserved_5_7   : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  External Reset Length
      ERSTL          : MR_ERSTL_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : Interfaces.Bit_Types.UInt12 := 16#0#;
      --  Write Access Password
      KEY            : KEY_Field_1 := Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_MR_Register use record
      URSTEN         at 0 range 0 .. 0;
      SCKSW          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
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
      --  No effect.
      No_Effect,
      --  If KEY is correct, asserts the system reset signal and stops the
      --  voltage regulator.
      Stop_Vreg)
     with Size => 1;
   for VROFF_Field use
     (No_Effect => 0,
      Stop_Vreg => 1);

   --  Crystal Oscillator Select
   type XTALSEL_Field is
     (
      --  No effect.
      No_Effect,
      --  If KEY is correct, switches the slow clock on the crystal oscillator
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
      Reserved_4_23 : Interfaces.Bit_Types.UInt20 := 16#FFB3E#;
      --  Write-only. Password
      KEY           : KEY_Field := Key_Field_Reset;
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
      --  Supply Monitor enables one SLCK period every 32 SLCK periods
      SMSMPL_Field_32Slck,
      --  Supply Monitor enables one SLCK period every 256 SLCK periods
      SMSMPL_Field_256Slck,
      --  Supply Monitor enables one SLCK period every 2,048 SLCK periods
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
      --  The core reset signal vddcore_nreset is not affected when a supply
      --  monitor detection occurs.
      Not_Enable,
      --  The core reset signal vddcore_nreset is asserted when a supply
      --  monitor detection occurs.
      Enable)
     with Size => 1;
   for SMRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Supply Monitor Interrupt Enable
   type SMIEN_Field is
     (
      --  The SUPC interrupt signal is not affected when a supply monitor
      --  detection occurs.
      Not_Enable,
      --  The SUPC interrupt signal is asserted when a supply monitor detection
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

   --  POR Core Reset Enable
   type BODRSTEN_Field is
     (
      --  The core reset signal vddcore_nreset is not affected when a brownout
      --  detection occurs.
      Not_Enable,
      --  The core reset signal vddcore_nreset is asserted when a brownout
      --  detection occurs.
      Enable)
     with Size => 1;
   for BODRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  POR Core Disable
   type BODDIS_Field is
     (
      --  The core brownout detector is enabled.
      Enable,
      --  The core brownout detector is disabled.
      Disable)
     with Size => 1;
   for BODDIS_Field use
     (Enable => 0,
      Disable => 1);

   --  Oscillator Bypass
   type OSCBYPASS_Field is
     (
      --  No effect. Clock selection depends on XTALSEL value.
      No_Effect,
      --  The 32 kHz crystal oscillator is selected and put in bypass mode.
      Bypass)
     with Size => 1;
   for OSCBYPASS_Field use
     (No_Effect => 0,
      Bypass => 1);

   --  Cache Data SRAM Power Switch
   type CDPSWITCH_Field is
     (
      --  The cache data SRAM is not powered.
      Off,
      --  The cache data SRAM is powered.
      On)
     with Size => 1;
   for CDPSWITCH_Field use
     (Off => 0,
      On => 1);

   --  Cache Tag SRAM Power Switch
   type CTPSWITCH_Field is
     (
      --  The cache tag SRAM is not powered.
      Off,
      --  The cache tag SRAM is powered.
      On)
     with Size => 1;
   for CTPSWITCH_Field use
     (Off => 0,
      On => 1);

   --  Supply Controller Mode Register
   type SUPC_MR_Register is record
      --  unspecified
      Reserved_0_11  : Interfaces.Bit_Types.UInt12 := 16#A00#;
      --  POR Core Reset Enable
      BODRSTEN       : BODRSTEN_Field := Enable;
      --  POR Core Disable
      BODDIS         : BODDIS_Field := Enable;
      --  unspecified
      Reserved_14_19 : Interfaces.Bit_Types.UInt6 := 16#1#;
      --  Oscillator Bypass
      OSCBYPASS      : OSCBYPASS_Field := No_Effect;
      --  Cache Data SRAM Power Switch
      CDPSWITCH      : CDPSWITCH_Field := On;
      --  Cache Tag SRAM Power Switch
      CTPSWITCH      : CTPSWITCH_Field := On;
      --  This bit must always be set to 1.
      ONE            : Boolean := True;
      --  Password Key
      KEY            : KEY_Field_1 := Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_MR_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      BODRSTEN       at 0 range 12 .. 12;
      BODDIS         at 0 range 13 .. 13;
      Reserved_14_19 at 0 range 14 .. 19;
      OSCBYPASS      at 0 range 20 .. 20;
      CDPSWITCH      at 0 range 21 .. 21;
      CTPSWITCH      at 0 range 22 .. 22;
      ONE            at 0 range 23 .. 23;
      KEY            at 0 range 24 .. 31;
   end record;

   ------------------------
   -- SUPC_WUMR_Register --
   ------------------------

   --  Supply Monitor Wake-up Enable
   type SMEN_Field is
     (
      --  The supply monitor detection has no wake-up effect.
      Not_Enable,
      --  The supply monitor detection forces the wake-up of the core power
      --  supply.
      Enable)
     with Size => 1;
   for SMEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Real-time Timer Wake-up Enable
   type RTTEN_Field is
     (
      --  Reset value for the field
      Rtten_Field_Reset,
      --  The RTT alarm signal forces the wake-up of the core power supply.
      Enable)
     with Size => 1;
   for RTTEN_Field use
     (Rtten_Field_Reset => 0,
      Enable => 1);

   --  Real-time Clock Wake-up Enable
   type RTCEN_Field is
     (
      --  Reset value for the field
      Rtcen_Field_Reset,
      --  The RTC alarm signal forces the wake-up of the core power supply.
      Enable)
     with Size => 1;
   for RTCEN_Field use
     (Rtcen_Field_Reset => 0,
      Enable => 1);

   --  Wake-up Inputs Debouncer Period
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

   --  Supply Controller Wake-up Mode Register
   type SUPC_WUMR_Register is record
      --  unspecified
      Reserved_0_0   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Supply Monitor Wake-up Enable
      SMEN           : SMEN_Field := Not_Enable;
      --  Real-time Timer Wake-up Enable
      RTTEN          : RTTEN_Field := Rtten_Field_Reset;
      --  Real-time Clock Wake-up Enable
      RTCEN          : RTCEN_Field := Rtcen_Field_Reset;
      --  unspecified
      Reserved_4_11  : Interfaces.Bit_Types.Byte := 16#0#;
      --  Wake-up Inputs Debouncer Period
      WKUPDBC        : WKUPDBC_Field := Immediate;
      --  unspecified
      Reserved_15_31 : Interfaces.Bit_Types.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_WUMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      SMEN           at 0 range 1 .. 1;
      RTTEN          at 0 range 2 .. 2;
      RTCEN          at 0 range 3 .. 3;
      Reserved_4_11  at 0 range 4 .. 11;
      WKUPDBC        at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   ------------------------
   -- SUPC_WUIR_Register --
   ------------------------

   --  Wake-up Input Enable 0
   type WKUPEN0_Field is
     (
      --  The corresponding wake-up input has no wake-up effect.
      Disable,
      --  The corresponding wake-up input forces the wake-up of the core power
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

   --  Wake-up Input Type 0
   type WKUPT0_Field is
     (
      --  A low level for a period defined by WKUPDBC on the corresponding
      --  wake-up input forces the wake-up of the core power supply.
      Low,
      --  A high level for a period defined by WKUPDBC on the corresponding
      --  wake-up input forces the wake-up of the core power supply.
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

   --  Supply Controller Wake-up Inputs Register
   type SUPC_WUIR_Register is record
      --  Wake-up Input Enable 0
      WKUPEN : WUIR_WKUPEN_Field := (As_Array => False, Val => 16#0#);
      --  Wake-up Input Type 0
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

   --  WKUP Wake-up Status
   type WKUPS_Field is
     (
      --  No wake-up due to the assertion of the WKUP pins has occurred since
      --  the last read of SUPC_SR.
      No,
      --  At least one wake-up due to the assertion of the WKUP pins has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for WKUPS_Field use
     (No => 0,
      Present => 1);

   --  Brownout Detector Reset Status
   type BODRSTS_Field is
     (
      --  No core brownout rising edge event has been detected since the last
      --  read of the SUPC_SR.
      No,
      --  At least one brownout output rising edge event has been detected
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for BODRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Reset Status
   type SMRSTS_Field is
     (
      --  No supply monitor detection has generated a core reset since the last
      --  read of the SUPC_SR.
      No,
      --  At least one supply monitor detection has generated a core reset
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for SMRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Status
   type SMS_Field is
     (
      --  No supply monitor detection since the last read of SUPC_SR.
      No,
      --  At least one supply monitor detection since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SMS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Output Status
   type SMOS_Field is
     (
      --  The supply monitor detected VDDIO higher than its threshold at its
      --  last measurement.
      High,
      --  The supply monitor detected VDDIO lower than its threshold at its
      --  last measurement.
      Low)
     with Size => 1;
   for SMOS_Field use
     (High => 0,
      Low => 1);

   --  32-kHz Oscillator Selection Status
   type OSCSEL_Field is
     (
      --  The slow clock SLCK is generated by the embedded 32 kHz RC
      --  oscillator.
      Rc,
      --  The slow clock SLCK is generated by the 32 kHz crystal oscillator.
      Cryst)
     with Size => 1;
   for OSCSEL_Field use
     (Rc => 0,
      Cryst => 1);

   --  WKUP Input Status 0
   type WKUPIS0_Field is
     (
      --  The corresponding wake-up input is disabled, or was inactive at the
      --  time the debouncer triggered a wake-up event.
      Disabled,
      --  The corresponding wake-up input was active at the time the debouncer
      --  triggered a wake-up event.
      Enabled)
     with Size => 1;
   for WKUPIS0_Field use
     (Disabled => 0,
      Enabled => 1);

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
      Reserved_0_0  : Interfaces.Bit_Types.Bit;
      --  Read-only. WKUP Wake-up Status
      WKUPS         : WKUPS_Field := No;
      --  unspecified
      Reserved_2_2  : Interfaces.Bit_Types.Bit;
      --  Read-only. Brownout Detector Reset Status
      BODRSTS       : BODRSTS_Field := No;
      --  Read-only. Supply Monitor Reset Status
      SMRSTS        : SMRSTS_Field := No;
      --  Read-only. Supply Monitor Status
      SMS           : SMS_Field := No;
      --  Read-only. Supply Monitor Output Status
      SMOS          : SMOS_Field := High;
      --  Read-only. 32-kHz Oscillator Selection Status
      OSCSEL        : OSCSEL_Field := Rc;
      --  unspecified
      Reserved_8_15 : Interfaces.Bit_Types.Byte;
      --  Read-only. WKUP Input Status 0
      WKUPIS        : SR_WKUPIS_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      WKUPS         at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      BODRSTS       at 0 range 3 .. 3;
      SMRSTS        at 0 range 4 .. 4;
      SMS           at 0 range 5 .. 5;
      SMOS          at 0 range 6 .. 6;
      OSCSEL        at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      WKUPIS        at 0 range 16 .. 31;
   end record;

   ------------------------
   -- SUPC_PWMR_Register --
   ------------------------

   --  Low Power Value Selection
   type LPOWERS_Field is
     (
      --  The trimming value applied to the regulator when the device is in
      --  wait mode. This value is factory-defined.
      Factory,
      --  The trimming value applied to the regulator is defined by the value
      --  programmed in the LPOWERx bits.
      User)
     with Size => 1;
   for LPOWERS_Field use
     (Factory => 0,
      User => 1);

   -----------------
   -- PWMR.LPOWER --
   -----------------

   --  PWMR_LPOWER array
   type PWMR_LPOWER_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWMR_LPOWER
   type PWMR_LPOWER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LPOWER as a value
            Val : Interfaces.Bit_Types.UInt4;
         when True =>
            --  LPOWER as an array
            Arr : PWMR_LPOWER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWMR_LPOWER_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Start-up Time when Resuming from Wait Mode
   type STUPTIME_Field is
     (
      --  Fast start-up.
      Fast,
      --  Slow start-up.
      Slow)
     with Size => 1;
   for STUPTIME_Field use
     (Fast => 0,
      Slow => 1);

   --  Enhanced Custom Power Value Selection
   type ECPWRS_Field is
     (
      --  The trimming value applied to the regulator when the device is in
      --  active mode. This value is factory-defined.
      Factory,
      --  The trimming value applied to the regulator is defined by the value
      --  programmed in ECPWRx bits.
      User)
     with Size => 1;
   for ECPWRS_Field use
     (Factory => 0,
      User => 1);

   ----------------
   -- PWMR.ECPWR --
   ----------------

   --  PWMR_ECPWR array
   type PWMR_ECPWR_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWMR_ECPWR
   type PWMR_ECPWR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ECPWR as a value
            Val : Interfaces.Bit_Types.UInt4;
         when True =>
            --  ECPWR as an array
            Arr : PWMR_ECPWR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWMR_ECPWR_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SRAM Power Control
   type SRAM0ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM0ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM1ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM1ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM2ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM2ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM3ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM3ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM4ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM4ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM5ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM5ON_Field use
     (Off => 0,
      On => 1);

   --  SRAM Power Control
   type SRAM6ON_Field is
     (
      --  SRAMx is not powered.
      Off,
      --  SRAMx is powered.
      On)
     with Size => 1;
   for SRAM6ON_Field use
     (Off => 0,
      On => 1);

   --  Dual-port RAM Power Control
   type DPRAMON_Field is
     (
      --  USB dual-port RAM is not powered.
      Off,
      --  USB dual-port RAM is powered.
      On)
     with Size => 1;
   for DPRAMON_Field use
     (Off => 0,
      On => 1);

   --  Password Key
   type KEY_Field_2 is
     (
      --  Reset value for the field
      Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for KEY_Field_2 use
     (Key_Field_Reset => 0,
      Passwd => 90);

   --  Supply Controller Power Mode Register
   type SUPC_PWMR_Register is record
      --  Low Power Value Selection
      LPOWERS        : LPOWERS_Field := Factory;
      --  Low Power Value
      LPOWER         : PWMR_LPOWER_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_5_6   : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  Start-up Time when Resuming from Wait Mode
      STUPTIME       : STUPTIME_Field := Fast;
      --  Enhanced Custom Power Value Selection
      ECPWRS         : ECPWRS_Field := Factory;
      --  Enhanced Custom Power Value
      ECPWR          : PWMR_ECPWR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_13_15 : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  SRAM Power Control
      SRAM0ON        : SRAM0ON_Field := Off;
      --  SRAM Power Control
      SRAM1ON        : SRAM1ON_Field := Off;
      --  SRAM Power Control
      SRAM2ON        : SRAM2ON_Field := Off;
      --  SRAM Power Control
      SRAM3ON        : SRAM3ON_Field := Off;
      --  SRAM Power Control
      SRAM4ON        : SRAM4ON_Field := Off;
      --  SRAM Power Control
      SRAM5ON        : SRAM5ON_Field := Off;
      --  SRAM Power Control
      SRAM6ON        : SRAM6ON_Field := Off;
      --  Dual-port RAM Power Control
      DPRAMON        : DPRAMON_Field := Off;
      --  Password Key
      KEY            : KEY_Field_2 := Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_PWMR_Register use record
      LPOWERS        at 0 range 0 .. 0;
      LPOWER         at 0 range 1 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      STUPTIME       at 0 range 7 .. 7;
      ECPWRS         at 0 range 8 .. 8;
      ECPWR          at 0 range 9 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      SRAM0ON        at 0 range 16 .. 16;
      SRAM1ON        at 0 range 17 .. 17;
      SRAM2ON        at 0 range 18 .. 18;
      SRAM3ON        at 0 range 19 .. 19;
      SRAM4ON        at 0 range 20 .. 20;
      SRAM5ON        at 0 range 21 .. 21;
      SRAM6ON        at 0 range 22 .. 22;
      DPRAMON        at 0 range 23 .. 23;
      KEY            at 0 range 24 .. 31;
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
      --  RTTINC2 Alarm Enable
      INC2AEN        : Boolean := False;
      --  Trigger Event Alarm Enable
      EVAEN          : Boolean := False;
      --  unspecified
      Reserved_23_23 : Interfaces.Bit_Types.Bit := 16#0#;
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
      INC2AEN        at 0 range 21 .. 21;
      EVAEN          at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      RTC1HZ         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   ---------------------
   -- RTT_SR_Register --
   ---------------------

   ---------------
   -- SR.RTTINC --
   ---------------

   --  SR_RTTINC array
   type SR_RTTINC_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SR_RTTINC
   type SR_RTTINC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RTTINC as a value
            Val : Interfaces.Bit_Types.UInt2;
         when True =>
            --  RTTINC as an array
            Arr : SR_RTTINC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SR_RTTINC_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Status Register
   type RTT_SR_Register is record
      --  Read-only. Real-time Alarm Status
      ALMS          : Boolean := False;
      --  Read-only. Prescaler Roll-over Status
      RTTINC        : SR_RTTINC_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_3_31 : Interfaces.Bit_Types.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTT_SR_Register use record
      ALMS          at 0 range 0 .. 0;
      RTTINC        at 0 range 1 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   -----------------------
   -- RTT_MODR_Register --
   -----------------------

   --  Selection of the 32-bit Counter Modulo to generate RTTINC2 flag
   type SELINC2_Field is
     (
      --  The RTTINC2 flag never rises
      No_Rttinc2,
      --  The RTTINC2 flag is set when CRTV modulo 64 equals 0
      Mod64,
      --  The RTTINC2 flag is set when CRTV modulo 128 equals 0
      Mod128,
      --  The RTTINC2 flag is set when CRTV modulo 256 equals 0
      Mod256,
      --  The RTTINC2 flag is set when CRTV modulo 512 equals 0
      Mod512,
      --  The RTTINC2 flag is set when CRTV modulo 1024 equals 0.Example: If
      --  RTPRES=32 then RTTINC2 flag rises once per second if the slow clock
      --  is 32.768 kHz.
      Mod1024,
      --  The RTTINC2 flag is set when CRTV modulo 2048 equals 0
      Mod2048,
      --  The RTTINC2 flag is set when CRTV modulo 4096 equals 0
      Mod4096)
     with Size => 3;
   for SELINC2_Field use
     (No_Rttinc2 => 0,
      Mod64 => 1,
      Mod128 => 2,
      Mod256 => 3,
      Mod512 => 4,
      Mod1024 => 5,
      Mod2048 => 6,
      Mod4096 => 7);

   --  Selection of the 32-bit Counter Modulo to generate the trigger event
   type SELTRGEV_Field is
     (
      --  No event generated
      No_Event,
      --  Event occurs when CRTV modulo 2 equals 0
      Mod2,
      --  Event occurs when CRTV modulo 4 equals 0
      Mod4,
      --  Event occurs when CRTV modulo 8 equals 0
      Mod8,
      --  Event occurs when CRTV modulo 16 equals 0
      Mod16,
      --  Event occurs when CRTV modulo 32 equals 0
      Mod32,
      --  Event occurs when CRTV modulo 64 equals 0
      Mod64,
      --  Event occurs when CRTV modulo 128 equals 0
      Mod128)
     with Size => 3;
   for SELTRGEV_Field use
     (No_Event => 0,
      Mod2 => 1,
      Mod4 => 2,
      Mod8 => 3,
      Mod16 => 4,
      Mod32 => 5,
      Mod64 => 6,
      Mod128 => 7);

   --  Modulo Selection Register
   type RTT_MODR_Register is record
      --  Selection of the 32-bit Counter Modulo to generate RTTINC2 flag
      SELINC2        : SELINC2_Field := No_Rttinc2;
      --  unspecified
      Reserved_3_7   : Interfaces.Bit_Types.UInt5 := 16#0#;
      --  Selection of the 32-bit Counter Modulo to generate the trigger event
      SELTRGEV       : SELTRGEV_Field := No_Event;
      --  unspecified
      Reserved_11_31 : Interfaces.Bit_Types.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTT_MODR_Register use record
      SELINC2        at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      SELTRGEV       at 0 range 8 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   ---------------------
   -- WDT_CR_Register --
   ---------------------

   --  Control Register
   type WDT_CR_Register is record
      --  Write-only. Watchdog Restart
      WDRSTT        : Boolean := False;
      --  unspecified
      Reserved_1_23 : Interfaces.Bit_Types.UInt23 := 16#7FD9F0#;
      --  Write-only. Password.
      KEY           : KEY_Field := Key_Field_Reset;
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
   -- WDT_SR_Register --
   ---------------------

   --  Status Register
   type WDT_SR_Register is record
      --  Read-only. Watchdog Underflow
      WDUNF         : Boolean := False;
      --  Read-only. Watchdog Error
      WDERR         : Boolean := False;
      --  unspecified
      Reserved_2_31 : Interfaces.Bit_Types.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_SR_Register use record
      WDUNF         at 0 range 0 .. 0;
      WDERR         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
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

   --  All ADC Channel Trigger Event Source Selection
   type OUT0_Field is
     (
      --  No waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  Output is a copy of the alarm flag
      Alarm_Flag)
     with Size => 3;
   for OUT0_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Flag => 6);

   --  ADC Last Channel Trigger Event Source Selection
   type OUT1_Field is
     (
      --  No waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  Output is a copy of the alarm flag
      Alarm_Flag)
     with Size => 3;
   for OUT1_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Flag => 6);

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
      --  All ADC Channel Trigger Event Source Selection
      OUT0           : OUT0_Field := No_Wave;
      --  unspecified
      Reserved_19_19 : Interfaces.Bit_Types.Bit := 16#0#;
      --  ADC Last Channel Trigger Event Source Selection
      OUT1           : OUT1_Field := No_Wave;
      --  unspecified
      Reserved_23_31 : Interfaces.Bit_Types.UInt9 := 16#0#;
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
      Reserved_23_31 at 0 range 23 .. 31;
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
      --  the last read of the Status Register (RTC_SR).
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
      TDERRCLR      : Boolean := True;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#17FECF#;
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
      TDERREN       : Boolean := True;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#17FECF#;
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
      TDERRDIS      : Boolean := True;
      --  unspecified
      Reserved_6_31 : Interfaces.Bit_Types.UInt26 := 16#17FECF#;
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

   ----------------------
   -- RTC_MSR_Register --
   ----------------------

   subtype MSR_MS_Field is Interfaces.Bit_Types.UInt10;

   --  Milliseconds Register
   type RTC_MSR_Register is record
      --  Read-only. Number of 1/1024 seconds elapsed within 1 second
      MS             : MSR_MS_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : Interfaces.Bit_Types.UInt22;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_MSR_Register use record
      MS             at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------------
   -- RTC_WPMR_Register --
   -----------------------

   --  Write Protection Key
   type WPKEY_Field is
     (
      --  Reset value for the field
      Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit.Always reads as 0.
      Passwd)
     with Size => 24;
   for WPKEY_Field use
     (Wpkey_Field_Reset => 0,
      Passwd => 5395523);

   --  Write Protection Mode Register
   type RTC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : WPKEY_Field := Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
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
      --  Supply Controller Wake-up Mode Register
      WUMR : SUPC_WUMR_Register;
      --  Supply Controller Wake-up Inputs Register
      WUIR : SUPC_WUIR_Register;
      --  Supply Controller Status Register
      SR   : SUPC_SR_Register;
      --  Supply Controller Power Mode Register
      PWMR : SUPC_PWMR_Register;
   end record
     with Volatile;

   for SUPC_Peripheral use record
      CR   at 0 range 0 .. 31;
      SMMR at 4 range 0 .. 31;
      MR   at 8 range 0 .. 31;
      WUMR at 12 range 0 .. 31;
      WUIR at 16 range 0 .. 31;
      SR   at 20 range 0 .. 31;
      PWMR at 28 range 0 .. 31;
   end record;

   --  Supply Controller
   SUPC_Periph : aliased SUPC_Peripheral
     with Import, Address => SUPC_Base;

   --  Real-time Timer
   type RTT_Peripheral is record
      --  Mode Register
      MR   : RTT_MR_Register;
      --  Alarm Register
      AR   : Interfaces.Bit_Types.Word;
      --  Value Register
      VR   : Interfaces.Bit_Types.Word;
      --  Status Register
      SR   : RTT_SR_Register;
      --  Modulo Selection Register
      MODR : RTT_MODR_Register;
   end record
     with Volatile;

   for RTT_Peripheral use record
      MR   at 0 range 0 .. 31;
      AR   at 4 range 0 .. 31;
      VR   at 8 range 0 .. 31;
      SR   at 12 range 0 .. 31;
      MODR at 16 range 0 .. 31;
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
      SR : WDT_SR_Register;
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
      --  Milliseconds Register
      MSR    : RTC_MSR_Register;
      --  Write Protection Mode Register
      WPMR   : RTC_WPMR_Register;
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
      MSR    at 208 range 0 .. 31;
      WPMR   at 228 range 0 .. 31;
   end record;

   --  Real-time Clock
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => RTC_Base;

   --  General Purpose Backup Registers
   type GPBR_Peripheral is record
      --  General Purpose Backup Register
      GPBR : GPBR_GPBR_Registers;
   end record
     with Volatile;

   for GPBR_Peripheral use record
      GPBR at 0 range 0 .. 255;
   end record;

   --  General Purpose Backup Registers
   GPBR_Periph : aliased GPBR_Peripheral
     with Import, Address => GPBR_Base;

end Interfaces.SAM.SYSC;
