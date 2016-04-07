--
--  Copyright (C) 2016, AdaCore
--

--  This spec has been automatically generated from ATSAMG55J19.svd

pragma Ada_2012;

with Interfaces.Bit_Types;
with System;

--  Power Management Controller
package Interfaces.SAM.PMC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   -----------------------
   -- PMC_SCER_Register --
   -----------------------

   ------------------
   -- PMC_SCER.PCK --
   ------------------

   --  PMC_SCER_PCK array
   type PMC_SCER_PCK_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_SCER_PCK
   type PMC_SCER_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCK as an array
            Arr : PMC_SCER_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_SCER_PCK_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  System Clock Enable Register
   type PMC_SCER_Register is record
      --  unspecified
      Reserved_0_5   : Interfaces.Bit_Types.UInt6 := 16#20#;
      --  Write-only. USB Host Port Clock Enable
      UHP            : Boolean := True;
      --  Write-only. USB Device Port Clock Enable
      UDP            : Boolean := True;
      --  Write-only. Programmable Clock 0 Output Enable
      PCK            : PMC_SCER_PCK_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short := 16#5FF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SCER_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      UHP            at 0 range 6 .. 6;
      UDP            at 0 range 7 .. 7;
      PCK            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- PMC_SCDR_Register --
   -----------------------

   ------------------
   -- PMC_SCDR.PCK --
   ------------------

   --  PMC_SCDR_PCK array
   type PMC_SCDR_PCK_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_SCDR_PCK
   type PMC_SCDR_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCK as an array
            Arr : PMC_SCDR_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_SCDR_PCK_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  System Clock Disable Register
   type PMC_SCDR_Register is record
      --  unspecified
      Reserved_0_5   : Interfaces.Bit_Types.UInt6 := 16#20#;
      --  Write-only. USB Host Port Clock Disable
      UHP            : Boolean := True;
      --  Write-only.
      UDP            : Boolean := True;
      --  Write-only. Programmable Clock 0 Output Disable
      PCK            : PMC_SCDR_PCK_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short := 16#5FF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SCDR_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      UHP            at 0 range 6 .. 6;
      UDP            at 0 range 7 .. 7;
      PCK            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- PMC_SCSR_Register --
   -----------------------

   ------------------
   -- PMC_SCSR.PCK --
   ------------------

   --  PMC_SCSR_PCK array
   type PMC_SCSR_PCK_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_SCSR_PCK
   type PMC_SCSR_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCK as an array
            Arr : PMC_SCSR_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_SCSR_PCK_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  System Clock Status Register
   type PMC_SCSR_Register is record
      --  unspecified
      Reserved_0_5   : Interfaces.Bit_Types.UInt6;
      --  Read-only. USB Host Port Clock Status
      UHP            : Boolean := False;
      --  Read-only.
      UDP            : Boolean := False;
      --  Read-only. Programmable Clock 0 Output Status
      PCK            : PMC_SCSR_PCK_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SCSR_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      UHP            at 0 range 6 .. 6;
      UDP            at 0 range 7 .. 7;
      PCK            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------------
   -- PMC_PCER0_Register --
   ------------------------

   -------------------
   -- PMC_PCER0.PID --
   -------------------

   --  PMC_PCER0_PID array
   type PMC_PCER0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_PCER0_PID
   type PMC_PCER0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_PCER0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_PCER0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  Peripheral Clock Enable Register 0
   type PMC_PCER0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte := 16#E0#;
      --  Write-only. Peripheral Clock 8 Enable
      PID            : PMC_PCER0_PID_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PCER0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ------------------------
   -- PMC_PCDR0_Register --
   ------------------------

   -------------------
   -- PMC_PCDR0.PID --
   -------------------

   --  PMC_PCDR0_PID array
   type PMC_PCDR0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_PCDR0_PID
   type PMC_PCDR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_PCDR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_PCDR0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  Peripheral Clock Disable Register 0
   type PMC_PCDR0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte := 16#E0#;
      --  Write-only. Peripheral Clock 8 Disable
      PID            : PMC_PCDR0_PID_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PCDR0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ------------------------
   -- PMC_PCSR0_Register --
   ------------------------

   -------------------
   -- PMC_PCSR0.PID --
   -------------------

   --  PMC_PCSR0_PID array
   type PMC_PCSR0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_PCSR0_PID
   type PMC_PCSR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_PCSR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_PCSR0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  Peripheral Clock Status Register 0
   type PMC_PCSR0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte;
      --  Read-only. Peripheral Clock 8 Status
      PID            : PMC_PCSR0_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PCSR0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------------
   -- CKGR_MOR_Register --
   -----------------------

   --  Main On-Chip RC Oscillator Frequency Selection
   type MOSCRCF_Field is
     (
      --  The Fast RC Oscillator Frequency is at 8 MHz (default)
      MOSCRCF_Field_8_Mhz,
      --  The Fast RC Oscillator Frequency is at 16 MHz
      MOSCRCF_Field_16_Mhz,
      --  The Fast RC Oscillator Frequency is at 24 MHz
      MOSCRCF_Field_24_Mhz)
     with Size => 3;
   for MOSCRCF_Field use
     (MOSCRCF_Field_8_Mhz => 0,
      MOSCRCF_Field_16_Mhz => 1,
      MOSCRCF_Field_24_Mhz => 2);

   subtype CKGR_MOR_MOSCXTST_Field is Interfaces.Bit_Types.Byte;

   --  Write Access Password
   type KEY_Field is
     (
      --  Reset value for the field
      Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for KEY_Field use
     (Key_Field_Reset => 0,
      Passwd => 55);

   --  Main Oscillator Register
   type CKGR_MOR_Register is record
      --  Main Crystal Oscillator Enable
      MOSCXTEN       : Boolean := False;
      --  Main Crystal Oscillator Bypass
      MOSCXTBY       : Boolean := False;
      --  Wait Mode Command (Write-only)
      WAITMODE       : Boolean := False;
      --  Main On-Chip RC Oscillator Enable
      MOSCRCEN       : Boolean := True;
      --  Main On-Chip RC Oscillator Frequency Selection
      MOSCRCF        : MOSCRCF_Field := MOSCRCF_Field_8_Mhz;
      --  unspecified
      Reserved_7_7   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Main Crystal Oscillator Start-up Time
      MOSCXTST       : CKGR_MOR_MOSCXTST_Field := 16#0#;
      --  Write Access Password
      KEY            : KEY_Field := Key_Field_Reset;
      --  Main Oscillator Selection
      MOSCSEL        : Boolean := False;
      --  Clock Failure Detector Enable
      CFDEN          : Boolean := False;
      --  unspecified
      Reserved_26_31 : Interfaces.Bit_Types.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKGR_MOR_Register use record
      MOSCXTEN       at 0 range 0 .. 0;
      MOSCXTBY       at 0 range 1 .. 1;
      WAITMODE       at 0 range 2 .. 2;
      MOSCRCEN       at 0 range 3 .. 3;
      MOSCRCF        at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MOSCXTST       at 0 range 8 .. 15;
      KEY            at 0 range 16 .. 23;
      MOSCSEL        at 0 range 24 .. 24;
      CFDEN          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   ------------------------
   -- CKGR_MCFR_Register --
   ------------------------

   subtype CKGR_MCFR_MAINF_Field is Interfaces.Bit_Types.Short;

   --  Main Clock Frequency Register
   type CKGR_MCFR_Register is record
      --  Main Clock Frequency
      MAINF          : CKGR_MCFR_MAINF_Field := 16#0#;
      --  Main Clock Frequency Measure Ready
      MAINFRDY       : Boolean := False;
      --  unspecified
      Reserved_17_19 : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  RC Oscillator Frequency Measure (write-only)
      RCMEAS         : Boolean := False;
      --  unspecified
      Reserved_21_31 : Interfaces.Bit_Types.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKGR_MCFR_Register use record
      MAINF          at 0 range 0 .. 15;
      MAINFRDY       at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      RCMEAS         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   -------------------------
   -- CKGR_PLLAR_Register --
   -------------------------

   subtype CKGR_PLLAR_PLLAEN_Field is Interfaces.Bit_Types.Byte;
   subtype CKGR_PLLAR_PLLACOUNT_Field is Interfaces.Bit_Types.UInt6;
   subtype CKGR_PLLAR_MULA_Field is Interfaces.Bit_Types.UInt12;

   --  PLLA Register
   type CKGR_PLLAR_Register is record
      --  PLLA Control
      PLLAEN         : CKGR_PLLAR_PLLAEN_Field := 16#0#;
      --  PLLA Counter
      PLLACOUNT      : CKGR_PLLAR_PLLACOUNT_Field := 16#3F#;
      --  unspecified
      Reserved_14_15 : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  PLLA Multiplier
      MULA           : CKGR_PLLAR_MULA_Field := 16#0#;
      --  unspecified
      Reserved_28_28 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Must be written to 0
      ZERO           : Boolean := False;
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKGR_PLLAR_Register use record
      PLLAEN         at 0 range 0 .. 7;
      PLLACOUNT      at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      MULA           at 0 range 16 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      ZERO           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -------------------------
   -- CKGR_PLLBR_Register --
   -------------------------

   subtype CKGR_PLLBR_PLLBEN_Field is Interfaces.Bit_Types.Byte;
   subtype CKGR_PLLBR_PLLBCOUNT_Field is Interfaces.Bit_Types.UInt6;
   subtype CKGR_PLLBR_MULB_Field is Interfaces.Bit_Types.UInt11;

   --  PLLB Register
   type CKGR_PLLBR_Register is record
      --  PLLB Control
      PLLBEN         : CKGR_PLLBR_PLLBEN_Field := 16#0#;
      --  PLLB Counter
      PLLBCOUNT      : CKGR_PLLBR_PLLBCOUNT_Field := 16#3F#;
      --  unspecified
      Reserved_14_15 : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  PLLB Multiplier
      MULB           : CKGR_PLLBR_MULB_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  Must be written to 0
      ZERO           : Boolean := False;
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CKGR_PLLBR_Register use record
      PLLBEN         at 0 range 0 .. 7;
      PLLBCOUNT      at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      MULB           at 0 range 16 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      ZERO           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------------
   -- PMC_MCKR_Register --
   -----------------------

   --  Master Clock Source Selection
   type CSS_Field is
     (
      --  Slow Clock is selected
      Slow_Clk,
      --  Main Clock is selected
      Main_Clk,
      --  PLLA Clock is selected
      Plla_Clk,
      --  PLLBClock is selected
      Pllb_Clk)
     with Size => 2;
   for CSS_Field use
     (Slow_Clk => 0,
      Main_Clk => 1,
      Plla_Clk => 2,
      Pllb_Clk => 3);

   --  Processor Clock Prescaler
   type PRES_Field is
     (
      --  Selected clock
      Clk_1,
      --  Selected clock divided by 2
      Clk_2,
      --  Selected clock divided by 4
      Clk_4,
      --  Selected clock divided by 8
      Clk_8,
      --  Selected clock divided by 16
      Clk_16,
      --  Selected clock divided by 32
      Clk_32,
      --  Selected clock divided by 64
      Clk_64,
      --  Selected clock divided by 3
      Clk_3)
     with Size => 3;
   for PRES_Field use
     (Clk_1 => 0,
      Clk_2 => 1,
      Clk_4 => 2,
      Clk_8 => 3,
      Clk_16 => 4,
      Clk_32 => 5,
      Clk_64 => 6,
      Clk_3 => 7);

   --  Master Clock Register
   type PMC_MCKR_Register is record
      --  Master Clock Source Selection
      CSS            : CSS_Field := Main_Clk;
      --  unspecified
      Reserved_2_3   : Interfaces.Bit_Types.UInt2 := 16#0#;
      --  Processor Clock Prescaler
      PRES           : PRES_Field := Clk_1;
      --  unspecified
      Reserved_7_11  : Interfaces.Bit_Types.UInt5 := 16#0#;
      --  PLLA Divisor by 2
      PLLADIV2       : Boolean := False;
      PLLBDIV2       : Boolean := False;
      --  unspecified
      Reserved_14_31 : Interfaces.Bit_Types.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_MCKR_Register use record
      CSS            at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      PRES           at 0 range 4 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      PLLADIV2       at 0 range 12 .. 12;
      PLLBDIV2       at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ----------------------
   -- PMC_USB_Register --
   ----------------------

   subtype PMC_USB_USBDIV_Field is Interfaces.Bit_Types.UInt4;

   --  USB Clock Register
   type PMC_USB_Register is record
      --  USB Input Clock Selection
      USBS           : Boolean := False;
      --  unspecified
      Reserved_1_7   : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Divider for USB Clock
      USBDIV         : PMC_USB_USBDIV_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : Interfaces.Bit_Types.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_USB_Register use record
      USBS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      USBDIV         at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   ----------------------
   -- PMC_PCK_Register --
   ----------------------

   --  Master Clock Source Selection
   type CSS_Field_1 is
     (
      --  Slow Clock is selected
      Slow_Clk,
      --  Main Clock is selected
      Main_Clk,
      --  PLLA Clock is selected
      Plla_Clk,
      --  PLLB Clock is selected
      Pllb_Clk,
      --  Master Clock is selected
      Mck)
     with Size => 3;
   for CSS_Field_1 use
     (Slow_Clk => 0,
      Main_Clk => 1,
      Plla_Clk => 2,
      Pllb_Clk => 3,
      Mck => 4);

   subtype PMC_PCK_PRES_Field is Interfaces.Bit_Types.Byte;

   --  Programmable Clock 0 Register
   type PMC_PCK_Register is record
      --  Master Clock Source Selection
      CSS            : CSS_Field_1 := Slow_Clk;
      --  unspecified
      Reserved_3_3   : Interfaces.Bit_Types.Bit := 16#0#;
      --  Programmable Clock Prescaler
      PRES           : PMC_PCK_PRES_Field := 16#3E#;
      --  unspecified
      Reserved_12_31 : Interfaces.Bit_Types.UInt20 := 16#5FFB#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PCK_Register use record
      CSS            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      PRES           at 0 range 4 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Programmable Clock 0 Register
   type PMC_PCK_Registers is array (0 .. 7) of PMC_PCK_Register;

   ----------------------
   -- PMC_IER_Register --
   ----------------------

   --------------------
   -- PMC_IER.PCKRDY --
   --------------------

   --  PMC_IER_PCKRDY array
   type PMC_IER_PCKRDY_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_IER_PCKRDY
   type PMC_IER_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_IER_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_IER_PCKRDY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Interrupt Enable Register
   type PMC_IER_Register is record
      --  Write-only. Main Crystal Oscillator Status Interrupt Enable
      MOSCXTS        : Boolean := False;
      --  Write-only. PLLA Lock Interrupt Enable
      LOCKA          : Boolean := False;
      --  Write-only. PLLB Lock Interrupt Enable
      LOCKB          : Boolean := False;
      --  Write-only. Master Clock Ready Interrupt Enable
      MCKRDY         : Boolean := False;
      --  unspecified
      Reserved_4_7   : Interfaces.Bit_Types.UInt4 := 16#E#;
      --  Write-only. Programmable Clock Ready 0 Interrupt Enable
      PCKRDY         : PMC_IER_PCKRDY_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Write-only. Main Oscillator Selection Status Interrupt Enable
      MOSCSELS       : Boolean := True;
      --  Write-only. Main On-Chip RC Status Interrupt Enable
      MOSCRCS        : Boolean := True;
      --  Write-only. Clock Failure Detector Event Interrupt Enable
      CFDEV          : Boolean := True;
      --  unspecified
      Reserved_19_31 : Interfaces.Bit_Types.UInt13 := 16#BF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_IER_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      LOCKB          at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      PCKRDY         at 0 range 8 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ----------------------
   -- PMC_IDR_Register --
   ----------------------

   --------------------
   -- PMC_IDR.PCKRDY --
   --------------------

   --  PMC_IDR_PCKRDY array
   type PMC_IDR_PCKRDY_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_IDR_PCKRDY
   type PMC_IDR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_IDR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_IDR_PCKRDY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Interrupt Disable Register
   type PMC_IDR_Register is record
      --  Write-only. Main Crystal Oscillator Status Interrupt Disable
      MOSCXTS        : Boolean := False;
      --  Write-only. PLLA Lock Interrupt Disable
      LOCKA          : Boolean := False;
      --  Write-only. PLLB Lock Interrupt Disable
      LOCKB          : Boolean := False;
      --  Write-only. Master Clock Ready Interrupt Disable
      MCKRDY         : Boolean := False;
      --  unspecified
      Reserved_4_7   : Interfaces.Bit_Types.UInt4 := 16#E#;
      --  Write-only. Programmable Clock Ready 0 Interrupt Disable
      PCKRDY         : PMC_IDR_PCKRDY_Field :=
                        (As_Array => False, Val => 16#1#);
      --  Write-only. Main Oscillator Selection Status Interrupt Disable
      MOSCSELS       : Boolean := True;
      --  Write-only. Main On-Chip RC Status Interrupt Disable
      MOSCRCS        : Boolean := True;
      --  Write-only. Clock Failure Detector Event Interrupt Disable
      CFDEV          : Boolean := True;
      --  unspecified
      Reserved_19_31 : Interfaces.Bit_Types.UInt13 := 16#BF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_IDR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      LOCKB          at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      PCKRDY         at 0 range 8 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ---------------------
   -- PMC_SR_Register --
   ---------------------

   -------------------
   -- PMC_SR.PCKRDY --
   -------------------

   --  PMC_SR_PCKRDY array
   type PMC_SR_PCKRDY_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PMC_SR_PCKRDY
   type PMC_SR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : Interfaces.Bit_Types.Byte;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_SR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PMC_SR_PCKRDY_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Status Register
   type PMC_SR_Register is record
      --  Read-only. Main Crystal Oscillator Status
      MOSCXTS        : Boolean := False;
      --  Read-only. PLLA Lock Status
      LOCKA          : Boolean := False;
      --  Read-only. PLLB Lock Status
      LOCKB          : Boolean := False;
      --  Read-only. Master Clock Status
      MCKRDY         : Boolean := True;
      --  unspecified
      Reserved_4_6   : Interfaces.Bit_Types.UInt3;
      --  Read-only. Slow Clock Oscillator Selection
      OSCSELS        : Boolean := False;
      --  Read-only. Programmable Clock Ready Status
      PCKRDY         : PMC_SR_PCKRDY_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Read-only. Main Oscillator Selection Status
      MOSCSELS       : Boolean := True;
      --  Read-only. Main On-Chip RC Oscillator Status
      MOSCRCS        : Boolean := True;
      --  Read-only. Clock Failure Detector Event
      CFDEV          : Boolean := False;
      --  Read-only. Clock Failure Detector Status
      CFDS           : Boolean := False;
      --  Read-only. Clock Failure Detector Fault Output Status
      FOS            : Boolean := False;
      --  unspecified
      Reserved_21_31 : Interfaces.Bit_Types.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      LOCKB          at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      OSCSELS        at 0 range 7 .. 7;
      PCKRDY         at 0 range 8 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      CFDS           at 0 range 19 .. 19;
      FOS            at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   ----------------------
   -- PMC_IMR_Register --
   ----------------------

   --------------------
   -- PMC_IMR.PCKRDY --
   --------------------

   --  PMC_IMR_PCKRDY array
   type PMC_IMR_PCKRDY_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for PMC_IMR_PCKRDY
   type PMC_IMR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : Interfaces.Bit_Types.UInt3;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_IMR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for PMC_IMR_PCKRDY_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Interrupt Mask Register
   type PMC_IMR_Register is record
      --  Read-only. Main Crystal Oscillator Status Interrupt Mask
      MOSCXTS        : Boolean := False;
      --  Read-only. PLLA Lock Interrupt Mask
      LOCKA          : Boolean := False;
      --  Read-only. PLLB Lock Interrupt Mask
      LOCKB          : Boolean := False;
      --  Read-only. Master Clock Ready Interrupt Mask
      MCKRDY         : Boolean := False;
      --  unspecified
      Reserved_4_7   : Interfaces.Bit_Types.UInt4;
      --  Read-only. Programmable Clock Ready 0 Interrupt Mask
      PCKRDY         : PMC_IMR_PCKRDY_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_11_15 : Interfaces.Bit_Types.UInt5;
      --  Read-only. Main Oscillator Selection Status Interrupt Mask
      MOSCSELS       : Boolean := False;
      --  Read-only. Main On-Chip RC Status Interrupt Mask
      MOSCRCS        : Boolean := False;
      --  Read-only. Clock Failure Detector Event Interrupt Mask
      CFDEV          : Boolean := False;
      --  unspecified
      Reserved_19_31 : Interfaces.Bit_Types.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_IMR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      LOCKB          at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      PCKRDY         at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------------
   -- PMC_FSMR_Register --
   -----------------------

   -------------------
   -- PMC_FSMR.FSTT --
   -------------------

   --  PMC_FSMR_FSTT array
   type PMC_FSMR_FSTT_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PMC_FSMR_FSTT
   type PMC_FSMR_FSTT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSTT as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  FSTT as an array
            Arr : PMC_FSMR_FSTT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PMC_FSMR_FSTT_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Flash Low-power Mode
   type FLPM_Field is
     (
      --  Flash is in Standby Mode when system enters Wait Mode
      Flash_Standby,
      --  Flash is in Deep-power-down mode when system enters Wait Mode
      Flash_Deep_Powerdown,
      --  Idle mode
      Flash_Idle)
     with Size => 2;
   for FLPM_Field use
     (Flash_Standby => 0,
      Flash_Deep_Powerdown => 1,
      Flash_Idle => 2);

   --  Fast Startup Mode Register
   type PMC_FSMR_Register is record
      --  Fast Startup Input Enable 0
      FSTT           : PMC_FSMR_FSTT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  RTT Alarm Enable
      RTTAL          : Boolean := False;
      --  RTC Alarm Enable
      RTCAL          : Boolean := False;
      --  USB Alarm Enable
      USBAL          : Boolean := False;
      --  unspecified
      Reserved_19_19 : Interfaces.Bit_Types.Bit := 16#0#;
      --  Low-power Mode
      LPM            : Boolean := False;
      --  Flash Low-power Mode
      FLPM           : FLPM_Field := Flash_Standby;
      --  unspecified
      Reserved_23_31 : Interfaces.Bit_Types.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_FSMR_Register use record
      FSTT           at 0 range 0 .. 15;
      RTTAL          at 0 range 16 .. 16;
      RTCAL          at 0 range 17 .. 17;
      USBAL          at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      LPM            at 0 range 20 .. 20;
      FLPM           at 0 range 21 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------------
   -- PMC_FSPR_Register --
   -----------------------

   -------------------
   -- PMC_FSPR.FSTP --
   -------------------

   --  PMC_FSPR_FSTP array
   type PMC_FSPR_FSTP_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PMC_FSPR_FSTP
   type PMC_FSPR_FSTP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSTP as a value
            Val : Interfaces.Bit_Types.Short;
         when True =>
            --  FSTP as an array
            Arr : PMC_FSPR_FSTP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PMC_FSPR_FSTP_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Fast Startup Polarity Register
   type PMC_FSPR_Register is record
      --  Fast Startup Input Polarityx
      FSTP           : PMC_FSPR_FSTP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : Interfaces.Bit_Types.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_FSPR_Register use record
      FSTP           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- PMC_FOCR_Register --
   -----------------------

   --  Fault Output Clear Register
   type PMC_FOCR_Register is record
      --  Write-only. Fault Output Clear
      FOCLR         : Boolean := False;
      --  unspecified
      Reserved_1_31 : Interfaces.Bit_Types.UInt31 := 16#2FFD9F0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_FOCR_Register use record
      FOCLR         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------------
   -- PMC_WPMR_Register --
   -----------------------

   --  Write Protection Key
   type WPKEY_Field is
     (
      --  Reset value for the field
      Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit. Always reads as 0.
      Passwd)
     with Size => 24;
   for WPKEY_Field use
     (Wpkey_Field_Reset => 0,
      Passwd => 5262659);

   --  Write Protection Mode Register
   type PMC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : Interfaces.Bit_Types.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : WPKEY_Field := Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   -----------------------
   -- PMC_WPSR_Register --
   -----------------------

   subtype PMC_WPSR_WPVSRC_Field is Interfaces.Bit_Types.Short;

   --  Write Protection Status Register
   type PMC_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean := False;
      --  unspecified
      Reserved_1_7   : Interfaces.Bit_Types.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : PMC_WPSR_WPVSRC_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : Interfaces.Bit_Types.Byte;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ----------------------
   -- PMC_PCR_Register --
   ----------------------

   subtype PMC_PCR_PID_Field is Interfaces.Bit_Types.UInt6;

   --  Divisor Value
   type DIV_Field is
     (
      --  Peripheral clock is MCK
      Periph_Div_Mck,
      --  Peripheral clock is MCK/2
      Periph_Div2_Mck,
      --  Peripheral clock is MCK/4
      Periph_Div4_Mck,
      --  Peripheral clock is MCK/8
      Periph_Div8_Mck)
     with Size => 2;
   for DIV_Field use
     (Periph_Div_Mck => 0,
      Periph_Div2_Mck => 1,
      Periph_Div4_Mck => 2,
      Periph_Div8_Mck => 3);

   --  Peripheral Control Register
   type PMC_PCR_Register is record
      --  Peripheral ID
      PID            : PMC_PCR_PID_Field := 16#0#;
      --  unspecified
      Reserved_6_11  : Interfaces.Bit_Types.UInt6 := 16#0#;
      --  Command
      CMD            : Boolean := False;
      --  unspecified
      Reserved_13_15 : Interfaces.Bit_Types.UInt3 := 16#0#;
      --  Divisor Value
      DIV            : DIV_Field := Periph_Div_Mck;
      --  unspecified
      Reserved_18_27 : Interfaces.Bit_Types.UInt10 := 16#0#;
      --  Enable
      EN             : Boolean := False;
      --  unspecified
      Reserved_29_31 : Interfaces.Bit_Types.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PCR_Register use record
      PID            at 0 range 0 .. 5;
      Reserved_6_11  at 0 range 6 .. 11;
      CMD            at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DIV            at 0 range 16 .. 17;
      Reserved_18_27 at 0 range 18 .. 27;
      EN             at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   ----------------------
   -- PMC_OCR_Register --
   ----------------------

   subtype PMC_OCR_CAL8_Field is Interfaces.Bit_Types.UInt7;
   subtype PMC_OCR_CAL16_Field is Interfaces.Bit_Types.UInt7;
   subtype PMC_OCR_CAL24_Field is Interfaces.Bit_Types.UInt7;

   --  Oscillator Calibration Register
   type PMC_OCR_Register is record
      --  RC Oscillator Calibration bits for 8 MHz
      CAL8           : PMC_OCR_CAL8_Field := 16#40#;
      --  Selection of RC Oscillator Calibration bits for 8 MHz
      SEL8           : Boolean := False;
      --  RC Oscillator Calibration bits for 16 MHz
      CAL16          : PMC_OCR_CAL16_Field := 16#40#;
      --  Selection of RC Oscillator Calibration bits for 16 MHz
      SEL16          : Boolean := False;
      --  RC Oscillator Calibration bits for 24 MHz
      CAL24          : PMC_OCR_CAL24_Field := 16#40#;
      --  Selection of RC Oscillator Calibration bits for 24 MHz
      SEL24          : Boolean := False;
      --  unspecified
      Reserved_24_31 : Interfaces.Bit_Types.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_OCR_Register use record
      CAL8           at 0 range 0 .. 6;
      SEL8           at 0 range 7 .. 7;
      CAL16          at 0 range 8 .. 14;
      SEL16          at 0 range 15 .. 15;
      CAL24          at 0 range 16 .. 22;
      SEL24          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ----------------------------
   -- PMC_SLPWK_ER0_Register --
   ----------------------------

   -----------------------
   -- PMC_SLPWK_ER0.PID --
   -----------------------

   --  PMC_SLPWK_ER0_PID array
   type PMC_SLPWK_ER0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_SLPWK_ER0_PID
   type PMC_SLPWK_ER0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_SLPWK_ER0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_SLPWK_ER0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  SleepWalking Enable Register 0
   type PMC_SLPWK_ER0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte := 16#E0#;
      --  Write-only. Peripheral 8 SleepWalking Enable
      PID            : PMC_SLPWK_ER0_PID_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SLPWK_ER0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ----------------------------
   -- PMC_SLPWK_DR0_Register --
   ----------------------------

   -----------------------
   -- PMC_SLPWK_DR0.PID --
   -----------------------

   --  PMC_SLPWK_DR0_PID array
   type PMC_SLPWK_DR0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_SLPWK_DR0_PID
   type PMC_SLPWK_DR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_SLPWK_DR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_SLPWK_DR0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  SleepWalking Disable Register 0
   type PMC_SLPWK_DR0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte := 16#E0#;
      --  Write-only. Peripheral 8 SleepWalking Disable
      PID            : PMC_SLPWK_DR0_PID_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SLPWK_DR0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ----------------------------
   -- PMC_SLPWK_SR0_Register --
   ----------------------------

   -----------------------
   -- PMC_SLPWK_SR0.PID --
   -----------------------

   --  PMC_SLPWK_SR0_PID array
   type PMC_SLPWK_SR0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_SLPWK_SR0_PID
   type PMC_SLPWK_SR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_SLPWK_SR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_SLPWK_SR0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  SleepWalking Status Register 0
   type PMC_SLPWK_SR0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte;
      --  Read-only. Peripheral 8 SleepWalking Status
      PID            : PMC_SLPWK_SR0_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SLPWK_SR0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------------------
   -- PMC_SLPWK_ASR0_Register --
   -----------------------------

   ------------------------
   -- PMC_SLPWK_ASR0.PID --
   ------------------------

   --  PMC_SLPWK_ASR0_PID array
   type PMC_SLPWK_ASR0_PID_Field_Array is array (8 .. 29) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for PMC_SLPWK_ASR0_PID
   type PMC_SLPWK_ASR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : Interfaces.Bit_Types.UInt22;
         when True =>
            --  PID as an array
            Arr : PMC_SLPWK_ASR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for PMC_SLPWK_ASR0_PID_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  SleepWalking Activity Status Register 0
   type PMC_SLPWK_ASR0_Register is record
      --  unspecified
      Reserved_0_7   : Interfaces.Bit_Types.Byte;
      --  Read-only. Peripheral 8 Activity Status
      PID            : PMC_SLPWK_ASR0_PID_Field :=
                        (As_Array => False, Val => 16#1#);
      --  unspecified
      Reserved_30_31 : Interfaces.Bit_Types.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_SLPWK_ASR0_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      PID            at 0 range 8 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -----------------------
   -- PMC_PMMR_Register --
   -----------------------

   subtype PMC_PMMR_PLLA_MMAX_Field is Interfaces.Bit_Types.UInt11;

   --  PLL Maximum Multiplier Value Register
   type PMC_PMMR_Register is record
      --  PLLA Maximum Allowed Multiplier Value
      PLLA_MMAX      : PMC_PMMR_PLLA_MMAX_Field := 16#7FF#;
      --  unspecified
      Reserved_11_31 : Interfaces.Bit_Types.UInt21 := 16#FFE0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMMR_Register use record
      PLLA_MMAX      at 0 range 0 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power Management Controller
   type PMC_Peripheral is record
      --  System Clock Enable Register
      PMC_SCER       : PMC_SCER_Register;
      --  System Clock Disable Register
      PMC_SCDR       : PMC_SCDR_Register;
      --  System Clock Status Register
      PMC_SCSR       : PMC_SCSR_Register;
      --  Peripheral Clock Enable Register 0
      PMC_PCER0      : PMC_PCER0_Register;
      --  Peripheral Clock Disable Register 0
      PMC_PCDR0      : PMC_PCDR0_Register;
      --  Peripheral Clock Status Register 0
      PMC_PCSR0      : PMC_PCSR0_Register;
      --  Main Oscillator Register
      CKGR_MOR       : CKGR_MOR_Register;
      --  Main Clock Frequency Register
      CKGR_MCFR      : CKGR_MCFR_Register;
      --  PLLA Register
      CKGR_PLLAR     : CKGR_PLLAR_Register;
      --  PLLB Register
      CKGR_PLLBR     : CKGR_PLLBR_Register;
      --  Master Clock Register
      PMC_MCKR       : PMC_MCKR_Register;
      --  USB Clock Register
      PMC_USB        : PMC_USB_Register;
      --  Programmable Clock 0 Register
      PMC_PCK        : PMC_PCK_Registers;
      --  Interrupt Enable Register
      PMC_IER        : PMC_IER_Register;
      --  Interrupt Disable Register
      PMC_IDR        : PMC_IDR_Register;
      --  Status Register
      PMC_SR         : PMC_SR_Register;
      --  Interrupt Mask Register
      PMC_IMR        : PMC_IMR_Register;
      --  Fast Startup Mode Register
      PMC_FSMR       : PMC_FSMR_Register;
      --  Fast Startup Polarity Register
      PMC_FSPR       : PMC_FSPR_Register;
      --  Fault Output Clear Register
      PMC_FOCR       : PMC_FOCR_Register;
      --  Write Protection Mode Register
      PMC_WPMR       : PMC_WPMR_Register;
      --  Write Protection Status Register
      PMC_WPSR       : PMC_WPSR_Register;
      --  Peripheral Control Register
      PMC_PCR        : PMC_PCR_Register;
      --  Oscillator Calibration Register
      PMC_OCR        : PMC_OCR_Register;
      --  SleepWalking Enable Register 0
      PMC_SLPWK_ER0  : PMC_SLPWK_ER0_Register;
      --  SleepWalking Disable Register 0
      PMC_SLPWK_DR0  : PMC_SLPWK_DR0_Register;
      --  SleepWalking Status Register 0
      PMC_SLPWK_SR0  : PMC_SLPWK_SR0_Register;
      --  SleepWalking Activity Status Register 0
      PMC_SLPWK_ASR0 : PMC_SLPWK_ASR0_Register;
      --  PLL Maximum Multiplier Value Register
      PMC_PMMR       : PMC_PMMR_Register;
   end record
     with Volatile;

   for PMC_Peripheral use record
      PMC_SCER       at 0 range 0 .. 31;
      PMC_SCDR       at 4 range 0 .. 31;
      PMC_SCSR       at 8 range 0 .. 31;
      PMC_PCER0      at 16 range 0 .. 31;
      PMC_PCDR0      at 20 range 0 .. 31;
      PMC_PCSR0      at 24 range 0 .. 31;
      CKGR_MOR       at 32 range 0 .. 31;
      CKGR_MCFR      at 36 range 0 .. 31;
      CKGR_PLLAR     at 40 range 0 .. 31;
      CKGR_PLLBR     at 44 range 0 .. 31;
      PMC_MCKR       at 48 range 0 .. 31;
      PMC_USB        at 56 range 0 .. 31;
      PMC_PCK        at 64 range 0 .. 255;
      PMC_IER        at 96 range 0 .. 31;
      PMC_IDR        at 100 range 0 .. 31;
      PMC_SR         at 104 range 0 .. 31;
      PMC_IMR        at 108 range 0 .. 31;
      PMC_FSMR       at 112 range 0 .. 31;
      PMC_FSPR       at 116 range 0 .. 31;
      PMC_FOCR       at 120 range 0 .. 31;
      PMC_WPMR       at 228 range 0 .. 31;
      PMC_WPSR       at 232 range 0 .. 31;
      PMC_PCR        at 268 range 0 .. 31;
      PMC_OCR        at 272 range 0 .. 31;
      PMC_SLPWK_ER0  at 276 range 0 .. 31;
      PMC_SLPWK_DR0  at 280 range 0 .. 31;
      PMC_SLPWK_SR0  at 284 range 0 .. 31;
      PMC_SLPWK_ASR0 at 288 range 0 .. 31;
      PMC_PMMR       at 304 range 0 .. 31;
   end record;

   --  Power Management Controller
   PMC_Periph : aliased PMC_Peripheral
     with Import, Address => PMC_Base;

end Interfaces.SAM.PMC;
