------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . C P U _ S P E C I F I C               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
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

--  This package contains the primitives which are dependent on the
--  underlying processor.

pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion;
with Interfaces;

package System.BB.CPU_Specific is
   pragma Preelaborate;

   ------------------
   -- CPU Features --
   ------------------

   type Intel_Family is mod 2 ** 4;
   type Intel_Model_Number is mod 2 ** 8;

   type CPU_Model is record
      Family : Intel_Family;
      Model  : Intel_Model_Number;
   end record;

   function My_CPU_Model return CPU_Model;
   --  Return the CPU model for the current CPU

   --  List of known CPUs

   No_Model  : constant CPU_Model := (0, 0);

   --  Client and Server Processors
   Sandy_Bridge_Client : constant CPU_Model := (16#6#, 16#2A#);
   Sandy_Bridge_Server : constant CPU_Model := (16#6#, 16#2D#);
   Ivy_Bridge_Client   : constant CPU_Model := (16#6#, 16#3A#);
   Ivy_Bridge_Server   : constant CPU_Model := (16#6#, 16#3E#);
   Haswell_Client      : constant CPU_Model := (16#6#, 16#3C#);
   Haswell_Client_L    : constant CPU_Model := (16#6#, 16#35#);
   Haswell_Client_G    : constant CPU_Model := (16#6#, 16#36#);
   Haswell_Server      : constant CPU_Model := (16#6#, 16#3F#);
   Broadwell_Client    : constant CPU_Model := (16#6#, 16#3D#);
   Broadwell_Client_G  : constant CPU_Model := (16#6#, 16#47#);
   Broadwell_Server    : constant CPU_Model := (16#6#, 16#4F#);
   Broadwell_Server_D  : constant CPU_Model := (16#6#, 16#56#);

   --  Small Core Processors

   Goldmont            : constant CPU_Model := (16#6#, 16#5C#);
   Denverton           : constant CPU_Model := (16#6#, 16#5F#);

   --------------------
   -- CPUID Features --
   --------------------

   Max_CPUID_Index  : Interfaces.Unsigned_32;
   --  Max CPU ID leaf index supported by the processor

   type Unsigned_4 is mod 2 ** 4;

   type Processor_Types is
     (Original_OEM_Processor, Intel_OverDrive_Processor,
      Dual_Processor, Reserved);

   type Feature_Information_EAX is record
      Stepping        : Unsigned_4;
      Model           : Unsigned_4;
      Family          : Intel_Family;
      Processor_Type  : Processor_Types;
      Extended_Model  : Unsigned_4;
      Extended_Family : Interfaces.Unsigned_8;
   end record with Size => 32;

   for Feature_Information_EAX use record
      Stepping        at 0 range  0 .. 3;
      Model           at 0 range  4 .. 7;
      Family          at 0 range  8 .. 11;
      Processor_Type  at 0 range 12 .. 13;
      Extended_Model  at 0 range 16 .. 19;
      Extended_Family at 0 range 20 .. 27;
   end record;

   type Feature_Information_ECX is record
      x2APIC       : Boolean;
      TSC_Deadline : Boolean;
      XSAVE        : Boolean;
      OSXSAVE      : Boolean;
      AVX          : Boolean;
   end record with Size => 32;

   for Feature_Information_ECX use record
      x2APIC       at 0 range 21 .. 21;
      TSC_Deadline at 0 range 24 .. 24;
      XSAVE        at 0 range 26 .. 26;
      OSXSAVE      at 0 range 27 .. 27;
      AVX          at 0 range 28 .. 28;
   end record;

   type Feature_Information_EDX is record
      TSC  : Boolean;
      APIC : Boolean;
   end record with Size => 32;

   for Feature_Information_EDX use record
      TSC   at 0 range 4 .. 4;
      APIC  at 0 range 9 .. 9;
   end record;

   type Extended_State_Subleaf_1_EAX is record
      XSAVEOPT : Boolean;
   end record;

   for Extended_State_Subleaf_1_EAX use record
      XSAVEOPT at 0 range 0 .. 0;
   end record;

   type Extended_Function_Time_EDX is record
      Invariant_TSC : Boolean;
   end record;

   for Extended_Function_Time_EDX use record
      Invariant_TSC at 0 range 8 .. 8;
   end record;

   ------------------------------
   -- Model Specific Registers --
   ------------------------------

   MSR_PLATFORM_INFO : constant := 16#CE#;

   type Platform_Infomation is record
      Maximum_Non_Turbo_Ratio : Interfaces.Unsigned_8;
   end record with Size => 32;

   for Platform_Infomation use record
      Maximum_Non_Turbo_Ratio at 0 range 8 .. 15;
   end record;

   IA32_GS_BASE : constant := 16#C0000101#;

   -----------------------
   -- Control Registers --
   -----------------------

   type Control_Register_4 is record
      Virtual_8086_Mode_Extensions               : Boolean;
      Protected_Mode_Virtual_Interrupts          : Boolean;
      Time_Stamp_Disable                         : Boolean;
      Debugging_Extensions                       : Boolean;
      Page_Size_Extensions                       : Boolean;
      Physical_Address_Extension                 : Boolean;
      Machine_Check_Enable                       : Boolean;
      Page_Global_Enable                         : Boolean;
      Performance_Monitoring_Counter_Enable      : Boolean;
      FXSAVE_FXRSTOR_And_SSE_Enable              : Boolean;
      OS_Support_for_Unmasked_SIMD_FP_Exceptions : Boolean;
      User_Mode_Instruction_Prevention           : Boolean;
      VMX_Enable                                 : Boolean;
      SMX_Enable                                 : Boolean;
      FSGSBASE_Enable                            : Boolean;
      PCID_Enable                                : Boolean;
      XSAVE_and_Processor_Extended_States_Enable : Boolean;
      SMEP_Enable                                : Boolean;
      SMAP_Enable                                : Boolean;
      Protection_Key_Enable                      : Boolean;
   end record with Size => 64;
   --  Important: when writing to a control register, the values of the reserve
   --  bits should not be changed.

   type Hardware_Piority_Level is mod 2 ** 4;

   type Control_Register_8 is record
      Task_Priority_Level : Hardware_Piority_Level;
   end record with Size => 64;

   for Control_Register_4 use record
      Virtual_8086_Mode_Extensions               at 0 range  0 .. 0;
      Protected_Mode_Virtual_Interrupts          at 0 range  1 .. 1;
      Time_Stamp_Disable                         at 0 range  2 .. 2;
      Debugging_Extensions                       at 0 range  3 .. 3;
      Page_Size_Extensions                       at 0 range  4 .. 4;
      Physical_Address_Extension                 at 0 range  5 .. 5;
      Machine_Check_Enable                       at 0 range  6 .. 6;
      Page_Global_Enable                         at 0 range  7 .. 7;
      Performance_Monitoring_Counter_Enable      at 0 range  8 .. 8;
      FXSAVE_FXRSTOR_And_SSE_Enable              at 0 range  9 .. 9;
      OS_Support_for_Unmasked_SIMD_FP_Exceptions at 0 range 10 .. 10;
      User_Mode_Instruction_Prevention           at 0 range 11 .. 11;
      VMX_Enable                                 at 0 range 13 .. 13;
      SMX_Enable                                 at 0 range 14 .. 14;
      FSGSBASE_Enable                            at 0 range 16 .. 16;
      PCID_Enable                                at 0 range 17 .. 17;
      XSAVE_and_Processor_Extended_States_Enable at 0 range 18 .. 18;
      SMEP_Enable                                at 0 range 20 .. 20;
      SMAP_Enable                                at 0 range 21 .. 21;
      Protection_Key_Enable                      at 0 range 22 .. 22;
   end record;

   for Control_Register_8 use record
      Task_Priority_Level at 0 range 0 .. 3;
   end record;

   ------------------------
   -- Context management --
   ------------------------

   --  EFLAG

   type IO_Privilege is mod 2;
   type Carry_Flag_Status is (False, True);
   for Carry_Flag_Status use (False => 2#10#, True => 2#11#);
   --  The bit next to the Carry Flag is always 1 so extend the carry flag to
   --  cover that bit as well to save us from having to have to define a
   --  useless "always one" component to the record.

   type EFLAGS is record
      Carry_Flag                     : Carry_Flag_Status;
      Parity_Flag                    : Boolean;
      Auxiliary_Carry_Flag           : Boolean;
      Zero_Flag                      : Boolean;
      Sign_Flag                      : Boolean;
      Trap_Flag                      : Boolean;
      Interrupt_Enable_Flag          : Boolean;
      Direction_Flag                 : Boolean;
      Overflow_Flag                  : Boolean;
      IO_Privilege_Level             : IO_Privilege;
      Nested_Task                    : Boolean;
      Resume_Flag                    : Boolean;
      Virtual_8086_Mode              : Boolean;
      Alignment_Check_Access_Control : Boolean;
      Virtual_Interrupt_Flag         : Boolean;
      Virtual_Interrupt_Pending      : Boolean;
      ID_Flag                        : Boolean;
   end record with Size => 64;

   for EFLAGS use record
      Carry_Flag                     at 0 range  0 .. 1;
      Parity_Flag                    at 0 range  2 .. 2;
      Auxiliary_Carry_Flag           at 0 range  4 .. 4;
      Zero_Flag                      at 0 range  6 .. 6;
      Sign_Flag                      at 0 range  7 .. 7;
      Trap_Flag                      at 0 range  8 .. 8;
      Interrupt_Enable_Flag          at 0 range  9 .. 9;
      Direction_Flag                 at 0 range 10 .. 10;
      Overflow_Flag                  at 0 range 11 .. 11;
      IO_Privilege_Level             at 0 range 12 .. 13;
      Nested_Task                    at 0 range 14 .. 14;
      Resume_Flag                    at 0 range 16 .. 16;
      Virtual_8086_Mode              at 0 range 17 .. 17;
      Alignment_Check_Access_Control at 0 range 18 .. 18;
      Virtual_Interrupt_Flag         at 0 range 19 .. 19;
      Virtual_Interrupt_Pending      at 0 range 20 .. 20;
      ID_Flag                        at 0 range 21 .. 21;
   end record;

   --  The context buffer is a type that represents thread's state and is not
   --  otherwise stored in main memory. This typically includes all user-
   --  visible registers, and possibly some other status as well.

   type Context_Buffer is record
      --  Only callee-saved registers need to be saved, as the context switch
      --  is always synchronous. We save/restore the link return register
      --  so we can load the initial stack pointer on start.

      RIP    : System.Address;  -- Link return register
      RFLAGS : EFLAGS;          -- FLAG register
      RSP    : System.Address;  -- Stack pointer

      RBX    : System.Address;
      RBP    : System.Address;
      R12    : System.Address;
      R13    : System.Address;
      R14    : System.Address;
      R15    : System.Address;
   end record;

   for Context_Buffer use record
      RIP          at 0  range 0 .. 63;
      RFLAGS       at 8  range 0 .. 63;
      RSP          at 16 range 0 .. 63;
      RBX          at 24 range 0 .. 63;
      RBP          at 32 range 0 .. 63;
      R12          at 40 range 0 .. 63;
      R13          at 48 range 0 .. 63;
      R14          at 56 range 0 .. 63;
      R15          at 64 range 0 .. 63;
   end record;

   --  State_Component_Bit_Map

   type State_Component_Bit_Map is record
      X87               : Boolean;
      SSE               : Boolean;
      AVX               : Boolean;
      MPX_BNDREGS       : Boolean;
      MPX_BNDCSR        : Boolean;
      AVX_512_Opmask    : Boolean;
      AVX_512_ZMM_Hi256 : Boolean;
      AVX_512_Hi16_ZMM  : Boolean;
      PT                : Boolean;
      PKRU              : Boolean;
      CET_U             : Boolean;
      CET_S             : Boolean;
      HDC               : Boolean;
   end record with Size => 64;

   for State_Component_Bit_Map use record
      X87               at 0 range  0 .. 0;
      SSE               at 0 range  1 .. 1;
      AVX               at 0 range  2 .. 2;
      MPX_BNDREGS       at 0 range  3 .. 3;
      MPX_BNDCSR        at 0 range  4 .. 4;
      AVX_512_Opmask    at 0 range  5 .. 5;
      AVX_512_ZMM_Hi256 at 0 range  6 .. 6;
      AVX_512_Hi16_ZMM  at 0 range  7 .. 7;
      PT                at 0 range  8 .. 8;
      PKRU              at 0 range  9 .. 9;
      CET_U             at 0 range 11 .. 11;
      CET_S             at 0 range 12 .. 12;
      HDC               at 0 range 13 .. 13;
   end record;

   BB_X86_Context_State : constant State_Component_Bit_Map :=
     (X87               => True,
      SSE               => True,
      AVX               => True,
      AVX_512_Opmask    => True,
      AVX_512_ZMM_Hi256 => True,
      AVX_512_Hi16_ZMM  => True,
      others            => False);
   --  The process state that we will save for the BB runtime. Keep in sync
   --  with the XSAVE_Area type above, otherwise we run the risk of writing
   --  outside the bounds of XSAVE_Area.

   Stack_Alignment : constant := 16;
   --  Hardware stack alignment requirement

   ----------------
   -- Local APIC --
   ----------------

   Local_APIC_Base_Address                 : constant := 16#FEE0_0000#;
   Local_APIC_ID_Offset_Address            : constant := 16#020#;
   Local_APIC_EOI_Offset_Address           : constant := 16#0B0#;
   Local_APIC_Spurious_Int_Offset_Address  : constant := 16#0F0#;
   Local_APIC_ICR_Low_Offset_Address       : constant := 16#300#;
   Local_APIC_ICR_High_Offset_Address      : constant := 16#310#;
   Local_APIC_LVT_Timer_Offset_Address     : constant := 16#320#;
   Local_APIC_Initial_Count_Offset_Address : constant := 16#380#;
   Local_APIC_Timer_Current_Count_Address  : constant := 16#390#;
   Local_APIC_Divide_Config_Offset_Address : constant := 16#3E0#;

   subtype LAPIC_Vector is Natural range 0 .. 255;
   --  Local APIC IDs

   type APIC_ID is new Interfaces.Unsigned_8;

   type Local_APIC_ID is record
      ID : APIC_ID;
   end record with Size => 32;

   for Local_APIC_ID use record
      ID at 0 range 24 .. 31;
   end record;

   type Local_APIC_EOI is (Signal) with Size => 32;
   for Local_APIC_EOI use (Signal => 0);

   type Local_APIC_Spurious_Interrupt is record
      Spurious_Vector          : LAPIC_Vector;
      APIC_Enabled             : Boolean;
      Focus_Processor_Checking : Boolean;
      Suppress_EOI_Broadcast   : Boolean;
   end record with Size => 32;

   for Local_APIC_Spurious_Interrupt use record
      Spurious_Vector          at 0 range  0 .. 7;
      APIC_Enabled             at 0 range  8 .. 8;
      Focus_Processor_Checking at 0 range  9 .. 9;
      Suppress_EOI_Broadcast   at 0 range 12 .. 12;
   end record;

   type Interrupt_Command_High is record
      Destination : Interfaces.Unsigned_8;
   end record;

   for Interrupt_Command_High use record
      Destination at 0 range 24 .. 31;
   end record;

   type Destination_Shorthand is
     (No_Shorthand, Self, All_Including_Self, All_Excluding_Self);
   type Trigger_Type is (Edge, Level);
   for Trigger_Type use (Edge => 2#01#, Level => 2#11#);
   type Delivery_Status is (Idle, Send_Pending);
   type Destination_Mode_Type is (Physical, Logical);
   type Delivery_Mode_Type is
     (Fixed, Lowest_Priority, SMI, NMI, INIT, Start_Up);
   for Delivery_Mode_Type use
     (Fixed => 2#000#, Lowest_Priority => 2#001#,
      SMI   => 2#010#, NMI             => 2#100#,
      INIT  => 2#101#, Start_Up        => 2#110#);

   type Interrupt_Command_Low is record
      Destination      : Destination_Shorthand;
      Trigger          : Trigger_Type;
      Delivery         : Delivery_Status;
      Destination_Mode : Destination_Mode_Type;
      Delivery_Mode    : Delivery_Mode_Type;
      Vector           : LAPIC_Vector;
   end record;

   for Interrupt_Command_Low use record
      Destination      at 0 range 18 .. 19;
      Trigger          at 0 range 14 .. 15;
      Delivery         at 0 range 12 .. 12;
      Destination_Mode at 0 range 11 .. 11;
      Delivery_Mode    at 0 range 8  .. 10;
      Vector           at 0 range 0  .. 7;
   end record;

   type Timer_Type is (One_Shot, Periodic, TSC_Deadline);
   for Timer_Type use (One_Shot => 0, Periodic => 1, TSC_Deadline => 2);

   type LVT_Timer is record
      Timer_Mode : Timer_Type;
      Mask       : Boolean;
      Delivery   : Delivery_Status;
      Vector     : LAPIC_Vector;
   end record with Size => 32;

   for LVT_Timer use record
      Timer_Mode at 0 range 17 .. 18;
      Mask       at 0 range 16 .. 16;
      Delivery   at 0 range 12 .. 12;
      Vector     at 0 range 0 .. 7;
   end record;

   type APIC_Time is new Interfaces.Unsigned_32;

   type Divide_Configuration is
     (Divide_by_2,  Divide_by_4,   Divide_by_8, Divide_by_16, Divide_by_32,
      Divide_by_64, Divide_by_128, Divide_by_1);

   for Divide_Configuration use
     (Divide_by_2   => 2#0000#,
      Divide_by_4   => 2#0001#,
      Divide_by_8   => 2#0010#,
      Divide_by_16  => 2#0011#,
      Divide_by_32  => 2#1000#,
      Divide_by_64  => 2#1001#,
      Divide_by_128 => 2#1010#,
      Divide_by_1   => 2#1011#);

   Local_APIC_ID_Register : Local_APIC_ID
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_ID_Offset_Address);

   Local_APIC_End_of_Interrupt : Local_APIC_EOI
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_EOI_Offset_Address);

   Local_APIC_Spurious_Interrupt_Register : Local_APIC_Spurious_Interrupt
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_Spurious_Int_Offset_Address);

   Interrupt_Command_Register_Low : Interrupt_Command_Low
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_ICR_Low_Offset_Address);

   Interrupt_Command_Register_High : Interrupt_Command_High
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_ICR_High_Offset_Address);

   Local_APIC_LVT_Timer_Register : LVT_Timer
     with Volatile_Full_Access,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_LVT_Timer_Offset_Address);

   Local_APIC_Timer_Initial_Count : APIC_Time
     with Volatile,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_Initial_Count_Offset_Address);

   Local_APIC_Timer_Current_Count : APIC_Time
     with Volatile,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_Timer_Current_Count_Address);

   Local_APIC_Timer_Divide_Configuration : Divide_Configuration
     with Volatile,
       Address =>
         System'To_Address
           (Local_APIC_Base_Address + Local_APIC_Divide_Config_Offset_Address);

   ----------------
   -- APIC Timer --
   ----------------

   APIC_Timer_Vector : constant LAPIC_Vector := 255;
   --  Local APIC vector for the APIC Timer

   APIC_Frequency_In_kHz : Interfaces.Unsigned_64;
   --  The frequency Local APIC Timer in kHz. We use kHz because is gives us
   --  more head room when converting from ticks to nanoseconds and vice
   --  versa (since we need to multiply ticks with the frequency, which caps
   --  the maximum number of ticks the clock can accumulate until we can no
   --  longer perform that conversion).

   TSC_Frequency_In_kHz : Interfaces.Unsigned_64;
   --  The frequency of the Time Stamp Counter in kHz. We use kHz because is
   --  gives us more head room when converting from ticks to nanoseconds and
   --  vice versa.

   ---------
   -- PIT --
   ---------

   PIT_Channel_0_Data_Port : constant := 16#40#;
   PIT_Mode_Command_Port   : constant := 16#43#;

   type PIT_Channel is (Channel_0, Channel_1, Channel_2, Read_Back);
   type PIT_Access_Mode is
     (Latch_Count_Value, Low_Byte, High_Byte, Low_High_Byte);
   type PIT_Operating_Mode is
     (Interrupt_On_Terminal_Count, One_Shot, Rate_Generator,
      Square_Wave_Generator, Software_Triggered_Strobe,
      Hardware_Triggered_Strobe);
   type PIT_BCD_Binary_Mode is (BCD, Binary);

   type PIT_Mode_Command_Register is record
      Channel         : PIT_Channel;
      Access_Mode     : PIT_Access_Mode;
      Operating_Mode  : PIT_Operating_Mode;
      BCD_Binary_Mode : PIT_BCD_Binary_Mode;
   end record with Size => 8;

   for PIT_Mode_Command_Register use record
      Channel         at 0 range 6 .. 7;
      Access_Mode     at 0 range 4 .. 5;
      Operating_Mode  at 0 range 1 .. 3;
      BCD_Binary_Mode at 0 range 0 .. 0;
   end record;

   function To_IO_Byte is new
     Ada.Unchecked_Conversion
       (PIT_Mode_Command_Register, Interfaces.Unsigned_8);

   ---------------
   -- CPU Clock --
   ---------------

   function Read_TSC return Interfaces.Unsigned_64
     with Inline;
   --  Read the TSC

   -------------------------
   -- Hardware Exceptions --
   -------------------------

   type Error_Code is mod 2 ** 64;
   --  A code the processor will push onto the exception stack in response to
   --  certain exceptions.

   --  Exceptions IDs

   Divide_Error_Exception         : constant LAPIC_Vector := 0;
   Dedug_Execption                : constant LAPIC_Vector := 1;
   NMI_Interrupt                  : constant LAPIC_Vector := 2;
   Breakpoint_Execption           : constant LAPIC_Vector := 3;
   Overflow_Exception             : constant LAPIC_Vector := 4;
   BOUND_Range_Exceeded_Exception : constant LAPIC_Vector := 5;
   Invalid_Opcode_Exception       : constant LAPIC_Vector := 6;
   Device_Not_Available_Exception : constant LAPIC_Vector := 7;
   Double_Fault_Exception         : constant LAPIC_Vector := 8;
   Invalid_TSS_Exception          : constant LAPIC_Vector := 10;
   Segment_Not_Present_Exception  : constant LAPIC_Vector := 11;
   Stack_Segment_Fault_Exception  : constant LAPIC_Vector := 12;
   General_Protection_Exception   : constant LAPIC_Vector := 13;
   Page_Fault_Exception           : constant LAPIC_Vector := 14;
   Math_Fault_Exception           : constant LAPIC_Vector := 16;
   Alignment_Check_Exception      : constant LAPIC_Vector := 17;
   Machine_Check_Exception        : constant LAPIC_Vector := 18;
   SIMD_Floating_Point_Exception  : constant LAPIC_Vector := 19;
   Virtualization_Exception       : constant LAPIC_Vector := 20;
   Control_Protection_Exception   : constant LAPIC_Vector := 21;

   -----------------------
   -- Hardware Platform --
   -----------------------

   --  Some hypervisors and bootloaders can provide extra information that can
   --  assist the setup of the runtime.

   type Hardware_Type is (Bare_Metal, LynxSecure);
   for Hardware_Type use (Bare_Metal => 1, LynxSecure => 2);
   --  Use a non-zero for our default Bare_Metal to ensure the Host_Hardware
   --  object is not placed in the .bss section. This is because our startup
   --  code will change it before the .bss has been initialised.

   Host_Hardware : Hardware_Type := Bare_Metal with
     Export, External_Name => "__gnat_host_hardware";
   --  The type of host we are running on. To be set by the target specific
   --  startup code.

   Host_Info : System.Address := System.Null_Address with
     Export, External_Name => "__gnat_host_info";
   --  Pointer to a data structure provided by the host on boot

   ------------------
   -- Helper Types --
   ------------------

   --  The Bytable types allow the individual bytes of an unsigned types to be
   --  easily accessed.

   type Bytable_View is (Full, Bytes);

   type Unsigned_16_Bytable (View : Bytable_View := Full) is record
      case View is
         when Full =>
            Value : Interfaces.Unsigned_16;
         when Bytes =>
            Low   : Interfaces.Unsigned_8;
            High  : Interfaces.Unsigned_8;
      end case;
   end record with Unchecked_Union, Size => 16;

   for Unsigned_16_Bytable use record
      Value at 0 range 0 .. 15;
      Low   at 0 range 0 .. 7;
      High  at 0 range 8 .. 15;
   end record;

end System.BB.CPU_Specific;
