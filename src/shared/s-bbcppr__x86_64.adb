------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements x86-64 architecture specific support for the GNAT
--  Ravenscar run time.

--  *** IMPORTANT ***
--
--  This package needs to be compiled at least at -O1 because the procedure
--  Interrupt_Handler is a naked function that starts with the following
--  instruction:
--
--    Asm
--      ("subq %0, %%gs:%1",
--       Inputs   =>
--         (Unsigned_32'Asm_Input ("i", Parameters.Interrupt_Stack_Frame_Size),
--          System.Address'Asm_Input ("m", TSS.IST1)));
--
--  While the %gs register offset has a known compile-time value provided as
--  an inline assembly input, GCC at -O0 will generate code that initially
--  loads the offset into a register first rather than treat the compile-time
--  value as an immediate constant. Since this is a naked function, the
--  register load will trash the effected register before we have a chance to
--  manually save it. (The offset is not hardcoded as a raw integer for
--  readability and maintainability purposes: it allows for the TSS record to
--  change without having to update the affected assembly).

pragma Suppress (All_Checks);

with Ada.Unchecked_Conversion;
with System.BB.Board_Support;
with System.BB.Threads.Queues;
with System.Multiprocessors;

with Interfaces;             use Interfaces;
with Interfaces.X86_64;      use Interfaces.X86_64;
with System.BB.CPU_Specific; use System.BB.CPU_Specific;
with System.BB.Interrupts;   use System.BB.Interrupts;
with System.BB.Parameters;   use System.BB.Parameters;
with System.Machine_Code;    use System.Machine_Code;

package body System.BB.CPU_Primitives is

   package SSE renames System.Storage_Elements;

   pragma Warnings (Off, "*bits of * unused");
   --  Suppress warning for unused bits in a record as this occurs often in
   --  Machine registers.

   ------------------------
   -- Task State Segment --
   ------------------------

   --  The Task State Segment is a processor-unique data record that holds two
   --  sets of data:
   --
   --    1. Interrupt stack pointers that the processor switches to when an
   --       interrupt is raised.
   --
   --    2. Run-time x86-64 specific data. We load the address of the
   --       processors' TSS into the %GS register so this data is easily
   --       accessed during interrupt handlers.

   type Task_State_Segment is record
      --  Intel defined components --

      RSP0 : System.Address;
      RSP1 : System.Address;
      RSP2 : System.Address;
      --  Privilege stack pointers

      IST1 : System.Address;
      IST2 : System.Address;
      IST3 : System.Address;
      IST4 : System.Address;
      IST5 : System.Address;
      IST6 : System.Address;
      IST7 : System.Address;
      --  Interrupt stack table pointers

      IO_Map_Base_Address : Unsigned_16;
      --  I/O permission bit map address

      --  Runtime defined components --

      CPU_ID          : Multiprocessors.CPU;
      --  The runtime CPU ID is different from the Local APIC ID (runtime CPU
      --  starts at 1, Local APIC ID starts at 0), so store the runtime CPU ID
      --  here for quick access.

      XSAVE_Size       : Unsigned_64;
      --  Size of the XSAVE state in bytes. Stored here so it can be quickly
      --  retrieved in the interrupt handler.
   end record;

   for Task_State_Segment use record
      RSP0                at 4   range 0 .. 63;
      RSP1                at 12  range 0 .. 63;
      RSP2                at 20  range 0 .. 63;
      IST1                at 36  range 0 .. 63;
      IST2                at 44  range 0 .. 63;
      IST3                at 52  range 0 .. 63;
      IST4                at 60  range 0 .. 63;
      IST5                at 68  range 0 .. 63;
      IST6                at 76  range 0 .. 63;
      IST7                at 84  range 0 .. 63;
      IO_Map_Base_Address at 104 range 0 .. 15;
      CPU_ID              at 108 range 0 .. 63;
      XSAVE_Size          at 116 range 0 .. 63;
   end record;
   --  Manually layout the record for GDB

   CPU_Task_State : array (Multiprocessors.CPU) of aliased Task_State_Segment
     with Alignment => 16;
   --  Task specific information for each task stored in the TSS table. Each
   --  CPU will store the address of their respective CPU_Task_State record in
   --  their %gs register.

   -----------------------------
   -- Global Descriptor Table --
   -----------------------------

   --  The Global Descriptor Table provides the descriptors that are used to
   --  configure x86-64 memory segmentation for our run-time. Our use of
   --  memory segmentation is limited to providing seperate data and code
   --  segments that span 4-GiB of RAM along with TSS segments for each CPU.

   --  x86 Descriptor Types
   --  See Intel 64 and IA-32 Architectures Software Developer's Manual
   --  Volume 3A, Chapter 3: Protected-Mode Memory Management.

   type Base_Address_Low_Part is mod 2 ** 24;
   type Base_Address_High_Long_Part is mod 2 ** 40;
   type Base_Address_High_Word_Part is mod 2 ** 8;

   type System_Segment_Type is
     (LDT, TSS_Available, TSS_Busy, Call_Gate, Interrupt_Gate, Trap_Gate);
   for System_Segment_Type use
      (LDT            => 2,
       TSS_Available  => 9,
       TSS_Busy       => 11,
       Call_Gate      => 12,
       Interrupt_Gate => 14,
       Trap_Gate      => 15);

   type Data_Code_Segment_Kind is (Data_Segment, Code_Segment);
   for Data_Code_Segment_Kind use
     (Data_Segment => 2#10#, Code_Segment => 2#11#);

   type Descriptor_Privilege_Level is mod 2 ** 2;
   type Segment_Limit_High_Part is mod 2 ** 4;
   type Granularity_Flag is (Limit_in_Byte_Units, Limit_In_4KBytes_Units);

   type Segment_Descriptor (Descriptor_Type : Data_Code_Segment_Kind) is record
      Segment_Limit_Low       : Unsigned_16;
      Base_Address_Low        : Base_Address_Low_Part;
      Accessed                : Boolean;
      Privilege_Level         : Descriptor_Privilege_Level;
      Segment_Present         : Boolean;
      Segment_Limit_High      : Segment_Limit_High_Part;
      System_Flag             : Boolean;
      --  The System Flag can be used the runtime for its own use
      Granularity             : Granularity_Flag;
      Base_Address_High       : Base_Address_High_Word_Part;
      case Descriptor_Type  is
         when Data_Segment =>
            Write_Enabled     : Boolean;
            Expand_Down       : Boolean;
            Big_Flag          : Boolean;
         when Code_Segment =>
            Long_Segment      : Boolean;
            Read_Enabled      : Boolean;
            Conforming        : Boolean;
            Default_Size_Flag : Boolean;
      end case;
   end record with Size => 8 * 8;

   for Segment_Descriptor use record
      Segment_Limit_Low  at 0 range  0 .. 15;
      Base_Address_Low   at 2 range  0 .. 23;
      Accessed           at 4 range  8 .. 8;
      Write_Enabled      at 4 range  9 .. 9;
      Read_Enabled       at 4 range  9 .. 9;
      Expand_Down        at 4 range 10 .. 10;
      Conforming         at 4 range 10 .. 10;
      Descriptor_Type    at 4 range 11 .. 12;
      Privilege_Level    at 4 range 13 .. 14;
      Segment_Present    at 4 range 15 .. 15;
      Segment_Limit_High at 4 range 16 .. 19;
      System_Flag        at 4 range 20 .. 20;
      Long_Segment       at 4 range 21 .. 21;
      Big_Flag           at 4 range 22 .. 22;
      Default_Size_Flag  at 4 range 22 .. 22;
      Granularity        at 4 range 23 .. 23;
      Base_Address_High  at 7 range  0 .. 7;
   end record;

   type TSS_Descriptor is record
      Segment_Limit_Low  : Unsigned_16;
      Base_Address_Low   : Base_Address_Low_Part;
      Segment_Type       : System_Segment_Type;
      Privilege_Level    : Descriptor_Privilege_Level;
      Segment_Present    : Boolean;
      Segment_Limit_High : Segment_Limit_High_Part;
      System_Flag        : Boolean;
      --  The System Flag can be used the runtime for its own use
      Granularity        : Granularity_Flag;
      Base_Address_High  : Base_Address_High_Long_Part;
   end record with Size => 128;

   for TSS_Descriptor use record
      Segment_Limit_Low  at 0 range  0 .. 15;
      Base_Address_Low   at 2 range  0 .. 23;
      Segment_Type       at 4 range  8 .. 12;
      Privilege_Level    at 4 range 13 .. 14;
      Segment_Present    at 4 range 15 .. 15;
      Segment_Limit_High at 4 range 16 .. 19;
      System_Flag        at 4 range 20 .. 20;
      Granularity        at 4 range 23 .. 23;
      Base_Address_High  at 7 range  0 .. 39;
   end record;

   type TSS_Entries is array (Multiprocessors.CPU) of TSS_Descriptor;

   type Runtime_Gobal_Descriptor_Table is record
      Null_Descriptor : Unsigned_64;                     -- As required by CPU
      Code            : Segment_Descriptor (Code_Segment);
      Data            : Segment_Descriptor (Data_Segment);
      CPU_TSS         : TSS_Entries;
   end record;

   type Descriptor_Pointer is record
      Limit        : Unsigned_16;
      Base_Address : System.Address;
   end record;

   for Descriptor_Pointer use record
      Limit        at 0 range 0 .. 15;
      Base_Address at 2 range 0 .. 63;
   end record;

   GDT : Runtime_Gobal_Descriptor_Table :=
     (Null_Descriptor       => 0,
      --  Code and Data segments
      --  Map to the full 4GB by using Segment Limit 16#FFFFF# and base of 0.
      Code =>
        (Descriptor_Type    => Code_Segment,
         Segment_Limit_Low  => 16#FFFF#,
         Base_Address_Low   => 0,
         Accessed           => False,
         Read_Enabled       => True,
         Conforming         => False,
         Privilege_Level    => 0,
         Segment_Present    => True,
         Segment_Limit_High => 16#F#,
         System_Flag        => False,
         Long_Segment       => True,
         Default_Size_Flag  => False,
         Granularity        => Limit_In_4KBytes_Units,
         Base_Address_High => 0),
      Data =>
        (Descriptor_Type    => Data_Segment,
         Segment_Limit_Low  => 16#FFFF#,
         Base_Address_Low   => 0,
         Accessed           => False,
         Write_Enabled      => True,
         Expand_Down        => False,
         Privilege_Level    => 0,
         Segment_Present    => True,
         Segment_Limit_High => 16#F#,
         System_Flag        => False,
         Big_Flag           => True,
         Granularity        => Limit_In_4KBytes_Units,
         Base_Address_High => 0),
      --  CPU TSS. One needed for each CPU.
      CPU_TSS =>
        (others =>
          (Segment_Limit_Low  => (Task_State_Segment'Size) / 8 - 1,
           --  Only the contents of the TSS Descriptor should be accessed via
           --  this segment.
           Base_Address_Low   => 0,
           --  We will set the processor's TSS base address in the
           --  Initialize_CPU procedure for two reasons:
           --     1. It's hard to split (or even just take the lower part) of
           --        System.Address in a preelaborate unit with no elaboration.
           --     2. Makes it easier to change the number of CPUs
           Segment_Type       => TSS_Available,
           Privilege_Level    => 0,
           Segment_Present    => True,
           Segment_Limit_High => 0,
           System_Flag        => False,
           Granularity        => Limit_in_Byte_Units,
           Base_Address_High  => 0)))
     with Alignment => 8;
   --  GDT used by the runtime. This will replace the default GDT in start.S
   --  to include the TSS Descriptors for each CPU.

   --------------------------------
   -- Interrupt Descriptor Table --
   --------------------------------

   --  The Interrupt Descriptor Table associates exception and interrupt
   --  vectors with interrupt handlers via trap and interrupt gates. The IDT
   --  is filled out in Setup_Interrupt_Descriptor_Table because we can't
   --  statically allocate the table since the handler address splits across
   --  different fields in the interrupt gate (or we can, but then we have to
   --  fix the location of the IDT in memory). The vector handlers are defined
   --  vector_handlers.S.
   --
   --  Interrupt Descriptor Table
   --  See Intel 64 and IA-32 Architectures Software Developer's Manual
   --  Volume 3A, Chapter 6: Interrupt and Exception Handling

   type Vector_Address_Low_Part is mod 2 ** 16;
   type Vector_Address_High_Part is mod 2 ** 48;

   type Interrupt_Stack_Table_Selector is mod 2 ** 3;

   type IDT_Gate is record
      Vector_Address_Low    : Vector_Address_Low_Part;
      Segment_Selector      : Unsigned_16;
      Interrupt_Stack_Table : Interrupt_Stack_Table_Selector;
      Segment_Type          : System_Segment_Type;
      Privilege_Level       : Descriptor_Privilege_Level;
      Segment_Present       : Boolean;
      Vector_Address_High   : Vector_Address_High_Part;
   end record with Size => 128;

   for IDT_Gate use record
      Vector_Address_Low    at 0 range  0 .. 15;
      Segment_Selector      at 0 range 16 .. 31;
      Interrupt_Stack_Table at 0 range 32 .. 34;
      Segment_Type          at 0 range 40 .. 43;
      Privilege_Level       at 0 range 45 .. 46;
      Segment_Present       at 0 range 47 .. 47;
      Vector_Address_High   at 0 range 48 .. 95;
   end record;

   IDT : array (LAPIC_Vector'Range) of IDT_Gate;

   procedure Setup_Interrupt_Descriptor_Table;
   --  Populate the Interrupt Descriptor Table with entries for our runtime's
   --  interrupt and exception handlers.

   ---------------------------
   -- Exception Stack Table --
   ---------------------------

   --  x86 exceptions use a seperate stack from interrupt handlers as they use
   --  traps which means they can occur at any time and do not disable
   --  interrupts when they execute.

   type Exception_Stack_Space is new Storage_Elements.Storage_Array
     (1 .. Storage_Elements.Storage_Offset (Parameters.Exception_Stack_Size));
   for Exception_Stack_Space'Alignment use CPU_Specific.Stack_Alignment;
   pragma Suppress_Initialization (Exception_Stack_Space);
   --  Type used to represent the stack area for each exception. The stack must
   --  be aligned to the CPU specific alignment.

   Exception_Stacks : array (Multiprocessors.CPU) of Exception_Stack_Space
     with Linker_Section => ".interrupt_stacks";
   --  Array that contains the stack used for exceptions on each CPU.

   procedure Exception_Handler
     with Export, External_Name => "__gnat_exception_handler";
   --  Handler for exceptions. This procedure takes into account that the
   --  processor or exception stub will push an error code on the stack that
   --  we need to remove before we exit. It's also a simpler implementation
   --  than the Interrupt_Handler because the hardware does not support nested
   --  exceptions (outside the double fault exception which is terminal).
   pragma Machine_Attribute (Exception_Handler, "naked");
   --  We do not want the compiler creating any prologue or epilogue sequences
   --  for our handler function.

   procedure Process_Exception (ID : LAPIC_Vector; Code : Error_Code);
   --  Process the exception that was raised and invoke any specific runtime
   --  response. This is seperate from the Exception_Handler above as we can
   --  more freely use Ada code.

   ------------------------
   -- Interrupt Handling --
   ------------------------

   procedure Interrupt_Handler
     with Export, External_Name => "__gnat_interrupt_handler";
   --  The interrupt handler routine is implemented in inline assembly rather
   --  than in a standalone assembly file because it uses constants defined
   --  in Ada code that are hard to use in assembly without resorting to
   --  duplicating them and creating a maintenance burden.

   pragma Machine_Attribute (Interrupt_Handler, "naked");
   --  We do not want the compiler creating any prologue or epilogue sequences
   --  for our handler function.

   Spurious_Interrupt_Vector : constant := 32;
   --  Vector for the spurious interrupts. Keep in sync with vector_table.S

   -----------------------
   -- Context Switching --
   -----------------------

   function Get_CR4 return Control_Register_4;
   procedure Set_CR4 (CR4 : Control_Register_4);
   --  Get and set the CR4 register

   procedure Thread_Start;
   pragma Machine_Attribute (Thread_Start, "naked");
   --  Starting point for a new thread. Since we use the normal context
   --  switching routines to switch to a new thread, we have to pass the thread
   --  argument through a callee-saved register and use this function to move
   --  it to %rdi before calling the thread's procedure.

   --------------------
   -- Floating-Point --
   --------------------

   Runtime_SSE_MXCSR : constant Unsigned_32 := 16#1F80#;
   --  Default SSE MXCSR with default 0x1F80

   -----------
   -- Timer --
   -----------

   procedure Determine_Clock_Frequencies;
   --  Attempt to find out what the TSC and APIC Timer frequencies are. An
   --  exception is raised if procedure cannot determine these clock
   --  frequencies.

   -------------------
   -- Miscellaneous --
   -------------------

   NL : constant String := ASCII.LF & ASCII.HT;
   --  New line separator in Asm templates

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
      use System.BB.Threads.Queues;
      use System.BB.Threads;

      CPU_Id : constant System.Multiprocessors.CPU :=
                 Board_Support.Multiprocessors.Current_CPU;

      New_Priority : constant Integer :=
                       First_Thread_Table (CPU_Id).Active_Priority;
   begin
      --  Set the hardware interrupt priority mask

      if New_Priority < Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (New_Priority);
      end if;

      --  Save the callee-saved values into the context save space of the
      --  curent thread.

      Asm
        ( --  RIP (Return address)
         "popq     %%rax"                                                & NL &
         "movq     %%rax, 0(%0)"                                         & NL &
         --  RFLAGS
         "pushfq"                                                        & NL &
         "popq     %%rax"                                                & NL &
         "movq     %%rax, 8(%0)"                                         & NL &
         --  Callee save registers
         "movq     %%rsp, 16(%0)"                                        & NL &
         "movq     %%rbx, 24(%0)"                                        & NL &
         "movq     %%rbp, 32(%0)"                                        & NL &
         "movq     %%r12, 40(%0)"                                        & NL &
         "movq     %%r13, 48(%0)"                                        & NL &
         "movq     %%r14, 56(%0)"                                        & NL &
         "movq     %%r15, 64(%0)",
        Inputs   => Thread_Id'Asm_Input ("r", Running_Thread_Table (CPU_Id)),
        Clobber  => "rax",
        Volatile => True);

      --  While the x87 FPU Control Word and SSE MXCSR Control and Status
      --  Register are callee save we don't save it as they are under control
      --  of the runtime (and shouldn't need to change between tasks).

      --  Note the new running task in Running_Thread_Table

      Running_Thread_Table (CPU_Id) := First_Thread_Table (CPU_Id);

      --  Restore the callee-saved registers of the task we are switching to

      Asm
        ( --  Callee save registers
         "movq     16(%0), %%rsp"                                        & NL &
         "movq     24(%0), %%rbx"                                        & NL &
         "movq     32(%0), %%rbp"                                        & NL &
         "movq     40(%0), %%r12"                                        & NL &
         "movq     48(%0), %%r13"                                        & NL &
         "movq     56(%0), %%r14"                                        & NL &
         "movq     64(%0), %%r15"                                        & NL &
         --  RIP (Return address)
         "pushq     0(%0)"                                               & NL &
         --  RFLAGS
         "pushq    8(%0)"                                                & NL &
         "popfq",
        Inputs   => Thread_Id'Asm_Input ("r", First_Thread_Table (CPU_Id)),
        Volatile => True);
   end Context_Switch;

   -----------------------------
   -- Determine_TSC_Frequency --
   -----------------------------

   --  Refer to Intel 64 and IA-32 Architectures Software Developer's Manual,
   --  Volume 3B, Section 18.7.3 for all the magic that happens here.

   procedure Determine_Clock_Frequencies  is
      TSC_Information_Leaf          : constant := 16#15#;
      Processor_Frequency_Info_Leaf : constant := 16#16#;
      --  CPUID leaf IDs

      Core_Crystal_Clock_Mhz    : Unsigned_64;
      TSC_Ratio_Numerator       : Unsigned_64;
      TSC_Ratio_Denominator     : Unsigned_64;
      Processor_Base_Frequency  : Unsigned_64;
      --  Clock information that we query from the CPU

   begin
      pragma Warnings (Off, "*condition is always*");

      if Local_APIC_Frequency /= 0 and then TSC_Frequency /= 0 then
         APIC_Frequency_In_kHz := Local_APIC_Frequency / 1_000;
         TSC_Frequency_In_kHz  := TSC_Frequency / 1_000;
         return;
      end if;

      if Max_CPUID_Index >= TSC_Information_Leaf then
         --  Try to see if the TSC Information Leaf 15H has the information we
         --  need.

         Asm ("cpuid",
              Inputs   => Unsigned_32'Asm_Input ("a", TSC_Information_Leaf),
              Outputs  =>
                (Unsigned_64'Asm_Output ("=a", TSC_Ratio_Denominator),
                 Unsigned_64'Asm_Output ("=b", TSC_Ratio_Numerator),
                 Unsigned_64'Asm_Output ("=c", Core_Crystal_Clock_Mhz)),
              Clobber  => "rdx",
              Volatile => True);

         --  The easiest case is the processor has enumerated all the TSC ratio
         --  in CPUID Leaf 15H.

         if TSC_Ratio_Numerator /= 0 and then TSC_Ratio_Denominator /= 0 then
            --  The processor has enumerated the core crystal clock frequency.
            --  (only newer processors do this sadly).

            if Core_Crystal_Clock_Mhz /= 0 then
               APIC_Frequency_In_kHz := Core_Crystal_Clock_Mhz * 1_000;
               TSC_Frequency_In_kHz :=
                 APIC_Frequency_In_kHz * TSC_Ratio_Numerator /
                                         TSC_Ratio_Denominator;

            elsif My_CPU_Model = Denverton then
               --  The Denverton SoC does not report the crystal clock and
               --  lacks the CPUID Leaf 16H used to calculate the TSC
               --  frequency below. A hardcoded 25 MHz crystal clock is used
               --  instead as that is the clock used in these chips.

               APIC_Frequency_In_kHz := 25 * 1_000;
               TSC_Frequency_In_kHz :=
                 APIC_Frequency_In_kHz * TSC_Ratio_Numerator /
                                         TSC_Ratio_Denominator;

            elsif Max_CPUID_Index >= Processor_Frequency_Info_Leaf then
               --  When the core crystal clock frequency is not enumerated
               --  the TSC uses the base frequency reported in CPUID Leaf 16H.

               Asm ("cpuid",
                    Inputs   =>
                      Unsigned_32'Asm_Input
                        ("a", Processor_Frequency_Info_Leaf),
                    Outputs  =>
                      Unsigned_64'Asm_Output ("=a", Processor_Base_Frequency),
                    Clobber => "rbx, rcx, rdx",
                    Volatile => True);

               TSC_Frequency_In_kHz := Processor_Base_Frequency * 1_000;
               APIC_Frequency_In_kHz :=
                 TSC_Frequency_In_kHz * TSC_Ratio_Denominator /
                                        TSC_Ratio_Numerator;
            end if;
         end if;

      --  Sandy Bridge, Ivy Bridge, Haswell and Broadwell microarchitectures
      --  derive their TSC from the bus frequency. Consequently, the TSC
      --  calculated here assumes the nominal 100 MHz bus frequency defined
      --  for these microarchitectures.

      elsif My_CPU_Model in
        Sandy_Bridge_Client | Sandy_Bridge_Server | Ivy_Bridge_Client   |
        Ivy_Bridge_Server   | Haswell_Client      | Haswell_Client_L    |
        Haswell_Client_G    | Haswell_Server      | Broadwell_Client    |
        Broadwell_Client_G  | Broadwell_Server    | Broadwell_Server_D  |
        Goldmont
      then
         declare
            Bus_Frequency_kHz : constant := 100_000;
            Platform_Info : Platform_Infomation;
         begin
            Asm ("rdmsr",
                 Inputs   => Unsigned_32'Asm_Input ("a", MSR_PLATFORM_INFO),
                 Outputs  =>
                   Platform_Infomation'Asm_Output ("=a", Platform_Info),
                 Volatile => True);

            APIC_Frequency_In_kHz := Bus_Frequency_kHz;
            TSC_Frequency_In_kHz :=
              Unsigned_64 (Platform_Info.Maximum_Non_Turbo_Ratio)
              * Bus_Frequency_kHz;
         end;

      --  If we are on LynxSecure, we can to retrieve the clock frequencies
      --  from the RO Page provided to us on boot to get the most accurate
      --  values.

      elsif Host_Hardware = LynxSecure then
         declare
            type RO_Page_Type is record
               TSC_Frequency  : Unsigned_64;
               APIC_Frequency : Unsigned_64;
            end record;
            --  RO Page layout. We are only interested in the clock frequency
            --  fields of the RO Page, so we leave out everything else.

            for RO_Page_Type use record
               TSC_Frequency  at 248 range 0 .. 63;
               APIC_Frequency at 304 range 0 .. 63;
            end record;

            RO_Page : RO_Page_Type with Import, Address => Host_Info;
            --  RO Page contains information passed by LynxSecure to the
            --  subject.

            subtype Valid_APIC_Frequencies is
              Unsigned_64 range 10_000 .. 5_000_000;
            subtype Valid_TSC_Frequencies is
              Unsigned_64 range 500_000 .. 5_000_000;
            --  Valid range of clock frequencies in kHz.

         begin
            APIC_Frequency_In_kHz := RO_Page.APIC_Frequency / 1000;
            TSC_Frequency_In_kHz  := RO_Page.TSC_Frequency / 1000;

            --  Sanity check the values to make sure the RO Page data structure
            --  hasn't changed and we are reading rubbish. Zero the frequencies
            --  if that's the case.

            if APIC_Frequency_In_kHz not in Valid_APIC_Frequencies
              or else TSC_Frequency_In_kHz not in Valid_TSC_Frequencies
            then
               APIC_Frequency_In_kHz := 0;
               TSC_Frequency_In_kHz  := 0;
            end if;
         end;
      end if;

      --  If we got to this point and have not been able to determine the
      --  clock frequencies, we have to determine the TSC and Local APIC
      --  Frequency by testing them against the Programmable Interval Timer
      --  (PIT), which has a known frequency. We perform the test five times
      --  to ensure we can get consistent results.

      if TSC_Frequency_In_kHz = 0 then
         declare
            type Clock_Values is record
               APIC, TSC : Unsigned_64;
            end record;

            function Calculate_Frequency return Clock_Values;
            --  Calculate the frequency of the TSC by comparing the number of
            --  TSC ticks against a set number of PIT ticks, which has a known
            --  frequency of 1.193182 MHz.

            -------------------------
            -- Calculate_Frequency --
            -------------------------

            function Calculate_Frequency return Clock_Values is
               PIT_Frequency : constant := 1_193_182;
               --  PIT operates at 1.193182 MHz

               PIT_Ticks_To_Count : constant := 59_660;
               --  The number of PIT tickets we want to a we want to count the
               --  TSC and APIC Timer over. This number corresponds to
               --  approximately 50ms.

               PIT_Reset_Count : constant Unsigned_16_Bytable :=
                 (View => Full, Value => Unsigned_16'Last);
               --  Value to start the PIT from

               APIC_Reset_Count : constant APIC_Time := APIC_Time'Last;
               --  Value to start the APIC Timer from

               Start_PIT      : Unsigned_16_Bytable;
               Current_PIT    : Unsigned_16_Bytable;
               Target_PIT     : Unsigned_16;
               PIT_Tick_Count : Unsigned_16;
               --  PIT values used to calculate the frequencies of the TSC and
               --  APIC Timer.

               End_APIC        : APIC_Time;
               APIC_Tick_Count : Unsigned_64;
               --  Start and end value of the TSC run and the resulting number
               --  of ticks.

               Start_TSC      : Unsigned_64;
               End_TSC        : Unsigned_64;
               TSC_Tick_Count : Unsigned_64;
               --  Start and end value of the TSC run and the resulting number
               --  of ticks.
            begin
               --  Configure Local APIC Timer for the timing run by running it
               --  at half the clock bus frequency for one shot.

               Local_APIC_Timer_Divide_Configuration := Divide_by_2;
               Local_APIC_LVT_Timer_Register :=
                 (Timer_Mode => One_Shot,
                  Mask       => True,
                  Delivery   => Idle,
                  Vector     => APIC_Timer_Vector);

               --  Set PIT reset value

               Write_IO_Byte (PIT_Reset_Count.Low, PIT_Channel_0_Data_Port);
               Write_IO_Byte (PIT_Reset_Count.High, PIT_Channel_0_Data_Port);

               --  Wait until the PIT clock ticks over so we do not start the
               --  timing run in the middle of a tick.

               Start_PIT.Low  := Read_IO_Byte (PIT_Channel_0_Data_Port);
               Start_PIT.High := Read_IO_Byte (PIT_Channel_0_Data_Port);

               loop
                  Current_PIT.Low  := Read_IO_Byte (PIT_Channel_0_Data_Port);
                  Current_PIT.High := Read_IO_Byte (PIT_Channel_0_Data_Port);

                  exit when Current_PIT.Value /= Start_PIT.Value;
               end loop;

               --  Start timing run

               Start_TSC := Read_TSC;
               Local_APIC_Timer_Initial_Count := APIC_Reset_Count;

               Start_PIT.Value := Current_PIT.Value;
               Target_PIT      := Start_PIT.Value - PIT_Ticks_To_Count;

               loop
                  Current_PIT.Low  := Read_IO_Byte (PIT_Channel_0_Data_Port);
                  Current_PIT.High := Read_IO_Byte (PIT_Channel_0_Data_Port);

                  --  QEMU may jump over PIT values on successive reads, so
                  --  exit once we have reached or past the target value.

                  exit when Current_PIT.Value <= Target_PIT;
               end loop;

               End_TSC := Read_TSC;
               End_APIC := Local_APIC_Timer_Current_Count;

               --  Stop the APIC Timer so it does not create any spurious
               --  interrupts once we enable interrupts.

               Local_APIC_Timer_Initial_Count := 0;

               --  Calculate clock frequency from the results of the timing run

               APIC_Tick_Count :=
                 Unsigned_64 (APIC_Reset_Count - End_APIC) * 2;
               PIT_Tick_Count := Start_PIT.Value - Current_PIT.Value;
               TSC_Tick_Count := End_TSC - Start_TSC;

               --  We can do the multiplication before the divison below since
               --  the maximum number of TSC ticks in a second is below the
               --  value that would cause Unsigned_64 to overflow (modern CPUs
               --  currently don't go above 5Ghz).
               return
                 (APIC => APIC_Tick_Count * PIT_Frequency /
                            Unsigned_64 (PIT_Tick_Count),
                  TSC  => TSC_Tick_Count * PIT_Frequency /
                            Unsigned_64 (PIT_Tick_Count));
            end Calculate_Frequency;

            PIT_Config : constant PIT_Mode_Command_Register :=
              (Channel         => Channel_0,
               Access_Mode     => Low_High_Byte,
               Operating_Mode  => Interrupt_On_Terminal_Count,
               BCD_Binary_Mode => Binary);
            --  PIT configuration settings

            type Clock_Runs is range 1 .. 5;
            --  Determine the clock frequencies over 5 runs to account for
            --  variations hardware and virtualized solutions.

            Frequency_Results : array (Clock_Runs) of Clock_Values;
            --  Array for the results of each clock determining run

            Median_Index : constant := (Frequency_Results'Length + 1) / 2;
            --  Helper constant to locate the median result in the array

            Acceptable_Clock_Difference : constant := 20;
            --  The maximum percentage difference between clock runs
            --  that we find acceptable.

            Max_Frequency_Error : Integer_64;
            --  The maximum difference between two clock frequency measurement
            --  we will tolerate.

            TSC_Frequency : Unsigned_64;
            --  Calculated TSC frequency

         begin
            --  Configure PIT

            Write_IO_Byte (To_IO_Byte (PIT_Config), PIT_Mode_Command_Port);

            --  Find the TSC and APIC frequency over the number of specified
            --  runs.

            for Run_Number in Frequency_Results'Range loop
               Frequency_Results (Run_Number) := Calculate_Frequency;
            end loop;

            --  Pick the TSC median result. Do this by first sorting the
            --  results.

            for J in Frequency_Results'First .. Frequency_Results'Last - 1 loop
               for K in J + 1 .. Frequency_Results'Last loop
                  declare
                     Swap_Temp : Clock_Values;
                  begin
                     if Frequency_Results (K).TSC < Frequency_Results (J).TSC
                     then
                        Swap_Temp := Frequency_Results (J);
                        Frequency_Results (J) := Frequency_Results (K);
                        Frequency_Results (K) := Swap_Temp;
                     end if;
                  end;
               end loop;
            end loop;

            TSC_Frequency := Frequency_Results (Median_Index).TSC;

            Max_Frequency_Error :=
              Integer_64 (TSC_Frequency / Acceptable_Clock_Difference);

            if abs
              (Integer_64 (TSC_Frequency) -
               Integer_64 (Frequency_Results (Median_Index - 1).TSC)) >
              Max_Frequency_Error
              or else
                abs
                  (Integer_64 (TSC_Frequency) -
                   Integer_64 (Frequency_Results (Median_Index + 1).TSC)) >
                Max_Frequency_Error
            then
               raise Program_Error with "Clock measurements lack precision";
            end if;

            APIC_Frequency_In_kHz :=
              Frequency_Results (Median_Index).APIC / 1_000;
            TSC_Frequency_In_kHz := TSC_Frequency / 1_000;
         end;
      end if;

      --  If the TSC Frequency is stil zero then we were not able to determine
      --  what the clock frequencies are.

      if TSC_Frequency_In_kHz = 0 then
         raise Program_Error with "Clock frequencies could not be determined";
      end if;

      pragma Warnings (On, "*condition is always*");
   end Determine_Clock_Frequencies;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : Integer) is
   begin
      if Level /= System.Interrupt_Priority'Last then
         Board_Support.Interrupts.Set_Current_Priority (Level);

         --  Really enable interrupts

         Asm ("sti", Volatile => True);
      end if;
   end Enable_Interrupts;

   -----------------------
   -- Exception_Handler --
   -----------------------

   --  Important note: this function is a naked assembly function and care
   --  should be taken when Ada code is used to ensure the registers and
   --  stack are not improperly manipulated.

   procedure Exception_Handler is
      Vector : LAPIC_Vector;
      Code   : Error_Code;
   begin
      --  The processor and the vector stub has pushed the following contents
      --  onto the interrupt stack:
      --
      --   SS
      --   RSP
      --   RFLAGS
      --   CS
      --   RIP
      --   Exception Code
      --   Exception Vector  <-- RSP
      --
      --  Save the rest of the integer caller-save registers.

      Asm
        ("pushq  %%rax"                                                  & NL &
         "pushq  %%rcx"                                                  & NL &
         "pushq  %%rdx"                                                  & NL &
         "pushq  %%rdi"                                                  & NL &
         "pushq  %%rsi"                                                  & NL &
         "pushq  %%r8"                                                   & NL &
         "pushq  %%r9"                                                   & NL &
         "pushq  %%r10"                                                  & NL &
         "pushq  %%r11",
         Volatile => True);

      --  Retrieve the exception vector and code off the stack

      Asm
        ("movq  80(%%rsp), %0"                                           & NL &
         "movl  72(%%rsp), %1",
         Outputs  =>
           (Error_Code'Asm_Output ("=r", Code),
            LAPIC_Vector'Asm_Output ("=r", Vector)),
         Volatile => True);

      --  We don't need to save the vector and floating point states as the
      --  exception handlers should not touch these registers.

      --  At this point it is safe to use limited Ada code since we've saved
      --  the caller-saved registers. Just note that there's no stack frame
      --  allocated for this procedure, so local stack objects should not be
      --  used. Instead we jump to proper Ada procedures. Before doing so, we
      --  need to push a NULL return address onto the stack so the unwinder
      --  knows to stop here in this function.

      Asm
        ("movq   $0, %%rax"                                              & NL &
         "pushq  %%rax",
         Clobber  => "rax, memory",
         Volatile => True);

      --  Handle the exception in proper Ada code

      Process_Exception (Vector, Code);

      --  Exception has been successfully handled. Time to clean up and exit.
      --  Since processing an exception will not modify the task queues no
      --  need to check if we need to context switch to another task.

      --  Ada code is no longer allowed at this point. Remove the NULL return
      --  address from the stack.

      Asm ("addq  $8, %%rsp", Clobber  => "memory", Volatile => True);

      Asm
        ("popq  %%r11"                                                   & NL &
         "popq  %%r10"                                                   & NL &
         "popq  %%r9"                                                    & NL &
         "popq  %%r8"                                                    & NL &
         "popq  %%rsi"                                                   & NL &
         "popq  %%rdi"                                                   & NL &
         "popq  %%rdx"                                                   & NL &
         "popq  %%rcx"                                                   & NL &
         "popq  %%rax",
         Volatile => True);

      --  Remove the Exception Number and Code from the stack

      Asm
        ("addq  $16, %%rsp",
         Volatile => True);

      --  Return to task

      Asm ("iretq", Volatile => True);
   end Exception_Handler;

   -------------
   -- Get_CR4 --
   -------------

   function Get_CR4 return Control_Register_4 is
      CR4 : Control_Register_4;
   begin
      Asm
        ("movq %%cr4, %0",
         Outputs  => Control_Register_4'Asm_Output ("=r", CR4),
         Volatile => True);
      return CR4;
   end Get_CR4;

   ----------------------
   -- Initialize_Stack --
   ----------------------

   procedure Initialize_Stack
     (Base          : Address;
      Size          : Storage_Elements.Storage_Offset;
      Stack_Pointer : out Address)
   is
      use type SSE.Storage_Offset;
   begin
      Stack_Pointer := Base + Size;

      --  Round the stack pointer down to the nearest stack alignment

      Stack_Pointer :=
        Stack_Pointer -
          (Stack_Pointer
             mod SSE.Storage_Offset (CPU_Specific.Stack_Alignment));
   end Initialize_Stack;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
      Initial_SP : Address;
   begin
      --  No need to initialize the context of the environment task

      if Program_Counter = Null_Address then
         return;
      end if;

      --  Call Initialize_Stack with a zero size to correctly align the stack
      --  pointer that was passed.

      Initialize_Stack (Stack_Pointer, 0, Initial_SP);

      --  Setup the initial context for the task

      --  Registers R12 and R13 are used to pass the start procedure and
      --  its parameter to the task start wrapper on the first context switch
      --  to the task.

      Buffer.all :=
        (RIP    => Thread_Start'Address,
         RFLAGS =>
           (Interrupt_Enable_Flag => True,
            IO_Privilege_Level    => 0,
            Carry_Flag            => False,
            others                => False),
         RSP    => Initial_SP,
         RBX    => Null_Address,
         RBP    => Null_Address,
         R12    => Argument,
         R13    => Program_Counter,
         R14    => Null_Address,
         R15    => Null_Address);
   end Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      Min_Required_CPUID_Index : constant := 16#0D#;
      --  The minimum Intel processor architecture we support is Sandy Bridge
      --  which has a max CPUID Index of DH. This leaf corresponds to the
      --  TSC Leaf which we use

   begin
      --  Verify CPU by checking the processors' vendor string. At this point
      --  we only support Intel. AMD support will come later once we verify it
      --  supports what we need.

      declare
         EAX : constant := 0;
         --  CPUID Leaf 0H, Vendor ID string

         subtype CPU_ID_String is String (1 .. 4);
         EBX, EDX, ECX : CPU_ID_String;
      begin
         Asm ("cpuid",
              Inputs  => Unsigned_32'Asm_Input ("a", EAX),
              Outputs =>
                (Unsigned_32'Asm_Output ("=a", Max_CPUID_Index),
                 CPU_ID_String'Asm_Output ("=b", EBX),
                 CPU_ID_String'Asm_Output ("=c", ECX),
                 CPU_ID_String'Asm_Output ("=d", EDX)),
              Volatile => True);

         --  Validate that we are on a supported processor

         if EBX = "Genu" and then EDX = "ineI" and then ECX = "ntel" then
            if Max_CPUID_Index < Min_Required_CPUID_Index then
               raise Program_Error with "Unsupported Intel processor";
            end if;
         else
            raise Program_Error with "Runtime only supports Intel processors";
         end if;
      end;

      --  Check processor supported features

      declare
         --  CPUID Leaf 01H, Feature Information

         Feature_Info_Leaf : constant := 16#01#;
         Features_ECX      : Feature_Information_ECX;
         Features_EDX      : Feature_Information_EDX;

         --  CPUID Leaf 0DH, Processor Extended State Enumeration

         Extended_State_Leaf  : constant := 16#0D#;
         Extended_State_1_EAX : Extended_State_Subleaf_1_EAX;

         --  CPUID Subleaf

         Sub_Leaf_1 : constant := 1;

      begin
         --  Check for XSAVE processor feature, builtin APIC and TSC

         Asm ("cpuid",
              Inputs   => Unsigned_32'Asm_Input ("a", Feature_Info_Leaf),
              Outputs  =>
                (Feature_Information_ECX'Asm_Output ("=c", Features_ECX),
                 Feature_Information_EDX'Asm_Output ("=d", Features_EDX)),
              Clobber  => "rbx",
              Volatile => True);

         if not Features_ECX.XSAVE then
            raise Program_Error
              with "Runtime requires XSAVE processor feature";
         end if;

         if not Features_EDX.APIC then
            raise Program_Error with "Processor is missing builtin APIC!";
         end if;

         if not Features_EDX.TSC then
            raise Program_Error
              with "Runtime requires Time Stamp Counter";
         end if;

         --  Check for XSAVEOPT instruction. This enables us to not store the
         --  floating-pointer and vector state on each interrupt handler if
         --  if the state has not changed.

         Asm ("cpuid",
              Inputs   =>
                (Unsigned_32'Asm_Input ("a", Extended_State_Leaf),
                 Unsigned_32'Asm_Input ("c", Sub_Leaf_1)),
              Outputs  =>
                Extended_State_Subleaf_1_EAX'Asm_Output
                   ("=a", Extended_State_1_EAX),
              Clobber  => "rbx, rdx",
              Volatile => True);

         if not Extended_State_1_EAX.XSAVEOPT then
            raise Program_Error
              with "Runtime requires XSAVEOPT instruction";
         end if;
      end;

      --  Setup Control Registers to enable SSE and XSAVE (the latter enabling
      --  AVX support).

      declare
         CR4 : Control_Register_4 := Get_CR4;
      begin
         CR4.FXSAVE_FXRSTOR_And_SSE_Enable := True;
         CR4.XSAVE_and_Processor_Extended_States_Enable := True;
         Set_CR4 (CR4);
      end;

      --  Set Extended Control Register XCR0 so that XSAVE can store x87, SSE
      --  AVX and AVX-512 registers. See Section 13.3, Vol 1. Note that
      --  XCR0 is set via EDX:EAX rather than a RAX, so we need to split our
      --  state into a high and low register. %rcx selects XCR0. To ensure we
      --  only set only valid bits we need to AND BB_X86_Context_State with the
      --  support state given in CPUID leaf 0DH, sub-leaf 0.

      declare
         --  CPUID Leaf 0DH, Processor Extended State Enumeration

         Extended_State_Leaf : constant := 16#0D#;
         Sub_Leaf_0          : constant := 0;
      begin
         Asm
           ("cpuid"                                                      & NL &
            --  rax:rdx have the supported state mask. Move our mask into
            --  %2:rcx.
            "movq  %2,    %%rcx"                                         & NL &
            "shrq  $32,   %%rcx"                                         & NL &
            "andq  %2,    %%rax"                                         & NL &
            "andq  %%rcx, %%rdx"                                         & NL &
            "movq  $0,    %%rcx"                                         & NL &
            "xsetbv",
            Inputs   =>
              (Unsigned_32'Asm_Input ("a", Extended_State_Leaf),
               Unsigned_32'Asm_Input ("c", Sub_Leaf_0),
               State_Component_Bit_Map'Asm_Input ("r", BB_X86_Context_State)),
            Clobber  => "rbx, rdx",
            Volatile => True);
      end;

      --  Initialize x87 FPU to defaults, masking floating point exceptions

      Asm ("fninit", Volatile => True);

      --  Initialize SSE MXCSR with default 0x1F80
      --  Note: check if we need it to raise divide by zero exceptions.

      Asm ("ldmxcsr %0",
           Inputs   => Unsigned_32'Asm_Input ("m", Runtime_SSE_MXCSR),
           Volatile => True);

      --  Finalize setup of GDT by setting each CPUs TSS record in the
      --  corresponding GDT TSS entry.

      declare
         type TSS_Address is record
            Low  : Base_Address_Low_Part;
            High : Base_Address_High_Long_Part;
         end record;

         for TSS_Address use record
            Low  at 0 range 0 .. 23;
            High at 3 range 0 .. 39;
         end record;

         function To_TSS_Address is new
           Ada.Unchecked_Conversion (System.Address, TSS_Address);
      begin
         for J in GDT.CPU_TSS'Range loop
            GDT.CPU_TSS (J).Base_Address_Low :=
              To_TSS_Address (CPU_Task_State (J)'Address).Low;
            GDT.CPU_TSS (J).Base_Address_High :=
              To_TSS_Address (CPU_Task_State (J)'Address).High;
         end loop;
      end;

      --  Setup CPU specific information

      declare
         use System.Multiprocessors;

         Interrupt_Stacks : array (CPU) of System.Address;
         pragma Import (Asm, Interrupt_Stacks, "interrupt_stack_table");
         --  The Interrupt_Stack_Table defined in the body of
         --  System.BB.Interrupts provides the initial interrupt stack pointer
         --  for each CPU. Unlike other targets, on x86-64 we relocate the
         --  processor's interrupt stack pointer to the x86-64 processor
         --  specific CPU_Specific_Information record because the x86-64
         --  processor will switch stacks for us.

         Exception_Stack_Pointer : System.Address;
         --  Like the interrupt stack pointer above, but for exceptions.

         My_ID : constant CPU := CPU_Range (Local_APIC_ID_Register.ID) + 1;
         --  Local APIC IDs start from 0 while type CPU starts from 1

         My_CPU_Task_State : Task_State_Segment renames CPU_Task_State (My_ID);

         GDT_Location : constant Descriptor_Pointer :=
            (Base_Address => GDT'Address, Limit => GDT'Size / 8 - 1);
         IDT_Location : constant Descriptor_Pointer :=
            (Base_Address => IDT'Address, Limit => IDT'Size / 8 - 1);
         --  Locations of the GDT and IDT tables in the format required by
         --  their respective load instructions.

      begin
         --  Initialize exception stack pointer. The interrupt stack pointer
         --  was initialized in BB.Interrupts.

         Initialize_Stack
           (Exception_Stacks (My_ID)'Address,
            Exception_Stack_Space'Size,
            Exception_Stack_Pointer);

         My_CPU_Task_State :=
           (CPU_ID => My_ID,
            IST1   => Interrupt_Stacks (My_ID),
            IST7   => Exception_Stack_Pointer,
            others => <>);

         --  Find the size of the XSAVE state. We capture this information in
         --  the task's CPU_Task_State record because cpuid can be very
         --  expensive in virtualised environments.

         declare
            XSAVE_Leaf   : constant := 16#0D#;
            Sub_Function : constant := 0;
         begin
            Asm
              ("cpuid",
               Inputs   =>
                 (Unsigned_32'Asm_Input ("a", XSAVE_Leaf),
                  Unsigned_32'Asm_Input ("c", Sub_Function)),
               Outputs  =>
                 Unsigned_64'Asm_Output ("=b", My_CPU_Task_State.XSAVE_Size),
               Clobber  => "rdx",
               Volatile => True);
         end;

         --  Write address of My_CPU_Task_State into %gs via the IA32_GS_BASE
         --  MSR

         Asm
           ("movq    %%rax, %%rdx"                                       & NL &
            "shrq    $32,   %%rdx"                                       & NL &
            "wrmsr",
             Inputs   =>
               (System.Address'Asm_Input ("a", My_CPU_Task_State'Address),
                Unsigned_32'Asm_Input ("c", IA32_GS_BASE)),
             Volatile => True);

         --  Switch GDT to our tasking version

         Asm
           ("lgdt %0",
            Inputs => Descriptor_Pointer'Asm_Input ("m", GDT_Location),
            Volatile => True);

         --  Switch to our new code and data segments

         Asm
           ("pushq   %0"                                                 & NL &
            "pushq   $1f"                                                & NL &
            "lretq"                                                      & NL &
         "1: movl    %1,   %%eax"                                        & NL &
            "movw    %%ax, %%ds"                                         & NL &
            "movw    %%ax, %%es"                                         & NL &
            "movw    %%ax, %%ss"                                         & NL &
            "movw    %%ax, %%fs",
            Inputs   =>
                (Unsigned_16'Asm_Input ("i", GDT.Code'Position),
                 Unsigned_16'Asm_Input ("i", GDT.Data'Position)),
            Clobber  => "rax",
            Volatile => True);

         --  Load the Task Register with the CPU's TSS. For the first CPU it's
         --  the first component of the CPU_TSS array.

         Asm
           ("ltr %0",
            Inputs   => Unsigned_16'Asm_Input ("r", GDT.CPU_TSS'Position),
            Volatile => True);

         --  Setup and load the IDT

         Setup_Interrupt_Descriptor_Table;

         Asm
           ("lidt %0",
            Inputs   => Descriptor_Pointer'Asm_Input ("m", IDT_Location),
            Volatile => True);
      end;

      --  Mask Intel 8259A PIC if it happens to be present on our x86-64
      --  machine.

      Write_IO_Byte (Data => 16#FF#, Port => 16#A1#);
      Write_IO_Byte (Data => 16#FF#, Port => 16#21#);

      --  Initialize Local APIC

      --  Set the Spurious Interrupt Register, which is required to allow the
      --  CPU to receive interrupts from the APIC.

      Local_APIC_Spurious_Interrupt_Register :=
        (Spurious_Vector           => Spurious_Interrupt_Vector,
         APIC_Enabled              => True,
         Focus_Processor_Checking  => False,
         Suppress_EOI_Broadcast    => False);

      --  By default all interrupt entries in the Local Vector Table are
      --  masked. Let's assume that is the case for now. In the future it may
      --  be useful to enable the Error, LINT0 and LINT1 vectors (for the
      --  latter need to decide whether to hard code or read the ACPI table).

      --  Determine system clock frequencies

      Determine_Clock_Frequencies;

      --  Setup Local APIC Timer

      --  Disable warnings for Local_APIC_Timer_Divide_Configuration

      pragma Warnings (Off, "condition is always*");

      Local_APIC_Timer_Divide_Configuration :=
        (case APIC_Timer_Divider is
            when 1  => Divide_by_1,  when 2   => Divide_by_2,
            when 4  => Divide_by_4,  when 8   => Divide_by_8,
            when 16 => Divide_by_16, when 32  => Divide_by_32,
            when 64 => Divide_by_64, when 128 => Divide_by_128,
            when others => raise Program_Error with
              "Invalid Local APIC Timer Divider value");

      Local_APIC_LVT_Timer_Register :=
        (Timer_Mode => One_Shot,
         Mask       => False,
         Delivery   => Idle,
         Vector     => APIC_Timer_Vector);

      --  Update APIC_Frequency_In_kHz to reflect the chosen divider

      APIC_Frequency_In_kHz := APIC_Frequency_In_kHz / APIC_Timer_Divider;

      --  Clear any pending interrupts that may have occurred before or during
      --  the setup.

      Local_APIC_End_of_Interrupt := Signal;
   end Initialize_CPU;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   --  Important note: this function is a naked assembly function and care
   --  should be taken when Ada code is used to ensure the registers and
   --  stack are not improperly manipulated.

   procedure Interrupt_Handler is
      TSS : Task_State_Segment with Import, Address => System'To_Address (0);
      --  We access the CPU's TSS directly via the %gs segment register,
      --  meaning for our purpose the TSS is at address Zero. Keep this in mind
      --  and do not read or write to TSS directly, only use it to pass TSS
      --  components to assembly instructions. Note for this to work properly
      --  this file needs to be compiled at -O1 or -O2.

      Vector : LAPIC_Vector;
   begin
      --  On entry we are on the processor's interrupt stack. Create a new
      --  frame on the stack that we will use as the stack for handling this
      --  interrupt. We need to work with interrupt stack frames on x86-64
      --  because the interrupt stack pointer the processor loads when an
      --  interrupt occurs is located in the CPU's TSS, which is only updated
      --  when we do the update (i.e. the changes to %rsp are not reflected
      --  in the CPU's TSS until we push the change).

      Asm
        ("subq %0, %%gs:%1",
         Inputs   =>
           (Unsigned_32'Asm_Input ("i", Parameters.Interrupt_Stack_Frame_Size),
            System.Address'Asm_Input ("m", TSS.IST1)),
         Clobber  => "memory",
         Volatile => True);

      --  The processor has pushed the following registers onto the interrupt
      --  stack:
      --
      --   SS
      --   RSP
      --   RFLAGS
      --   CS
      --   RIP
      --
      --  With the vector stub pushing the following afterwards:
      --
      --  IRQ Number  <-- RSP
      --
      --  Save the rest of the integer caller-save registers.

      Asm
        ("pushq  %%rax"                                                  & NL &
         "pushq  %%rcx"                                                  & NL &
         "pushq  %%rdx"                                                  & NL &
         "pushq  %%rdi"                                                  & NL &
         "pushq  %%rsi"                                                  & NL &
         "pushq  %%r8"                                                   & NL &
         "pushq  %%r9"                                                   & NL &
         "pushq  %%r10"                                                  & NL &
         "pushq  %%r11",
         Volatile => True);

      --  Retrieve the IRQ number off the stack

      Asm
        ("movl  72(%%rsp) , %0",
         Outputs  => LAPIC_Vector'Asm_Output ("=r", Vector),
         Clobber  => "memory",
         Volatile => True);

      --  The XSAVE feature is used to capture the state of the floating point
      --  and vector registers. XSAVE is used over FXSAVE since it can store
      --  the AVX registers and has the ability to only save the state if it
      --  has been modified. Downside, it requires 64-byte alignment while the
      --  processor only aligns the interrupt stack on 16-bytes. Consequently,
      --  create an alignment adjusted frame for the XSAVE state and store the
      --  current stack pointer just below it so we know where we restore the
      --  stack to.

      --  Create the space for the XSAVE state on the stack, aligned to 64-byte
      --  boundary. We do this in a temporary register as we want to push the
      --  current value of the stack after the XSAVE state so we know where to
      --  restore the stack pointer to after the XSAVE space has been restored
      --  (since we do not know how large the XSAVE space is once adjusted for
      --  alignment).

      Asm
        ("movq  %%rsp,   %%rsi"                                          & NL &
         "subq  %%gs:%0, %%rsi"                                          & NL &
         "and   $-64,    %%rsi"                                          & NL &

      --  Swap in the new stack pointer and save the old stack pointer.

         "movq  %%rsp, %%rax"                                            & NL &
         "movq  %%rsi, %%rsp"                                            & NL &
         "pushq %%rax",
         Inputs   => Unsigned_64'Asm_Input ("m", TSS.XSAVE_Size),
         Clobber  => "rax, rsi, memory",
         Volatile => True);

      --  Load the XSAVE state mask requesting the states we want to save into
      --  %edx:%eax. Then XSAVE! We use XSAVEOPT to do this as we only want to
      --  save and restore states that are used and have changed.

      Asm
        ("movq      %0,    %%rax"                                        & NL &
         "movq      %%rax, %%rdx"                                        & NL &
         "shrq      $32,   %%rdx"                                        & NL &
         "xsaveopt  8(%%rsp)",
         Inputs   =>
           State_Component_Bit_Map'Asm_Input ("m", BB_X86_Context_State),
         Clobber  => "rdx, memory",
         Volatile => True);

      --  At this point it is safe to use limited Ada code since we've saved
      --  the caller-saved registers. Just note that there's no stack frame
      --  allocated for this procedure, so local stack objects should not be
      --  used. Instead we jump to proper Ada procedures. Before doing so, we
      --  need to push a NULL return address onto the stack so the unwinder
      --  knows to stop here in this function.

      Asm
        ("movq   $0, %%rax"                                              & NL &
         "pushq  %%rax",
         Clobber  => "rax, memory",
         Volatile => True);

      --  Handle the interrupt at the runtime level

      Interrupt_Wrapper (Vector);

      --  Signal to the Local APIC that the interrupt is finished

      Local_APIC_End_of_Interrupt := Signal;

      --  Interrupt has been handled. Time to clean up and exit, noting that
      --  we may have to switch to another task if the interrupt event has
      --  caused a scheduling change.

      --  Do a context switch if we are required to.

      if System.BB.Threads.Queues.Context_Switch_Needed then
         Context_Switch;
      end if;

      --  Ada code is no longer allowed at this point. Remove the NULL return
      --  address from the stack.

      Asm ("addq  $8, %%rsp", Clobber  => "memory", Volatile => True);

      --  Time to restore the state of the interrupted task and return to it

      --  Restore XSAVE state, noting the current value of RSP points to the
      --  address of the stack before the XSAVE state space was allocated. The
      --  XSAVE state is then stored at an offset of 8 bytes from RSP.

      Asm
        ("movq      %0,    %%rax"                                        & NL &
         "movq      %%rax, %%rdx"                                        & NL &
         "shrq      $32,   %%rdx"                                        & NL &
         "xrstor    8(%%rsp)",
         Inputs   =>
           State_Component_Bit_Map'Asm_Input ("m", BB_X86_Context_State),
         Clobber  => "rdx, memory",
         Volatile => True);

      --  Remove the XSAVE state from the stack

      Asm ("popq  %%rsp", Clobber => "memory", Volatile => True);

      --  Restore the caller-saved registers we saved

      Asm
        ("popq  %%r11"                                                   & NL &
         "popq  %%r10"                                                   & NL &
         "popq  %%r9"                                                    & NL &
         "popq  %%r8"                                                    & NL &
         "popq  %%rsi"                                                   & NL &
         "popq  %%rdi"                                                   & NL &
         "popq  %%rdx"                                                   & NL &
         "popq  %%rcx"                                                   & NL &
         "popq  %%rax",
         Clobber  => "memory",
         Volatile => True);

      --  Remove the IRQ number from the stack and release the interrupt stack
      --  frame.

      Asm
        ("addq  $8, %%rsp"                                               & NL &
         "addq  %0, %%gs:%1",
         Inputs   =>
           (Unsigned_32'Asm_Input ("i", Parameters.Interrupt_Stack_Frame_Size),
            System.Address'Asm_Input ("m", TSS.IST1)),
         Clobber  => "memory",
         Volatile => True);

      --  Return to interrupted task

      Asm ("iretq", Volatile => True);
   end Interrupt_Handler;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
   begin
      null;
   end Install_Error_Handlers;

   --------------------------------------
   -- Setup_Interrupt_Descriptor_Table --
   --------------------------------------

   procedure Setup_Interrupt_Descriptor_Table is

      procedure Add_Gate
        (Vector    : LAPIC_Vector;
         Handler   : System.Address;
         Gate_Type : System_Segment_Type;
         Stack     : Interrupt_Stack_Table_Selector);
      --  Add a new gate entry to the Interrupt Descriptor Table

      --------------
      -- Add_Gate --
      --------------

      procedure Add_Gate
        (Vector    : LAPIC_Vector;
         Handler   : System.Address;
         Gate_Type : System_Segment_Type;
         Stack     : Interrupt_Stack_Table_Selector)
      is
         type Gate_Address is record
            Low  : Vector_Address_Low_Part;
            High : Vector_Address_High_Part;
         end record;
         --  Type used to split the vector handler address into the low and
         --  high components used by the gate descriptor.

         for Gate_Address use record
            Low  at 0 range 0 .. 15;
            High at 0 range 16 .. 63;
         end record;

         function To_Gate_Address is new
           Ada.Unchecked_Conversion (System.Address, Gate_Address);

         Handler_Address : constant Gate_Address := To_Gate_Address (Handler);
         --  Handler address in form that we can extract the fields required
         --  for the gate entry.

      begin
         IDT (Vector) :=
           (Vector_Address_Low    => Handler_Address.Low,
            Segment_Selector      => GDT.Code'Position,
            Interrupt_Stack_Table => Stack,
            Segment_Type          => Gate_Type,
            Privilege_Level       => 0,
            Segment_Present       => True,
            Vector_Address_High   => Handler_Address.High);
      end Add_Gate;

      Interrupt_Stack : constant := 1;
      Exception_Stack : constant := 7;
      --  IST stack choices. We use sepeate stacks for interrupts and
      --  exceptions since we always want stack space available in case an
      --  exception occurs during an interrupt handler.

      --  Interrupt and exception vector handlers

      procedure Vector_0   with Import, External_Name => "vector_0";
      procedure Vector_1   with Import, External_Name => "vector_1";
      procedure Vector_2   with Import, External_Name => "vector_2";
      procedure Vector_3   with Import, External_Name => "vector_3";
      procedure Vector_4   with Import, External_Name => "vector_4";
      procedure Vector_5   with Import, External_Name => "vector_5";
      procedure Vector_6   with Import, External_Name => "vector_6";
      procedure Vector_7   with Import, External_Name => "vector_7";
      procedure Vector_8   with Import, External_Name => "vector_8";
      procedure Vector_9   with Import, External_Name => "vector_9";
      procedure Vector_10  with Import, External_Name => "vector_10";
      procedure Vector_11  with Import, External_Name => "vector_11";
      procedure Vector_12  with Import, External_Name => "vector_12";
      procedure Vector_13  with Import, External_Name => "vector_13";
      procedure Vector_14  with Import, External_Name => "vector_14";
      procedure Vector_15  with Import, External_Name => "vector_15";
      procedure Vector_16  with Import, External_Name => "vector_16";
      procedure Vector_17  with Import, External_Name => "vector_17";
      procedure Vector_18  with Import, External_Name => "vector_18";
      procedure Vector_19  with Import, External_Name => "vector_19";
      procedure Vector_20  with Import, External_Name => "vector_20";
      procedure Vector_21  with Import, External_Name => "vector_21";
      procedure Vector_22  with Import, External_Name => "vector_22";
      procedure Vector_23  with Import, External_Name => "vector_23";
      procedure Vector_24  with Import, External_Name => "vector_24";
      procedure Vector_25  with Import, External_Name => "vector_25";
      procedure Vector_26  with Import, External_Name => "vector_26";
      procedure Vector_27  with Import, External_Name => "vector_27";
      procedure Vector_28  with Import, External_Name => "vector_28";
      procedure Vector_29  with Import, External_Name => "vector_29";
      procedure Vector_30  with Import, External_Name => "vector_30";
      procedure Vector_31  with Import, External_Name => "vector_31";
      procedure Vector_32  with Import, External_Name => "vector_32";
      procedure Vector_33  with Import, External_Name => "vector_33";
      procedure Vector_34  with Import, External_Name => "vector_34";
      procedure Vector_35  with Import, External_Name => "vector_35";
      procedure Vector_36  with Import, External_Name => "vector_36";
      procedure Vector_37  with Import, External_Name => "vector_37";
      procedure Vector_38  with Import, External_Name => "vector_38";
      procedure Vector_39  with Import, External_Name => "vector_39";
      procedure Vector_40  with Import, External_Name => "vector_40";
      procedure Vector_41  with Import, External_Name => "vector_41";
      procedure Vector_42  with Import, External_Name => "vector_42";
      procedure Vector_43  with Import, External_Name => "vector_43";
      procedure Vector_44  with Import, External_Name => "vector_44";
      procedure Vector_45  with Import, External_Name => "vector_45";
      procedure Vector_46  with Import, External_Name => "vector_46";
      procedure Vector_47  with Import, External_Name => "vector_47";
      procedure Vector_48  with Import, External_Name => "vector_48";
      procedure Vector_49  with Import, External_Name => "vector_49";
      procedure Vector_50  with Import, External_Name => "vector_50";
      procedure Vector_51  with Import, External_Name => "vector_51";
      procedure Vector_52  with Import, External_Name => "vector_52";
      procedure Vector_53  with Import, External_Name => "vector_53";
      procedure Vector_54  with Import, External_Name => "vector_54";
      procedure Vector_55  with Import, External_Name => "vector_55";
      procedure Vector_56  with Import, External_Name => "vector_56";
      procedure Vector_57  with Import, External_Name => "vector_57";
      procedure Vector_58  with Import, External_Name => "vector_58";
      procedure Vector_59  with Import, External_Name => "vector_59";
      procedure Vector_60  with Import, External_Name => "vector_60";
      procedure Vector_61  with Import, External_Name => "vector_61";
      procedure Vector_62  with Import, External_Name => "vector_62";
      procedure Vector_63  with Import, External_Name => "vector_63";
      procedure Vector_64  with Import, External_Name => "vector_64";
      procedure Vector_65  with Import, External_Name => "vector_65";
      procedure Vector_66  with Import, External_Name => "vector_66";
      procedure Vector_67  with Import, External_Name => "vector_67";
      procedure Vector_68  with Import, External_Name => "vector_68";
      procedure Vector_69  with Import, External_Name => "vector_69";
      procedure Vector_70  with Import, External_Name => "vector_70";
      procedure Vector_71  with Import, External_Name => "vector_71";
      procedure Vector_72  with Import, External_Name => "vector_72";
      procedure Vector_73  with Import, External_Name => "vector_73";
      procedure Vector_74  with Import, External_Name => "vector_74";
      procedure Vector_75  with Import, External_Name => "vector_75";
      procedure Vector_76  with Import, External_Name => "vector_76";
      procedure Vector_77  with Import, External_Name => "vector_77";
      procedure Vector_78  with Import, External_Name => "vector_78";
      procedure Vector_79  with Import, External_Name => "vector_79";
      procedure Vector_80  with Import, External_Name => "vector_80";
      procedure Vector_81  with Import, External_Name => "vector_81";
      procedure Vector_82  with Import, External_Name => "vector_82";
      procedure Vector_83  with Import, External_Name => "vector_83";
      procedure Vector_84  with Import, External_Name => "vector_84";
      procedure Vector_85  with Import, External_Name => "vector_85";
      procedure Vector_86  with Import, External_Name => "vector_86";
      procedure Vector_87  with Import, External_Name => "vector_87";
      procedure Vector_88  with Import, External_Name => "vector_88";
      procedure Vector_89  with Import, External_Name => "vector_89";
      procedure Vector_90  with Import, External_Name => "vector_90";
      procedure Vector_91  with Import, External_Name => "vector_91";
      procedure Vector_92  with Import, External_Name => "vector_92";
      procedure Vector_93  with Import, External_Name => "vector_93";
      procedure Vector_94  with Import, External_Name => "vector_94";
      procedure Vector_95  with Import, External_Name => "vector_95";
      procedure Vector_96  with Import, External_Name => "vector_96";
      procedure Vector_97  with Import, External_Name => "vector_97";
      procedure Vector_98  with Import, External_Name => "vector_98";
      procedure Vector_99  with Import, External_Name => "vector_99";
      procedure Vector_100 with Import, External_Name => "vector_100";
      procedure Vector_101 with Import, External_Name => "vector_101";
      procedure Vector_102 with Import, External_Name => "vector_102";
      procedure Vector_103 with Import, External_Name => "vector_103";
      procedure Vector_104 with Import, External_Name => "vector_104";
      procedure Vector_105 with Import, External_Name => "vector_105";
      procedure Vector_106 with Import, External_Name => "vector_106";
      procedure Vector_107 with Import, External_Name => "vector_107";
      procedure Vector_108 with Import, External_Name => "vector_108";
      procedure Vector_109 with Import, External_Name => "vector_109";
      procedure Vector_110 with Import, External_Name => "vector_110";
      procedure Vector_111 with Import, External_Name => "vector_111";
      procedure Vector_112 with Import, External_Name => "vector_112";
      procedure Vector_113 with Import, External_Name => "vector_113";
      procedure Vector_114 with Import, External_Name => "vector_114";
      procedure Vector_115 with Import, External_Name => "vector_115";
      procedure Vector_116 with Import, External_Name => "vector_116";
      procedure Vector_117 with Import, External_Name => "vector_117";
      procedure Vector_118 with Import, External_Name => "vector_118";
      procedure Vector_119 with Import, External_Name => "vector_119";
      procedure Vector_120 with Import, External_Name => "vector_120";
      procedure Vector_121 with Import, External_Name => "vector_121";
      procedure Vector_122 with Import, External_Name => "vector_122";
      procedure Vector_123 with Import, External_Name => "vector_123";
      procedure Vector_124 with Import, External_Name => "vector_124";
      procedure Vector_125 with Import, External_Name => "vector_125";
      procedure Vector_126 with Import, External_Name => "vector_126";
      procedure Vector_127 with Import, External_Name => "vector_127";
      procedure Vector_128 with Import, External_Name => "vector_128";
      procedure Vector_129 with Import, External_Name => "vector_129";
      procedure Vector_130 with Import, External_Name => "vector_130";
      procedure Vector_131 with Import, External_Name => "vector_131";
      procedure Vector_132 with Import, External_Name => "vector_132";
      procedure Vector_133 with Import, External_Name => "vector_133";
      procedure Vector_134 with Import, External_Name => "vector_134";
      procedure Vector_135 with Import, External_Name => "vector_135";
      procedure Vector_136 with Import, External_Name => "vector_136";
      procedure Vector_137 with Import, External_Name => "vector_137";
      procedure Vector_138 with Import, External_Name => "vector_138";
      procedure Vector_139 with Import, External_Name => "vector_139";
      procedure Vector_140 with Import, External_Name => "vector_140";
      procedure Vector_141 with Import, External_Name => "vector_141";
      procedure Vector_142 with Import, External_Name => "vector_142";
      procedure Vector_143 with Import, External_Name => "vector_143";
      procedure Vector_144 with Import, External_Name => "vector_144";
      procedure Vector_145 with Import, External_Name => "vector_145";
      procedure Vector_146 with Import, External_Name => "vector_146";
      procedure Vector_147 with Import, External_Name => "vector_147";
      procedure Vector_148 with Import, External_Name => "vector_148";
      procedure Vector_149 with Import, External_Name => "vector_149";
      procedure Vector_150 with Import, External_Name => "vector_150";
      procedure Vector_151 with Import, External_Name => "vector_151";
      procedure Vector_152 with Import, External_Name => "vector_152";
      procedure Vector_153 with Import, External_Name => "vector_153";
      procedure Vector_154 with Import, External_Name => "vector_154";
      procedure Vector_155 with Import, External_Name => "vector_155";
      procedure Vector_156 with Import, External_Name => "vector_156";
      procedure Vector_157 with Import, External_Name => "vector_157";
      procedure Vector_158 with Import, External_Name => "vector_158";
      procedure Vector_159 with Import, External_Name => "vector_159";
      procedure Vector_160 with Import, External_Name => "vector_160";
      procedure Vector_161 with Import, External_Name => "vector_161";
      procedure Vector_162 with Import, External_Name => "vector_162";
      procedure Vector_163 with Import, External_Name => "vector_163";
      procedure Vector_164 with Import, External_Name => "vector_164";
      procedure Vector_165 with Import, External_Name => "vector_165";
      procedure Vector_166 with Import, External_Name => "vector_166";
      procedure Vector_167 with Import, External_Name => "vector_167";
      procedure Vector_168 with Import, External_Name => "vector_168";
      procedure Vector_169 with Import, External_Name => "vector_169";
      procedure Vector_170 with Import, External_Name => "vector_170";
      procedure Vector_171 with Import, External_Name => "vector_171";
      procedure Vector_172 with Import, External_Name => "vector_172";
      procedure Vector_173 with Import, External_Name => "vector_173";
      procedure Vector_174 with Import, External_Name => "vector_174";
      procedure Vector_175 with Import, External_Name => "vector_175";
      procedure Vector_176 with Import, External_Name => "vector_176";
      procedure Vector_177 with Import, External_Name => "vector_177";
      procedure Vector_178 with Import, External_Name => "vector_178";
      procedure Vector_179 with Import, External_Name => "vector_179";
      procedure Vector_180 with Import, External_Name => "vector_180";
      procedure Vector_181 with Import, External_Name => "vector_181";
      procedure Vector_182 with Import, External_Name => "vector_182";
      procedure Vector_183 with Import, External_Name => "vector_183";
      procedure Vector_184 with Import, External_Name => "vector_184";
      procedure Vector_185 with Import, External_Name => "vector_185";
      procedure Vector_186 with Import, External_Name => "vector_186";
      procedure Vector_187 with Import, External_Name => "vector_187";
      procedure Vector_188 with Import, External_Name => "vector_188";
      procedure Vector_189 with Import, External_Name => "vector_189";
      procedure Vector_190 with Import, External_Name => "vector_190";
      procedure Vector_191 with Import, External_Name => "vector_191";
      procedure Vector_192 with Import, External_Name => "vector_192";
      procedure Vector_193 with Import, External_Name => "vector_193";
      procedure Vector_194 with Import, External_Name => "vector_194";
      procedure Vector_195 with Import, External_Name => "vector_195";
      procedure Vector_196 with Import, External_Name => "vector_196";
      procedure Vector_197 with Import, External_Name => "vector_197";
      procedure Vector_198 with Import, External_Name => "vector_198";
      procedure Vector_199 with Import, External_Name => "vector_199";
      procedure Vector_200 with Import, External_Name => "vector_200";
      procedure Vector_201 with Import, External_Name => "vector_201";
      procedure Vector_202 with Import, External_Name => "vector_202";
      procedure Vector_203 with Import, External_Name => "vector_203";
      procedure Vector_204 with Import, External_Name => "vector_204";
      procedure Vector_205 with Import, External_Name => "vector_205";
      procedure Vector_206 with Import, External_Name => "vector_206";
      procedure Vector_207 with Import, External_Name => "vector_207";
      procedure Vector_208 with Import, External_Name => "vector_208";
      procedure Vector_209 with Import, External_Name => "vector_209";
      procedure Vector_210 with Import, External_Name => "vector_210";
      procedure Vector_211 with Import, External_Name => "vector_211";
      procedure Vector_212 with Import, External_Name => "vector_212";
      procedure Vector_213 with Import, External_Name => "vector_213";
      procedure Vector_214 with Import, External_Name => "vector_214";
      procedure Vector_215 with Import, External_Name => "vector_215";
      procedure Vector_216 with Import, External_Name => "vector_216";
      procedure Vector_217 with Import, External_Name => "vector_217";
      procedure Vector_218 with Import, External_Name => "vector_218";
      procedure Vector_219 with Import, External_Name => "vector_219";
      procedure Vector_220 with Import, External_Name => "vector_220";
      procedure Vector_221 with Import, External_Name => "vector_221";
      procedure Vector_222 with Import, External_Name => "vector_222";
      procedure Vector_223 with Import, External_Name => "vector_223";
      procedure Vector_224 with Import, External_Name => "vector_224";
      procedure Vector_225 with Import, External_Name => "vector_225";
      procedure Vector_226 with Import, External_Name => "vector_226";
      procedure Vector_227 with Import, External_Name => "vector_227";
      procedure Vector_228 with Import, External_Name => "vector_228";
      procedure Vector_229 with Import, External_Name => "vector_229";
      procedure Vector_230 with Import, External_Name => "vector_230";
      procedure Vector_231 with Import, External_Name => "vector_231";
      procedure Vector_232 with Import, External_Name => "vector_232";
      procedure Vector_233 with Import, External_Name => "vector_233";
      procedure Vector_234 with Import, External_Name => "vector_234";
      procedure Vector_235 with Import, External_Name => "vector_235";
      procedure Vector_236 with Import, External_Name => "vector_236";
      procedure Vector_237 with Import, External_Name => "vector_237";
      procedure Vector_238 with Import, External_Name => "vector_238";
      procedure Vector_239 with Import, External_Name => "vector_239";
      procedure Vector_240 with Import, External_Name => "vector_240";
      procedure Vector_241 with Import, External_Name => "vector_241";
      procedure Vector_242 with Import, External_Name => "vector_242";
      procedure Vector_243 with Import, External_Name => "vector_243";
      procedure Vector_244 with Import, External_Name => "vector_244";
      procedure Vector_245 with Import, External_Name => "vector_245";
      procedure Vector_246 with Import, External_Name => "vector_246";
      procedure Vector_247 with Import, External_Name => "vector_247";
      procedure Vector_248 with Import, External_Name => "vector_248";
      procedure Vector_249 with Import, External_Name => "vector_249";
      procedure Vector_250 with Import, External_Name => "vector_250";
      procedure Vector_251 with Import, External_Name => "vector_251";
      procedure Vector_252 with Import, External_Name => "vector_252";
      procedure Vector_253 with Import, External_Name => "vector_253";
      procedure Vector_254 with Import, External_Name => "vector_254";
      procedure Vector_255 with Import, External_Name => "vector_255";

   begin
      Add_Gate (0,   Vector_0'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (1,   Vector_1'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (2,   Vector_2'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (3,   Vector_3'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (4,   Vector_4'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (5,   Vector_5'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (6,   Vector_6'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (7,   Vector_7'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (8,   Vector_8'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (9,   Vector_9'Address,   Trap_Gate,      Exception_Stack);
      Add_Gate (10,  Vector_10'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (11,  Vector_11'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (12,  Vector_12'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (13,  Vector_13'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (14,  Vector_14'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (15,  Vector_15'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (16,  Vector_16'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (17,  Vector_17'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (18,  Vector_18'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (19,  Vector_19'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (20,  Vector_20'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (21,  Vector_21'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (22,  Vector_22'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (23,  Vector_23'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (24,  Vector_24'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (25,  Vector_25'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (26,  Vector_26'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (27,  Vector_27'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (28,  Vector_28'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (29,  Vector_29'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (30,  Vector_30'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (31,  Vector_31'Address,  Trap_Gate,      Exception_Stack);
      Add_Gate (32,  Vector_32'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (33,  Vector_33'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (34,  Vector_34'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (35,  Vector_35'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (36,  Vector_36'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (37,  Vector_37'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (38,  Vector_38'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (39,  Vector_39'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (40,  Vector_40'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (41,  Vector_41'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (42,  Vector_42'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (43,  Vector_43'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (44,  Vector_44'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (45,  Vector_45'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (46,  Vector_46'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (47,  Vector_47'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (48,  Vector_48'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (49,  Vector_49'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (50,  Vector_50'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (51,  Vector_51'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (52,  Vector_52'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (53,  Vector_53'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (54,  Vector_54'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (55,  Vector_55'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (56,  Vector_56'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (57,  Vector_57'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (58,  Vector_58'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (59,  Vector_59'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (60,  Vector_60'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (61,  Vector_61'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (62,  Vector_62'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (63,  Vector_63'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (64,  Vector_64'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (65,  Vector_65'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (66,  Vector_66'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (67,  Vector_67'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (68,  Vector_68'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (69,  Vector_69'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (70,  Vector_70'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (71,  Vector_71'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (72,  Vector_72'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (73,  Vector_73'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (74,  Vector_74'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (75,  Vector_75'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (76,  Vector_76'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (77,  Vector_77'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (78,  Vector_78'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (79,  Vector_79'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (80,  Vector_80'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (81,  Vector_81'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (82,  Vector_82'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (83,  Vector_83'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (84,  Vector_84'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (85,  Vector_85'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (86,  Vector_86'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (87,  Vector_87'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (88,  Vector_88'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (89,  Vector_89'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (90,  Vector_90'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (91,  Vector_91'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (92,  Vector_92'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (93,  Vector_93'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (94,  Vector_94'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (95,  Vector_95'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (96,  Vector_96'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (97,  Vector_97'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (98,  Vector_98'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (99,  Vector_99'Address,  Interrupt_Gate, Interrupt_Stack);
      Add_Gate (100, Vector_100'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (101, Vector_101'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (102, Vector_102'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (103, Vector_103'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (104, Vector_104'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (105, Vector_105'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (106, Vector_106'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (107, Vector_107'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (108, Vector_108'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (109, Vector_109'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (110, Vector_110'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (111, Vector_111'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (112, Vector_112'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (113, Vector_113'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (114, Vector_114'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (115, Vector_115'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (116, Vector_116'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (117, Vector_117'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (118, Vector_118'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (119, Vector_119'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (120, Vector_120'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (121, Vector_121'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (122, Vector_122'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (123, Vector_123'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (124, Vector_124'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (125, Vector_125'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (126, Vector_126'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (127, Vector_127'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (128, Vector_128'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (129, Vector_129'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (130, Vector_130'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (131, Vector_131'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (132, Vector_132'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (133, Vector_133'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (134, Vector_134'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (135, Vector_135'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (136, Vector_136'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (137, Vector_137'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (138, Vector_138'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (139, Vector_139'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (140, Vector_140'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (141, Vector_141'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (142, Vector_142'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (143, Vector_143'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (144, Vector_144'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (145, Vector_145'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (146, Vector_146'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (147, Vector_147'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (148, Vector_148'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (149, Vector_149'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (150, Vector_150'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (151, Vector_151'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (152, Vector_152'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (153, Vector_153'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (154, Vector_154'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (155, Vector_155'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (156, Vector_156'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (157, Vector_157'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (158, Vector_158'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (159, Vector_159'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (160, Vector_160'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (161, Vector_161'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (162, Vector_162'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (163, Vector_163'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (164, Vector_164'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (165, Vector_165'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (166, Vector_166'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (167, Vector_167'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (168, Vector_168'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (169, Vector_169'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (170, Vector_170'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (171, Vector_171'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (172, Vector_172'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (173, Vector_173'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (174, Vector_174'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (175, Vector_175'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (176, Vector_176'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (177, Vector_177'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (178, Vector_178'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (179, Vector_179'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (180, Vector_180'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (181, Vector_181'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (182, Vector_182'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (183, Vector_183'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (184, Vector_184'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (185, Vector_185'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (186, Vector_186'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (187, Vector_187'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (188, Vector_188'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (189, Vector_189'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (190, Vector_190'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (191, Vector_191'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (192, Vector_192'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (193, Vector_193'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (194, Vector_194'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (195, Vector_195'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (196, Vector_196'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (197, Vector_197'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (198, Vector_198'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (199, Vector_199'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (200, Vector_200'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (201, Vector_201'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (202, Vector_202'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (203, Vector_203'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (204, Vector_204'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (205, Vector_205'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (206, Vector_206'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (207, Vector_207'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (208, Vector_208'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (209, Vector_209'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (210, Vector_210'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (211, Vector_211'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (212, Vector_212'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (213, Vector_213'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (214, Vector_214'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (215, Vector_215'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (216, Vector_216'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (217, Vector_217'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (218, Vector_218'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (219, Vector_219'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (220, Vector_220'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (221, Vector_221'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (222, Vector_222'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (223, Vector_223'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (224, Vector_224'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (225, Vector_225'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (226, Vector_226'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (227, Vector_227'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (228, Vector_228'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (229, Vector_229'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (230, Vector_230'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (231, Vector_231'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (232, Vector_232'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (233, Vector_233'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (234, Vector_234'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (235, Vector_235'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (236, Vector_236'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (237, Vector_237'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (238, Vector_238'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (239, Vector_239'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (240, Vector_240'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (241, Vector_241'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (242, Vector_242'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (243, Vector_243'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (244, Vector_244'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (245, Vector_245'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (246, Vector_246'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (247, Vector_247'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (248, Vector_248'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (249, Vector_249'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (250, Vector_250'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (251, Vector_251'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (252, Vector_252'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (253, Vector_253'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (254, Vector_254'Address, Interrupt_Gate, Interrupt_Stack);
      Add_Gate (255, Vector_255'Address, Interrupt_Gate, Interrupt_Stack);
   end Setup_Interrupt_Descriptor_Table;

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception (ID : LAPIC_Vector; Code : Error_Code) is
      procedure Fatal_Exception (ID : LAPIC_Vector; Code : Error_Code)
        with Import, External_Name => "__gnat_fatal_exception";

   begin
      case ID is
         when Divide_Error_Exception =>
            raise Constraint_Error with "hardware divide by zero exception";
         when Dedug_Execption =>
            raise Program_Error with "debug exception";
         when Breakpoint_Execption =>
            raise Program_Error with "breakpoint exception";
         when Overflow_Exception =>
            raise Constraint_Error with "hardware overflow exception";
         when BOUND_Range_Exceeded_Exception =>
            raise Constraint_Error with "hardware BOUND check failed";
         when Segment_Not_Present_Exception =>
            raise Storage_Error with "segment not present: " & Code'Image;
         when Stack_Segment_Fault_Exception =>
            raise Storage_Error with "stack segment fault " & Code'Image;
         when Math_Fault_Exception =>
            raise Constraint_Error with "floating point exception";
         when SIMD_Floating_Point_Exception =>
            raise Constraint_Error with "SSE exception";
         when Page_Fault_Exception =>
            raise Storage_Error with "page fault exception: " & Code'Image;
         when others =>
            Fatal_Exception (ID, Code);
      end case;
   end Process_Exception;

   -------------
   -- Set_CR4 --
   -------------

   procedure Set_CR4 (CR4 : Control_Register_4) is
   begin
      Asm
        ("movq %0, %%cr4",
         Inputs   => Control_Register_4'Asm_Input ("r", CR4),
         Volatile => True);
   end Set_CR4;

   ------------------
   -- Thread_Start --
   ------------------

   procedure Thread_Start is
   begin
      --  Move the argument from R12, where it was stored as part of the
      --  creation of the task, to RDI (the first parameter passing register).
      --  Call the task procedure whose address is in R13.
      Asm
        ("movq  %%r12, %%rdi"                                            & NL &
         "call   *%%r13",
         Volatile => True);
   end Thread_Start;

end System.BB.CPU_Primitives;
