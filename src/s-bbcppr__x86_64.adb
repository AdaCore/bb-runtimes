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
   use type SSE.Storage_Offset;

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
     with Linker_Section => ".tss";
   --  Each CPU will store the address of their respective CPU_Task_State
   --  record in their %gs register.

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
   --  GDT used by the runtime. This will replace the default GDT in start.S
   --  by including the TSS Descriptors for each CPU.
   --  Note: the IDT uses the position of the Code segment. If the position of
   --  the code segment changes then the IDT in vector_table.S needs updating.

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
           --     2. Makes it easier to change the number of CPU's
           Segment_Type       => TSS_Available,
           Privilege_Level    => 0,
           Segment_Present    => True,
           Segment_Limit_High => 0,
           System_Flag        => False,
           Granularity        => Limit_in_Byte_Units,
           Base_Address_High  => 0)))
     with Alignment => 8, Linker_Section => ".gdt";

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

   procedure Process_Exception (ID : Interrupt_ID; Code : Error_Code);
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

   function Determine_TSC_Frequency return Unsigned_64;
   --  Attempt to find out what the TSC frequency (kHz) from the CPU. If it
   --  returns zero it means we were not able to determine the frequency of the
   --  processor.

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

   function Determine_TSC_Frequency return Unsigned_64 is
      TSC_Information_Leaf          : constant := 16#15#;
      Processor_Frequency_Info_Leaf : constant := 16#16#;
      --  CPUID leaf IDs

      Core_Crystal_Clock_Mhz    : Unsigned_64;
      TSC_Ratio_Numerator       : Unsigned_64;
      TSC_Ratio_Denominator     : Unsigned_64;
      Processor_Base_Frequency  : Unsigned_64;
      --  Clock information that we query from the CPU

      TSC_Frequency             : Unsigned_64 := 0;
      --  The Frequency of our Clock. If zero it means we were not able to
      --  determine the frequency of the processor.

      MHz : constant := 1_000_000_000;
   begin
      if Max_CPUID_Index >= TSC_Information_Leaf then
         --  Try to see if the TSC Information Leaf 15H has the information we
         --  need.

         Asm ("cpuid",
              Inputs   => Unsigned_32'Asm_Input ("a", TSC_Information_Leaf),
              Outputs  =>
                (Unsigned_64'Asm_Output ("=a", TSC_Ratio_Denominator),
                 Unsigned_64'Asm_Output ("=b", TSC_Ratio_Numerator),
                 Unsigned_64'Asm_Output ("=c", Core_Crystal_Clock_Mhz)),
              Volatile => True);

         --  The easiest case is the processor has enumerated all the TSC ratio
         --  in CPUID Leaf 15H.

         if TSC_Ratio_Numerator /= 0 and then TSC_Ratio_Denominator /= 0 then
            --  The processor has enumerated the core crystal clock frequency.
            --  (only newer processors do this sadly).

            if Core_Crystal_Clock_Mhz /= 0 then
               TSC_Frequency :=
                 Core_Crystal_Clock_Mhz * MHz * TSC_Ratio_Numerator /
                                                TSC_Ratio_Denominator;

            --  The Denverton SoC does not report the crystal clock and lacks
            --  the CPUID Leaf 16H used to calculate the TSC frequency below. A
            --  hardcoded 25 MHz crystal clock is used instead.

            elsif My_CPU_Model = Denverton then
               Core_Crystal_Clock_Mhz := 25_000_000;
               TSC_Frequency :=
                 Core_Crystal_Clock_Mhz * MHz * TSC_Ratio_Numerator /
                                                TSC_Ratio_Denominator;

            --  When the core crystal clock frequency is not enumerated the TSC
            --  appears to use the base frequency reported in CPUID Leaf 16H.

            elsif Max_CPUID_Index >= Processor_Frequency_Info_Leaf then
               Asm ("cpuid",
                    Inputs   =>
                      Unsigned_32'Asm_Input
                        ("a", Processor_Frequency_Info_Leaf),
                    Outputs  =>
                      Unsigned_64'Asm_Output ("=a", Processor_Base_Frequency),
                    Volatile => True);

               TSC_Frequency := Processor_Base_Frequency * MHz;
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
            Bus_Frequency : constant := 100 * MHz;
            Platform_Info : Platform_Infomation;
         begin

            Asm ("rdmsr",
                 Inputs   => Unsigned_32'Asm_Input ("a", MSR_PLATFORM_INFO),
                 Outputs  =>
                   Platform_Infomation'Asm_Output ("=a", Platform_Info),
                 Volatile => True);

            TSC_Frequency :=
              Unsigned_64 (Platform_Info.Maximum_Non_Turbo_Ratio)
              * Bus_Frequency;
         end;
      end if;

      --  If we got to this point and have not been able to determine the
      --  clock frequency, we have to determine the TSC Frequency by testing
      --  it against the Programmable Interval Timer (PIT), which has a known
      --  frequency.

      if TSC_Frequency = 0 then
         declare
            function Calculate_Frequency return Unsigned_64;
            --  Calculate the frequency of the TSC by comparing the number of
            --  TSC ticks against a set number of PIT ticks, which has a known
            --  frequency of 1.193182 MHz.

            -------------------------
            -- Calculate_Frequency --
            -------------------------

            function Calculate_Frequency return Unsigned_64 is
               PIT_Frequency : constant := 1_193_182;
               --  PIT operates at 1.193182 MHz

               PIT_Ticks_To_Count : constant := 11_932;
               --  The number of PIT tickets we want to a we want to count the
               --  TSC over. This number corresponds to approximately 10ms.

               PIT_Reset_Count : constant Unsigned_16_Bytable :=
                 (View => Full, Value => Unsigned_16'Last);
               --  Value we want the PIT to start from

               Start_PIT      : Unsigned_16_Bytable;
               Current_PIT    : Unsigned_16_Bytable;
               Target_PIT     : Unsigned_16;
               PIT_Tick_Count : Unsigned_16;
               --  Start and end value of the PIT run and the target PIT value
               --  that we'll stop the run at

               Start_TSC      : Unsigned_64;
               End_TSC        : Unsigned_64;
               TSC_Tick_Count : Unsigned_64;
               --  Start and end value of the TSC run and the resulting number
               --  of ticks.

            begin
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

               Start_TSC := Read_Raw_Clock;
               Start_PIT.Value := Current_PIT.Value;
               Target_PIT := Start_PIT.Value - PIT_Ticks_To_Count;

               loop
                  Current_PIT.Low  := Read_IO_Byte (PIT_Channel_0_Data_Port);
                  Current_PIT.High := Read_IO_Byte (PIT_Channel_0_Data_Port);

                  --  QEMU may jump over PIT values on successive reads so
                  --  exit once we have reached or past the target value.

                  exit when Current_PIT.Value <= Target_PIT;
               end loop;

               End_TSC := Read_Raw_Clock;

               --  Calculate TSC frequency from the results of the timing run

               TSC_Tick_Count := End_TSC - Start_TSC;
               PIT_Tick_Count := Start_PIT.Value - Current_PIT.Value;

               --  We can do the following multiplication first since we won't
               --  overflow the Unsigned_64 type because the maximum number of
               --  TSC ticks in a second is at most the frequency of the CPU,
               --  which currently tops out at around 5 GHz.
               return
                 TSC_Tick_Count * PIT_Frequency / Unsigned_64 (PIT_Tick_Count);
            end Calculate_Frequency;

            PIT_Config : constant PIT_Mode_Command_Register :=
              (Channel         => Channel_0,
               Access_Mode     => Low_High_Byte,
               Operating_Mode  => Interrupt_On_Terminal_Count,
               BCD_Binary_Mode => Binary);
            --  PIT configuration settings

            type Clock_Runs is range 1 .. 5;
            --  We determine the TSC clock frequency over 5 runs to account
            --  for variations in the PIT and TSC sources that we see on
            --  virtualized hosts.

            Frequency_Results : array (Clock_Runs) of Unsigned_64;
            --  Array for the results of each clock determining run

            Median_Index : constant := (Frequency_Results'Length + 1) / 2;
            --  Helper constant to locate the median result in the array

            Acceptable_Clock_Difference : constant := 20;
            --  The maximum percentage difference between clock runs
            --  that we find acceptable.

            Max_Frequency_Error : Integer_64;
            --  The maximum difference between two clock frequency measurement
            --  we will tolerate.

         begin
            --  Configure PIT

            Write_IO_Byte (To_IO_Byte (PIT_Config), PIT_Mode_Command_Port);

            --  Find the TSC frequency over the number of specified runs

            for Run_Number in Frequency_Results'Range loop
               Frequency_Results (Run_Number) := Calculate_Frequency;
            end loop;

            --  Pick the median result. Do this by first sorting the results

            for J in Frequency_Results'First .. Frequency_Results'Last - 1 loop
               for K in J + 1 .. Frequency_Results'Last loop
                  declare
                     Swap_Temp : Unsigned_64;
                  begin
                     if Frequency_Results (K) < Frequency_Results (J) then
                        Swap_Temp := Frequency_Results (J);
                        Frequency_Results (J) := Frequency_Results (K);
                        Frequency_Results (K) := Swap_Temp;
                     end if;
                  end;
               end loop;
            end loop;

            TSC_Frequency := Frequency_Results (Median_Index);

            Max_Frequency_Error :=
              Integer_64 (TSC_Frequency / Acceptable_Clock_Difference);

            if abs
              (Integer_64 (TSC_Frequency) -
               Integer_64 (Frequency_Results (Median_Index - 1))) >
              Max_Frequency_Error
              or else
                abs
                  (Integer_64 (TSC_Frequency) -
                   Integer_64 (Frequency_Results (Median_Index + 1))) >
                Max_Frequency_Error
            then
               raise Program_Error with "Clock measurements lack precision";
            end if;
         end;
      end if;

      return TSC_Frequency;
   end Determine_TSC_Frequency;

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
      Number : Interrupt_ID;
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
      --   Exception Number  <-- RSP
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

      --  Retrieve the exception number and code off the stack

      Asm
        ("movq  80(%%rsp), %0"                                           & NL &
         "movl  72(%%rsp), %1",
         Outputs  =>
           (Error_Code'Asm_Output ("=r", Code),
            Interrupt_ID'Asm_Output ("=r", Number)),
         Volatile => True);

      --  We don't need to save the vector and floating point states as the
      --  exception handlers should not touch these registers.

      --  Handle the exception in proper Ada code

      Process_Exception (Number, Code);

      --  Exception has been successfully handled. Time to clean up and exit.
      --  Since processing an exception will not modify the task queues no
      --  need to check if we need to context switch to another task.

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
      use System.Storage_Elements;
   begin
      --  Round down Stack Pointer to next aligned address to point to last
      --  valid stack address.

      Stack_Pointer := Base + (Size - (Size mod CPU_Specific.Stack_Alignment));
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

      --  We cheat as we don't know the stack size nor the stack base

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
         if EBX = "Genu" and then EDX = "ineI" and then ECX = "ntel" then
            --  Capture the highest CPUID index value
            if Max_CPUID_Index >= Min_Required_CPUID_Index then
               Max_CPUID_Index := EAX;
            else
               raise Program_Error with
                 "Unsupported Intel processor";
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
         IDT_Location : constant Descriptor_Pointer
            with Import, External_Name => "__idt_access";
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

         --  Load the IDT
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
         Vector     => APIC_Timer_Interrupt_ID);

      TSC_Frequency_In_kHz :=
        (if TSC_Frequency /= 0
           then TSC_Frequency
           else Determine_TSC_Frequency) / 1_000;

      if TSC_Frequency_In_kHz = 0 then
         raise Program_Error with "TSC frequency could not be determined";
      end if;

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

      IRQ_Number : Interrupt_ID;
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
         Outputs  => Interrupt_ID'Asm_Output ("=r", IRQ_Number),
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
      --  allocated for this procedure, so we do very little here and just jump
      --  off to a proper Ada procedure.

      --  Handle the interrupt at the runtime level

      Interrupt_Wrapper (IRQ_Number);

      --  Signal to the Local APIC that the interrupt is finished

      Local_APIC_End_of_Interrupt := Signal;

      --  Interrupt has been handled. Time to clean up and exit, noting that
      --  we may have to switch to another task if the interrupt event has
      --  caused a scheduling change.

      --  Do a context switch if we are required to.

      if System.BB.Threads.Queues.Context_Switch_Needed then
         Context_Switch;
      end if;

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

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception (ID : Interrupt_ID; Code : Error_Code) is
      procedure Fatal_Exception (ID : Interrupt_ID; Code : Error_Code)
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
            raise Storage_Error with "segment not present";
         when Stack_Segment_Fault_Exception =>
            raise Storage_Error with "stack segment fault";
         when Math_Fault_Exception =>
            raise Constraint_Error with "floating point exception";
         when SIMD_Floating_Point_Exception =>
            raise Constraint_Error with "SSE exception";
         when Page_Fault_Exception =>
            raise Storage_Error with "page fault exception";
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
      --  Move the argument from R12 where it was stored as part of the
      --  creation of the task to RDI (the first parameter passing register)
      --  and then jump to the task procedure whose address is in R13
      Asm
        ("movq  %%r12, %%rdi"                                            & NL &
         "jmp   *%%r13",
         Volatile => True);
   end Thread_Start;

end System.BB.CPU_Primitives;
