------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

with Interfaces; use Interfaces;

with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.BB.Parameters;
with System.BB.Protection;

package body System.BB.Board_Support is

   --  Mapping between priorities and interrupt source.

   --    HPI is not used; MIXA, MIXB, SYSA, SYSD groups are grouped.

   --    All external and internal interrupts are routed to /int.

   --    Interrupt ipp_ind_ext_int[0] is configured as an external maskable
   --    interrupt (See initialization of SEMSR).

   --    MCP interrupts are not handled by the runtime.

   --    External IRQ interrupts are configured as level sensitive.

   --  Here is the table between IPIC priority and Interrupt source. This is
   --  extracted from Table 8-28, using the fact that only grouped scheme are
   --  used. Interrupt_ID is the interrupt number for the IPIC, which according
   --  to Table 8-6 is also the interrupt vector, which by definition is also
   --  the Ada Interrupt_ID.

   --  Priority  Interrupt source              Interrupt_ID
   --     1      (HPI)
   --     2      MIXA0 ipi_int_internal[32]    64
   --     3      MIXA1 ipi_int_internal[33]    65
   --     4      MIXA2 ipi_int_internal[34]    66
   --     5      MIXA3 ipi_int_internal[35]    67
   --     6      (MIXB0 - Spread)
   --     7-10   (Reserved)
   --    11      (MIXA1 - Spread)
   --    12-15   (Reserved)
   --    16      MIXB0 RTC ALR                 68
   --    17      MIXB1 MU                      69
   --    18      MIXB2 SBA                     70
   --    19      MIXB3 DMA                     71
   --    20      (MIXB1 - Spread)
   --    21      SYSA0 TSEC1 Tx                32
   --    22      SYSA1 TSEC1 Rx                33
   --    23      SYSA2 TSEC1 Err               34
   --    24      SYSA3 TSEC2 Tx                35
   --    25      (MIXA2 - Spread)
   --    26      SYSA4 TSEC2 Rx                36
   --    27      SYSA5 TSEC2 Err               37
   --    28      SYSA6 USB DR                  38
   --    29      SYSA7 USB MPH                 39
   --    30      MIXA4 ipp_ind_ext_int[0]      48
   --    31      MIXA5 ipp_ind_ext_int[1]      17
   --    32      MIXA6 ipp_ind_ext_int[2]      18
   --    33      MIXA7 ipp_ind_ext_int[3]      19
   --    34      (MIXB2 - Spread)
   --    35-38   (Reserved)
   --    39      (MIXA3 - Spread)
   --    40-43   (Reserved)
   --    44      MIXB4 IRQ4                    20
   --    45      MIXB5 IRQ5                    21
   --    46      MIXB6 IRQ6                    22
   --    47      MIXB7 IRQ7                    23
   --    48      (MIXB3 - Spread)
   --    49      SYSD0 UART1                    9
   --    50      SYSD1 UART2                   10
   --    51      SYSD2 SEC                     11
   --    52      (SYSD3 - Reserved)
   --    53      (MIXA4 - Spread)
   --    54      (SYSD4 - Reserved)
   --    55      SYSD5 I2C1                    14
   --    56      SYSD6 I2C2                    15
   --    57      SYSD7 SPI                     16
   --    58      (MIXB4 - Spread)
   --    59      GTM4                          72
   --    60      (Reserved)
   --    61      (SYSA0 - Spread)
   --    62      GTM8                          73
   --    63      (Reserved)
   --    64      (SYSD0 - Spread)
   --    65      (Reserved)
   --    66      GPIO1                         74
   --    67      (MIXA5 - Spread)
   --    68      GPIO2                         75
   --    69      (Reserved)
   --    70      (SYSA1 - Spread)
   --    71      DDR                           76
   --    72      (Reserved)
   --    73      (SYSD1 - Spread)
   --    74      (Reserved)
   --    75      LBC                           77
   --    76      (MIXB5 - Spread)
   --    77      GTM2                          78
   --    78      (Reserved)
   --    79      (SYSA2 - Spread)
   --    80      GTM6                          79
   --    81      (Reserved)
   --    82      (SYSD2 - Spread)
   --    83      (Reserved)
   --    84      PMC                           80
   --    85      (MIXA6 - Spread)
   --    86      (Reserved)
   --    87      (Reserved)
   --    88      (SYSA3 - Spread)
   --    89      (Reserved)
   --    90      (Reserved)
   --    91      (SYSD3 (Spread))
   --    92      (Reserved)
   --    93      (Reserved)
   --    94      (MIXB6 (Spread))
   --    95      GTM3                          84
   --    96      (Reserved)
   --    97      (SYSA4 (Spread))
   --    98      GTM7                          85
   --    99      (Reserved)
   --   100      (SYSD4 - Spread)
   --   101      (Reserved)
   --   102      (Reserved)
   --   103      (MIXA7 - Spread)
   --   104      (Reserved)
   --   105      (Reserved)
   --   106      (SYSA5 - Spread)
   --   107      (Reserved)
   --   108      (Reserved)
   --   109      (SYSD5 - Spread)
   --   110      (Reserved)
   --   111      (Reserved)
   --   112      (MIXB7 - Spread)
   --   113      GTM1                          90
   --   114      (Reserved)
   --   115      (SYSA6 - Spread)
   --   116      GTM5                          91
   --   117      (Reserved)
   --   118      (SYSD6 - Spread)
   --   119      (Reserved)
   --   120      (Reserved)
   --   121      (Reserved)
   --   122      (Reserved)
   --   123      (SYSA7 - Spread)
   --   124      (Reserved)
   --   125      (Reserved)
   --   126      (SYSD7 - Spread)
   --   127      (Reserved)
   --   128      (Reserved)

   procedure Define_Interrupt_Priority
     (Interrupt : System.BB.Interrupts.Interrupt_ID;
      Priority : Interrupt_Priority;
      Mask_Bit : Natural);
   pragma Export (Ada, Define_Interrupt_Priority,
                 "__gnat_define_interrupt_priority");
   --  This is a user service to define the priority of interrupts and its
   --  corresponding mask bit. It can be called at most once per interrupt.

   use System.BB.Interrupts;

   subtype Mask_Bit_Number is Natural range 0 .. 95;
   --  Bit number in the SIMSR_H/SIMSR_L/SEMSR registers. Bit numbers 0 to 31
   --  means bit 0 (MSB) to 31 (LSB) of SIMSR_H, bit numbers 32 to 63 means
   --  bit 0 (MSB) to 31 (LSB) of SIMSR_L, and bit numbers 64 to 95 means
   --  bit 0 (MSB) to 31 (LSB) of SEMSR.

   type Interrupt_Priorities_Type is
     array (System.BB.Interrupts.Interrupt_ID
              range 0 .. System.BB.Parameters.Number_Of_Interrupt_ID - 1)
     of Any_Priority;
   --  Type for the interrupt to priority map

   Interrupt_Priorities : Interrupt_Priorities_Type := (others => 0);
   --  Map that associate an interrupt id with its priority

   type Priority_Mask is record
      SIMSR_H : Unsigned_32;
      --  SIMSR_H value

      SIMSR_L : Unsigned_32;
      --  SIMSR_L value

      SEMSR : Unsigned_32;
      --  SEMSR value
   end record;

   type Priority_Mask_Map_Type is
     array (Priority'Last .. Interrupt_Priority'Last) of Priority_Mask;
   --  Interrupt mask values for each interrupt priority and for non-interrupt
   --  priorities (Priority'Last is used for that case).

   Priority_Masks : Priority_Mask_Map_Type := (others => (0, 0, 0));
   --  Mask for eash priority. Initially all interrupts are masked

   SEMSR : Unsigned_32;
   for SEMSR'Address use IMMRBAR + 16#0738#;
   pragma Volatile (SEMSR);
   pragma Import (Ada, SEMSR);
   --  System External interrupt Mask Register

   SIMSR_H : Unsigned_32;
   for SIMSR_H'Address use IMMRBAR + 16#0720#;
   pragma Volatile (SIMSR_H);
   pragma Import (Ada, SIMSR_H);
   --  System Internal interrupt Mask Register (High)

   SIMSR_L : Unsigned_32;
   for SIMSR_L'Address use IMMRBAR + 16#0724#;
   pragma Volatile (SIMSR_L);
   pragma Import (Ada, SIMSR_L);
   --  System Internal interrupt Mask Register (Low)

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      SICFR : Unsigned_32;
      for SICFR'Address use IMMRBAR + 16#0700#;
      pragma Volatile (SICFR);
      pragma Import (Ada, SICFR);

      SIPRR_A : Unsigned_32;
      for SIPRR_A'Address use IMMRBAR + 16#0710#;
      pragma Volatile (SIPRR_A);
      pragma Import (Ada, SIPRR_A);

      SIPRR_D : Unsigned_32;
      for SIPRR_D'Address use IMMRBAR + 16#071C#;
      pragma Volatile (SIPRR_D);
      pragma Import (Ada, SIPRR_D);

      SICNR : Unsigned_32;
      for SICNR'Address use IMMRBAR + 16#0728#;
      pragma Volatile (SICNR);
      pragma Import (Ada, SICNR);

      SMPRR_A : Unsigned_32;
      for SMPRR_A'Address use IMMRBAR + 16#0730#;
      pragma Volatile (SMPRR_A);
      pragma Import (Ada, SMPRR_A);

      SMPRR_B : Unsigned_32;
      for SMPRR_B'Address use IMMRBAR + 16#0734#;
      pragma Volatile (SMPRR_B);
      pragma Import (Ada, SMPRR_B);

      SECNR : Unsigned_32;
      for SECNR'Address use IMMRBAR + 16#073C#;
      pragma Volatile (SECNR);
      pragma Import (Ada, SECNR);

   begin
      --  Initialize IPIC

      --  At that point, all interrupts should be masked in the MSR

      --           H         M M   I    I        H
      --           P         P P   P    P        P
      --           I         S S   S    S        E
      --                     B A   D    A        T

      SICFR := 2#0_0000000_0_0_0_0_0_00_0_000000_00_00000000#;

      --  SYSA      0P  1P  2P  3P  --   4P  5P  6P  7P  --

      SIPRR_A := 2#000_001_010_011_0000_100_101_110_111_0000#;

      --  SYSD      0P  1P  2P  3P  --   4P  5P  6P  7P  --

      SIPRR_D := 2#000_001_010_011_0000_100_101_110_111_0000#;

      --     SYSD0T 1T                   SYSA0T 1T

      SICNR := 2#00_00_000000000000_00000000_00_00_0000#;

      --  SMPRR_A   0P  1P  2P  3P  --   4P  5P  6P  7P  --

      SMPRR_A := 2#000_001_010_011_0000_100_101_110_111_0000#;

      --  SMPRR_B   0P  1P  2P  3P  --   4P  5P  6P  7P  --

      SMPRR_B := 2#000_001_010_011_0000_100_101_110_111_0000#;

      --     MIXB0T 1T  MIXA0T 1T      EDI

      SECNR := 2#00_00_0000_00_00_0000_00000000_00000000#;

      --  Mask all interrupts, and steer ipp_ind_ext_int[0] as external
      --  interrupt request.

      SIMSR_H := 2#00000000_00000000_00000000_000_00_000#;
      SIMSR_L := 2#00000000_00000000_0_000_00_0000_00_0000#;
      SEMSR   := 2#00000000_00000000_0_000000000000000#;
   end Initialize_Board;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Nothing to do on standard powerpc

      null;
   end Clear_Alarm_Interrupt;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Specific.Vector_Id) return Interrupt_ID
   is
      pragma Unreferenced (Vector);

      SIVCR : Unsigned_32;
      for SIVCR'Address use IMMRBAR + 16#0704#;
      pragma Volatile (SIVCR);
      pragma Import (Ada, SIVCR);
      --  The SIVCR register contains the regular unmasked interrupt source
      --  of the highest priority level.

   begin
      return System.BB.Interrupts.Interrupt_ID (SIVCR and 16#7F#);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      pragma Unreferenced (Interrupt, Prio);
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler, CPU_Specific.External_Interrupt_Excp);
   end Install_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID) return System.Any_Priority
   is
      Result : constant Any_Priority := Interrupt_Priorities (Interrupt);

   begin
      --  The priority of an interrupt should be in the Interrupt_Priority
      --  range. A failure indicates a spurious interrupt, or an interrupt
      --  that was unmasked directly.

      pragma Assert (Result in Interrupt_Priority);

      return Result;
   end Priority_Of_Interrupt;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      null;
   end Power_Down;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
   begin
      --  Nothing to do for the IPIC
      null;
   end Clear_Interrupt_Request;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      --  Note that Priority cannot be the last one, as this procedure is
      --  unable to disable the decrementer interrupt.

      pragma Assert (Priority /= Interrupt_Priority'Last);

      --  Must be called with MSR.IE set to 0, to avoid unprioritized
      --  interrupt delivery.

      if Priority < System.Interrupt_Priority'First then
         SIMSR_H := Priority_Masks (System.Priority'Last).SIMSR_H;
         SIMSR_L := Priority_Masks (System.Priority'Last).SIMSR_L;
         SEMSR   := Priority_Masks (System.Priority'Last).SEMSR;
      else
         SIMSR_H := Priority_Masks (Priority).SIMSR_H;
         SIMSR_L := Priority_Masks (Priority).SIMSR_L;
         SEMSR   := Priority_Masks (Priority).SEMSR;
      end if;
   end Set_Current_Priority;

   -------------------------------
   -- Define_Interrupt_Priority --
   -------------------------------

   procedure Define_Interrupt_Priority
     (Interrupt : System.BB.Interrupts.Interrupt_ID;
      Priority  : Interrupt_Priority;
      Mask_Bit  : Natural)
   is
      Mask : Unsigned_32;
      --  Bit to set in the mask register

      subtype Int32_Bit_Number is Natural range 0 .. 31;
      --  Constrain the right operand of exponentiation so that the compiler
      --  is able to replace it by a shift.

   begin
      --  Check the priority was never defined for this interrupt

      pragma Assert (Interrupt_Priorities (Interrupt) = 0);

      Protection.Enter_Kernel;

      --  Save the values

      Interrupt_Priorities (Interrupt) := Priority;

      --  Regenerate masks

      case Mask_Bit_Number (Mask_Bit) is
         when 0 .. 31 =>
            Mask := 2 ** Int32_Bit_Number (31 - Mask_Bit);

            for P in System.Priority'Last .. Priority - 1 loop
               Priority_Masks (P).SIMSR_H :=
                 Priority_Masks (P).SIMSR_H or Mask;
            end loop;

         when 32 .. 63 =>
            Mask := 2 ** Int32_Bit_Number (63 - Mask_Bit);

            for P in System.Priority'Last .. Priority - 1 loop
               Priority_Masks (P).SIMSR_L :=
                 Priority_Masks (P).SIMSR_L or Mask;
            end loop;

         when 64 .. 95 =>
            Mask := 2 ** Int32_Bit_Number (95 - Mask_Bit);

            for P in System.Priority'Last .. Priority - 1 loop
               Priority_Masks (P).SEMSR :=
                 Priority_Masks (P).SEMSR or Mask;
            end loop;
      end case;

      --  Will set mask registers

      Protection.Leave_Kernel;
   end Define_Interrupt_Priority;

end System.BB.Board_Support;
