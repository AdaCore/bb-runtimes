------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                    Copyright (C) 2016-2019, AdaCore                      --
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

--  This package contains the primitives which are dependent on the underlying
--  processor.

pragma Restrictions (No_Elaboration_Code);

with System.BB.Interrupts;

package System.BB.CPU_Specific is
   pragma Preelaborate;

   ------------------------
   -- Context management --
   ------------------------

   --  The context buffer is a type that represents thread's state and is not
   --  otherwise stored in main memory. This typically includes all user-
   --  visible registers, and possibly some other status as well.

   --  In case different contexts have different amounts of state (for example,
   --  due to absence of a floating-point unit in a particular configuration,
   --  or just the FPU not being used), it is expected that these details are
   --  handled in the implementation.

   type Context_Buffer is record

      RA  : System.Address; -- X1

      --  Callee-saved registers

      SP  : System.Address; -- X2
      S0  : System.Address; -- X8
      S1  : System.Address; -- X9
      S2  : System.Address; -- X18
      S3  : System.Address; -- X19
      S4  : System.Address; -- X20
      S5  : System.Address; -- X21
      S6  : System.Address; -- X22
      S7  : System.Address; -- X23
      S8  : System.Address; -- X24
      S9  : System.Address; -- X25
      S10 : System.Address; -- X26
      S11 : System.Address; -- X27

      FS0  : System.Address;
      FS1  : System.Address;
      FS2  : System.Address;
      FS3  : System.Address;
      FS4  : System.Address;
      FS5  : System.Address;
      FS6  : System.Address;
      FS7  : System.Address;
      FS8  : System.Address;
      FS9  : System.Address;
      FS10 : System.Address;
      FS11 : System.Address;
   end record;

   Stack_Alignment : constant := 16;
   --  Stack alignment defined by the ABI (RV32I and RV64I)

   -----------
   -- Traps --
   -----------

   type Trap_Type is (Timer_Trap, External_Interrupt_Trap);

   procedure Install_Trap_Handler
     (Service_Routine : BB.Interrupts.Interrupt_Handler;
      Trap            : Trap_Type);
   --  Install a new handler in the CPU trap table

   ----------------------------------
   -- Control and Status Registers --
   ----------------------------------

   type Register_Word is mod 2**System.Word_Size
     with Size => System.Word_Size;

   generic
      Register_Name : String;
   function Read_CSR return Register_Word;
   --  Return the value the CSR

   generic
      Register_Name : String;
   procedure Clear_CSR_Bits (Bits : Register_Word);
   --  Bits is a mask that specifies bit positions to be cleared in the CSR

   generic
      Register_Name : String;
   procedure Set_CSR_Bits (Bits : Register_Word);
   --  Bits is a mask that specifies bit positions to be set in the CSR

   function  Mie                is new Read_CSR ("mie");
   function  Mip                is new Read_CSR ("mip");
   function  Mcause             is new Read_CSR ("mcause");
   procedure Clear_Mstatus_Bits is new Clear_CSR_Bits ("mstatus");
   procedure Set_Mstatus_Bits   is new Set_CSR_Bits ("mstatus");
   procedure Set_Mie_Bits       is new Set_CSR_Bits ("mie");

   Mstatus_MIE : constant Register_Word := 2#1000#;

   Mie_MTIE : constant Register_Word := 2#0000_1000_0000#;
   --  Machine Time Interrupt Enable

   Mie_MEIE : constant Register_Word := 2#1000_0000_0000#;
   --  Machine External Interrupt Enable

   Mip_MEIP : constant Register_Word := Mie_MEIE;
   --  Machine External Interrupt Pending

end System.BB.CPU_Specific;
