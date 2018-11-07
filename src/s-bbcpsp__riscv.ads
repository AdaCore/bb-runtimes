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

      X1  : System.Address;
      X2  : System.Address;
      X3  : System.Address;
      X4  : System.Address;
      X5  : System.Address;
      X6  : System.Address;
      X7  : System.Address;
      X8  : System.Address;
      X9  : System.Address;
      X10 : System.Address;
      X11 : System.Address;
      X12 : System.Address;
      X13 : System.Address;
      X14 : System.Address;
      X15 : System.Address;
      X16 : System.Address;
      X17 : System.Address;
      X18 : System.Address;
      X19 : System.Address;
      X20 : System.Address;
      X21 : System.Address;
      X22 : System.Address;
      X23 : System.Address;
      X24 : System.Address;
      X25 : System.Address;
      X26 : System.Address;
      X27 : System.Address;
      X28 : System.Address;
      X29 : System.Address;
      X30 : System.Address;
      X31 : System.Address;

      --  FIXME: FPU context
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
