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
--                     Copyright (C) 2003-2019, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;
with System.Multiprocessors;
with System.BB.Threads;

package body System.BB.CPU_Primitives is
   use BB.Parameters;
   use System.BB.Threads;

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   ----------------
   -- Local data --
   ----------------

   SP       : constant Context_Id :=  6;
   PC       : constant Context_Id :=  7;
   PSR      : constant Context_Id :=  8;
   WIM      : constant Context_Id := 17;
   WIN      : constant Context_Id := 18;
   FSR      : constant Context_Id := 19;
   O0       : constant Context_Id :=  0;
   INT      : constant Context_Id := 52;
   --  these are the registers that are initialized: Program Counter, Stack
   --  Pointer, Window Invalid Mask, Floating-Point State Register, and
   --  Processor State Register. Moreover, the first input argument, the number
   --  of register windows to be restored, and the interrupt nesting level are
   --  also initialized.

   Base_CCR : constant Context_Id := Base_CCR_Context_Index;
   CCR      : constant Context_Id := CCR_Context_Index;
   pragma Assert (Base_CCR_Context_Index = 53 and then CCR_Context_Index = 54);
   --  For LEON we allocate two slots for the cache control register at the
   --  end of the buffer.

   FP : constant SSE.Storage_Offset := 56;
   --  The frame pointer needs also to be initialized; however, it is not kept
   --  in the thread descriptor but in the stack, and this value represents the
   --  offset from the stack pointer (expressed in bytes).

   ----------------------
   -- Trap definitions --
   ----------------------

   Instruction_Access_Exception : constant Vector_Id := 16#01#;
   Illegal_Instruction          : constant Vector_Id := 16#02#;
   Address_Not_Aligned          : constant Vector_Id := 16#07#;
   FP_Exception                 : constant Vector_Id := 16#08#;
   Data_Access_Exception        : constant Vector_Id := 16#09#;
   Instruction_Access_Error     : constant Vector_Id := 16#21#;
   Data_Access_Error            : constant Vector_Id := 16#29#;
   Division_By_Zero_Hw          : constant Vector_Id := 16#2A#;
   Data_Store_Error             : constant Vector_Id := 16#2B#;
   Division_By_Zero_Sw          : constant Vector_Id := 16#82#;

   type Trap_Entry is
      record
         First_Instr  : SSE.Integer_Address;
         Second_Instr : SSE.Integer_Address;
         Third_Instr  : SSE.Integer_Address;
         Fourth_Instr : SSE.Integer_Address;
      end record;
   --  Each entry in the trap table contains the four first instructions that
   --  will be executed as part of the handler. A trap is a vectored transfer
   --  of control to the supervisor through a special trap table that contains
   --  the first four instructions of each trap handler. The base address of
   --  the table is established by supervisor and the displacement, within the
   --  table, is determined by the trap type.

   type Trap_Entries_Table is array (Vector_Id) of Trap_Entry;
   Trap_Table : Trap_Entries_Table;
   pragma Import (Asm, Trap_Table, "trap_table");
   --  This is the trap table, that is defined in the crt0 file. This table
   --  contains the trap entry for all the traps (synchronous and asynchronous)
   --  defined by the SPARC architecture.

   Common_Handler : Address;
   pragma Import (Asm, Common_Handler, "common_handler");
   --  There is a common part that is executed for every trap. This common
   --  handler executes some prologue, then jumps to the user code, and after
   --  that executes an epilogue.

   type Vector_Table is array (Vector_Id) of System.Address;
   User_Vector_Table : Vector_Table;
   pragma Export (Asm, User_Vector_Table, "user_vector_table");
   --  In addition to the trap table there is another table that contains the
   --  addresses for the trap handlers defined by the user. This is used by
   --  the common wrapper to invoke the correct user-defined handler.

   function Get_CCR return System.Address;
   pragma Import (Asm, Get_CCR, "get_ccr");
   pragma Weak_External (Get_CCR);
   --  Get the value from the hardware Cache Control Register.

   procedure GNAT_Error_Handler (Trap : Vector_Id);
   --  Trap handler converting synchronous traps to exceptions

   ----------------------------
   -- Floating point context --
   ----------------------------

   type Thread_Table is array (System.Multiprocessors.CPU) of Thread_Id;
   pragma Volatile_Components (Thread_Table);
   Float_Latest_User_Table : Thread_Table := (others => Null_Thread_Id);
   pragma Export (Asm, Float_Latest_User_Table, "float_latest_user_table");
   --  This variable contains the last thread that used the floating point unit
   --  for each CPU. Hence, it points to the place where the floating point
   --  state must be stored.

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
      procedure Asm_Context_Switch;
      pragma Import (Asm, Asm_Context_Switch, "__gnat_context_switch");
   begin
      Asm_Context_Switch;
   end Context_Switch;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
      procedure Asm_Disable_Interrupts;
      pragma Import (Asm, Asm_Disable_Interrupts, "disable_interrupts");
   begin
      Asm_Disable_Interrupts; -- Replace by inline Asm ???
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts (Level : Integer) is
      procedure Asm_Enable_Interrupts (Level : Natural);
      pragma Import (Asm, Asm_Enable_Interrupts, "enable_interrupts");
   begin
      if Level in Interrupt_Priority then
         Asm_Enable_Interrupts (Level - Interrupt_Priority'First + 1);
      else
         Asm_Enable_Interrupts (0);
      end if;
   end Enable_Interrupts;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (Context : Context_Buffer;
      Index   : Context_Id) return Word
   is
   begin
      return Word (Context (Index));
   end Get_Context;

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Trap : Vector_Id) is
   begin
      case Trap is
         when Instruction_Access_Exception =>
            raise Storage_Error with "instruction access exception";
         when Illegal_Instruction =>
            raise Constraint_Error with "illegal instruction";
         when Address_Not_Aligned =>
            raise Constraint_Error with "address not aligned";
         when FP_Exception =>
            raise Constraint_Error with "floating point exception";
         when Data_Access_Exception =>
            raise Constraint_Error with "data access exception";
         when Instruction_Access_Error =>
            raise Constraint_Error with "instruction access exception";
         when Data_Access_Error =>
            raise Constraint_Error with "data access error";
         when Division_By_Zero_Hw | Division_By_Zero_Sw =>
            raise Constraint_Error with "division by zero";
         when Data_Store_Error =>
            raise Constraint_Error with "data store error";
         when others =>
            raise Program_Error with "unhandled trap";
      end case;
   end GNAT_Error_Handler;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
   begin
      --  The stack must be aligned to 16. 96 bytes are needed for storing
      --  a whole register window (96 bytes).

      Buffer (SP) :=
        SSE.To_Address ((SSE.To_Integer (Stack_Pointer) / 16) * 16 - 96);

      --  Initialize PSR with the state expected by the context switch routine.
      --  Floating point unit is disabled. Traps are enabled, although
      --  interrupts are disabled (after the context switch only interrupts
      --  with a lower priority than the task will be masked). The supervisor
      --  and previous supervisor are set to 1 (the system always operates in
      --  supervisor mode).

      --  CWP = 0, ET = 1, PS = 1, S = 1, and PIL = 15

      Buffer (PSR) := SSE.To_Address (16#0FE0#);

      --  The WIM is initialized to 2 (corresponding to CWP = 1)

      Buffer (WIM) := SSE.To_Address (2);

      --  The number of windows that must be flushed is initially set to 0
      --  (only the current window).

      Buffer (WIN) := SSE.To_Address (0);

      --  Initialize PC with the starting address of the task. Substract 8
      --  to compensate the adjustment made in the context switch routine.

      Buffer (PC) := SSE.To_Address (SSE.To_Integer (Program_Counter) - 8);

      --  The argument to be used by the task wrapper function must be
      --  passed through the o0 register.

      Buffer (O0) := Argument;

      --  The frame pointer is initialized to be the top of the stack

      declare
         FP_In_Stack : System.Address;
         for FP_In_Stack'Address use (Buffer (SP) + FP);

      begin
         --  Mark the deepest stack frame by setting the frame pointer to zero

         FP_In_Stack := SSE.To_Address (0);
      end;

      --  The interrupt nesting level is initialized to 0

      Buffer (INT) := SSE.To_Address (0);

      --  For LEON we initialize the cache control register to its value at
      --  initialization time.

      Buffer (CCR) :=
        (if Get_CCR'Address = Null_Address then Null_Address else Get_CCR);
      Buffer (Base_CCR) := Buffer (CCR);

      --  The Floating-Point State Register is initialized to 0

      Buffer (FSR) := SSE.To_Address (0);

      --  The rest of registers do not need to be initialized

   end Initialize_Context;

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
   begin
      null;
   end Initialize_CPU;

   ----------------------------
   -- Install_Error_Handlers --
   ----------------------------

   procedure Install_Error_Handlers is
      --  Set up trap handler to map synchronous signals to appropriate
      --  exceptions. Make sure that the handler isn't interrupted by
      --  another signal that might cause a scheduling event.

   begin
      --  The division by zero trap may be either a hardware trap (trap type
      --  16#2A#) when the integer division istruction is used (on SPARC V8 and
      --  later) or a software trap (trap type 16#82#) caused by the software
      --  division implementation in libgcc.

      for J in Vector_Id'Range loop
         if J in Instruction_Access_Exception
             | Illegal_Instruction | Address_Not_Aligned | FP_Exception
             | Data_Access_Exception | Instruction_Access_Error
             | Data_Access_Error | Division_By_Zero_Hw | Division_By_Zero_Sw
             | Data_Store_Error
         then
            Install_Trap_Handler (GNAT_Error_Handler'Address, J);
         end if;
      end loop;
   end Install_Error_Handlers;

   --------------------------
   -- Install_Trap_Handler --
   --------------------------

   procedure Install_Trap_Handler
     (Service_Routine : Address;
      Vector          : Vector_Id;
      Synchronous     : Boolean := False) is separate;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context : in out Context_Buffer;
      Index   : Context_Id;
      Value   : Word)
   is
   begin
      Context (Index) := Address (Value);
   end Set_Context;

end System.BB.CPU_Primitives;
