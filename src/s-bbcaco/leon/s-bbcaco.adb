------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               S Y S T E M . B B . C A C H E _ C O N T R O L              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                      Copyright (C) 2010-2016, AdaCore                    --
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
------------------------------------------------------------------------------

--  This is the LEON version of this package

with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.CPU_Primitives; use System.BB.CPU_Primitives;
with System.BB.Parameters;     use System.BB.Parameters;

with Ada.Unchecked_Conversion; use Ada;

package body System.BB.Cache_Control is

   --  As we like to share a single implementation of this
   --  package between different versions of LEON, we only define
   --  those bits that actually used and common to all implementations.
   --  A new port can only use this package if its register definition
   --  is compatible with the one declared below.

   type Status_2 is mod 2**2;
   for Status_2'Size use 2;

   type Cache_Control_Register is
      record
         Ics   : Status_2;
         Dcs   : Status_2;
         Icf   : Boolean;
         Dcf   : Boolean;
         Fi    : Boolean;
         Fd    : Boolean;
      end record;

   for Cache_Control_Register use
     record
         Ics   at 0 range 30 .. 31;
         Dcs   at 0 range 28 .. 29;
         Icf   at 0 range 27 .. 27;
         Dcf   at 0 range 26 .. 26;
         Fi    at 0 range 10 .. 10;
         Fd    at 0 range  9 ..  9;
     end record;

   for Cache_Control_Register'Size use 32;
   for Cache_Control_Register'Alignment use 4;
   pragma Suppress_Initialization (Cache_Control_Register);

   type Action_Type is (Disable, Enable, Freeze,
                        Enable_Freeze_Interrupt, Disable_Freeze_Interrupt);
   --  Actions that can be performed on the cache control register

   procedure Modify_Cache_Register
     (Register : in out Cache_Control_Register;
      Cache    : Cache_Type;
      Action   : Action_Type);
   --  Update the required fields in the CCR to enable/disable/freeze the
   --  requested cache type.

   procedure Modify_Cache_Register
     (Context : in out Context_Buffer;
      Index   : Context_Id;
      Cache   : Cache_Type;
      Action  : Action_Type);
   --  Version of Modify_Cache_Register that updates an existing context

   procedure Modify_Cache
     (Cache          : Cache_Type;
      Action         : Action_Type;
      Partition_Wide : Boolean);
   --  Update the required fields in the CCR to enable/disable/freeze
   --  (depending on Action) the requested cache type (depending on Cache)
   --  for the current task or all (depending on Partition_Wide).

   procedure Set_CCR (Value : Word);
   pragma Import (Asm, Set_CCR, "set_ccr");
   --  Set the specified value in the hardware Cache Control Register.
   --  NOTE: Ideally, this function should use type Cache_Control_Register
   --  as parameter. We use a simple 32-bit word to pass the parameter
   --  by value (this is the convention expected by the assembly
   --  implementation of set_ccr) instead of by reference (as it would
   --  be the case if the parameter were a structure).

   function Get_CCR return Word;
   pragma Import (Asm, Get_CCR, "get_ccr");
   --  Get the value from the hardware Cache Control Register.
   --  NOTE: Ideally, this function should return a type
   --  Cache_Control_Register. However, it returns a simple 32-bit word
   --  to avoid having a function returning a structure. In SPARC, functions
   --  returning structure, union, or quad-precision values require a more
   --  complex handshaking mechanism between the caller and the callee (see
   --  SPARC ABI) that is complicated and inefficient.

   function To_Word is new Unchecked_Conversion (Cache_Control_Register, Word);
   --  Necessary conversion to store the CCR in the Context

   function To_CCR is new Unchecked_Conversion (Word, Cache_Control_Register);
   --  Necessary conversion to retrieve the CCR from the Context

   -----------------
   -- Cache_Flush --
   -----------------

   procedure Cache_Flush (Cache : Cache_Type) is
      Tmp_CCR : Cache_Control_Register;

   begin
      --  Get current value

      Tmp_CCR := To_CCR (Get_CCR);

      --  Set the appropriate bit to 1 to flush the required cache

      case Cache is
         when Instruction => Tmp_CCR.Fi := True;
         when Data        => Tmp_CCR.Fd := True;
      end case;

      --  Store modified value

      Set_CCR (To_Word (Tmp_CCR));
   end Cache_Flush;

   ---------------------------------------
   -- Disable_Cache_Freeze_On_Interrupt --
   ---------------------------------------

   procedure Disable_Cache_Freeze_On_Interrupt
     (Cache          : Cache_Type;
      Partition_Wide : Boolean := False)
   is
   begin
      Modify_Cache (Cache, Disable_Freeze_Interrupt, Partition_Wide);
   end Disable_Cache_Freeze_On_Interrupt;

   --------------------------------------
   -- Enable_Cache_Freeze_On_Interrupt --
   --------------------------------------

   procedure Enable_Cache_Freeze_On_Interrupt
     (Cache          : Cache_Type;
      Partition_Wide : Boolean := False)
   is
   begin
      Modify_Cache (Cache, Enable_Freeze_Interrupt, Partition_Wide);
   end Enable_Cache_Freeze_On_Interrupt;

   ---------------------
   -- Get_Cache_State --
   ---------------------

   function Get_Cache_State (Cache : Cache_Type) return Cache_State is
      Status : Status_2;

   begin
      --  Get the requested state

      case Cache is
         when Instruction => Status := To_CCR (Get_CCR).Ics;
         when Data        => Status := To_CCR (Get_CCR).Dcs;
      end case;

      --  Interpret state

      case Status is
         when 2#00# | 2#10# => return Disabled;
         when 2#11#         => return Enabled;
         when 2#01#         => return Frozen;
      end case;
   end Get_Cache_State;

   ------------------
   -- Modify_Cache --
   ------------------

   procedure Modify_Cache
     (Cache          : Cache_Type;
      Action         : Action_Type;
      Partition_Wide : Boolean)
   is
      Tmp_CCR  : Cache_Control_Register;
      Self_Id  : constant Threads.Thread_Id := Threads.Thread_Self;

      use type Threads.Thread_Id;

   begin
      --  Get current value

      Tmp_CCR := To_CCR (Get_CCR);

      --  Update status for the currently executing task

      Modify_Cache_Register (Tmp_CCR, Cache, Action);

      --  Store modified value both in the task buffer and in the actual
      --  Cache Control Register. The goal of storing the value in the
      --  base register is to be able to set the desired state when leaving
      --  from interrupt handlers.

      Modify_Cache_Register
        (Self_Id.Context, Base_CCR_Context_Index, Cache, Action);

      Set_CCR (To_Word (Tmp_CCR));

      if Partition_Wide then

         --  Update the stored cache control register for all tasks. We modify
         --  both the base and the actual register to be able to set the
         --  required state both for the next time the task will execute and
         --  after the execution of interrupt handlers.

         declare
            Next_Thread : Threads.Thread_Id;

         begin
            Next_Thread := Threads.Queues.Global_List;
            while Next_Thread /= null loop
               Modify_Cache_Register
                 (Next_Thread.Context, Base_CCR_Context_Index, Cache, Action);

               Modify_Cache_Register
                 (Next_Thread.Context, CCR_Context_Index, Cache, Action);

               Next_Thread := Next_Thread.Global_List;
            end loop;
         end;
      end if;
   end Modify_Cache;

   ---------------------------
   -- Modify_Cache_Register --
   ---------------------------

   procedure Modify_Cache_Register
     (Register : in out Cache_Control_Register;
      Cache    : Cache_Type;
      Action   : Action_Type)
   is
   begin
      case Action is

         --  For cache control

         when Disable
            | Enable
            | Freeze
         =>
            declare
               Value : constant Status_2 :=
                         (if Action = Enable then 2#11#
                          elsif Action = Freeze then 2#01#
                          else 2#00#);
            begin
               case Cache is
                  when Instruction => Register.Ics := Value;
                  when Data        => Register.Dcs := Value;
               end case;
            end;

            --  For freeze-on-interrupt

         when Disable_Freeze_Interrupt
            | Enable_Freeze_Interrupt
         =>
            case Cache is
               when Instruction =>
                  Register.Icf := Action = Enable_Freeze_Interrupt;
               when Data =>
                  Register.Dcf := Action = Enable_Freeze_Interrupt;
            end case;
      end case;
   end Modify_Cache_Register;

   procedure Modify_Cache_Register
     (Context : in out Context_Buffer;
      Index   : Context_Id;
      Cache   : Cache_Type;
      Action  : Action_Type)
   is
      CCR : Cache_Control_Register := To_CCR (Get_Context (Context, Index));
   begin
      Modify_Cache_Register (CCR, Cache, Action);
      Set_Context (Context, Index, To_Word (CCR));
   end Modify_Cache_Register;

   ---------------------
   -- Set_Cache_State --
   ---------------------

   procedure Set_Cache_State
     (Cache          : Cache_Type;
      State          : Cache_State;
      Partition_Wide : Boolean := False)
   is
      Action : constant Action_Type :=
                 (if State = Disabled then Disable
                  elsif State = Enabled then Enable
                  else Freeze);
   begin
      Modify_Cache (Cache, Action, Partition_Wide);
   end Set_Cache_State;

end System.BB.Cache_Control;
