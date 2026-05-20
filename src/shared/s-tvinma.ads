------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
-- S Y S T E M . T I . V E C T O R E D _ I N T E R R U P T _ M A N A G E R  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  Provides the low level interface to TI's Vectored Interrupt Manager for the
--  TI Keystone Cortex-R5F.

with System.BB.Interrupts;
with System.BB.Parameters;

package System.TI.Vectored_Interrupt_Manager with Preelaborate is

   package Params renames System.BB.Parameters;
   package BB_I renames System.BB.Interrupts;

   type VIM_Interrupt_Priority is range 0 .. 15;

   type VIM_Interrupt_Request is record
      Valid            : Boolean;
      Pending_Priority : VIM_Interrupt_Priority;
      Number           : BB_I.Interrupt_ID;
   end record with Size => 32;

   for VIM_Interrupt_Request use record
      Valid            at 0 range 31 .. 31;
      Pending_Priority at 0 range 16 .. 19;
      Number           at 0 range 0 .. 9;
   end record;

   subtype Interrupt_Group is BB.Parameters.Interrupt_Range range 0 .. 31;

   type VIM_Interrupt_Status is (Inactive, Active_Clear);
   for VIM_Interrupt_Status use (Inactive => 0, Active_Clear => 1);

   type VIM_Group_Status is array (Interrupt_Group) of
     VIM_Interrupt_Status with Pack;

   type VIM_Interrupt_Raw is (Inactive, Active_Set);
   for VIM_Interrupt_Raw use (Inactive => 0, Active_Set => 1);

   type VIM_Group_Raw is array (Interrupt_Group) of VIM_Interrupt_Raw
     with Pack, Size => 32;

   type Enable_Bit is (Disable, Enable);
   for Enable_Bit use (Disable => 0, Enable => 1);

   type VIM_Group_Enable is array (Interrupt_Group) of Enable_Bit
     with Pack, Size => 32;

   type Disable_Bit is (Inactive, Disable);
   for Disable_Bit use (Inactive => 0, Disable => 1);

   type VIM_Group_Disable is array (Interrupt_Group) of Disable_Bit
     with Pack, Size => 32;

   type Interrupt_Kind is (IRQ, FIQ);
   for Interrupt_Kind use (IRQ => 0, FIQ => 1);

   type Interrupt_Map_Groups is array (Interrupt_Group) of Interrupt_Kind
     with Pack, Size => 32;

   type Interrupt_Type is (Level, Pulse);
   for Interrupt_Type use (Level => 0, Pulse => 1);

   type Interrupt_Type_Groups is array (Interrupt_Group) of Interrupt_Type
     with Pack, Size => 32;

   type VIM_Priority is record
      Priority : VIM_Interrupt_Priority;
   end record with Size => 32;

   for VIM_Priority use record
      Priority at 0 range 0 .. 3;
   end record;

   type VIM_Interrupt_Group is record
      Raw_Status_Register             : VIM_Group_Raw;
      Interrupt_Status_Register       : VIM_Group_Status;
      Interrupt_Enable_Register       : VIM_Group_Enable;
      Interrupt_Disable_Register      : VIM_Group_Disable;
      IRQ_Status_Register             : VIM_Group_Status;
      FIQ_Status_Register             : VIM_Group_Status;
      Interrupt_Map_Register          : Interrupt_Map_Groups;
      Interrupt_Type_Register         : Interrupt_Type_Groups;
   end record;

   for VIM_Interrupt_Group use record
      Raw_Status_Register             at 16#00# range 0 .. 31;
      Interrupt_Status_Register       at 16#04# range 0 .. 31;
      Interrupt_Enable_Register       at 16#08# range 0 .. 31;
      Interrupt_Disable_Register      at 16#0C# range 0 .. 31;
      IRQ_Status_Register             at 16#10# range 0 .. 31;
      FIQ_Status_Register             at 16#14# range 0 .. 31;
      Interrupt_Map_Register          at 16#18# range 0 .. 31;
      Interrupt_Type_Register         at 16#1C# range 0 .. 31;
   end record;

   VIM_IRQ_Vector_Register : Address
     with Address => System'To_Address (Params.VIM_Base_Address + 16#18#),
          Volatile;

   VIM_FIQ_Vector_Register : Address
     with Address => System'To_Address (Params.VIM_Base_Address + 16#1C#),
          Volatile;

   VIM_Active_IRQ_Register : VIM_Interrupt_Request
     with Address => System'To_Address (Params.VIM_Base_Address + 16#20#),
          Volatile_Full_Access;

   VIM_Active_FIQ_Register : VIM_Interrupt_Request
     with Address => System'To_Address (Params.VIM_Base_Address + 16#24#),
          Volatile_Full_Access;

   VIM_Interrupt_Group_Register : array (0 .. 8) of VIM_Interrupt_Group
     with Address => System'To_Address (Params.VIM_Base_Address + 16#400#),
          Volatile_Components;

   VIM_Priority_Register : array (BB_I.Interrupt_ID) of VIM_Priority
     with Address => System'To_Address (Params.VIM_Base_Address + 16#1000#),
          Volatile_Components;

   VIM_Interrupt_Vector_Register : array (BB_I.Interrupt_ID) of Address
     with Address => System'To_Address (Params.VIM_Base_Address + 16#2000#),
          Volatile_Components;

   ----------------------
   -- Helper Functions --
   ----------------------

   function VIM_Group_ID (Interrupt : BB_I.Interrupt_ID) return Interrupt_Group
     is (Interrupt / (Interrupt_Group'Last + 1));
   --  Get the group number for the interrupt

   function VIM_Sub_ID (Interrupt : BB_I.Interrupt_ID) return Interrupt_Group
     is (Interrupt mod (Interrupt_Group'Last + 1));
   --  Get the ID of the interrupt within the group

   --------------------
   -- VIM Operations --
   --------------------

   procedure Initialize;
   --  Initialize the VIM

   ----------------------------------------------------------
   -- Implementation of System.BB.Board_Support.Interrupts --
   ----------------------------------------------------------

   procedure Install_Interrupt_Handler
     (Interrupt  : System.BB.Interrupts.Interrupt_ID;
      Prio       : System.Interrupt_Priority);

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
     return System.Any_Priority;

   procedure Set_Current_Priority (Priority : Integer);

   procedure Power_Down;

end System.TI.Vectored_Interrupt_Manager;
