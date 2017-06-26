------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with IOEmu; use IOEmu;
with Interfaces; use Interfaces;

package Emu_GIC is
   type GIC_Dev is new IOEmu_Dev32 with private;

   function Read32 (Dev : in out GIC_Dev; Off : Off_T)
                   return Unsigned_32;
   procedure Write32_Mask
     (Dev : in out GIC_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32);

   procedure Init (Dev : access GIC_Dev; Cpu : Interrupt_Dev_Acc);
   function Get_Interrupt_Dev (Dev : access GIC_Dev) return Interrupt_Dev_Acc;
   procedure Dump (Dev : GIC_Dev);

private
   Nbr_Int : constant := 64;

   type GIC_Dev_Acc is access all GIC_Dev;

   type GIC_Interrupt_Dev is new Interrupt_Dev with record
      Parent : GIC_Dev_Acc;
   end record;
   procedure Set_Level
     (Dev : in out GIC_Interrupt_Dev; Id : Natural; Level : Boolean);
   procedure Set_Ack_Cb
     (Dev : in out GIC_Interrupt_Dev; Id : Natural; Cb : Interrupt_Ack_Cb_Acc);

   type State_Array is array (16 .. Nbr_Int - 1) of Boolean;

   type GIC_State is record
      IT : aliased GIC_Interrupt_Dev;

      --  The cpu for the interrupt
      Cpu : Interrupt_Dev_Acc;

      --  Callbacks for PPI when they are ack-ed.
      PPI_Acks : Interrupt_Ack_Cb_Array (16 .. 31);

      --  Inputs.
      Lines : State_Array;

      --  Dist interface.
      DCTLR : Unsigned_32;
      ICFGR : Unsigned_32_Arr (2 .. (Nbr_Int / 16) - 1);
      PRIORITY : Unsigned_32_Arr (0 .. (Nbr_Int / 4) - 1);
      ACTIVE : Unsigned_32_Arr (0 .. (Nbr_Int / 32) - 1);
      ENABLE : Unsigned_32_Arr (0 .. (Nbr_Int / 32) - 1);
      PEND   : Unsigned_32_Arr (0 .. (Nbr_Int / 32) - 1);

      --  CPU interface.
      CCTLR : Unsigned_32;
      PMR : Unsigned_32;
      BPR : Unsigned_32;
      CRPR : Unsigned_32;
      HPPIR : Unsigned_32;
      HPPIR_Prio : Unsigned_32;  -- The priority of the HPPIR interrupt

      --  Outputs
      Irq : Boolean;
      Fiq : Boolean;
   end record;

   type GIC_Dev is new IOEmu_Dev32 with record
      S : GIC_State;
   end record;
end Emu_GIC;
