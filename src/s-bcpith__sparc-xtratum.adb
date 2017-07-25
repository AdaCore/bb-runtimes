------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                SYSTEM.BB.CPU_PRIMITIVES.INSTALL_TRAP_HANDLER             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

--  This is a SPARC (LEON3) XtratuM version of this procedure

with System.Machine_Code; use System.Machine_Code;

separate (System.BB.CPU_Primitives)
procedure Install_Trap_Handler
  (Service_Routine : Address;
   Vector          : Vector_Id;
   Synchronous     : Boolean := False)
is
   XM_DCACHE : constant := 1;
   --  Id for the data cache

   procedure XM_Flush_Cache (Cache : Integer);
   pragma Import (C, XM_Flush_Cache, "XM_flush_cache");
   --  Flush cache. Use Cache => 1 for data cache, Cache => 2 for instruction
   --  cache, or Cache => 3 for both.

begin
   --  Install the 4 instructions in the SPARC trap table to point to the
   --  shared interrupt service routine (Common_ISR).

   --  sethi %hi(Common_Handler), %l4

   --  The 22 most significant bits of Common_Handler are obtained shifting 10
   --  times to the right, which is equivalent to divide by 2**10.

   Trap_Table (Vector).First_Instr :=
     16#29000000# + SSE.To_Integer (Common_Handler'Address) / 2**10;

   --  jmp   %l4 + %lo(Common_Handler)

   --  The 10 least significant bits of Common_Handler are obtained by masking,
   --  which is equivalent to mod by 2**10.

   Trap_Table (Vector).Second_Instr :=
     16#81C52000# + SSE.To_Integer (Common_Handler'Address) mod 2**10;

   --  mov   Vector, %l3

   Trap_Table (Vector).Third_Instr :=
     16#A6102000# + SSE.Integer_Address (Vector);

   --  If the handler is for a synchronous trap then we set bit number
   --  8 (in l3) to 1. This bit indicates to the underlying trap handler
   --  (common_handler) that this is a synchronous trap and it must return
   --  to the instruction following the trap.

   if Synchronous then
      Trap_Table (Vector).Third_Instr :=
        Trap_Table (Vector).Third_Instr + SSE.Integer_Address (16#100#);
   end if;

   --  nop

   Trap_Table (Vector).Fourth_Instr := 16#01000000#;

   --  Flush the data cache since we are modifying the memory area that
   --  corresponds to instructions (trap table). This is a good idea even for
   --  systems where we don't support cache control, such as ERC32, as there
   --  still may be variants with cache.

   --  As installing interrupt handlers is not time critical, the few extra
   --  instructions do not matter much.

   XM_Flush_Cache (XM_DCACHE);

   --  Put the address of the user handler in the User_Vector_Table, so that
   --  the Common_Handler can know where to jump when a trap (synchronous or
   --  asynchronous) occurs.

   User_Vector_Table (Vector) := Service_Routine;
end Install_Trap_Handler;
