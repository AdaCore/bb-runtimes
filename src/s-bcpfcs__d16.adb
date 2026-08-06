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
--                     Copyright (C) 2003-2022, AdaCore                     --
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

--  This file is a separate procedure to handle FPU_Context_Switch, which is
--  dependent on whether or not the target uses 16 or 32 registers. This
--  file specifically handles the 16 register case.

separate (System.BB.CPU_Primitives)

------------------------
-- FPU_Context_Switch --
------------------------

procedure FPU_Context_Switch (To : VFPU_Context_Access) is
   CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;
   C : constant VFPU_Context_Access := Current_FPU_Context (CPU_Id);

begin
   if C /= To then
      if C /= null then
         Asm (Template => "vstm %1, {d0-d15}" & NL & "fmrx %0, fpscr",
               Outputs  =>
                  (Unsigned_32'Asm_Output ("=r", C.FPSCR)),
               Inputs   =>
                  (Address'Asm_Input ("r", C.V'Address)),
               Clobber  => "memory",
               Volatile => True);
         C.V_Init := True;
      end if;

      if To /= null then
         Asm (Template => "vldm %1, {d0-d15}" & NL & "fmxr fpscr, %0",
               Inputs   =>
                  (Unsigned_32'Asm_Input ("r", To.FPSCR),
                  Address'Asm_Input ("r", To.V'Address)),
               Clobber  => "memory",
               Volatile => True);
      end if;

      Current_FPU_Context (CPU_Id) := To;
   end if;
end FPU_Context_Switch;
