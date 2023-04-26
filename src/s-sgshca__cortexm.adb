------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    S Y S T E M . S E M I H O S T I N G . G E N E R I C _ S H _ C A L L   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2017-2023, Free Software Foundation, Inc.       --
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

--  This is the implementation for Cortex-M profile targets

separate (System.Semihosting)
function Generic_SH_Call
  (Op    : Syscall;
   Param : System.Address)
   return SH_Word is

   Ret : SH_Word;
begin
   Asm ("mov r0, %1" & ASCII.LF & ASCII.HT &
        "mov r1, %2" & ASCII.LF & ASCII.HT &
        "bkpt #0xAB" & ASCII.LF & ASCII.HT &
        "mov %0, r0",
        Outputs  => (SH_Word'Asm_Output ("=r", Ret)),
        Inputs   => (Syscall'Asm_Input ("r", Op),
                    System.Address'Asm_Input ("r", Param)),
        Volatile => True,
        Clobber => ("r1, r0"));
   return Ret;
end Generic_SH_Call;