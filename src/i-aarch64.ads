------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     I N T E R F A C E S . A A R C H 6 4                  --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                         Copyright (C) 2017, AdaCore                      --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with Interfaces;

package Interfaces.AArch64 is
   pragma Preelaborate;

--     function Runtime_EL return Exception_Level with Inline_Always;
   --  Current exception level

   function Get_CPACR_EL1 return Interfaces.Unsigned_64
     with Inline_Always;
   procedure Set_CPACR_EL1 (Val : Interfaces.Unsigned_64)
     with Inline_Always;
   --  Low-level access to CPACR_EL1 register

   CPACR_FPEN : constant := 16#100000#;
   --  FPU enable bit of CPACR

   function Get_CPTR_EL2 return Interfaces.Unsigned_64
     with Inline_Always;
   procedure Set_CPTR_EL2 (Val : Interfaces.Unsigned_64)
     with Inline_Always;
   --  Low-level access to CPTR_EL2 register

   CPTR_TFP : constant := 16#400#;
   --  FPU trap bit of CPTR

   procedure Set_CNTP_CTL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTHP_CTL_EL2 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTP_CTL (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTP_CTL register

   procedure Set_CNTV_CTL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTV_CTL_EL0 register

   procedure Set_CNTP_TVAL_EL0 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTHP_TVAL_EL2 (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   procedure Set_CNTP_TVAL (Val : Interfaces.Unsigned_32)
     with Inline_Always;
   --  Set the CNTP_TVAL register

   function Get_CNTPCT_EL0 return Interfaces.Unsigned_64
     with Inline_Always;
   --  Get the CNTPCT register

end Interfaces.AArch64;
