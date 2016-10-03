------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ppc; use Ppc;

package Mpc55xx is
   pragma Elaborate_Body;

   function Get_Tcr is new Get_Spr (340);
   procedure Set_Tcr is new Set_Spr (340);
   function Get_Tsr is new Get_Spr (336);

   function Get_Pir is new Get_Spr (286);
   function Get_Pvr is new Get_Spr (287);
   function Get_Svr is new Get_Spr (1023);
   function Get_Mmucfg is new Get_Spr (1015);

   function Get_Pid0 is new Get_Spr (48);
   function Get_Pid1 is new Get_Spr (633);
   function Get_Pid2 is new Get_Spr (634);

   function Get_Tlb0cfg is new Get_Spr (688);
   function Get_Tlb1cfg is new Get_Spr (689);

   procedure Set_Mas0 is new Set_Spr (624);
   function Get_Mas1 is new Get_Spr (625);
   procedure Set_Mas1 is new Set_Spr (625);
   function Get_Mas2 is new Get_Spr (626);
   procedure Set_Mas2 is new Set_Spr (626);
   function Get_Mas3 is new Get_Spr (627);
   procedure Set_Mas3 is new Set_Spr (627);

   function Get_Hid0 is new Get_Spr (1008);
   function Get_Hid1 is new Get_Spr (1009);

   function Get_L1csr0 is new Get_Spr (1010);
   function Get_L1cfg0 is new Get_Spr (515);

   --  System clock
   Fsys : constant := 128_000_000;
end Mpc55xx;
