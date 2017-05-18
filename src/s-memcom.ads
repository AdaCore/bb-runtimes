------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M .  M E M O R Y _ C O M P A R E              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2014, Free Software Foundation, Inc.       --
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

with Interfaces.C;

package System.Memory_Compare is
   pragma Preelaborate;

   function memcmp
     (S1 : Address;
      S2 : Address;
      N  : Interfaces.C.size_t) return Interfaces.C.int;
   pragma Export (C, memcmp, "memcmp");
   --  Compares the first n bytes of the memory areas s1 and s2.  It returns
   --  an integer less than, equal  to,  or  greater  than zero  if  s1  is
   --  found, respectively, to be less than, to match, or be greater than s2.

end System.Memory_Compare;
