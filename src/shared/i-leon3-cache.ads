------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 I N T E R F A C E S . L E O N 3 . C A C H E              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

package Interfaces.Leon3.Cache is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   ----------------------------
   -- Cache Control Register --
   ----------------------------

   type Status_2 is mod 2 ** 2;
   for Status_2'Size use 2;

   type Test_Bit_Status is mod 2 ** 4;
   for Test_Bit_Status'Size use 4;

   type Cache_Control_Register is record
      Ics       : Status_2;
      Dcs       : Status_2;
      Icf       : Boolean;
      Dcf       : Boolean;
      Dde       : Status_2;
      Dte       : Status_2;
      Ide       : Status_2;
      Ite       : Status_2;
      Dp        : Boolean;
      Ip        : Boolean;
      Ib        : Boolean;
      Reserved1 : Reserved_2;
      Ft        : Status_2;
      Fi        : Boolean;
      Fd        : Boolean;
      Reserved2 : Boolean;
      Tb        : Test_Bit_Status;
      Ps        : Boolean;
      Reserved3 : Reserved_3;
   end record;

   for Cache_Control_Register use record
      Reserved3  at 0 range Bit31 .. Bit29;
      Ps         at 0 range Bit28 .. Bit28;
      Tb         at 0 range Bit27 .. Bit24;
      Reserved2  at 0 range Bit23 .. Bit23;
      Fd         at 0 range Bit22 .. Bit22;
      Fi         at 0 range Bit21 .. Bit21;
      Ft         at 0 range Bit20 .. Bit19;
      Reserved1  at 0 range Bit18 .. Bit17;
      Ib         at 0 range Bit16 .. Bit16;
      Ip         at 0 range Bit15 .. Bit15;
      Dp         at 0 range Bit14 .. Bit14;
      Ite        at 0 range Bit13 .. Bit12;
      Ide        at 0 range Bit11 .. Bit10;
      Dte        at 0 range Bit09 .. Bit08;
      Dde        at 0 range Bit07 .. Bit06;
      Dcf        at 0 range Bit05 .. Bit05;
      Icf        at 0 range Bit04 .. Bit04;
      Dcs        at 0 range Bit03 .. Bit02;
      Ics        at 0 range Bit01 .. Bit00;
   end record;

   for  Cache_Control_Register'Size use 32;

   pragma Suppress_Initialization (Cache_Control_Register);
   pragma Volatile_Full_Access (Cache_Control_Register);

end Interfaces.Leon3.Cache;
