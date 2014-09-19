------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
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

pragma Restrictions (No_Elaboration_Code);

with Interfaces; use Interfaces;
with System;

procedure Setup_Pll is
   RIS : Unsigned_32;
   for RIS'Address use System'To_Address (16#400f_e000# + 16#50#);
   pragma Import (Ada, RIS);
   pragma Volatile (RIS);

   RCC : Unsigned_32;
   for RCC'Address use System'To_Address (16#400f_e000# + 16#60#);
   pragma Import (Ada, RCC);
   pragma Volatile (RCC);

   RCC_Val : Unsigned_32;
begin
   --  PLL configuration.  See 5.3 Initialization and Configuration
   --  We are slightly more careful not to disrupt jtag probe

   --  1. Bypass the PLL and system clock divider, enable main oscillator,
   --     clear USESYS.
   RCC_Val := RCC;
   RCC_Val := (RCC_Val and not 16#40_0001#) or 16#0800#;
   RCC := RCC_Val;

   --  2. Select the crystal value (8Mhz), clear PWRDWN
   RCC_Val := (RCC_Val and not 16#23c0#) or 16#0380#;
   RCC := RCC_Val;

   --  4. Wait a little bit
   for I in 1 .. 4096 loop
      null;
   end loop;

   --  4. Wait for the PLL to lock
   while (RIS and 16#40#) = 0 loop
      null;
   end loop;

   --  3. Select the desired system divider (50Mhz)
   RCC_Val := (RCC_Val and not 16#0780_0000#) or 16#0180_0000#; -- 50Mhz
   RCC := RCC_Val;

   --  5. Enable use of the PLL by clearing the BYPASS bit, disable internal
   --     oscillator
   RCC_Val := RCC_Val and not 16#0800#; -- BYPASS
   RCC_Val := RCC_Val or 16#40_0000#; -- USESYS
   RCC := RCC_Val;

   RCC_Val := RCC_Val or 16#02#; -- IOSCDIS
   RCC := RCC_Val;
end Setup_Pll;
