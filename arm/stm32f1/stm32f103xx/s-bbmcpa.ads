------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . B B . M C U _ P A R A M E T E R S             --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this library; see the file COPYING3. If not, see:                   --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines MCU parameters for the STM32F40x family

with Interfaces.STM32.PWR;
with Interfaces.Bit_Types;

package System.BB.MCU_Parameters is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate;
   use type Interfaces.Bit_Types.Bit;

   Number_Of_Interrupts : constant := 83;

   procedure PWR_Initialize;

   procedure PWR_Overdrive_Enable;

   function Is_PWR_Stabilized return Boolean
     is (Interfaces.STM32.PWR.PWR_Periph.CSR.VOSRDY = 1);

end System.BB.MCU_Parameters;
