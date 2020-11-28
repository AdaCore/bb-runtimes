------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . B B . M C U _ P A R A M E T E R S             --
--                                                                          --
--                                  B o d y                                 --
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

with Interfaces.STM32.PWR; use Interfaces.STM32.PWR;

package body System.BB.MCU_Parameters is

   --------------------
   -- PWR_Initialize --
   --------------------

   procedure PWR_Initialize
   is
   begin
      --  Set the PWR to Scale 1 mode to stabilize the MCU when in high
      --  performance mode.
      PWR_Periph.CR.VOS := 1;
   end PWR_Initialize;

   --------------------------
   -- PWR_Overdrive_Enable --
   --------------------------

   procedure PWR_Overdrive_Enable
   is
   begin
      null;
   end PWR_Overdrive_Enable;

end System.BB.MCU_Parameters;
