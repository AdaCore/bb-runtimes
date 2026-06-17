------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . S A M V 7 1                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

--  This file provides register definitions for the SAMV71 (ARM Cortex M4)
--  microcontrollers from Atmel. Definitions are taken from 'SAMV71'
--  datasheet (document  Atmel-44003E-ATARM-SAM V71-Datasheet_12-Oct-16).

package System.SAMV71 is
   pragma No_Elaboration_Code_All;
   pragma Preelaborate (System.SAMV71);

   --  Peripheral IDs
   PIOA_ID : constant := 10;
   PIOB_ID : constant := 11;
   PIOC_ID : constant := 12;
   PIOD_ID : constant := 16;
   PIOE_ID : constant := 17;

   UART0_ID : constant := 7;
   UART1_ID : constant := 8;
end System.SAMV71;
