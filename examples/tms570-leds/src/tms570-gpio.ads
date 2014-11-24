------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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

--  This file provides register definitions for the TMS570LS31 (ARM Cortex R4F)
--  microcontrollers from Texas Instruments.

pragma Restrictions (No_Elaboration_Code);

package TMS570.GPIO is

   type GPIO_Register is record
    DIR    : Word; --  Data Direction Register
    DIN    : Word; --  Data Input Register
    DOUT   : Word; --  Data Output Register
    DSET   : Word; --  Data Output Set Register
    DCLR   : Word; --  Data Output Clear Register
    PDR    : Word; --  Open Drain Register
    PULDIS : Word; --  Pullup Disable Register
    PSL    : Word; --  Pull Up/Down Selection Register
   end record;

   for GPIO_Register use record
      DIR    at 0  range 0 .. 31;
      DIN    at 4  range 0 .. 31;
      DOUT   at 8  range 0 .. 31;
      DSET   at 12 range 0 .. 31;
      DCLR   at 16 range 0 .. 31;
      PDR    at 20 range 0 .. 31;
      PULDIS at 24 range 0 .. 31;
      PSL    at 28 range 0 .. 31;
   end record;

   --  GPIO pin definitions
   GPIO_Pin_0   : constant := 16#0000_0001#;
   GPIO_Pin_1   : constant := 16#0000_0002#;
   GPIO_Pin_2   : constant := 16#0000_0004#;
   GPIO_Pin_3   : constant := 16#0000_0008#;
   GPIO_Pin_4   : constant := 16#0000_0010#;
   GPIO_Pin_5   : constant := 16#0000_0020#;
   GPIO_Pin_6   : constant := 16#0000_0040#;
   GPIO_Pin_7   : constant := 16#0000_0080#;
   GPIO_Pin_8   : constant := 16#0000_0100#;
   GPIO_Pin_9   : constant := 16#0000_0200#;
   GPIO_Pin_10  : constant := 16#0000_0400#;
   GPIO_Pin_11  : constant := 16#0000_0800#;
   GPIO_Pin_12  : constant := 16#0000_1000#;
   GPIO_Pin_13  : constant := 16#0000_2000#;
   GPIO_Pin_14  : constant := 16#0000_4000#;
   GPIO_Pin_15  : constant := 16#0000_8000#;
   GPIO_Pin_16  : constant := 16#0001_0000#;
   GPIO_Pin_17  : constant := 16#0002_0000#;
   GPIO_Pin_18  : constant := 16#0004_0000#;
   GPIO_Pin_19  : constant := 16#0008_0000#;
   GPIO_Pin_20  : constant := 16#0010_0000#;
   GPIO_Pin_21  : constant := 16#0020_0000#;
   GPIO_Pin_22  : constant := 16#0040_0000#;
   GPIO_Pin_23  : constant := 16#0080_0000#;
   GPIO_Pin_24  : constant := 16#0100_0000#;
   GPIO_Pin_25  : constant := 16#0200_0000#;
   GPIO_Pin_26  : constant := 16#0400_0000#;
   GPIO_Pin_27  : constant := 16#0800_0000#;
   GPIO_Pin_28  : constant := 16#1000_0000#;
   GPIO_Pin_29  : constant := 16#2000_0000#;
   GPIO_Pin_30  : constant := 16#4000_0000#;
   GPIO_Pin_31  : constant := 16#8000_0000#;
   GPIO_Pin_All : constant := 16#FFFF_FFFF#;  -- All pins selected

   subtype GPIO_Pin_Number is Word;

   function Set (This : GPIO_Register;  Pin : GPIO_Pin_Number) return Boolean;
   pragma Inline (Set);

   procedure Set_Pin (This : in out GPIO_Register;  Pin : GPIO_Pin_Number);
   pragma Inline (Set_Pin);

   procedure Clear_Pin (This : in out GPIO_Register;  Pin : GPIO_Pin_Number);
   pragma Inline (Clear_Pin);

   procedure Toggle_Pin (This : in out GPIO_Register;  Pin : GPIO_Pin_Number);
   pragma Inline (Toggle_Pin);

end TMS570.GPIO;
