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

package body TMS570.GPIO is

   ---------
   -- Set --
   ---------

   function Set (This : GPIO_Register; Pin : GPIO_Pin_Number) return Boolean is
   begin
      return (This.DIN and Pin) /= 0;
   end Set;

   -------------
   -- Set_Pin --
   -------------

   procedure Set_Pin
     (This : in out GPIO_Register;
      Pin : GPIO_Pin_Number)
   is
   begin
      This.DSET := Pin;
   end Set_Pin;

   ---------------
   -- Clear_Pin --
   ---------------

   procedure Clear_Pin
     (This : in out GPIO_Register;
      Pin : GPIO_Pin_Number)
   is
   begin
      This.DCLR := Pin;
   end Clear_Pin;

   ----------------
   -- Toggle_Pin --
   ----------------

   procedure Toggle_Pin
     (This : in out GPIO_Register;
      Pin : GPIO_Pin_Number)
   is
   begin
      if Set (This, Pin) then
         Clear_Pin (This, Pin);
      else
         Set_Pin (This, Pin);
      end if;
   end Toggle_Pin;

end TMS570.GPIO;
