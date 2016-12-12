------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

--  Describe I2C Master registers

with Interfaces; use Interfaces;
with System; use System;

package I2cm is
   type Reserved_2  is mod 2**2;
   type Reserved_3  is mod 2**3;
   type Reserved_6  is mod 2**6;
   type Reserved_24 is mod 2**24;

   pragma Warnings (Off, "reverse bit order in machine scalar*");

   type I2cm_Clock_Prescale_Register is record
      Prescale : Unsigned_16;
      Reserved : Unsigned_16;
   end record;
   for I2cm_Clock_Prescale_Register'Bit_Order use Low_Order_First;

   for I2cm_Clock_Prescale_Register use record
      Reserved at 0 range 16 .. 31;
      Prescale at 0 range 0 .. 15;
   end record;

   type I2cm_Control_Register is record
      Res_0 : Reserved_6;
      Ien : Boolean;
      En : Boolean;
      Res_1 : Reserved_24;
   end record;
   for I2cm_Control_Register'Bit_Order use Low_Order_First;

   for I2cm_Control_Register use record
      Res_0 at 0 range 0 .. 5;
      Ien at 0 range 6 .. 6;
      En at 0 range 7 .. 7;
      Res_1 at 0 range 8 .. 31;
   end record;

   type I2cm_Data_Register is record
      Data : Unsigned_8;
      Res : Reserved_24;
   end record;

   for I2cm_Data_Register'Bit_Order use Low_Order_First;
   for I2cm_Data_Register use record
      Data at 0 range 0 .. 7;
      Res at 0 range 8 .. 31;
   end record;

   type I2cm_Command_Register is record
      Iack : Boolean;
      Res_0 : Reserved_2;
      Ack : Boolean;
      Wr : Boolean;
      Rd : Boolean;
      Sto : Boolean;
      Sta : Boolean;
      Res_1 : Reserved_24;
   end record;
   for I2cm_Command_Register'Bit_Order use Low_Order_First;
   for I2cm_Command_Register use record
      Iack at 0 range 0 .. 0;
      Res_0 at 0 range 1 .. 2;
      Ack at 0 range 3 .. 3;
      Wr at 0 range 4 .. 4;
      Rd at 0 range 5 .. 5;
      Sto at 0 range 6 .. 6;
      Sta at 0 range 7 .. 7;
      Res_1 at 0 range 8 .. 31;
   end record;

   type I2cm_Status_Register is record
      Iflg : Boolean;
      Tip : Boolean;
      Res_0 : Reserved_3;
      Al : Boolean;
      Busy : Boolean;
      Rxack : Boolean;
      Res_1 : Reserved_24;
   end record;
   for I2cm_Status_Register'Bit_Order use Low_Order_First;
   for I2cm_Status_Register use record
      Iflg at 0 range 0 .. 0;
      Tip at 0 range 1 .. 1;
      Res_0 at 0 range 2 .. 4;
      Al at 0 range 5 .. 5;
      Busy at 0 range 6 .. 6;
      Rxack at 0 range 7 .. 7;
      Res_1 at 0 range 8 .. 31;
   end record;

   pragma Warnings (On, "reverse bit order in machine scalar*");
end I2cm;
