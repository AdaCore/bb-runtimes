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

with System;
with Interfaces; use Interfaces;

package Lm3s8962 is
   RCC : Unsigned_32;
   for RCC'Address use System'To_Address (16#400f_e000# + 16#60#);
   pragma Import (Ada, RCC);
   pragma Volatile (RCC);

   RCGC0 : Unsigned_32;
   for RCGC0'Address use System'To_Address (16#400f_e000# + 16#100#);
   pragma Import (Ada, RCGC0);
   pragma Volatile (RCGC0);

   RCGC1 : Unsigned_32;
   for RCGC1'Address use System'To_Address (16#400f_e000# + 16#104#);
   pragma Import (Ada, RCGC1);
   pragma Volatile (RCGC1);

   RCGC2 : Unsigned_32;
   for RCGC2'Address use System'To_Address (16#400f_e000# + 16#108#);
   pragma Import (Ada, RCGC2);
   pragma Volatile (RCGC2);

   SSICR0 : Unsigned_32;
   for SSICR0'Address use System'To_Address (16#4000_8000# + 16#00#);
   pragma Import (Ada, SSICR0);
   pragma Volatile (SSICR0);

   SSICR1 : Unsigned_32;
   for SSICR1'Address use System'To_Address (16#4000_8000# + 16#04#);
   pragma Import (Ada, SSICR1);
   pragma Volatile (SSICR1);

   SSIDR : Unsigned_32;
   for SSIDR'Address use System'To_Address (16#4000_8000# + 16#08#);
   pragma Import (Ada, SSIDR);
   pragma Volatile (SSIDR);

   SSISR : Unsigned_32;
   for SSISR'Address use System'To_Address (16#4000_8000# + 16#0c#);
   pragma Import (Ada, SSISR);
   pragma Volatile (SSISR);
   SSISR_TFE : constant := 2#1#;
   SSISR_RNE : constant := 2#100#;

   SSICPSR : Unsigned_32;
   for SSICPSR'Address use System'To_Address (16#4000_8000# + 16#10#);
   pragma Import (Ada, SSICPSR);
   pragma Volatile (SSICPSR);

   GPIOA_DATA : array (0 .. 255) of Unsigned_32;
   for GPIOA_DATA'Address use System'To_Address (16#4000_4000# + 16#000#);
   pragma Import (Ada, GPIOA_DATA);
   pragma Volatile (GPIOA_DATA);

   GPIOA_DIR : Unsigned_32;
   for GPIOA_DIR'Address use System'To_Address (16#4000_4000# + 16#400#);
   pragma Import (Ada, GPIOA_DIR);
   pragma Volatile (GPIOA_DIR);

   GPIOA_AFSEL : Unsigned_32;
   for GPIOA_AFSEL'Address use System'To_Address (16#4000_4000# + 16#420#);
   pragma Import (Ada, GPIOA_AFSEL);
   pragma Volatile (GPIOA_AFSEL);

   GPIOA_DEN : Unsigned_32;
   for GPIOA_DEN'Address use System'To_Address (16#4000_4000# + 16#51c#);
   pragma Import (Ada, GPIOA_DEN);
   pragma Volatile (GPIOA_DEN);

   GPIOE_DATA : array (0 .. 15) of Unsigned_32;
   for GPIOE_DATA'Address use System'To_Address (16#4002_4000# + 16#000#);
   pragma Import (Ada, GPIOE_DATA);
   pragma Volatile (GPIOE_DATA);

   GPIOE_DIR : Unsigned_32;
   for GPIOE_DIR'Address use System'To_Address (16#4002_4000# + 16#400#);
   pragma Import (Ada, GPIOE_DIR);
   pragma Volatile (GPIOE_DIR);

   GPIOE_AFSEL : Unsigned_32;
   for GPIOE_AFSEL'Address use System'To_Address (16#4002_4000# + 16#420#);
   pragma Import (Ada, GPIOE_AFSEL);
   pragma Volatile (GPIOE_AFSEL);

   GPIOE_PUR : Unsigned_32;
   for GPIOE_PUR'Address use System'To_Address (16#4002_4000# + 16#510#);
   pragma Import (Ada, GPIOE_PUR);
   pragma Volatile (GPIOE_PUR);

   GPIOE_DEN : Unsigned_32;
   for GPIOE_DEN'Address use System'To_Address (16#4002_4000# + 16#51c#);
   pragma Import (Ada, GPIOE_DEN);
   pragma Volatile (GPIOE_DEN);

   GPIOG_AFSEL : Unsigned_32;
   for GPIOG_AFSEL'Address use System'To_Address (16#4002_6000# + 16#420#);
   pragma Import (Ada, GPIOG_AFSEL);
   pragma Volatile (GPIOG_AFSEL);

   GPIOG_DEN : Unsigned_32;
   for GPIOG_DEN'Address use System'To_Address (16#4002_6000# + 16#51c#);
   pragma Import (Ada, GPIOG_DEN);
   pragma Volatile (GPIOG_DEN);

   GPIOF_AFSEL : Unsigned_32;
   for GPIOF_AFSEL'Address use System'To_Address (16#4002_5000# + 16#420#);
   pragma Import (Ada, GPIOF_AFSEL);
   pragma Volatile (GPIOF_AFSEL);

   GPIOF_DEN : Unsigned_32;
   for GPIOF_DEN'Address use System'To_Address (16#4002_5000# + 16#51c#);
   pragma Import (Ada, GPIOF_DEN);
   pragma Volatile (GPIOF_DEN);

   --  PWM
   PWMENABLE : Unsigned_32;
   for PWMENABLE'Address use System'To_Address (16#4002_8000# + 16#008#);
   pragma Import (Ada, PWMENABLE);
   pragma Volatile (PWMENABLE);

   PWM0CTL : Unsigned_32;
   for PWM0CTL'Address use System'To_Address (16#4002_8000# + 16#040#);
   pragma Import (Ada, PWM0CTL);
   pragma Volatile (PWM0CTL);

   PWM0LOAD : Unsigned_32;
   for PWM0LOAD'Address use System'To_Address (16#4002_8000# + 16#050#);
   pragma Import (Ada, PWM0LOAD);
   pragma Volatile (PWM0LOAD);

   PWM0CMPA : Unsigned_32;
   for PWM0CMPA'Address use System'To_Address (16#4002_8000# + 16#058#);
   pragma Import (Ada, PWM0CMPA);
   pragma Volatile (PWM0CMPA);

   PWM0CMPB : Unsigned_32;
   for PWM0CMPB'Address use System'To_Address (16#4002_8000# + 16#05c#);
   pragma Import (Ada, PWM0CMPB);
   pragma Volatile (PWM0CMPB);
   PWM0GENA : Unsigned_32;
   for PWM0GENA'Address use System'To_Address (16#4002_8000# + 16#060#);
   pragma Import (Ada, PWM0GENA);
   pragma Volatile (PWM0GENA);

   PWM0GENB : Unsigned_32;
   for PWM0GENB'Address use System'To_Address (16#4002_8000# + 16#064#);
   pragma Import (Ada, PWM0GENB);
   pragma Volatile (PWM0GENB);

   --  Ethernet

   MACRCTL : Unsigned_32;
   for MACRCTL'Address use System'To_Address (16#4004_8000# + 16#008#);
   pragma Import (Ada, MACRCTL);
   pragma Volatile (MACRCTL);

   MACTCTL : Unsigned_32;
   for MACTCTL'Address use System'To_Address (16#4004_8000# + 16#00c#);
   pragma Import (Ada, MACTCTL);
   pragma Volatile (MACTCTL);

   MACDATA : Unsigned_32;
   for MACDATA'Address use System'To_Address (16#4004_8000# + 16#010#);
   pragma Import (Ada, MACDATA);
   pragma Volatile (MACDATA);

   MACIA0 : Unsigned_32;
   for MACIA0'Address use System'To_Address (16#4004_8000# + 16#014#);
   pragma Import (Ada, MACIA0);
   pragma Volatile (MACIA0);

   MACIA1 : Unsigned_32;
   for MACIA1'Address use System'To_Address (16#4004_8000# + 16#018#);
   pragma Import (Ada, MACIA1);
   pragma Volatile (MACIA1);

   MACMDV : Unsigned_32;
   for MACMDV'Address use System'To_Address (16#4004_8000# + 16#024#);
   pragma Import (Ada, MACMDV);
   pragma Volatile (MACMDV);

   MACNP : Unsigned_32;
   for MACNP'Address use System'To_Address (16#4004_8000# + 16#034#);
   pragma Import (Ada, MACNP);
   pragma Volatile (MACNP);

end Lm3s8962;
