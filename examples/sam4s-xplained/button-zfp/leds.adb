------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2013, Free Software Foundation, Inc.           --
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
with Interfaces; use Interfaces;
with System;

procedure Leds is
   pragma Suppress (Alignment_Check);

   Peripheral_Base : constant := 16#4000_0000#;

   System_Controller_Base : constant := Peripheral_Base + 16#e_0000#;
   PMC_Base : constant := System_Controller_Base + 16#0400#;

   PIOA_Base : constant := System_Controller_Base + 16#0e00#;
   PIOC_Base : constant := System_Controller_Base + 16#1200#;

   type PMC_Registers is record
      SCER : Unsigned_32;
      SCDR : Unsigned_32;
      SCSR : Unsigned_32;
      Pad0 : Unsigned_32;

      PCER0 : Unsigned_32;
      PCDR0 : Unsigned_32;
      PCSR0 : Unsigned_32;
      Pad1  : Unsigned_32;

      --  Not complete
   end record;

   PMC : PMC_Registers with Volatile, Import,
                            Address => System'To_Address (PMC_Base);

   type PIO_Registers is record
      PER : Unsigned_32;
      PDR : Unsigned_32;
      PSR : Unsigned_32;
      Pad0 : Unsigned_32;

      OER : Unsigned_32;
      ODR : Unsigned_32;
      OSR : Unsigned_32;
      Pad1 : Unsigned_32;

      IFER : Unsigned_32;
      IFDR : Unsigned_32;
      IFSR : Unsigned_32;
      Pad2 : Unsigned_32;

      SODR : Unsigned_32;
      CODR : Unsigned_32;
      ODSR : Unsigned_32;
      PDSR : Unsigned_32;

      IER : Unsigned_32;
      IDR : Unsigned_32;
      IMR : Unsigned_32;
      ISR : Unsigned_32;

      MDER : Unsigned_32;
      MDDR : Unsigned_32;
      MDSR : Unsigned_32;
      Pad5 : Unsigned_32;

      PUDR : Unsigned_32;
      PUER : Unsigned_32;
      PUSR : Unsigned_32;
      Pad6 : Unsigned_32;

      ABCDSR1 : Unsigned_32;
      ABCDSR2 : Unsigned_32;
      Pad7_8 : Unsigned_32;
      Pad7_C : Unsigned_32;

      IFSCDR : Unsigned_32;
      IFSCER : Unsigned_32;
      IFSCSR : Unsigned_32;
      SCDR : Unsigned_32;

      PPDDR : Unsigned_32;
      PPDER : Unsigned_32;
      PPDSR : Unsigned_32;
      Pad9 : Unsigned_32;

      OWER : Unsigned_32;
      OWDR : Unsigned_32;
      OWSR : Unsigned_32;
      Pada : Unsigned_32;

      AIMER : Unsigned_32;
      AIMDR : Unsigned_32;
      AIMMR : Unsigned_32;
      Padb  : Unsigned_32;

      ESR : Unsigned_32;
      LSR : Unsigned_32;
      ELSR : Unsigned_32;
      Padc : Unsigned_32;

      FELLSR : Unsigned_32;
      REHLSR : Unsigned_32;
      FRLHSR : Unsigned_32;
      Padd   : Unsigned_32;

      LOCKSR : Unsigned_32;
      WPMR   : Unsigned_32;
      WPSR   : Unsigned_32;
      PadE_C : Unsigned_32;
   end record;

   PIOA : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOA_Base);
   PIOC : PIO_Registers with Volatile, Import,
                             Address => System'To_Address (PIOC_Base);

   PIOA_ID : constant := 11;
   PIOC_ID : constant := 13;

   Led_Pin  : constant := 2 ** 23; -- PC23
   Led1_Pin : constant := 2 ** 20; -- PC20
   Led2_Pin : constant := 2 ** 16; -- PA16
   Led3_Pin : constant := 2 ** 22; -- PC22;

   But_Pin  : constant := 2 ** 2;  -- PA2
   But1_Pin : constant := 2 ** 0;  -- PA0
   But2_Pin : constant := 2 ** 29; -- PC29
   But3_Pin : constant := 2 ** 30; -- PC30

   procedure Wait is
   begin
      for I in 1 .. 16#ffff# loop
         null;
      end loop;
   end Wait;
begin
   --  Enable clock for GPIO-A and GPIO-C

   PMC.PCER0 := 2 ** PIOA_ID + 2 ** PIOC_ID;

   --  Configure Led
   PIOC.PER := Led_Pin + Led1_Pin + Led3_Pin + But2_Pin + But3_Pin;
   PIOC.OER := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.CODR := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.MDDR := Led_Pin + Led1_Pin + Led3_Pin;
   PIOC.PUER := But2_Pin + But3_Pin;

   PIOA.PER := Led2_Pin + But_Pin + But1_Pin;
   PIOA.OER := Led2_Pin;
   PIOA.CODR := Led2_Pin;
   PIOA.MDDR := Led2_Pin;
   PIOA.PUER := But_Pin + But1_Pin;

   loop
      --  Off

      if (PIOA.PDSR and But_Pin) /= 0 then
         PIOC.SODR := Led_Pin;
      end if;
      PIOA.SODR := Led2_Pin;
      PIOC.CODR := Led1_Pin + Led3_Pin;
      Wait;

      --  On
      if (PIOA.PDSR and But_Pin) /= 0 then
         PIOC.CODR := Led_Pin;
      end if;
      PIOA.CODR := Led2_Pin;
      PIOC.SODR := Led1_Pin + Led3_Pin;
      Wait;
   end loop;
end Leds;
