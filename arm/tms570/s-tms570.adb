------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      Copyright (C) 2016-2020, AdaCore                    --
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

with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with Interfaces.ARM_V7AR;     use Interfaces.ARM_V7AR;
with System;                  use System;

package body System.TMS570 is

   --  MPU table constants
   pragma Warnings (Off, "*is not referenced");
   Size_32B   : constant Unsigned_32 := 2#00100_0#; --  0x04 << 1
   Size_64B   : constant Unsigned_32 := 2#00101_0#; --  0x05 << 1
   Size_126B  : constant Unsigned_32 := 2#00110_0#; --  0x06 << 1
   Size_256B  : constant Unsigned_32 := 2#00111_0#; --  0x07 << 1
   Size_512B  : constant Unsigned_32 := 2#01000_0#; --  0x08 << 1
   Size_1kB   : constant Unsigned_32 := 2#01001_0#; --  0x09 << 1
   Size_2kB   : constant Unsigned_32 := 2#01010_0#; --  0x0A << 1
   Size_4kB   : constant Unsigned_32 := 2#01011_0#; --  0x0B << 1
   Size_8kB   : constant Unsigned_32 := 2#01100_0#; --  0x0C << 1
   Size_16kB  : constant Unsigned_32 := 2#01101_0#; --  0x0D << 1
   Size_32kB  : constant Unsigned_32 := 2#01110_0#; --  0x0E << 1
   Size_64kB  : constant Unsigned_32 := 2#01111_0#; --  0x0F << 1
   Size_128kB : constant Unsigned_32 := 2#10000_0#; --  0x10 << 1
   Size_256kB : constant Unsigned_32 := 2#10001_0#; --  0x11 << 1
   Size_512kB : constant Unsigned_32 := 2#10010_0#; --  0x12 << 1
   Size_1MB   : constant Unsigned_32 := 2#10011_0#; --  0x13 << 1
   Size_2MB   : constant Unsigned_32 := 2#10100_0#; --  0x14 << 1
   Size_4MB   : constant Unsigned_32 := 2#10101_0#; --  0x15 << 1
   Size_8MB   : constant Unsigned_32 := 2#10110_0#; --  0x16 << 1
   Size_16MB  : constant Unsigned_32 := 2#10111_0#; --  0x17 << 1
   Size_32MB  : constant Unsigned_32 := 2#11000_0#; --  0x18 << 1
   Size_64MB  : constant Unsigned_32 := 2#11001_0#; --  0x19 << 1
   Size_128MB : constant Unsigned_32 := 2#11010_0#; --  0x1A << 1
   Size_256MB : constant Unsigned_32 := 2#11011_0#; --  0x1B << 1
   Size_512MB : constant Unsigned_32 := 2#11100_0#; --  0x1C << 1
   Size_1GB   : constant Unsigned_32 := 2#11101_0#; --  0x1D << 1
   Size_2GB   : constant Unsigned_32 := 2#11110_0#; --  0x1E << 1
   Size_4GB   : constant Unsigned_32 := 2#11111_0#; --  0x1F << 1

   XN         : constant Unsigned_32 := 16#1000#;
   AP_NA_NA   : constant Unsigned_32 := 16#000#;
   AP_RW_NA   : constant Unsigned_32 := 16#100#;
   AP_RW_RO   : constant Unsigned_32 := 16#200#;
   AP_RW_RW   : constant Unsigned_32 := 16#300#;
   AP_RO_NA   : constant Unsigned_32 := 16#500#;
   AP_RO_RO   : constant Unsigned_32 := 16#600#;

   STRONGLY_ORDERED : constant Unsigned_32 := 16#00#; --  TEX: 0, C: 0, B: 0
   SHAREABLE_DEVICE : constant Unsigned_32 := 16#01#; --  TEX: 0, C: 0, B: 1
   WT_NO_WA         : constant Unsigned_32 := 16#02#; --  TEX: 0, C: 1, B: 0
   WB_NO_WA         : constant Unsigned_32 := 16#03#; --  TEX: 0, C: 1, B: 1
   NO_CACHE         : constant Unsigned_32 := 16#08#; --  TEX: 1, C: 0, B: 0
   WB_WA            : constant Unsigned_32 := 16#0B#; --  TEX: 1, C: 1, B: 1
   NON_SHAREABLE    : constant Unsigned_32 := 16#10#; --  TEX: 2, C: 0, B: 0
   SHARED           : constant Unsigned_32 := 16#04#;
   pragma Warnings (On, "*is not referenced");

   procedure System_Init;
   pragma Import (C, System_Init, "__gnat_system_init");
   --  Initializes clocks and peripherals. This part is MCU specific.
   --  Code generated using HalCoGen.get

   procedure Enable_Event_Bus_Export;
   --  Allows the CPU to signal any single-bit or double-bit errors
   --  detected by its ECC logic for accesses to program flash or data
   --  RAM

   procedure Enable_ECC;
   --  Enables Checksum Checks on RAM and FLASH.

   procedure Setup_MPU;
   --  Setup the Memory Protection Unit regions and enables it

   procedure Enable_Cache;
   --  Enables instruction and data cache

   -----------------------------
   -- Enable_Event_Bus_Export --
   -----------------------------

   procedure Enable_Event_Bus_Export
   is
   begin
      --  Set the X bit of the PMCR sys register
      CP15.Set_PMCR (CP15.Get_PMCR or 16#10#);
   end Enable_Event_Bus_Export;

   ---------------
   -- Setup_MPU --
   ---------------

   procedure Setup_MPU
   is
      SRAM_Start : Character;
      pragma Import (Asm, SRAM_Start, "__gnat_ram_start");
      SRAM_End : Character;
      pragma Import (Asm, SRAM_End, "__gnat_ram_end");

      function As_Int is new Ada.Unchecked_Conversion
        (System.Address, Unsigned_32);

      SRAM_At_0 : constant Boolean := SRAM_Start'Address = System.Null_Address;

      SRAM_Size : constant Unsigned_32 :=
                    As_Int (SRAM_End'Address) - As_Int (SRAM_Start'Address);
      --  Cannot use System.Storage_Elements.Storage_Count here as
      --  System.Storage_Elements does not declare No_Elaboration_Code_All

      SCTLR   : Unsigned_32;
      MPUIR   : Unsigned_32;
      Num_Rgn : Unsigned_32;
      Policy  : constant Unsigned_32 := NO_CACHE;

   begin
      --  First disable the MPU
      SCTLR := CP15.Get_SCTLR;
      if (SCTLR and 1) = 1 then
         SCTLR := SCTLR and 16#FFFF_FFFE#; --  Clear the MPU Enable bit
         Barriers.DSB;
         CP15.Set_SCTLR (SCTLR);
         Barriers.ISB;
      end if;

      --  Disable background region for MPU
      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR and (not (2 ** 17));
      CP15.Set_SCTLR (SCTLR);

      --  Now will the MPU table

      MPUIR := CP15.Get_MPUIR;
      Num_Rgn := Shift_Right (MPUIR and 16#FF00#, 8);

      --  Region 1: background region
      CP15.Set_MPU_Region_Number (0);
      CP15.Set_MPU_Region_Base_Address    (16#0000_0000#);
      CP15.Set_MPU_Region_Size_And_Enable
        (Shift_Left (16#FF#, 8) or Size_4GB or 1);
      CP15.Set_MPU_Region_Access_Control  (XN or AP_NA_NA or Policy);

      --  Region 2: FLASH @ 0x0, or SRAM @ 0x0 if mem swapped
      CP15.Set_MPU_Region_Number (1);
      CP15.Set_MPU_Region_Base_Address (16#0000_0000#);

      if SRAM_At_0 then
         declare
            Sz   : Unsigned_32;
         begin
            --  Supported configurations: 256kB SRAM, 512kB SRAM, or 16MB RAM
            --  at address 0x0.
            if SRAM_Size <= 256 * 1024 then
               Sz := Size_256kB;
            elsif SRAM_Size <= 512 * 1024 then
               Sz := Size_512kB;
            else
               Sz := Size_16MB;
            end if;

            CP15.Set_MPU_Region_Size_And_Enable (Sz or 1);
         end;

         CP15.Set_MPU_Region_Access_Control  (AP_RW_RW or Policy);

      else
         CP15.Set_MPU_Region_Size_And_Enable (Size_4MB or 1);
         CP15.Set_MPU_Region_Access_Control  (AP_RO_RO or Policy);
      end if;

      --  Region 3: SRAM @ 0x0800_0000 or FLASH is mem swapped
      CP15.Set_MPU_Region_Number (2);
      CP15.Set_MPU_Region_Base_Address (16#0800_0000#);

      if SRAM_At_0 then
         --  FLASH Region
         --  Only 512kB of FLASH is accessible in this case
         CP15.Set_MPU_Region_Size_And_Enable (Size_512kB or 1);
         CP15.Set_MPU_Region_Access_Control  (AP_RO_RO or Policy);

      else
         declare
            Sz   : Unsigned_32;
         begin
            --  Supported configurations: 256kB or 512kB SRAM.
            if SRAM_Size <= 256 * 1024 then
               Sz := Size_256kB;
            else
               Sz := Size_512kB;
            end if;

            CP15.Set_MPU_Region_Size_And_Enable (Sz or 1);
         end;

         CP15.Set_MPU_Region_Access_Control  (AP_RW_RW or Policy);
      end if;

      --  Region 4: Async RAM
      CP15.Set_MPU_Region_Number (3);
      CP15.Set_MPU_Region_Base_Address    (16#6000_0000#);
      CP15.Set_MPU_Region_Size_And_Enable (Size_64MB or 1);
      CP15.Set_MPU_Region_Access_Control  (AP_RW_RW or STRONGLY_ORDERED);

      --  Region 5: SDRAM
      CP15.Set_MPU_Region_Number (4);
      CP15.Set_MPU_Region_Base_Address    (16#8000_0000#);
      CP15.Set_MPU_Region_Size_And_Enable (Size_128MB or 1);
      CP15.Set_MPU_Region_Access_Control  (AP_RW_RW or Policy or SHARED);

      --  Region 6: 6MB Flash OTP, ECC, EEPROM Bank
      CP15.Set_MPU_Region_Number (5);
      CP15.Set_MPU_Region_Base_Address    (16#F000_0000#);
      --  Disable sub-regions 7 and 8
      CP15.Set_MPU_Region_Size_And_Enable (16#C000# or Size_8MB or 1);
      CP15.Set_MPU_Region_Access_Control  (XN or AP_RW_RW or NO_CACHE);

      --  Region 7: 16MB peripheral segment 2
      CP15.Set_MPU_Region_Number (6);
      CP15.Set_MPU_Region_Base_Address    (16#FC00_0000#);
      CP15.Set_MPU_Region_Size_And_Enable (Size_16MB or 1);
      CP15.Set_MPU_Region_Access_Control  (XN or AP_RW_RW or NON_SHAREABLE);

      --  Region 8: 512B accessible CRC module
      CP15.Set_MPU_Region_Number (7);
      CP15.Set_MPU_Region_Base_Address    (16#FE00_0000#);
      CP15.Set_MPU_Region_Size_And_Enable (Size_512B or 1);
      CP15.Set_MPU_Region_Access_Control  (XN or AP_RW_RW or NON_SHAREABLE);

      --  Region 9: 16MB peripheral segment 3, including sys regs
      CP15.Set_MPU_Region_Number (8);
      CP15.Set_MPU_Region_Base_Address    (16#FF00_0000#);
      CP15.Set_MPU_Region_Size_And_Enable (Size_16MB or 1);
      CP15.Set_MPU_Region_Access_Control  (XN or AP_RW_RW or NON_SHAREABLE);

      --  Disable the unused regions
      for J in 9 .. Num_Rgn loop
         CP15.Set_MPU_Region_Number (J);
         CP15.Set_MPU_Region_Base_Address    (16#0000_0000#);
         CP15.Set_MPU_Region_Size_And_Enable (Size_32B or 0);
         CP15.Set_MPU_Region_Access_Control  (XN);
      end loop;

      --  Enable background region for MPU
      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR or (2 ** 17);
      CP15.Set_SCTLR (SCTLR);

      --  Enable the MPU
      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR or 1;
      CP15.Set_SCTLR (SCTLR);
      Barriers.ISB;
   end Setup_MPU;

   ----------------
   -- Enable_ECC --
   ----------------

   procedure Enable_ECC
   is
      FEDACCTRL1 : Unsigned_32 with
        Volatile, Import, Address => System'To_Address (16#FFF8_7008#);
      MINITGCR   : Unsigned_32 with
        Volatile, Import, Address => System'To_Address (16#FFFF_FF5C#);
      MSIENA     : Unsigned_32 with
        Volatile, Import, Address => System'To_Address (16#FFFF_FF60#);
      MSTCGSTAT  : Unsigned_32 with
        Volatile, Import, Address => System'To_Address (16#FFFF_FF68#);
      ACTLR      : Unsigned_32;

   begin
      --  First enable response to ECC errors indicated by CPU for accesses to

      --  flash.
      --  FEDACCTRL1:
      --  EDACEN    [0-3]:   error detection and correction enabled
      --  EPEN      [8]:     error profiling is disabled
      --  EZFEN     [9]:     event on zeros fail enable
      --  EOFEN     [10]:    event on ones fail enable
      --  EDACMODE  [16-19]: error correction mode selection as uncorrectable
      --                     errors
      --  SUSP_IGNR [24]:    cpu suspend signal blocks error bits setting and
      --                     un-freezing
      FEDACCTRL1 := 16#000a_060A#;

      --  Initialize the CPU RAM ECC locations

      MINITGCR   := 16#A#; --  enable global memroy hw initialization
      MSIENA     := 16#1#; --  enable auto-hw init of SRAM
      loop
         --  check the MINIDONE bit
         exit when (MSTCGSTAT and 16#100#) /= 0;
      end loop;
      MINITGCR   := 16#5#; --  remove the key from the hw initialization lock

      --  Enable CPU ECC checking for flash accesses
      ACTLR := CP15.Get_ACTLR;
      ACTLR := ACTLR or 16#C000000#; --  DICDI and DIB2DI bits
      Barriers.DMB;
      CP15.Set_ACTLR (ACTLR);
      Barriers.ISB;
   end Enable_ECC;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache
   is
      ACTLR : Unsigned_32;
      SCTLR : Unsigned_32;
   begin
      ACTLR := CP15.Get_ACTLR;
      --  Clear bit 5 (enable ECC)
      ACTLR := ACTLR and not (2 ** 5);
      CP15.Set_ACTLR (ACTLR);

      SCTLR := CP15.Get_SCTLR;
      SCTLR := SCTLR or (2 ** 2) or (2 ** 12); --  resp D and I bits
      Barriers.DSB;
      Cache.Invalidate_DCache;
      Barriers.DSB;
      Cache.Invalidate_ICache;
      Barriers.DSB;
      CP15.Set_SCTLR (SCTLR);
      Barriers.ISB;
   end Enable_Cache;

   ----------------
   -- Board_Init --
   ----------------

   procedure Board_Init
   is
      SYSESR : Unsigned_32
        with Volatile, Import, Address => System'To_Address (16#FFFF_FFe4#);
   begin
      --  Check reset condition at startup
      if SYSESR = 0 then
         --  Reset condition is 0: the boot has already been taken care of, so
         --  let's just return.
         return;

      elsif (SYSESR and 16#8800#) /= 0 then
         --  Power-on reset or Debug reset:

         --  do a full system init first
         System_Init;

         Enable_Event_Bus_Export;
         Enable_ECC;

         --  Finally clear the reset flag
         SYSESR := 16#8800#;

      elsif (SYSESR and 16#20#) /= 0 then
         --  CPU reset:

         --  do a full system init first
         System_Init;

         Enable_Event_Bus_Export;

         --  Note: leave the ECC settings for RAM and FLASH as those have
         --  not changed after a CPU reset

         SYSESR := 16#20#;
      end if;

      Setup_MPU;
      Enable_Cache;
   end Board_Init;

end System.TMS570;
