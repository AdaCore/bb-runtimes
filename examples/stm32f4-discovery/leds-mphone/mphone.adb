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

with Ada.Interrupts.Names;

package body Mphone is
   protected Input is
      pragma Interrupt_Priority;

      procedure Init;

      entry Wait_Spd_Sample (N : out Natural);
   private
      procedure Handler;
      pragma Attach_Handler (Handler, Ada.Interrupts.Names.SPI2_Interrupt);
      --  Interrupt handler

      Ready : Boolean := False;

      Last_Spd : Natural;
      Spd : Natural;
      Spd_Count : Natural := 0;
   end Input;

   Sample_Spd : constant array (Sample_Type) of Natural :=
     (0  => 16 * 16, 1  => 15 * 15, 2  => 14 * 14, 3  => 13 * 13,
      4  => 12 * 12, 5  => 11 * 11, 6  => 10 * 10, 7  => 9 * 9,
      8  => 8 * 8,   9  => 7 * 7,   10 => 6 * 6,   11 => 5 * 5,
      12 => 4 * 4,   13 => 3 * 3,   14 => 2 * 2,   15 => 1 * 1,
      16 => 0 * 0,
      17 => 1 * 1,   18 => 2 * 2,   19 => 3 * 3,   20 => 4 * 4,
      21 => 5 * 5,   22 => 6 * 6,   23 => 7 * 7,   24 => 8 * 8,
      25 => 9 * 9,   26 => 10 * 10, 27 => 11 * 11, 28 => 12 * 12,
      29 => 13 * 13, 30 => 14 * 14, 31 => 15 * 15, 32 => 16 * 16);

   Spd_Divisor : constant := 160 * 16;

   protected body Input is
      procedure Init is
      begin
         SPI2.I2SCFGR := SPI2.I2SCFGR or SPI_I2SCFGR.I2SE;
         SPI2.CR2 := SPI_CR2.RXNEIE or SPI_CR2.ERRIE;
      end Init;

      entry Wait_Spd_Sample (N : out Natural) when Ready is
      begin
         N := Last_Spd;
         Ready := False;
      end Wait_Spd_Sample;

      procedure Handler is
         SR : constant Word := SPI2.SR;
         V : Word;
         Sample : Sample_Type;
         pragma Volatile (Sample);
      begin
         if (SR and SPI_SR.RXNE) /= 0 then
            --  Read two word (32 bits), LSB first
            V := SPI2.DR;
            V := V + SPI2.DR * 2**16;

            --  Sum by packets of 1 bit
            V := ((V / 2**1) and 16#5555_5555#) + (V and 16#5555_5555#);

            --  Sum by packets of 2 bits
            V := ((V / 2**2) and 16#3333_3333#) + (V and 16#3333_3333#);

            --  Sum by packets of 4 bits
            V := ((V / 2**4) and 16#0f0f_0f0f#) + (V and 16#0f0f_0f0f#);

            --  Sum by packets of 8 bits
            V := ((V / 2**8) and 16#00ff_00ff#) + (V and 16#00ff_00ff#);

            --  Sum by packets of 16 bits
            V := ((V / 2**16) and 16#0000_ffff#) + (V and 16#0000_ffff#);

            Sample := Sample_Type (V);
            Spd := Spd + Sample_Spd (Sample);
            Spd_Count := Spd_Count + 1;
            if Spd_Count = Spd_Divisor - 1 then
               Spd_Count := 0;
               Last_Spd := Spd / (Spd_Divisor / 64);
               Spd := 0;
               Ready := True;
            else
               Spd_Count := Spd_Count + 1;
            end if;
         end if;
      end Handler;
   end Input;

   function Get_Spd_Sample return Natural is
      Res : Natural;
   begin
      Input.Wait_Spd_Sample (Res);
      return Res;
   end Get_Spd_Sample;
begin
   --  Enable clock for GPIO-B (clk) and GPIO-C (pdm)

   RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOB or RCC_AHB1ENR_GPIOC;

   --  Configure PC3 and PB10
   declare
      use GPIO;
      V : Bits_8x4;
   begin
      GPIOB.MODER   (10) := Mode_AF;

      V := GPIOB.AFRH;
      V (10 - 8) := 5; -- I2S2_CLK (cf p61)
      GPIOB.AFRH := V;

      GPIOB.OTYPER  (10) := Type_PP;
      GPIOB.OSPEEDR (10) := Speed_50MHz;
      GPIOB.PUPDR   (10) := No_Pull;

      GPIOC.MODER   (3) := Mode_AF;
      GPIOC.AFRL    (3) := 5; -- I2S2_SD (cf p62)
      GPIOC.OTYPER  (3) := Type_PP;
      GPIOC.OSPEEDR (3) := Speed_50MHz;
      GPIOC.PUPDR   (3) := No_Pull;
   end;

   --  Enable SPI clock
   RCC.APB1ENR := RCC.APB1ENR or RCC_APB1ENR_SPI2;

   --  Init PLLI2S clock
   declare
      --  HSE = 8 Mhz, PLL_M = 8
      PLLI2S_N : constant := 192;
      PLLI2S_R : constant := 3;

      --   I2SCLK = HSE / PLL_M * PLLI2S_N / PLLI2S_R
      --   I2SCLK = 8 / 8 * 192 / 3 = 64 Mhz
   begin
      --  Select PLLI2S clock source
      RCC.CFGR := RCC.CFGR and not RCC_CFGR.I2SSRC_PCKIN;

      --  Configure
      RCC.PLLI2SCFGR := PLLI2S_R * 2**28 or PLLI2S_N * 2**6;

      --  Enable
      RCC.CR := RCC.CR or RCC_CR.PLLI2SON;

      --  Wait until ready
      loop
         exit when (RCC.CR and RCC_CR.PLLI2SRDY) /= 0;
      end loop;
   end;

   --  Init SPI
   SPI2.I2SCFGR := SPI_I2SCFGR.I2SMOD or SPI_I2SCFGR.I2SCFG_MasterRx
     or SPI_I2SCFGR.I2SSTD_LSB or SPI_I2SCFGR.CKPOL
     or SPI_I2SCFGR.DATLEN_32;

   --  Divisor = 62  -- 125
   SPI2.I2SPR := 62 or SPI_I2SPR.ODD;

   Input.Init;
end Mphone;
