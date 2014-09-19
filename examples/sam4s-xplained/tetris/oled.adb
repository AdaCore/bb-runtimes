------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2014, AdaCore                      --
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
pragma Warnings (Off);
with System.SAM4S; use System.SAM4S;
pragma Warnings (On);
with Ada.Interrupts.Names;
with Ada.Real_Time;

package body Oled is
   --  Pin definition.
   Spi_DC_Pin_C    : constant := 2 ** 21; -- PC21
   Spi_Reset_Pin_C : constant := 2 ** 31; -- PC31
   Spi_MOSI_Pin_A  : constant := 2 ** 13; -- PA13
   Spi_SPCK_Pin_A  : constant := 2 ** 14; -- PA14
   Spi_NPC_Pin_A   : constant := 2 ** 10; -- PA10

   --  SSD1306 commands.
   package SSD1306 is
      pragma Warnings (Off, "*is not referenced");
      CMD_SET_LOW_COL                    : constant := 16#00#;
      CMD_SET_HIGH_COL                   : constant := 16#10#;
      CMD_SET_MEMORY_ADDRESSING_MODE     : constant := 16#20#;
      CMD_SET_COLUMN_ADDRESS             : constant := 16#21#;
      CMD_SET_PAGE_ADDRESS               : constant := 16#22#;
      CMD_SET_START_LINE                 : constant := 16#40#;
      CMD_SET_CONTRAST_CONTROL_FOR_BANK0 : constant := 16#81#;
      CMD_SET_CHARGE_PUMP_SETTING        : constant := 16#8D#;
      CMD_SET_SEGMENT_RE_MAP_COL0_SEG0   : constant := 16#A0#;
      CMD_SET_SEGMENT_RE_MAP_COL127_SEG0 : constant := 16#A1#;
      CMD_ENTIRE_DISPLAY_AND_GDDRAM_ON   : constant := 16#A4#;
      CMD_ENTIRE_DISPLAY_ON              : constant := 16#A5#;
      CMD_SET_NORMAL_DISPLAY             : constant := 16#A6#;
      CMD_SET_INVERSE_DISPLAY            : constant := 16#A7#;
      CMD_SET_MULTIPLEX_RATIO            : constant := 16#A8#;
      CMD_SET_DISPLAY_ON                 : constant := 16#AF#;
      CMD_SET_DISPLAY_OFF                : constant := 16#AE#;
      CMD_SET_PAGE_START_ADDRESS         : constant := 16#B0#;
      CMD_SET_COM_OUTPUT_SCAN_UP         : constant := 16#C0#;
      CMD_SET_COM_OUTPUT_SCAN_DOWN       : constant := 16#C8#;
      CMD_SET_DISPLAY_OFFSET             : constant := 16#D3#;
      CMD_SET_DISPLAY_CLOCK_DIVIDE_RATIO : constant := 16#D5#;
      CMD_SET_PRE_CHARGE_PERIOD          : constant := 16#D9#;
      CMD_SET_COM_PINS                   : constant := 16#DA#;
      CMD_SET_VCOMH_DESELECT_LEVEL       : constant := 16#DB#;
      CMD_NOP                            : constant := 16#E3#;

      CMD_SCROLL_H_RIGHT                  : constant := 16#26#;
      CMD_SCROLL_H_LEFT                   : constant := 16#27#;
      CMD_CONTINUOUS_SCROLL_V_AND_H_RIGHT : constant := 16#29#;
      CMD_CONTINUOUS_SCROLL_V_AND_H_LEFT  : constant := 16#2A#;
      CMD_DEACTIVATE_SCROLL               : constant := 16#2E#;
      CMD_ACTIVATE_SCROLL                : constant := 16#2F#;
      CMD_SET_VERTICAL_SCROLL_AREA       : constant := 16#A3#;
      pragma Warnings (On, "*is not referenced");
   end SSD1306;

   procedure Oled_Configure is
   begin
      --  Enable clock for SPI

      PMC.PMC_PCER0 := PMC.PMC_PCER0 + 2 ** SPI_ID;

      --  Configure SPI pins

      PIOC.PER := Spi_DC_Pin_C + Spi_Reset_Pin_C;
      PIOC.OER := Spi_DC_Pin_C + Spi_Reset_Pin_C;
      PIOC.CODR := Spi_DC_Pin_C + Spi_Reset_Pin_C;
      PIOC.MDDR := Spi_DC_Pin_C + Spi_Reset_Pin_C;
      PIOC.PUER := Spi_DC_Pin_C + Spi_Reset_Pin_C;

      PIOA.ODR := Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A;
      PIOA.CODR := Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A;
      PIOA.MDDR := Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A;
      PIOA.PUER := Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A;
      PIOA.ABCDSR1 := (PIOA.ABCDSR1 and not (Spi_MOSI_Pin_A + Spi_SPCK_Pin_A))
        or Spi_NPC_Pin_A;
      PIOA.ABCDSR2 := PIOA.ABCDSR2
        and not (Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A);
      PIOA.PDR := Spi_MOSI_Pin_A + Spi_SPCK_Pin_A + Spi_NPC_Pin_A;
   end Oled_Configure;

   procedure Oled_Reset
   is
      use Ada.Real_Time;
      Now : Time := Clock;
      Period_3us : constant Time_Span := Microseconds (3);
   begin
      --  Lower reset
      PIOC.CODR := Spi_Reset_Pin_C;
      Now := Now + Period_3us;
      delay until Now;

      --  Raise reset
      PIOC.SODR := Spi_Reset_Pin_C;
      Now := Now + Period_3us;
      delay until Now;
   end Oled_Reset;

   protected Spi_Prot is
      pragma Interrupt_Priority (System.Interrupt_Priority'First);

      procedure Write_Byte (Cmd : Unsigned_8);

      procedure Interrupt;
      pragma Attach_Handler (Interrupt, Ada.Interrupts.Names.SPI_Interrupt);

      entry Wait_Tx;
   private
      Tx_Done : Boolean := False;
   end Spi_Prot;

   protected body Spi_Prot is
      procedure Write_Byte (Cmd : Unsigned_8) is
      begin
         --  Set PCS #2
         SPI.SPI_MR := (SPI.SPI_MR and not SPI_MR.PCS_Mask)
           or (SPI_MR.PCS_Mask and not ((2 ** 2) * SPI_MR.PCS));

         --  Write cmd
         SPI.SPI_TDR := Word (Cmd) * SPI_TDR.TD;

         --  Enable TXEMPTY interrupt.
         SPI.SPI_IER := SPI_SR.TXEMPTY;
      end Write_Byte;

      procedure Interrupt is
      begin
         if (SPI.SPI_SR and SPI_SR.TXEMPTY) /= 0 then
            --  Disable TXEMPTY interrupt.
            SPI.SPI_IDR := SPI_SR.TXEMPTY;

            Tx_Done := True;
         end if;
      end Interrupt;

      entry Wait_Tx when Tx_Done is
      begin
         Tx_Done := False;

         --  Deselect device
         SPI.SPI_MR := SPI.SPI_MR or SPI_MR.PCS_Mask;

         --  Last transfer
         SPI.SPI_CR := SPI_CR.LASTXFER;
      end Wait_Tx;
   end Spi_Prot;

   procedure Spi_Init is
      Baudrate : constant := 200; -- 120_000_000 / 5_000_000;
   begin
      --  Reset SPI
      SPI.SPI_CR := SPI_CR.SWRST;

      --  Set mode register.
      --  Set master mode, disable mode fault, disable loopback, set chip
      --  select value, set fixed peripheral select, disable select decode.
      SPI.SPI_MR := (SPI.SPI_MR and not (SPI_MR.LLB or SPI_MR.PCS_Mask
                                           or SPI_MR.PS or SPI_MR.PCSDEC
                                           or SPI_MR.DLYBCS_Mask))
        or SPI_MR.MODFDIS or SPI_MR.MSTR or 0 * SPI_MR.DLYBCS;

      --  Set chip select register.
      SPI.SPI_CSR2 := 0 * SPI_CSR.DLYBCT or 0 * SPI_CSR.DLYBS
        or Baudrate * SPI_CSR.SCBR or (8 - 8) * SPI_CSR.BITS
        or SPI_CSR.CSAAT or 0 * SPI_CSR.CPOL or SPI_CSR.NCPHA;

      --  enable
      SPI.SPI_CR := SPI_CR.SPIEN;
   end Spi_Init;

   procedure Ssd1306_Write (Cmd : Unsigned_8) is
   begin
      Spi_Prot.Write_Byte (Cmd);

      Spi_Prot.Wait_Tx;
   end Ssd1306_Write;

   procedure Ssd1306_Cmd (Cmd : Unsigned_8) is
   begin
      --  Select cmd
      PIOC.CODR := Spi_DC_Pin_C;

      Ssd1306_Write (Cmd);
   end Ssd1306_Cmd;

   procedure Ssd1306_Data (Cmd : Unsigned_8) is
   begin
      --  Select data
      PIOC.SODR := Spi_DC_Pin_C;

      Ssd1306_Write (Cmd);
   end Ssd1306_Data;

   procedure Ssd1306_Init is
   begin
      --  1/32 duty
      Ssd1306_Cmd (SSD1306.CMD_SET_MULTIPLEX_RATIO);
      Ssd1306_Cmd (31);

      --  Set ram counter.
      Ssd1306_Cmd (SSD1306.CMD_SET_DISPLAY_OFFSET);
      Ssd1306_Cmd (0);

      --  Set start line.
      Ssd1306_Cmd (SSD1306.CMD_SET_START_LINE + 0);

      Ssd1306_Cmd (SSD1306.CMD_SET_SEGMENT_RE_MAP_COL127_SEG0);
      Ssd1306_Cmd (SSD1306.CMD_SET_COM_OUTPUT_SCAN_DOWN);

      Ssd1306_Cmd (SSD1306.CMD_SET_COM_PINS);
      Ssd1306_Cmd (2);

      --  Set contrast
      Ssd1306_Cmd (SSD1306.CMD_SET_CONTRAST_CONTROL_FOR_BANK0);
      Ssd1306_Cmd (16#8f#);

      Ssd1306_Cmd (SSD1306.CMD_ENTIRE_DISPLAY_AND_GDDRAM_ON);
      Ssd1306_Cmd (SSD1306.CMD_SET_NORMAL_DISPLAY);

      --  Display clock divide ratio
      Ssd1306_Cmd (SSD1306.CMD_SET_DISPLAY_CLOCK_DIVIDE_RATIO);
      Ssd1306_Cmd (16#80#);

      --  Enable charge pump regulator
      Ssd1306_Cmd (SSD1306.CMD_SET_CHARGE_PUMP_SETTING);
      Ssd1306_Cmd (16#14#);

      --  VCOMH deselect level
      Ssd1306_Cmd (SSD1306.CMD_SET_VCOMH_DESELECT_LEVEL);
      Ssd1306_Cmd (16#40#);

      --  Pre-charge: 15 clocks, discharge: 1 clock.
      Ssd1306_Cmd (SSD1306.CMD_SET_PRE_CHARGE_PERIOD);
      Ssd1306_Cmd (16#f1#);

      Ssd1306_Cmd (SSD1306.CMD_SET_DISPLAY_ON);
   end Ssd1306_Init;

   procedure Oled_Set_Page (Page : Unsigned_8) is
   begin
      Ssd1306_Cmd (SSD1306.CMD_SET_PAGE_START_ADDRESS + (Page and 16#f#));
      Ssd1306_Cmd (SSD1306.CMD_SET_HIGH_COL + 0);
      Ssd1306_Cmd (SSD1306.CMD_SET_LOW_COL + 0);
   end Oled_Set_Page;

   procedure Ssd1306_Clear is
   begin
      for Page in Unsigned_8 range 0 .. 3 loop
         Oled_Set_Page (Page);
         for B in Unsigned_8 range 0 .. 127 loop
            Ssd1306_Data (0);
         end loop;
      end loop;
   end Ssd1306_Clear;

   procedure Oled_Clear renames Ssd1306_Clear;

   procedure Oled_Draw (B : Unsigned_8) renames Ssd1306_Data;

   procedure Oled_Init is
   begin
      Oled_Configure;

      Oled_Reset;

      --  Init spi master
      Spi_Init;

      Ssd1306_Init;
      Ssd1306_Clear;
   end Oled_Init;
end Oled;
