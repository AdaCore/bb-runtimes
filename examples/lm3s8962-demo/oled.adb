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
with Lm3s8962; use Lm3s8962;

package body Oled is
   PORT_OLED_CS  : constant := 2#0000_1000#; -- GPIOA3
   PORT_OLED_DC  : constant := 2#0100_0000#; -- GPIOA6
   PORT_OLED_15V : constant := 2#1000_0000#; -- GPIOA7

   procedure Oled_Init is
   begin
      --  Enable SSI0, GPIO port A (for CS, D/C and +15V)
      RCGC2 := RCGC2 or 16#1#;
      RCGC1 := RCGC1 or 16#10#;

      --  Configure SSI0 CLK, TX and RX for SSI, /CS, D/C, +15V
      GPIOA_AFSEL := (GPIOA_AFSEL or 2#110100#)
        and not (PORT_OLED_CS or PORT_OLED_15V or PORT_OLED_DC);
      GPIOA_DEN := GPIOA_DEN or 2#110100#
        or PORT_OLED_CS or PORT_OLED_15V or PORT_OLED_DC;
      GPIOA_DIR := GPIOA_DIR
        or PORT_OLED_CS or PORT_OLED_15V or PORT_OLED_DC;

      --  Enable power, no select
      GPIOA_DATA (PORT_OLED_15V + PORT_OLED_CS) :=
        PORT_OLED_15V or PORT_OLED_CS;

      --  Configure SSI0.
      SSICR1 := 2#000#; --  Master, disable
      SSICPSR := 10; -- 10 for 1 Mhz, 20 for 500 Khz
      SSICR0 := 16#4_C_7#; --  SCR=4, SPH=1, SPO=1, FRF=SPI, DSS=8bit
      SSICR1 := 2#010#; --  Master, Enable

      Oled_Cmd ((16#fd#, 16#12#, 16#e3#,  --  Unlock + Nop
                 16#ae#,         16#e3#,  --  Display off, sleep + Nop
                 16#94#, 16#00#, 16#e3#,  --  Icon off + Nop
                 16#a8#, 95,     16#e3#,  --  Multiplex = 95 + 1 + Nop
                 16#81#, 16#b7#, 16#e3#,  --  Contrast
                 16#82#, 16#3f#, 16#e3#,  --  Pre-charge current + Nop
                 16#a0#, 16#52#, 16#e3#,  --  Init re-map
                 16#a1#, 0,      16#e3#,  --  Display start line
                 16#a2#, 0,      16#e3#,  --  Display offset
                 16#a4#,         16#e3#,  --  Normal display
                 16#b1#, 16#11#, 16#e3#,  --  Phase length
                 16#b2#, 16#23#, 16#e3#,  --  Frame frequency
                 16#b3#, 16#e2#, 16#e3#,  --  Front clock divider
                 16#b7#,         16#e3#,  --  Default gray scale table
                 16#bb#, 16#01#, 16#e3#,  --  Second pre-charge period
                 16#bc#, 16#3f#, 16#e3#,  --  First pre-charge voltage
                 16#af#,         16#e3#,  --  Display on
                 16#e3#));

      Oled_Clear;
   end Oled_Init;

   procedure Oled_Write (Byte : Unsigned_8) is
      Dummy : Unsigned_32;
   begin
      SSIDR := Unsigned_32 (Byte);
      --  Wait until empty
      while (SSISR and SSISR_TFE) = 0 loop
         null;
      end loop;
      --  Wait until receive not empty
      while (SSISR and SSISR_RNE) = 0 loop
         null;
      end loop;
      --  Drain RX fifo
      Dummy := SSIDR;
   end Oled_Write;

   procedure Oled_Write (Bytes : Byte_Array) is
   begin
      --  Write bytes.
      for I in Bytes'Range loop
         Oled_Write (Bytes (I));
      end loop;
   end Oled_Write;

   procedure Oled_Cmd (Bytes : Byte_Array) is
   begin
      --  Set D/C to command, enable /CS
      GPIOA_DATA (PORT_OLED_CS + PORT_OLED_DC) := 0;

      Oled_Write (Bytes);
   end Oled_Cmd;

   procedure Oled_Select_Data is
   begin
      --  Set D/C to data, enable /CS
      GPIOA_DATA (PORT_OLED_CS + PORT_OLED_DC) := PORT_OLED_DC;
   end Oled_Select_Data;

   procedure Oled_Data (Bytes : Byte_Array) is
   begin
      Oled_Select_Data;

      Oled_Write (Bytes);
   end Oled_Data;

   procedure Oled_Clear is
   begin
      Oled_Cmd ((16#15#, 0, 63,  --  Columns 0-63
                 16#75#, 0, 127, --  Lines 0-127
                 16#a0#, 16#52#, --  Horizontal inc
                 16#e3#));
      Oled_Select_Data;
      for I in 1 .. 96 loop
         for J in 1 .. 128 / 16 loop
            Oled_Write ((0 .. 7 => 0));
         end loop;
      end loop;
   end Oled_Clear;

   procedure Draw_Image (X : Col_Type; Y : Line_Type; Image : Image_Type)
   is
   begin
      --  Define box
      Oled_Cmd
        ((16#15#, Unsigned_8 (X), Unsigned_8 (X) + Image'Length (2) - 1,
          16#75#, Unsigned_8 (Y), Unsigned_8 (Y) + Image'Length (1) - 1,
          16#a0#, 16#52#, --  Horizontal inc
          16#e3#));
      Oled_Select_Data;
      for I in Image'Range (1) loop
         for J in Image'Range (2) loop
            Oled_Write (Image (I, J));
         end loop;
      end loop;
   end Draw_Image;

end Oled;
