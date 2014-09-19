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

with Oled; use Oled;
with Interfaces; use Interfaces;
with Lm3s8962; use Lm3s8962;

procedure Main is
   procedure Draw_Ball (X : Col_Type;
                        Y : Line_Type;
                        Show : Boolean := True) is
   begin
      if Show then
         Draw_Image (X, Y,
                     ((16#0f#, 16#f0#),
                      (16#ff#, 16#ff#),
                      (16#ff#, 16#ff#),
                      (16#0f#, 16#f0#)));
      else
         Draw_Image (X, Y,
                     ((16#00#, 16#00#),
                      (16#00#, 16#00#),
                      (16#00#, 16#00#),
                      (16#00#, 16#00#)));
      end if;
   end Draw_Ball;

   Bat_Size : constant := 16;

   procedure Draw_Bat (X : Col_Type;
                       Y : Line_Type;
                       Show : Boolean := True) is
      V : Unsigned_8;
   begin
      if Show then
         V := 16#f0#;
      else
         V := 16#00#;
      end if;
      Draw_Image (X, Y,
                  (1 .. Bat_Size => (1 => V)));
   end Draw_Bat;

   Ball_Size : constant := 4;
   X : Col_Type := Col_Type'Last / 2;
   Y : Line_Type := Line_Type'Last / 2;
   Inc_X : Integer := 1;
   Inc_Y : Integer := 1;

   Bat_X : constant Col_Type := 10;
   Bat_Y : Line_Type := Line_Type'Last / 2;

   SW_UP    : constant := 2#0001#;
   SW_DOWN  : constant := 2#0010#;
   SW_LEFT  : constant := 2#0100#;
   SW_RIGHT : constant := 2#1000#;

   Beep : Boolean;
begin
   Oled_Init;

   --  Power-on GPIO E
   RCGC2 := RCGC2 or 2#1_0000#;

   --  GPIO E as input
   GPIOE_DIR := 0;
   GPIOE_DEN := SW_UP or SW_DOWN or SW_LEFT or SW_RIGHT;
   GPIOE_PUR := SW_UP or SW_DOWN or SW_LEFT or SW_RIGHT;

   --  PWM
   RCGC0 := RCGC0 or 16#10_0000#;  -- Enable PWM module
   RCGC2 := RCGC2 or 2#100_0000#;  -- Enable GPIO G module
   GPIOG_AFSEL := GPIOG_AFSEL or 2#10#;  -- PWM1 output
   GPIOG_DEN := GPIOG_DEN or 2#10#;
   RCC := (RCC and not 16#001e_0000#) or 16#001e_0000#; -- PWMclk=SYSclk / 64
   PWM0CTL := 0;
   PWM0GENA := 0;
   PWM0GENB := 2#00_00_10_00_11_00#; --  1 on load, 0 when = A
   PWM0LOAD := (50_000_000 / 64) / 440;
   PWM0CMPA := PWM0LOAD / 2;
   PWM0CTL := 1; -- Enable
   PWMENABLE := 2#10#;

   loop
      Draw_Bat (Bat_X, Bat_Y);
      Draw_Ball (X, Y);
      for J in 0 .. 5 * 10000 loop
         null;
      end loop;
      Draw_Ball (X, Y, False);
      Draw_Bat (Bat_X, Bat_Y, False);

      Beep := False;
      if X + Inc_X + Ball_Size > Col_Type'Last then
         Inc_X := -1;
         Beep := True;
      elsif Inc_X < 0 and then X + Inc_X = Bat_X then
         if Y >= Bat_Y - Ball_Size / 2
           and then Y <= Bat_Y + Bat_Size + Ball_Size / 2
         then
            Inc_X := 1;
            Beep := True;
         else
            --  You loose
            null;
         end if;
      elsif X + Inc_X < 0 then
         Inc_X := 1;
         Beep := True;
      end if;
      if Y + Inc_Y + Ball_Size > Line_Type'Last then
         Inc_Y := -1;
         Beep := True;
      elsif Y + Inc_Y < 0 then
         Inc_Y := 1;
         Beep := True;
      end if;
      X := X + Inc_X;
      Y := Y + Inc_Y;
      if GPIOE_DATA (SW_UP) = 0 and then Bat_Y > 0 then
         Bat_Y := Bat_Y - 1;
      end if;
      if GPIOE_DATA (SW_DOWN) = 0
        and then Bat_Y + Bat_Size < Line_Type'Last
      then
         Bat_Y := Bat_Y + 1;
      end if;
      if Beep then
         PWMENABLE := 2#10#;
      else
         PWMENABLE := 0;
      end if;
   end loop;
end Main;
