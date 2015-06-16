------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                    Copyright (C) 2013-2015, AdaCore                      --
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
pragma Warnings (Off);
with System.SAM4S; use System.SAM4S;
pragma Warnings (On);
with Ada.Real_Time; use Ada.Real_Time;
with Fonts;
with Leds; use Leds;
with Oled; use Oled;
with Tetris; use Tetris;

procedure Main is
   Now : Time := Clock;
   --  The current time.  We don't really use a ravenscar architecture: no
   --  periodic task.

   Slow_Fall_Period  : constant Time_Span := Milliseconds (1000 / 2 / 4);
   --  Quarter of time to wait before a piece falls.  We allow 2 rotations
   --  or moves per fall, and 4 falls per second.

   Quick_Fall_Period  : constant Time_Span := Milliseconds (50);
   --  Time to wait when a piece is dropped

   Blink_Period : constant Time_Span := Milliseconds (100);
   --  Blinking period for leds at the end of the game

   Fall_Period : Time_Span;

   Nbr_Pieces : Natural;
   --  Number of pieces fallen.  This is the score.

   procedure Draw_Board (With_Piece : Boolean);
   --  Draw on the OLED screen the board and the falling piece

   procedure Draw_Board (With_Piece : Boolean) is
      type Screen_Type is array (0 .. 3, Y_Coord) of Unsigned_8;
      Screen : Screen_Type := (others => (others => 0));

      X_Start : constant := 1;

      Zoom : constant := 3;
      Pixels_Per_Byte : constant := 8;
      subtype Screen_Col is Natural range 0 .. 31;

      procedure Set_Screen_Pixel (X : Screen_Col; Y : Y_Coord) is
         Mask  : constant Unsigned_8 := 2 ** (X mod Pixels_Per_Byte);
         Byte : Unsigned_8 renames Screen (X / Pixels_Per_Byte, Y);
      begin
         Byte := Byte or Mask;
      end Set_Screen_Pixel;

      procedure Set_Pixel (X : X_Coord; Y : Y_Coord) is
         X_Pos : constant Natural := 3 * (X - X_Coord'First);
      begin
         for I in 1 .. Zoom loop
            Set_Screen_Pixel (X_Pos + I - 1 + X_Start, Y);
         end loop;
      end Set_Pixel;

      Num : Natural;
      Digit : Natural;
   begin
      --  Vertical borders
      for Y in Y_Coord loop
         Set_Screen_Pixel (X_Start - 1, Y);
         Set_Screen_Pixel (X_Start + X_Size * Zoom, Y);
      end loop;

      --  The board
      for Y in Y_Coord loop
         for X in X_Coord loop
            if Cur_Board (Y)(X) /= Empty then
               Set_Pixel (X, Y);
            end if;
         end loop;
      end loop;

      --  The current piece
      if With_Piece then
         case Cur_Piece.S is
            when O =>
               Set_Pixel (Cur_Piece.X, Cur_Piece.Y);
               Set_Pixel (Cur_Piece.X, Cur_Piece.Y + 1);
               Set_Pixel (Cur_Piece.X + 1, Cur_Piece.Y);
               Set_Pixel (Cur_Piece.X + 1, Cur_Piece.Y + 1);

            when I =>
               for Y in I_Delta loop
                  for X in I_Delta loop
                     if Possible_I_Shapes (Cur_Piece.D) (Y, X) then
                        Set_Pixel (Cur_Piece.X + X, Cur_Piece.Y + Y);
                     end if;
                  end loop;
               end loop;

            when Three_Shape =>
               for Y in Three_Delta loop
                  for X in Three_Delta loop
                     if Possible_Three_Shapes
                       (Cur_Piece.S, Cur_Piece.D) (Y, X)
                     then
                        Set_Pixel (Cur_Piece.X + X, Cur_Piece.Y + Y);
                     end if;
                  end loop;
               end loop;
         end case;
      end if;

      --  Draw
      Num := Nbr_Pieces;
      for X in Screen'Range (1) loop
         Oled_Set_Page (Unsigned_8 (X));

         for Y in Screen'Range (2) loop
            for I in 1 .. Zoom loop
               Oled_Draw (Screen (X, Y));
            end loop;
         end loop;

         --  Border
         Oled_Draw (16#ff#);

         --  Score
         if Num /= 0 or X = Screen'First (1) then
            Oled_Draw (0);
            Oled_Draw (0);

            Digit := Num mod 10;
            for J in Fonts.Glyph_Type'Range loop
               Oled_Draw (Fonts.Digit_Font (Digit) (J));
            end loop;
            Num := Num / 10;
         end if;
      end loop;
   end Draw_Board;

   Rnd : Unsigned_32 := 137;
   --  Simple random generator.

   Success : Boolean;

   type Led_Step_Type is mod 3;
   Led_Step : Led_Step_Type;
   --  Led chaser
begin
   --  Init the devices
   Leds.Init;
   Oled_Init;

   --  Game loop
   Game : loop
      --  Clear board
      Cur_Board := (others => (others => Empty));
      Nbr_Pieces := 0;
      Oled_Clear;
      Led_Step := 0;

      Piece : loop
         --  Add a new piece
         Rnd := Rnd * 1103515245 + 12345;

         Cur_Piece := (S => Cell'Val (1 + ((Rnd / 65536) mod 7)),
                       D => North,
                       X => X_Size / 2,
                       Y => Y_Coord'First);

         Cur_State := Piece_Falling;

         --  Stop the game when the piece cannot appear
         exit Piece when not Valid_Configuration;

         --  Draw the board, including the new piece
         Draw_Board (True);

         Fall_Period := Slow_Fall_Period;

         --  Make the piece falling
         Fall : loop
            Set_Led (False);

            --  Led chaser
            Set_Led1 (Led_Step = 0);
            Set_Led2 (Led_Step = 1);
            Set_Led3 (Led_Step = 2);
            Led_Step := Led_Step + 1;

            --  At most 2 rotations or moves per fall
            for J in 1 .. 2 loop
               --  Check button
               if Button1_Pressed then
                  Do_Action (Move_Right, Success);
               elsif Button2_Pressed then
                  Do_Action (Turn_Clockwise, Success);
               elsif Button3_Pressed then
                  Do_Action (Move_Left, Success);
               elsif Button_Pressed then
                  Fall_Period := Quick_Fall_Period;
               end if;

               Now := Now + Fall_Period;
               delay until Now;

               Draw_Board (True);
            end loop;

            --  Fall and continue unless the piece hits the ground
            Do_Action (Move_Down, Success);
            exit Fall when not Success;

            Draw_Board (True);
         end loop Fall;

         --  Done with that piece
         Cur_State := Piece_Blocked;
         Nbr_Pieces := Nbr_Pieces + 1;

         Set_Led (True);
         Set_Led1 (True);
         Set_Led2 (True);
         Set_Led3 (True);

         Include_Piece_In_Board;
         Delete_Complete_Lines;

         Now := Now + Fall_Period;
         delay until Now;
      end loop Piece;

      --  Lost!
      declare
         Toggle : Boolean := True;
      begin
         loop
            --  Wait for any button
            exit when Button1_Pressed or Button2_Pressed or Button3_Pressed
              or Button_Pressed;

            --  Change the seed of the random generator
            Rnd := Rnd + 1;

            --  Toggle leds
            Set_Led (Toggle);
            Set_Led1 (not Toggle);
            Set_Led2 (Toggle);
            Set_Led3 (not Toggle);

            Now := Now + Blink_Period;
            delay until Now;

            Toggle := not Toggle;
         end loop;
      end;
   end loop Game;
end Main;
