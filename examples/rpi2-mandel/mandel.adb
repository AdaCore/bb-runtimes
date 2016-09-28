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

with Ada.Text_IO; use Ada.Text_IO;
with Video; use Video;
with Interfaces; use Interfaces;

procedure Mandel is
   --  Initial picture
   X0_Min : constant Float := -2.0;
   X0_Max : constant Float := 0.7;

   Y0_Min : constant Float := -1.0;
   Y0_Max : constant Float := 1.0;

   --  Current picture
   X_Min, X_Max : Float;
   Y_Min, Y_Max : Float;

   --  Per pixel.
   Xstep, Ystep : Float;

   Max_Iter : constant Natural := 256;
   procedure Draw_Mandelbrot
   is
      X0, Y0 : Float;
      X, Y : Float;
      Xn, Yn : Float;

      Nbr_Iter : Natural;
      Col : Unsigned_32;
   begin
      Xstep := (X_Max - X_Min) / Float (Width);
      Ystep := (Y_Max - Y_Min) / Float (Height);

      for Pix_X in 0 .. Width - 1 loop
         X0 := X_Min + Float (Pix_X) * Xstep;
         for Pix_Y in 0 .. Height - 1 loop
            Y0 := Y_Min + Float (Pix_Y) * Ystep;

            Nbr_Iter := 0;
            X := X0;
            Y := Y0;
            while X * X + Y * Y < 4.0 and then Nbr_Iter < Max_Iter loop
               Xn := X * X - Y * Y;
               Yn := 2.0 * X * Y;
               X := Xn + X0;
               Y := Yn + Y0;
               Nbr_Iter := Nbr_Iter + 1;
            end loop;

            Col := 16#ff_000000#
              + Unsigned_32 (16#00_7fffff# * Nbr_Iter / Max_Iter);
            Fb (Pix_Y, Pix_X) := Col;
         end loop;
      end loop;
   end Draw_Mandelbrot;
begin
   Init_Video;

   X_Min := X0_Min;
   X_Max := X0_Max;
   Y_Min := Y0_Min;
   Y_Max := Y0_Max;

   Draw_Mandelbrot;

   Put_Line ("It's done");
   for I in 1 .. 64 loop
      Put ('.');
   end loop;
   loop
      null;
   end loop;
end Mandel;
