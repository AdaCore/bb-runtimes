------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with System.Multiprocessors; use System.Multiprocessors;

package body Mandel is
   --  Initial picture
   X0_Min : constant Float := -2.0;
   X0_Max : constant Float := 0.7;

   Y0_Min : constant Float := -1.0;
   Y0_Max : constant Float := 1.0;

   Max_Iter : constant Natural := 256;

   function Compute_Color (X0, Y0 : Float) return Unsigned_32
   is
      X, Y : Float;
      Xn, Yn : Float;
      Nbr_Iter : Natural;
      Col : Unsigned_32;
   begin
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

      if Nbr_Iter < 32 then
         Col := 16#ff_000000#
           + 16#00_01_00_00# * Unsigned_32 (16#ff# * Nbr_Iter / 32);
      elsif Nbr_Iter < Max_Iter then
         Col := 16#ff_ff0000#
           + 16#00_00_01_00#
           * Unsigned_32 (16#ff# * (Nbr_Iter - 32) / (Max_Iter - 32));
      else
         Col := 16#ff_000000#;
      end if;

      return Col;
   end Compute_Color;

   procedure Draw_Mandelbrot (X0, Y0 : Natural; Width, Height : Natural)
   is
      X, Y : Float;
      Col : Unsigned_32;

      X_Min, X_Max : Float;
      Y_Min, Y_Max : Float;

      --  Per pixel.
      Xstep, Ystep : Float;
   begin
      X_Min := X0_Min;
      X_Max := X0_Max;
      Y_Min := Y0_Min;
      Y_Max := Y0_Max;

      --  Adjust size
      declare
         S_Width : constant Float := Float (Width);
         S_Height : constant Float := Float (Height);
         M_Width : constant Float := X_Max - X_Min;
         M_Height : constant Float := Y_Max - Y_Min;
         Extra : Float;
      begin
         if S_Width * M_Height >= S_Height * M_Width then
            --  Screen is wider
            Extra := (S_Width * M_Height / S_Height) - M_Width;
            X_Min := X_Min - Extra / 2.0;
            X_Max := X_Max + Extra / 2.0;
         else
            --  Screen is taller
            Extra := (S_Height * M_Width / S_Width) - M_Height;
            Y_Min := Y_Min - Extra / 2.0;
            Y_Max := Y_Max + Extra / 2.0;
         end if;
      end;

      Xstep := (X_Max - X_Min) / Float (Width);
      Ystep := (Y_Max - Y_Min) / Float (Height);

      for S in reverse 1 .. 3 loop
         declare
            Size : constant Natural := 2 ** S;
         begin
            for Pix_X in 0 .. Width / Size - 1 loop
               X := X_Min + Float (Pix_X * Size) * Xstep;
               for Pix_Y in 0 .. Height / Size - 1 loop
                  Y := Y_Min + Float (Pix_Y * Size) * Ystep;

                  Col := Compute_Color (X, Y);

                  for Yo in 0 .. Size - 1 loop
                     for Xo in 0 .. Size - 1 loop
                        Fb (Y0 + Pix_Y * Size + Yo,
                            X0 + Pix_X * Size + Xo) := Col;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end;
      end loop;

      for Pix_X in 0 .. Width - 1 loop
         X := X_Min + Float (Pix_X) * Xstep;
         for Pix_Y in 0 .. Height - 1 loop
            Y := Y_Min + Float (Pix_Y) * Ystep;

            Col := Compute_Color (X, Y);
            Fb (Y0 + Pix_Y, X0 + Pix_X) := Col;
         end loop;
      end loop;
   end Draw_Mandelbrot;

   task type T (Id : Natural) is
      pragma Cpu (CPU (Id));
      pragma Priority (System.Default_Priority - 1);
   end T;

   task body T is
      T0 : Time;
   begin
      if Id = 2 then
         delay until Clock + Milliseconds (200);
      end if;

      loop
         Suspend_Until_True (Starts (Id));
         T0 := Clock;
         for I in 1 .. 5 loop
            Draw_Mandelbrot (Regions (Id).X, Regions (Id).Y,
                             Regions (Id).Width, Regions (Id).Height);
            delay until Time_First;
         end loop;
         Times (Id) := Clock - T0;
         Set_True (Wait (Id));
      end loop;
   end T;

   T1 : T (1);
   T2 : T (2);
   T3 : T (3);
   T4 : T (4);
end Mandel;
