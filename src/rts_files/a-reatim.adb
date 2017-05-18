------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for generic bare board
--  targets. Note that the operations here assume that Time is a 64-bit
--  unsigned integer and Time_Span is a 64-bit signed integer.

with System.Tasking;
with System.Task_Primitives.Operations;

with Ada.Unchecked_Conversion;

package body Ada.Real_Time with
  SPARK_Mode => Off
is
   pragma Suppress (Overflow_Check);
   --  This package has careful manual overflow checks, and unsuppresses them
   --  where appropriate. This default enables compilation with checks enabled
   --  on Ravenscar SFP, where 64-bit multiplication with overflow checking is
   --  not available.

   package OSI renames System.OS_Interface;
   subtype LLI is Long_Long_Integer;

   ------------------------------------------------------------
   -- Handling of Conversions Between Duration and Time_Span --
   ------------------------------------------------------------

   --  For the To_Duration and To_Time_Span conversion functions, we use the
   --  intermediate Integer representation of Duration (64-bit) to allow for
   --  simple Integer operations instead of Float. We take advantage of the
   --  fact that Duration is represented as an Integer with units of Small.
   --  Within these conversions we perform the range checks required by
   --  AI-00432 manually.

   --  Note that in the past, within To_Duration and To_Time_Span, we were
   --  first computing the conversion factor between Duration and Time_Span
   --  (10 ** 9 / Clock_Frecuency) and then we multiplied or divided by it. The
   --  advantage of this approach was that the operations were simple, and we
   --  limited a lot the number of occurrences of overflows, but the accuracy
   --  loss could be significant depending on the clock frequency. For example,
   --  with a clock frequency of 600 MHz the factor was 1.66, which was rounded
   --  to 1 (Integer), and hence with a 67% difference.

   --  We tried also to have a better tradeoff (Constraint_Error being raised
   --  when transforming very big values, but limiting a lot the loss of
   --  accuracy) using Clock_Frequency in MHz instead of Hz. Therefore, we
   --  multiplied first by 10 ** 3 (or Clock_Frequency / 10 ** 6 which is
   --  typically smaller than 1000), and hence overflow could occur only with
   --  really big values). The problem of this approach was that some processor
   --  frequencies may not be expressed in multiples of MHz (for example,
   --  33.3333 MHz). The approach finally followed is to do the operations
   --  "by hand" on the upper and the lower part of the 64-bit value. This is
   --  slightly heavier, but we can preserve the best accuracy and the lowest
   --  occurrence of overflows.

   pragma Compile_Time_Error
     (Duration'Size /= 64,
      "this version of Ada.Real_Time requires 64-bit Duration");

   -----------------------
   -- Local definitions --
   -----------------------

   type Uint_64 is mod 2 ** 64;
   --  Type used to represent intermediate results of arithmetic operations

   Max_Pos_Time_Span : constant := Uint_64 (Time_Span_Last);
   Max_Neg_Time_Span : constant := Uint_64 (2 ** 63);
   --  Absolute value of Time_Span_Last and Time_Span_First. Used in overflow
   --  checks. Note that we avoid using abs on Time_Span_First everywhere.

   -----------------------
   -- Local subprograms --
   -----------------------

   function Mul_Div (V : LLI; M : Natural; D : Positive) return LLI;
   --  Compute V * M / D where division rounds to the nearest integer, away
   --  from zero if exactly halfway between. If the result would overflow then
   --  Constraint_Error is raised.

   function Rounded_Div (L, R : LLI) return LLI;
   pragma Inline (Rounded_Div);
   --  Return L / R rounded to the nearest integer, away from zero if exactly
   --  halfway between; required to implement ARM D.8 (26). Assumes R > 0.

   function To_Duration is
     new Ada.Unchecked_Conversion (LLI, Duration);

   function To_Integer is
     new Ada.Unchecked_Conversion (Duration, LLI);

   function To_Integer is
     new Ada.Unchecked_Conversion (Time_Span, LLI);

   ---------------------
   -- Local constants --
   ---------------------

   Duration_Units : constant Positive := Positive (1.0 / Duration'Small);
   --  Number of units of Duration in one second. The result is correct (not
   --  rounded) as Duration'Small is 10.0**(-9).

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
      Is_Negative : constant Boolean :=
        (if Left > 0 then
            Right < 0
         elsif Left < 0
            then Right > 0
         else
            False);
      --  Sign of the result

      Max_Value : constant Uint_64 :=
        (if Is_Negative then
            Max_Neg_Time_Span
         else
            Max_Pos_Time_Span);
      --  Maximum absolute value that can be returned by the multiplication
      --  taking into account the sign of the operators.

      Abs_Left : constant Uint_64 :=
        (if Left = Time_Span_First then
            Max_Neg_Time_Span
         else
            Uint_64 (abs (Left)));
      --  Remove sign of left operator

      Abs_Right : constant Uint_64 := Uint_64 (abs (LLI (Right)));
      --  Remove sign of right operator

   begin
      --  Overflow check is performed by hand assuming that Time_Span is a
      --  64-bit signed integer. Otherwise these checks would need an
      --  intermediate type with more than 64-bit. The sign of the operators
      --  is removed to simplify the intermediate computation of the overflow
      --  check.

      if Abs_Right /= 0 and then Max_Value / Abs_Right < Abs_Left then
         raise Constraint_Error;
      else
         return Left * Time_Span (Right);
      end if;
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Right * Left;
   end "*";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks are performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64 bits.

      if Right >= 0
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (Right)
      then
         return Time (Uint_64 (Left) + Uint_64 (Right));

      --  The case of Right = Time_Span'First needs to be treated differently
      --  because the absolute value of -2 ** 63 is not within the range of
      --  Time_Span.

      elsif Right = Time_Span'First and then Left >= Max_Neg_Time_Span then
         return Time (Uint_64 (Left) - Max_Neg_Time_Span);

      elsif Right < 0 and then Right > Time_Span'First
        and then Left >= Time (abs (Right))
      then
         return Time (Uint_64 (Left) - Uint_64 (abs (Right)));

      else
         raise Constraint_Error;
      end if;
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (LLI (Left) + LLI (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Right >= 0 and then Left >= Time (Right) then
         return Time (Uint_64 (Left) - Uint_64 (Right));

      --  The case of Right = Time_Span'First needs to be treated differently
      --  because the absolute value of -2 ** 63 is not within the range of
      --  Time_Span.

      elsif Right = Time_Span'First
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Max_Neg_Time_Span
      then
         return Left + Time (Max_Neg_Time_Span);

      elsif Right < 0 and then Right > Time_Span'First
        and then Uint_64 (Time_Last) - Uint_64 (Left) >= Uint_64 (abs (Right))
      then
         return Left + Time (abs (Right));

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      --  Overflow checks must be performed by hand assuming that Time and
      --  Time_Span are 64-bit unsigned and signed integers respectively.
      --  Otherwise these checks would need an intermediate type with more
      --  than 64-bit.

      if Left >= Right
        and then Uint_64 (Left) - Uint_64 (Right) <= Max_Pos_Time_Span
      then
         return Time_Span (Uint_64 (Left) - Uint_64 (Right));

      elsif Left < Right
        and then Uint_64 (Right) - Uint_64 (Left) <= Max_Neg_Time_Span
      then
         return -1 - Time_Span (Uint_64 (Right) - Uint_64 (Left) - 1);

      else
         raise Constraint_Error;
      end if;
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (LLI (Left) - LLI (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (-LLI (Right));
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
      return Integer (LLI (Left) / LLI (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
      return Left / Time_Span (Right);
   end "/";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.Task_Primitives.Operations.Monotonic_Clock);
   end Clock;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (US) * LLI (OSI.Ticks_Per_Second), 1E6));
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (MS) * LLI (OSI.Ticks_Per_Second), 1E3));
   end Milliseconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
      Min_M : constant LLI := LLI'First / LLI (OSI.Ticks_Per_Second);
      Max_M : constant LLI := LLI'Last  / LLI (OSI.Ticks_Per_Second);
      --  Bounds for Sec_M. Note that we can't use unsuppress overflow checks,
      --  as this would require the use of arit64.

      Sec_M : constant LLI := LLI (M) * 60;
      --  M converted to seconds

   begin
      if Sec_M < Min_M or else Sec_M > Max_M then
         raise Constraint_Error;
      else
         return Time_Span (Sec_M * LLI (OSI.Ticks_Per_Second));
      end if;
   end Minutes;

   -------------
   -- Mul_Div --
   -------------

   function Mul_Div (V : LLI; M : Natural; D : Positive) return LLI is

      --  We first multiply V * M and then divide the result by D, while
      --  avoiding overflow in intermediate calculations and detecting it in
      --  the final result. To get the rounding to the nearest integer, away
      --  from zero if exactly halfway between two values, we add +/- D/2
      --  (depending on the sign on V) directly at the end of multiplication.
      --
      --  ----------------------------------------
      --  Multiplication (and rounding adjustment)
      --  ----------------------------------------
      --
      --  Since V is a signed 64-bit integer and M is signed (but non-negative)
      --  32-bit integer, their product may not fit in 64-bits. To avoid
      --  overflow we split V and into high and low parts
      --
      --    V_Hi = V  /  2 ** 32
      --    V_Lo = V rem 2 ** 32
      --
      --  where each part is either zero or has the sign of the dividend; thus
      --
      --    V = V_Hi * 2 ** 32 + V_Lo
      --
      --  In either case V_Hi and V_Lo are in range of 32-bit signed integer,
      --  yet stored in 64-bit signed variables. When multiplied by M, which is
      --  in range of 0 .. 2 ** 31 - 1, the results will still fit in 64-bit
      --  integer, even if we extend it by D/2 as required to implement
      --  rounding. We will get the value of V * M ± D/2 as low and high part:
      --
      --    (V * M ± D/2)_Lo = (V_Lo * M ± D/2) with carry zeroed
      --    (V * M ± D/2)_Hi = (V_Hi * M) with carry from (V_Lo * M ± D/2)
      --
      --  (carry flows only from low to high part), or mathematically speaking:
      --
      --    (V * M ± D/2)_Lo = (V * M ± D/2) rem 2 ** 32
      --    (V * M ± D/2)_Hi = (V * M ± D/2)  /  2 ** 32
      --
      --  and thus
      --
      --    V * M ± D/2 = (V * M ± D/2)_Hi * 2 ** 32 + (V * M ± D/2)_Lo
      --
      --  with signs just like described for V_Hi and V_Lo.
      --
      --  --------
      --  Division
      --  --------
      --
      --  The final result (V * M ± D/2) / D is computed as a high and low
      --  parts:
      --
      --    ((V * M ± D/2) / D)_Hi = (V * M ± D/2)_Hi / D
      --    ((V * M ± D/2) / D)_Lo =
      --        ((V * M ± D/2)_Lo + remainder from high part division) / D
      --
      --  (remainder flows only from high to low part, opposite to carry),
      --  or mathematically speaking:
      --
      --    ((V * M ± D/2) / D)_Hi = ((V * M ± D/2) / D)  /  2 ** 32
      --    ((V * M ± D/2) / D)_Lo = ((V * M ± D/2) / D) rem 2 ** 32
      --
      --  and thus
      --
      --    (V * M ± D/2) / D = ((V * M ± D/2) / D)_Hi * 2 ** 32
      --                      + ((V * M ± D/2) / D)_Lo
      --
      --  with signs just like described for V_Hi and V_Lo.
      --
      --  References: this calculation is partly inspired by Knuth's algorithm
      --  in TAoCP Vol.2, section 4.3.1, excercise 16. However, here it is
      --  adapted it for signed arithmetic; has no loop (since the input number
      --  has fixed width); and discard the remainder of the result.

      V_Hi : constant LLI := V  /  2 ** 32;
      V_Lo : constant LLI := V rem 2 ** 32;
      --  High and low parts of V

      V_M_Hi : LLI;
      V_M_Lo : LLI;
      --  High and low parts of V * M (+-) D / 2

      Result_Hi : LLI;
      --  High part of the result

      Result_Lo : LLI;
      --  Low part of the result

      Remainder : LLI;
      --  Remainder of the first division

   begin
      --  Multiply V * M and add/subtract D/2

      V_M_Lo := V_Lo * LLI (M) + (if V >= 0 then 1 else -1) * LLI (D / 2);
      V_M_Hi := V_Hi * LLI (M) + V_M_Lo / 2 ** 32;
      V_M_Lo := V_M_Lo rem 2 ** 32;

      --  First quotient

      Result_Hi := V_M_Hi / LLI (D);

      --  The final result would overflow

      if Result_Hi not in -(2 ** 31) .. 2 ** 31 - 1 then
         raise Constraint_Error;
      end if;

      Remainder := V_M_Hi rem LLI (D);
      Result_Hi := Result_Hi * 2 ** 32;

      --  Second quotient

      Result_Lo := (V_M_Lo + Remainder * 2 ** 32) / LLI (D);

      --  Combine low and high parts of the result

      return Result_Hi + Result_Lo;
   end Mul_Div;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return
        Time_Span (Rounded_Div (LLI (NS) * LLI (OSI.Ticks_Per_Second), 1E9));
   end Nanoseconds;

   -----------------
   -- Rounded_Div --
   -----------------

   function Rounded_Div (L, R : LLI) return LLI is
      Left : LLI;
   begin
      if L >= 0 then
         Left := L + R / 2;
      else
         Left := L - R / 2;
      end if;

      return Left / R;
   end Rounded_Div;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      --  Overflow can't happen (Ticks_Per_Second is Natural)

      return Time_Span (LLI (S) * LLI (OSI.Ticks_Per_Second));
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
      Res : constant Time := Time (OSI.Ticks_Per_Second);
   begin
      SC := Seconds_Count (T / Res);
      TS := T - Time (SC) * Res;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
      Res : constant Time := Time (OSI.Ticks_Per_Second);

   begin
      --  We want to return SC * Resolution + TS. To avoid spurious overflows
      --  in the intermediate result (SC * Resolution) we take advantage of the
      --  different signs in SC and TS (when that is the case).

      --  If signs of SC and TS are different then we avoid converting SC to
      --  Time (as we do in the else part). The reason for that is that SC
      --  converted to Time may overflow the range of Time, while the addition
      --  of SC plus TS does not overflow (because of their different signs).
      --  The approach is to first extract the number of seconds from TS, then
      --  add the result to SC, and finally include the remainder from TS.

      --  Note that SC is always nonnegative

      if TS < 0 then
         declare
            Seconds_From_Ts : constant Seconds_Count :=
              Seconds_Count (abs (TS / Time_Span (Res))) +
                (if TS rem Time_Span (Res) = 0 then 0 else 1);
            --  Absolute value of the number of seconds in TS. Round towards
            --  infinity so that Remainder_Ts is always positive.

            Remainder_Ts : constant Time :=
              TS + Time_Span (Seconds_From_Ts - 1) * Time_Span (Res) + Res;
            --  Remainder from TS that needs to be added to the result once
            --  we removed the number of seconds. Note that we do not add
            --  Time_Span (Seconds_From_Ts) * Time_Span (Res) directly with
            --  a single operation because for values of TS close to
            --  Time_Span_First this multiplication would overflow.

         begin
            --  Both operands in the inner subtraction are positive. Hence,
            --  there will be no positive overflow in SC - Seconds_From_Ts. If
            --  there is a negative overflow then the result of adding SC and
            --  TS would overflow anyway.

            if SC < Seconds_From_Ts
              or else Time_Last / Res < Time (SC - Seconds_From_Ts)
            then
               raise Constraint_Error;
            else
               return Time (SC - Seconds_From_Ts) * Res + Remainder_Ts;
            end if;
         end;

      --  SC and TS are nonnegative. Check whether Time (SC) * Res overflows

      elsif Time_Last / Res < Time (SC) then
         raise Constraint_Error;

      --  Both operands have the same sign, so we can convert SC into Time
      --  right away; if this conversion overflows then the result of adding SC
      --  and TS would overflow anyway (so we would just be detecting the
      --  overflow a bit earlier).

      else
         return Time (SC) * Res + TS;
      end if;
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return
        To_Duration
          (Mul_Div (To_Integer (TS), Duration_Units, OSI.Ticks_Per_Second));
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return
        Time_Span
          (Mul_Div (To_Integer (D), OSI.Ticks_Per_Second, Duration_Units));
   end To_Time_Span;

begin
   --  Ensure that the tasking run time is initialized when using clock and/or
   --  delay operations. The initialization routine has the required machinery
   --  to prevent multiple calls to Initialize.

   System.Tasking.Initialize;
end Ada.Real_Time;
