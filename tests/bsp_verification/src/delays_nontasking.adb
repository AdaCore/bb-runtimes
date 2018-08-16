------------------------------------------------------------------------------
--                                                                          --
--                             GNAT BSP Test                                --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Report;
with Ada.Real_Time;  use Ada.Real_Time;

package body Delays_Nontasking is

   function Int_MS (TS : Time_Span) return Integer;

   ------------
   -- Int_MS --
   ------------

   function Int_MS (TS : Time_Span) return Integer is
     (TS / Milliseconds (1));

   ----------
   -- Test --
   ----------

   procedure Test is
   begin
      Report.Test ("c_rf04_04",
                   "Check delay-until statement semantics. " &
                   "Non tasking case");

      ---------------------------------------------

      declare   -- (A)
         X        : constant Time_Span := Milliseconds (1000);
         Old_Time : Time;
         Lapse    : Time_Span;
      begin     -- (A)
         loop
            Old_Time := Clock;
            delay until Clock + X;
            Lapse := Clock - Old_Time;
            exit;
         end loop;
         if Lapse < X then
            Report.Failed ("Delay did not lapse at least 1.0 " &
                             "seconds - (A)");
         else
            Report.Passed ("(A): Delay lapsed at least 1.0 seconds");
         end if;
      end;

      ---------------------------------------------

      declare   -- (B)
         Old_Time : Time;
         Lapse    : Time_Span;
      begin     -- (B)
         loop
            Old_Time := Clock;
            delay until Clock - Milliseconds (5_000);
            Lapse := Clock - Old_Time;
            exit;
         end loop;
         Report.Comment ("(B) - Negative delay lapsed for " &
                           Report.Image (Int_MS (Lapse)) & " Milliseconds");
      end;

      ---------------------------------------------

      declare   -- (C)
         X        : constant Time_Span := Milliseconds (0);
         Old_Time : Time;
         Lapse    : Time_Span;
      begin     -- (C)
         loop
            Old_Time := Clock;
            delay until Clock + X;
            Lapse := Clock - Old_Time;
            exit;
         end loop;
         Report.Comment ("(C) - Zero delay lapsed for " &
                           Report.Image (Int_MS (Lapse)) & " Milliseconds");
      end;

      ---------------------------------------------

      declare   -- (D)
         X        : constant Time_Span := To_Time_Span (Duration'Small);
         Old_Time : Time;
         Lapse    : Time_Span;
      begin     -- (D)
         loop
            Old_Time := Clock;
            delay until Clock + X;
            Lapse := Clock - Old_Time;
            exit;
         end loop;
         if Lapse < X then
            if Tick < To_Time_Span (Duration'Small) then
               Report.Failed ("Delay did not lapse at least " &
                                "Duration'small seconds - (D)");
            else
               Report.Comment ("TICK > Duration'Small so Delay in " &
                                 "'(D)' is not measurable");
            end if;
         else
            Report.Passed ("(D): Delay was at least Duration'Small seconds");
         end if;
      end;
   end Test;

end Delays_Nontasking;
