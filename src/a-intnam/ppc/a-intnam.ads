------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . I N T E R R U P T S . N A M E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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
------------------------------------------------------------------------------

--  This is a generic definition only for the external interrupts

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   External_Interrupt_1          : constant Interrupt_ID := 1;
   External_Interrupt_1_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_2          : constant Interrupt_ID := 2;
   External_Interrupt_2_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_3          : constant Interrupt_ID := 3;
   External_Interrupt_3_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_4          : constant Interrupt_ID := 4;
   External_Interrupt_4_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_5          : constant Interrupt_ID := 5;
   External_Interrupt_5_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_6          : constant Interrupt_ID := 6;
   External_Interrupt_6_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_7          : constant Interrupt_ID := 7;
   External_Interrupt_7_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_8          : constant Interrupt_ID := 8;
   External_Interrupt_8_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_9          : constant Interrupt_ID := 9;
   External_Interrupt_9_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_10          : constant Interrupt_ID := 10;
   External_Interrupt_10_Priority : constant System.Interrupt_Priority :=
                                     System.Interrupt_Priority'First;

   External_Interrupt_11          : constant Interrupt_ID := 11;
   External_Interrupt_11_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

   External_Interrupt_12          : constant Interrupt_ID := 12;
   External_Interrupt_12_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

   External_Interrupt_13          : constant Interrupt_ID := 13;
   External_Interrupt_13_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

   External_Interrupt_14          : constant Interrupt_ID := 14;
   External_Interrupt_14_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

   External_Interrupt_15          : constant Interrupt_ID := 15;
   External_Interrupt_15_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

   External_Interrupt_16          : constant Interrupt_ID := 16;
   External_Interrupt_16_Priority : constant System.Interrupt_Priority :=
                                      System.Interrupt_Priority'First;

end Ada.Interrupts.Names;
