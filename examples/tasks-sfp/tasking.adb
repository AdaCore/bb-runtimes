------------------------------------------------------------------------------
--                                                                          --
--                  OPEN RAVENSCAR VALIDATION TEST SUITE                    --
--                                                                          --
--                                                                          --
--            Copyright (C) 1999-2000, C.A.S.A. - Space Division            --
--            Copyright (C) 2004, DIT-UPM                                   --
--            Copyright (C) 2004, The European Space Agency                 --
--            Copyright (C) 2004-2010, AdaCore                              --
--                                                                          --
-- The  Open  Ravenscar  Validation  Test Suite is  free  software; you can --
-- redistribute  it  and/or  modify  it under  terms  of  the  GNU  General --
-- Public  License as published  by  the Free Software  Foundation;  either --
-- version 2,  or (at your option)  any  later  version.  The test suite is --
-- distributed  in  the  hope  that  it  will  be  useful, but WITHOUT  ANY --
-- WARRANTY;  without  even  the  implied  warranty  of  MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details. You should have received a copy of the GNU  General Public --
-- License  distributed  with  this  test  suite; see file COPYING. If not, --
-- write to the Free  Software  Foundation,  59  Temple Place  -  Suite 30, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- The  original Validation Test Suite was developed by the Space Division  --
-- of Construcciones Aeronauticas S.A. (CASA).                              --
--                                                                          --
-- The Open Ravenscar Validation Test Suite was then evolved and maintained --
-- by DIT-UPM and AdaCore.                                                  --
--                                                                          --
-- The current version of the Open Ravenscar Validation Test Suite is being --
-- developed and maintained by AdaCore.                                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Tasking is

   procedure Wait_Forever is
   begin
      loop
         delay until Clock + Milliseconds (5_000);
      end loop;
   end Wait_Forever;

   List : array (1 .. 10) of Task_Control_Container;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
   begin
      Put_Line ("Unexpected exception");
   end Last_Chance_Handler;

   protected body Task_Ctrl is

      entry Wait when Barrier is
      begin
         Barrier := False;
      end Wait;

      procedure Start is
      begin
         Barrier := True;
      end Start;

   end Task_Ctrl;

   task body Task_Type is
   begin
      Wrapper.Controller.Start;
      Wrapper.Controller.Wait;
      Wait_Forever;
   end Task_Type;

   task body Test_Driver is
      Test_Duration : Time_Span := Milliseconds (5_000);
   begin
      Put_line ("Check task type and task array initialization " &
                  "reaches its finalization");

      delay until Clock + Test_Duration;
      Put_Line ("Done");
      Wait_Forever;
   end Test_Driver;
end Tasking;
