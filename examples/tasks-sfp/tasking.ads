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

with System;
with Ada.Real_Time; use Ada.Real_Time;

package Tasking is

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   procedure Wait_Forever;

   type Task_Control_Container;

   protected type Task_Ctrl is
      entry Wait;
      procedure Start;
   private
      Barrier : Boolean := False;
   end Task_Ctrl;

   task type Task_Type (Wrapper : not null access Task_Control_Container);

   type Task_Control_Container is limited
      record
         Controller : Task_Ctrl;
         T          : Task_Type (Task_Control_Container'Access);
      end record;
   --  Associate a task to a protected object

   task Test_Driver is
      pragma Priority (System.Priority'Last);
   end Test_Driver;

end Tasking;
