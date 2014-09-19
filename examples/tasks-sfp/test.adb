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
--
--  RF01:Task types and objects at the library level
--
--  This test creates an array of 10 tasks and verifies that they execute
--  correctly.
--
------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Tasking; use Tasking;
with System;

procedure Test is
   pragma Priority (System.Priority'First);
begin
   Wait_Forever;
end Test;
