------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 S Y S T E M . T A S K I N G . Q U E U I N G              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--         Copyright (C) 1992-2016, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  Queue implementation for extended ravenscar runtime

with System.Tasking.Protected_Objects.Entries;

package System.Tasking.Queuing is

   package POE renames System.Tasking.Protected_Objects.Entries;

   procedure Enqueue (E : in out Entry_Queue; Call : Entry_Call_Link);
   --  Enqueue Call at the end of entry_queue E

   function Count_Waiting (E : Entry_Queue) return Natural;
   --  Return number of calls on the waiting queue of E

   procedure Select_Protected_Entry_Call
     (Self_ID : Task_Id;
      Object  : POE.Protection_Entries_Access;
      Call    : out Entry_Call_Link);
   --  Select an entry of a protected object

end System.Tasking.Queuing;
