------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 S Y S T E M . T A S K I N G . Q U E U I N G              --
--                                                                          --
--                                  B o d y                                 --
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

--  This version of the body implements queueing policy according to the policy
--  specified by the pragma Queuing_Policy. When no such pragma is specified
--  FIFO policy is used as default.

with System.Task_Primitives.Operations;
with System.Parameters;

package body System.Tasking.Queuing is

   use Parameters;
   use Task_Primitives.Operations;
   use Protected_Objects;
   use Protected_Objects.Entries;

   procedure Dequeue_Head
     (E    : in out Entry_Queue;
      Call : out Entry_Call_Link);
   --  Remove and return the head of entry_queue E

   function Head (E : Entry_Queue) return Entry_Call_Link;
   --  Return the head of entry_queue E

   --  Entry Queues implemented as doubly linked list

   function Check_Queue (E : Entry_Queue) return Boolean;
   --  Check the validity of E.
   --  Return True if E is valid, raise Assert_Failure if assertions are
   --  enabled and False otherwise.

   -----------------
   -- Check_Queue --
   -----------------

   function Check_Queue (E : Entry_Queue) return Boolean is
      Valid   : Boolean := True;
      C, Prev : Entry_Call_Link;

   begin
      if E.Head = null then
         if E.Tail /= null then
            Valid := False;
            pragma Assert (Valid);
         end if;
      else
         if E.Tail = null
           or else E.Tail.Next /= E.Head
         then
            Valid := False;
            pragma Assert (Valid);

         else
            C := E.Head;

            loop
               Prev := C;
               C := C.Next;

               if C = null then
                  Valid := False;
                  pragma Assert (Valid);
                  exit;
               end if;

               if Prev /= C.Prev then
                  Valid := False;
                  pragma Assert (Valid);
                  exit;
               end if;

               exit when C = E.Head;
            end loop;

            if Prev /= E.Tail then
               Valid := False;
               pragma Assert (Valid);
            end if;
         end if;
      end if;

      return Valid;
   end Check_Queue;

   -------------------
   -- Count_Waiting --
   -------------------

   --  Return number of calls on the waiting queue of E

   function Count_Waiting (E : Entry_Queue) return Natural is
      Count   : Natural;
      Temp    : Entry_Call_Link;

   begin
      pragma Assert (Check_Queue (E));

      Count := 0;

      if E.Head /= null then
         Temp := E.Head;

         loop
            Count := Count + 1;
            exit when E.Tail = Temp;
            Temp := Temp.Next;
         end loop;
      end if;

      return Count;
   end Count_Waiting;

   ------------------
   -- Dequeue_Head --
   ------------------

   --  Remove and return the head of entry_queue E

   procedure Dequeue_Head
     (E    : in out Entry_Queue;
      Call : out Entry_Call_Link)
   is
      Temp : Entry_Call_Link;

   begin
      pragma Assert (Check_Queue (E));
      --  If empty queue, return null pointer

      if E.Head = null then
         Call := null;
         return;
      end if;

      Temp := E.Head;

      --  Case of one element

      if E.Head = E.Tail then
         E.Head := null;
         E.Tail := null;

      --  More than one element

      else
         pragma Assert (Temp /= null);
         pragma Assert (Temp.Next /= null);
         pragma Assert (Temp.Prev /= null);

         E.Head := Temp.Next;
         Temp.Prev.Next := Temp.Next;
         Temp.Next.Prev := Temp.Prev;
      end if;

      --  Successfully dequeued

      Temp.Prev := null;
      Temp.Next := null;
      Call := Temp;
      pragma Assert (Check_Queue (E));
   end Dequeue_Head;

   -------------
   -- Enqueue --
   -------------

   --  Enqueue call at the end of entry_queue E, for FIFO queuing policy.
   --  Enqueue call priority ordered, FIFO at same priority level, for
   --  Priority queuing policy.

   procedure Enqueue (E : in out Entry_Queue; Call : Entry_Call_Link) is
   begin
      pragma Assert (Check_Queue (E));
      pragma Assert (Call /= null);

      --  FIFO Queuing

      if E.Head = null then
         E.Head := Call;
      else
         E.Tail.Next := Call;
         Call.Prev   := E.Tail;
      end if;

      E.Head.Prev := Call;
      E.Tail      := Call;
      Call.Next   := E.Head;
      pragma Assert (Check_Queue (E));
   end Enqueue;

   ----------
   -- Head --
   ----------

   --  Return the head of entry_queue E

   function Head (E : Entry_Queue) return Entry_Call_Link is
   begin
      pragma Assert (Check_Queue (E));
      return E.Head;
   end Head;

   ---------------------------------
   -- Select_Protected_Entry_Call --
   ---------------------------------

   --  Select an entry of a protected object. Selection depends on the
   --  queuing policy being used.

   procedure Select_Protected_Entry_Call
     (Self_ID : Task_Id;
      Object  : Protection_Entries_Access;
      Call    : out Entry_Call_Link)
   is
      pragma Unreferenced (Self_ID);

      Entry_Call  : Entry_Call_Link;
      Temp_Call   : Entry_Call_Link;
      Entry_Index : Protected_Entry_Index := Null_Entry; -- stop warning

   begin
      Entry_Call := null;

      --  FIFO queueing case

      for J in Object.Entry_Queues'Range loop
         Temp_Call := Head (Object.Entry_Queues (J));

         if Temp_Call /= null
           and then
             Object.Entry_Bodies
               (Object.Find_Body_Index
                  (Object.Compiler_Info, J)).
                 Barrier (Object.Compiler_Info, J)
         then
            Entry_Call := Temp_Call;
            Entry_Index := J;
            exit;
         end if;
      end loop;

      --  If a call was selected, dequeue it and return it for service

      if Entry_Call /= null then
         Temp_Call := Entry_Call;
         Dequeue_Head (Object.Entry_Queues (Entry_Index), Entry_Call);
         pragma Assert (Temp_Call = Entry_Call);
      end if;

      Call := Entry_Call;
   end Select_Protected_Entry_Call;

end System.Tasking.Queuing;
