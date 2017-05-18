------------------------------------------------------------------------------
--                                                                          --
--                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  --
--                                                                          --
--                SYSTEM.TASKING.PROTECTED_OBJECTS.ENTRIES                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

--  This package contains all simple primitives related to Protected_Objects
--  with entries (i.e init, lock, unlock).

--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the complex routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Operations.

--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Unchecked_Conversion;

package System.Tasking.Protected_Objects.Entries is
   pragma Elaborate_Body;

   subtype Positive_Protected_Entry_Index is
     Protected_Entry_Index range  1 .. Protected_Entry_Index'Last;
   --  Index of the entry (and in some cases of the queue)

   type Find_Body_Index_Access is access
     function
       (O : System.Address;
        E : Protected_Entry_Index)
        return Protected_Entry_Index;
   --  Convert a queue index to an entry index (an entry family has one entry
   --  index for several queue indexes).

   type Protected_Entry_Body_Array is
     array (Positive_Protected_Entry_Index range <>) of Entry_Body;
   --  Contains executable code for all entry bodies of a protected type

   type Protected_Entry_Body_Access is
     access constant Protected_Entry_Body_Array;

   type Protected_Entry_Queue_Array is
     array (Protected_Entry_Index range <>) of Entry_Queue;

   type Protected_Entry_Queue_Max_Array is
     array (Positive_Protected_Entry_Index range <>) of Natural;
   --  Contains Max_Queue_Length values

   type Protected_Entry_Queue_Max_Access is
     access constant Protected_Entry_Queue_Max_Array;

   --  The following declarations define an array that contains the string
   --  names of entries and entry family members, together with an associated
   --  access type.

   --  The following type contains the GNARL state of a protected object.
   --  The application-defined portion of the state (i.e. private objects)
   --  is maintained by the compiler-generated code. Note that there is a
   --  simplified version of this type declared in System.Tasking.PO_Simple
   --  that handle the simple case (no entries).

   type Protection_Entries (Num_Entries : Protected_Entry_Index) is record
      Common : aliased Protection;
      --  State of the protected object. This part is common to any protected
      --  object, including those without entries.

      Compiler_Info : System.Address;
      --  Pointer to compiler-generated record representing protected object

      Call_In_Progress : Entry_Call_Link;
      --  Pointer to the entry call being executed (if any)

      Entry_Bodies : Protected_Entry_Body_Access;
      --  Pointer to an array containing the executable code for all entry
      --  bodies of a protected type.

      Find_Body_Index : Find_Body_Index_Access;
      --  A function which maps the entry index in a call (which denotes the
      --  queue of the proper entry) into the body of the entry.

      Entry_Queue_Maxes : Protected_Entry_Queue_Max_Access;
      --  Access to an array of naturals representing the max value for each
      --  entry's queue length. A value of 0 signifies no max.

      Entry_Queues : Protected_Entry_Queue_Array (1 .. Num_Entries);
      --  Action and barrier subprograms for the protected type.
   end record;

   --  No default initial values for this type, since call records will need to
   --  be re-initialized before every use.

   type Protection_Entries_Access is access all Protection_Entries;

   function To_Address is
     new Ada.Unchecked_Conversion (Protection_Entries_Access, System.Address);
   function To_Protection is
     new Ada.Unchecked_Conversion (System.Address, Protection_Entries_Access);

   procedure Initialize_Protection_Entries
     (Object            : Protection_Entries_Access;
      Ceiling_Priority  : Integer;
      Compiler_Info     : System.Address;
      Entry_Queue_Maxes : Protected_Entry_Queue_Max_Access;
      Entry_Bodies      : Protected_Entry_Body_Access;
      Find_Body_Index   : Find_Body_Index_Access);
   --  Initialize the Object parameter so that it can be used by the runtime
   --  to keep track of the runtime state of a protected object.

   procedure Lock_Entries (Object : Protection_Entries_Access);
   pragma Inline (Lock_Entries);
   --  Lock a protected object for write access. Upon return, the caller owns
   --  the lock to this object, and no other call to Lock or Lock_Read_Only
   --  with the same argument will return until the corresponding call to
   --  Unlock has been made by the caller. Program_Error is raised in case of
   --  ceiling violation.

   procedure Unlock_Entries (Object : Protection_Entries_Access);
   pragma Inline (Unlock_Entries);
   --  Relinquish ownership of the lock for the object represented by the
   --  Object parameter. If this ownership was for write access, or if it was
   --  for read access where there are no other read access locks outstanding,
   --  one (or more, in the case of Lock_Read_Only) of the tasks waiting on
   --  this lock (if any) will be given the lock and allowed to return from
   --  the Lock or Lock_Read_Only call.

end System.Tasking.Protected_Objects.Entries;
