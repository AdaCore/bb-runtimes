------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               S Y S T E M . B B . C A C H E _ C O N T R O L              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                         Copyright (C) 2010, AdaCore                      --
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

--  This package defines basic primitives to control the behavior of the cache

pragma Restrictions (No_Elaboration_Code);

package System.BB.Cache_Control is

   type Cache_Type is (Instruction, Data);
   --  The granularity of the operations can be on either the instruction or
   --  the data cache.

   type Cache_State is (Disabled, Enabled, Frozen);
   --  The three different states for the cache

   Invalid_Cache_Control : exception;
   --  Exception indicating that the operation on the cache is not supported

   procedure Set_Cache_State
     (Cache          : Cache_Type;
      State          : Cache_State;
      Partition_Wide : Boolean := False);
   --  Change the state of the indicated cache memory for the task that
   --  performs the call. If Partition_Wide is set to True the cache state is
   --  changed for the whole partition (all the tasks in the system).
   --
   --  Invalid_Cache_Control is raised if the operation is not supported

   function Get_Cache_State (Cache : Cache_Type) return Cache_State;
   --  Returns the cache state for the calling task

   procedure Enable_Cache_Freeze_On_Interrupt
     (Cache          : Cache_Type;
      Partition_Wide : Boolean := False);
   --  The indicated cache will automatically be frozen when an asynchronous
   --  interrupt is taken. If Partition_Wide is set to True then freeze on
   --  interrupt is set for the whole partition (all the tasks in the system).
   --
   --  Invalid_Cache_Control is raised if the operation is not supported

   procedure Disable_Cache_Freeze_On_Interrupt
     (Cache          : Cache_Type;
      Partition_Wide : Boolean := False);
   --  Disable the freeze on interrupt capability for the indicated cache. If
   --  Partition_Wide is set to True then freeze on interrupt is set for the
   --  whole partition (all the tasks in the system).
   --
   --  Invalid_Cache_Control is raised if the operation is not supported

   procedure Cache_Flush (Cache : Cache_Type);
   --  Flush the indicated cache
   --
   --  Invalid_Cache_Control is raised if the operation is not supported

end System.BB.Cache_Control;
