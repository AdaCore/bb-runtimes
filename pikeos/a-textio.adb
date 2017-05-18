------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

--  Version for use with ravenscar run time. This version consider that IO
--  calls are blocking (which is true according to arm 9.5.1/18) and that
--  pragma Detect_Blocking is present. So calls to IO procedures from a
--  protected subprogram .

with System.Text_IO; use System.Text_IO;
with System.Task_Primitives.Operations;

with System.Tasking.Restricted.Stages;
pragma Unreferenced (System.Tasking.Restricted.Stages);
--  Be sure tasking is initialized so that Self can be called

package body Ada.Text_IO is

   procedure Blocking_Operation;
   --  Implement pragma Detect_Blocking: raise PE if currently executing in a
   --  context where blocking operations are not allowed. According to ARM
   --  9.5.1/18, language defined input-output packages are potentially that
   --  manipulate files are potentially blocking.

   package STPO renames System.Task_Primitives.Operations;

   ------------------------
   -- Blocking_Operation --
   ------------------------

   procedure Blocking_Operation is
   begin
      if STPO.Self.Common.Protected_Action_Nesting > 0 then
         raise Program_Error;
      end if;
   end Blocking_Operation;

   ---------
   -- Get --
   ---------

   procedure Get (C : out Character) is
   begin
      --  Detect blocking operation

      Blocking_Operation;

      while not Is_Rx_Ready loop
         null;
      end loop;

      C := System.Text_IO.Get;
   end Get;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Use_Cr_Lf_For_New_Line then
         Put (ASCII.CR);
      end if;

      Put (ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Character) is
   begin
      --  Detect blocking operation

      Blocking_Operation;

      while not Is_Tx_Ready loop
         null;
      end loop;

      System.Text_IO.Put (Item);
   end Put;

   procedure Put (Item : String) is
   begin
      for J in Item'Range loop
         Put (Item (J));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

begin
   if not Initialized then
      Initialize;
   end if;
end Ada.Text_IO;
