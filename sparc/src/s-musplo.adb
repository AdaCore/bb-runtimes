------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--    S Y S T E M . M U L T I P R O C E S S O R S . S P I N _ L O C K S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2010-2017, AdaCore                      --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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

with System.Machine_Code;
with System.BB.Parameters;

package body System.Multiprocessors.Spin_Locks is

   use System.Machine_Code;

   ----------
   -- Lock --
   ----------

   procedure Lock (Slock : in out Spin_Lock) is
      Succeeded : Boolean;

   begin
      --  Loop until we can get the lock

      loop
         Try_Lock (Slock, Succeeded);
         exit when Succeeded;
      end loop;
   end Lock;

   ------------
   -- Locked --
   ------------

   function Locked (Slock : Spin_Lock) return Boolean is
      Lock_Value   : Atomic_Flag;
      Lock_Address : constant System.Address := Slock.Flag'Address;

   begin

      if System.BB.Parameters.Multiprocessor then

         --  Only for multiprocessor

         --  Read with "Alternate space" number 1 (Leon2/3 only) so we can skip
         --  the data cache and work directly in RAM.

         Asm ("lduba [%1] 1, %0" & ASCII.LF & ASCII.HT,
              Outputs  => Atomic_Flag'Asm_Output ("=r", Lock_Value),
              Inputs   => System.Address'Asm_Input ("r", Lock_Address),
              Volatile => True,
              Clobber  => "memory");

         return Lock_Value /= Unlocked;
      else
         return True;
      end if;
   end Locked;

   --------------
   -- Try_Lock --
   --------------

   procedure Try_Lock (Slock : in out Spin_Lock; Succeeded : out Boolean) is
      Prev_Lock_Value : Atomic_Flag;
      Lock_Address : constant System.Address := Slock.Flag'Address;

   begin
      if System.BB.Parameters.Multiprocessor then

         --  Only for multiprocessor

         --  Atomic test-and-set with "Alternate space" number 1 (Leon2/3 only)
         --  so we can skip the data cache and work directly in RAM.

         --  Issue GRLIB TN 0009 (2.2 Description): stores that are part of an
         --  atomic instruction (e.g. ldst / casa) are not affected.

         Asm ("ldstuba [%1] 1, %0" & ASCII.LF & ASCII.HT,
              Outputs  => Atomic_Flag'Asm_Output ("=r", Prev_Lock_Value),
              Inputs   => System.Address'Asm_Input ("r", Lock_Address),
              Volatile => True,
              Clobber  => "memory");
         Succeeded := (Prev_Lock_Value = Unlocked);
      else
         Succeeded := True;
      end if;
   end Try_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Slock : in out Spin_Lock) is
      Lock_Address : constant System.Address := Slock.Flag'Address;

   begin
      if System.BB.Parameters.Multiprocessor then

         --  Only for multiprocessor

         --  Set lock value using "Alternate space" number 1 (Leon2/3 only) so
         --  we can skip the data cache and directly write into RAM.

         Asm ("nop; nop" & ASCII.LF & ASCII.HT &  --  Issue GRLIB TN 0009
              "stuba %%g0, [%0] 1" & ASCII.LF & ASCII.HT,
              Inputs   => System.Address'Asm_Input ("r", Lock_Address),
              Volatile => True,
              Clobber  => "memory");
      end if;
   end Unlock;

end System.Multiprocessors.Spin_Locks;
