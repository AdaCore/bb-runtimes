------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S E M I H O S T I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
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

with Interfaces;               use Interfaces;
with System.Machine_Code;      use System.Machine_Code;
with Ada.Unchecked_Conversion;

package body System.Semihosting is

   type SH_Word is new Interfaces.Unsigned_32;

   function To_SH_Word is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => SH_Word);

   function Generic_SH_Call (R0, R1 : SH_Word) return SH_Word;
   --  Handles the low-level part of semihosting, setting the registers and
   --  executing a breakpoint instruction.

   subtype Syscall is SH_Word;

   SYS_WRITEC : constant Syscall := 16#03#;
   SYS_WRITE0 : constant Syscall := 16#04#;
   SYS_READC  : constant Syscall := 16#07#;

   ---------------------
   -- Generic_SH_Call --
   ---------------------

   function Generic_SH_Call (R0, R1 : SH_Word) return SH_Word is
      Ret : SH_Word;
   begin
      Asm ("mov r0, %1" & ASCII.LF & ASCII.HT &
           "mov r1, %2" & ASCII.LF & ASCII.HT &
           "bkpt #0xAB" & ASCII.LF & ASCII.HT &
           "mov %0, r0",
           Outputs  => (SH_Word'Asm_Output ("=r", Ret)),
           Inputs   => (SH_Word'Asm_Input ("r", R0),
                        SH_Word'Asm_Input ("r", R1)),
           Volatile => True,
           Clobber => ("r1, r0"));
      return Ret;
   end Generic_SH_Call;

   ---------
   -- Put --
   ---------
   procedure Put (Item : Character) is
      Unref : SH_Word;
      pragma Unreferenced (Unref);

      C : Character with Volatile;
      --  Use a volatile variable to avoid compiler's optimization
   begin
      C := Item;
      Unref := Generic_SH_Call (SYS_WRITEC, To_SH_Word (C'Address));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
      Data  : array (Item'First .. Item'Last + 1) of Unsigned_8;
      Unref : SH_Word;
      pragma Unreferenced (Unref);
   begin
      --  Convert the Item string to a null-terminated array of unsigned_8 as
      --  required for ARM semihosting SYS_WRITE0. Sending characters one by
      --  one using SYS_WRITEC would be too slow.

      for Index in Item'Range loop
         Data (Index) := Character'Pos (Item (Index));
      end loop;

      --  Add trailing null
      Data (Item'Last + 1) := 0;

      Unref := Generic_SH_Call (SYS_WRITE0,  To_SH_Word (Data'Address));
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Character) is
      Ret : SH_Word;
   begin
      Ret := Generic_SH_Call (SYS_READC, 0);
      Item := Character'Val (Ret);
   end Get;

end System.Semihosting;
