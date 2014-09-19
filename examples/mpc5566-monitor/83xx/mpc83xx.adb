------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ppc; use Ppc;
with Ppc6xx; use Ppc6xx;
with Commands; use Commands;
pragma Warnings (Off);
with System.BB.Board_Parameters; use System.BB.Board_Parameters;
with System.Machine_Reset;
pragma Warnings (On);

package body Mpc83xx is
   --  Memory map:
   --  0000_0000 - 00ff_ffff ( 16 MB): Ram (RWX)
   --  IMMRBAR               (  1 MB): IO (RW,UC)

   --  0010_0000 - 001f_ffff (128 KB): SIL0 (RWX)

   SIL0_Addr : constant := 16#0100_0000#; -- At 16MB

   --  Memory map
   Ram_Base : constant := 16#0000_0000#;

   procedure Enable_MMU is
   begin
      Put_Line ("configure mmu");

      --  Initialize I and D bats
      --  Ram Base: RWX
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Ibat0u),
                      Unsigned_32'Asm_Input ("r",
                                             Ram_Base or BL_16MB or Vs_Mask)),
           Volatile => True);
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Ibat0l),
                      Unsigned_32'Asm_Input ("r",
                                             Ram_Base or WIMG_CE or Pp_RO)),
           Volatile => True);

      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat0u),
                      Unsigned_32'Asm_Input ("r",
                                             Ram_Base or BL_32MB or Vs_Mask)),
           Volatile => True);
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat0l),
                      Unsigned_32'Asm_Input ("r",
                                             Ram_Base or WIMG_WB or Pp_RW)),
           Volatile => True);

      --  IMMRBAR: RW
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat2u),
                      Unsigned_32'Asm_Input ("r",
                                             IMMRBAR or BL_1MB or Vs_Mask)),
           Volatile => True);
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat2l),
                      Unsigned_32'Asm_Input ("r",
                                             IMMRBAR or WIMG_UC or Pp_RW)),
           Volatile => True);

      --  SIL4
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Ibat3u),
                      Unsigned_32'Asm_Input ("r",
                                             0 or BL_128KB or Vp_Mask)),
           Volatile => True);
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Ibat3l),
                      Unsigned_32'Asm_Input ("r",
                                             SIL0_Addr or WIMG_CE or Pp_RO)),
           Volatile => True);

      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat3u),
                      Unsigned_32'Asm_Input ("r",
                                             0 or BL_128KB or Vp_Mask)),
           Volatile => True);
      Asm ("mtspr %0,%1",
           Inputs => (Unsigned_32'Asm_Input ("i", Dbat3l),
                      Unsigned_32'Asm_Input ("r",
                                             SIL0_Addr or WIMG_WB or Pp_RW)),
           Volatile => True);

      Put_Line ("Clear SR");
      --  Clear SR
      Asm ("mtsr 0,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 1,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 2,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 3,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 4,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 5,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 6,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 7,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 8,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 9,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 10,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 11,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 12,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 13,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 14,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);
      Asm ("mtsr 15,%0",
           Inputs => (Unsigned_32'Asm_Input ("r", 0)),
           Volatile => True);

      Put_Line ("Enable MMU");

      --  Enable MMU
      declare
         Msr : Unsigned_32;
      begin
         Msr := Get_Msr;
         Msr := Msr or MSR_IR or MSR_DR;
         Asm ("mtmsr %0",
              Inputs => Unsigned_32'Asm_Input ("r", Msr),
              Volatile => True);
         Asm ("isync", Volatile => True);
      end;
   end Enable_MMU;

   Commands : aliased Command_List :=
     (1,
      (1 => (new String'("mmu - Switch MMU on"),
             Enable_MMU'Access)),
      null);
begin
   Register_Commands (Commands'Access);
end Mpc83xx;
