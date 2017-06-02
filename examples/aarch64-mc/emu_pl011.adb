------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body Emu_PL011 is
   --  Registers
   Reg_DR     : constant := 16#00#;
   Reg_ECRRSR : constant := 16#04#;
   Reg_FR     : constant := 16#18#;
   Reg_ILPR   : constant := 16#20#;
   Reg_IBRD   : constant := 16#24#;
   Reg_FBRD   : constant := 16#28#;
   Reg_LCR    : constant := 16#2c#;
   Reg_CR     : constant := 16#30#;
   Reg_IFLS   : constant := 16#34#;
   Reg_IMSC   : constant := 16#38#;
   Reg_RIS    : constant := 16#3c#;
   Reg_MIS    : constant := 16#40#;
   Reg_ICR    : constant := 16#44#;
   Reg_DMACR  : constant := 16#48#;

   FR_TXFE : constant := 2**7;
   FR_TXFF : constant := 2**5;
   FR_RXFE : constant := 2**4;
   FR_BUSY : constant := 2**3;

   MASK_RT : constant := 2**6;
   MASK_TX : constant := 2**5;
   MASK_RX : constant := 2**4;

   procedure Disp_Reg_Name (Off : Off_T) is
   begin
      case Off is
         when Reg_DR     =>
            Put ("DR");
         when Reg_ECRRSR =>
            Put ("ECRRSR");
         when Reg_FR     =>
            Put ("FR");
         when Reg_ILPR   =>
            Put ("ILPR");
         when Reg_IBRD   =>
            Put ("IBRD");
         when Reg_FBRD   =>
            Put ("FBRD");
         when Reg_LCR    =>
            Put ("LCR");
         when Reg_CR     =>
            Put ("CR");
         when Reg_IFLS   =>
            Put ("IFLS");
         when Reg_IMSC   =>
            Put ("IMSC");
         when Reg_RIS    =>
            Put ("RIS");
         when Reg_MIS    =>
            Put ("MIS");
         when Reg_ICR    =>
            Put ("ICR");
         when Reg_DMACR  =>
            Put ("DMACR");
         when others =>
            Put ("???");
      end case;
   end Disp_Reg_Name;

   function Read32 (Dev : in out PL011_Uart_Dev; Off : Off_T)
                   return Unsigned_32 is
   begin
      case Off is
         when Reg_DR =>
            --  Now empty.
            Dev.FR := Dev.FR or FR_RXFE;
            return Dev.DR_Rx;
         when Reg_FR =>
            return Dev.FR;
         when others =>
            Put ("uart.read ");
            Disp_Reg_Name (Off);
            New_Line;
            return 0;
      end case;
   end Read32;

   procedure Write32_Mask
     (Dev : in out PL011_Uart_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32) is
   begin
      case Off is
         when Reg_DR =>
            Put (Character'Val (Val and 16#ff#));
         when others =>
            Put ("uart.write ");
            Disp_Reg_Name (Off);
            New_Line;
      end case;
   end Write32_Mask;

   procedure Put (Dev : in out PL011_Uart_Emu; C : Character) is
   begin
      Dev.Parent.DR_Rx := Character'Pos (C);
      Dev.Parent.FR := Dev.Parent.FR and not FR_RXFE;
   end Put;

   procedure Init (Dev : access PL011_Uart_Dev) is
   begin
      Uart.Register_Client (Dev.Emu'Unrestricted_Access);

      Dev.DR_Rx := 0;
      Dev.FR := FR_RXFE or FR_TXFE;
      Dev.Emu.Parent := Dev.all'Unrestricted_Access;
   end Init;

end Emu_PL011;
