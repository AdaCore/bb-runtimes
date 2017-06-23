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

   Reg_PeriphID0 : constant := 16#fe0#;
   Reg_PeriphID1 : constant := 16#fe4#;
   Reg_PeriphID2 : constant := 16#fe8#;
   Reg_PeriphID3 : constant := 16#fec#;

   Reg_CellID0 : constant := 16#ff0#;
   Reg_CellID1 : constant := 16#ff4#;
   Reg_CellID2 : constant := 16#ff8#;
   Reg_CellID3 : constant := 16#ffc#;

   FR_TXFE : constant := 2**7;
   FR_TXFF : constant := 2**5;
   FR_RXFE : constant := 2**4;
   FR_BUSY : constant := 2**3;
   pragma Unreferenced (FR_TXFF, FR_BUSY);

   CR_RXE : constant := 2**9;
   CR_TXE : constant := 2**8;
   CR_UARTEN : constant := 2**0;

   MASK_BE : constant := 2**9;
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
            Put_Hex4 (Unsigned_32 (Off));
      end case;
   end Disp_Reg_Name;

   procedure Check_Interrupts (Dev : in out PL011_Uart_Dev) is
   begin
      if (Dev.RIS and Dev.IMSC) /= 0 then
         Set_Level (Dev.IT_Dev.all, Dev.IT_Id, True);
      else
         Set_Level (Dev.IT_Dev.all, Dev.IT_Id, False);
      end if;
   end Check_Interrupts;

   function Read32 (Dev : in out PL011_Uart_Dev; Off : Off_T)
                    return Unsigned_32 is
   begin
      case Off is
         when Reg_DR =>
            --  Now empty.
            Dev.FR := Dev.FR or FR_RXFE;
            Dev.RIS := Dev.RIS and not (MASK_RX or MASK_RT);
            Check_Interrupts (Dev);
            if False then
               Put ("PL011.DR <- ");
               Put_Hex4 (Dev.DR_Rx);
               New_Line;
            end if;
            return Dev.DR_Rx;
         when Reg_FR =>
            return Dev.FR;
         when Reg_CR =>
            return Dev.CR;
         when Reg_IMSC =>
            return Dev.IMSC;
         when Reg_IFLS =>
            return Dev.IFLS;
         when Reg_IBRD =>
            return Dev.IBRD;
         when Reg_FBRD =>
            return Dev.FBRD;
         when Reg_RIS =>
            return Dev.RIS;

         when Reg_PeriphID0 =>
            return 16#11#;
         when Reg_PeriphID1 =>
            return 16#10#;
         when Reg_PeriphID2 =>
            return 16#14#;
         when Reg_PeriphID3 =>
            return 16#00#;
         when Reg_CellID0 =>
            return 16#0d#;
         when Reg_CellID1 =>
            return 16#f0#;
         when Reg_CellID2 =>
            return 16#05#;
         when Reg_CellID3 =>
            return 16#b1#;

         when others =>
            Put ("uart.read ");
            Disp_Reg_Name (Off);
            New_Line;
            raise Program_Error;
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
            if (Dev.CR and (CR_UARTEN or CR_TXE)) = (CR_UARTEN or CR_TXE) then
               Put (Character'Val (Val and 16#ff#));
               Dev.RIS := Dev.RIS or MASK_TX;
               Check_Interrupts (Dev);
            end if;
         when Reg_CR =>
            Update (Dev.CR, Val and 16#ffff#, Mask);
         when Reg_IMSC =>
            Update (Dev.IMSC, Val and 16#3ff#, Mask);
            Check_Interrupts (Dev);
         when Reg_ICR =>
            Set_Disable (Dev.RIS, Val);
            Check_Interrupts (Dev);
         when Reg_LCR =>
            Update (Dev.LCR, Val and 16#ff#, Mask);
         when Reg_IFLS =>
            Update (Dev.IFLS, Val and 16#3f#, Mask);
         when Reg_IBRD =>
            Update (Dev.IBRD, Val and 16#ffff#, Mask);
         when Reg_FBRD =>
            Update (Dev.FBRD, Val and 16#3f#, Mask);
         when others =>
            Put ("uart.write ");
            Disp_Reg_Name (Off);
            New_Line;
      end case;
   end Write32_Mask;

   procedure Put (Dev : in out PL011_Uart_Emu; C : Unsigned_32)
   is
      D : PL011_Uart_Dev renames Dev.Parent.all;
   begin
      if C = 9 then
         --  C-i
         Debug (D.Debug.all);
         return;
      end if;
      if (D.CR and (CR_UARTEN or CR_RXE)) = (CR_UARTEN or CR_RXE) then
         if C = Break then
            D.DR_Rx := 16#400#;
            D.RIS := D.RIS or MASK_BE;
         elsif C < 256 then
            D.DR_Rx := C;
         else
            raise Program_Error;
         end if;
         D.FR := D.FR and not FR_RXFE;
         --  FIXME: handle overrun
         D.RIS := D.RIS or MASK_RX or MASK_RT;
         Check_Interrupts (D);
      end if;
   end Put;

   procedure Init (Dev : access PL011_Uart_Dev;
                   IT_Dev : Interrupt_Dev_Acc; IT_Id : Natural;
                  Debug : Debug_Dev_Acc) is
   begin
      Dev.all := (Emu => <>,
                  Debug => Debug,
                  IT_Dev => IT_Dev,
                  IT_Id => IT_Id,
                  DR_Rx => 0,
                  FR => FR_RXFE or FR_TXFE,
                  CR => CR_RXE or CR_TXE or CR_UARTEN,
                  RIS => MASK_TX or MASK_RT,
                  LCR => 0,
                  IFLS => 0,
                  IMSC => 0,
                  IBRD => 16,  -- non 0
                  FBRD => 0);

      Uart.Register_Client (Dev.Emu'Unrestricted_Access);

      Dev.Emu.Parent := Dev.all'Unrestricted_Access;
   end Init;

   procedure Dump (Dev : PL011_Uart_Dev) is
   begin
      Put ("UART: FR:");
      Put_Hex4 (Dev.FR);
      Put (" CR:");
      Put_Hex4 (Dev.CR);
      Put (" RIS:");
      Put_Hex4 (Dev.RIS);
      Put (" MSC:");
      Put_Hex4 (Dev.IMSC);
      New_Line;
   end Dump;

end Emu_PL011;
