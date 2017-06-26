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

   LCR_FEN : constant := 2**4;

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

   protected body PL011_Prot is
      function Fifo_En return Boolean is
      begin
         return (S.LCR and LCR_FEN) /= 0;
      end Fifo_En;

      procedure Check_Interrupts is
      begin
         if (S.RIS and S.IMSC) /= 0 then
            Set_Level (S.IT_Dev.all, S.IT_Id, True);
         else
            Set_Level (S.IT_Dev.all, S.IT_Id, False);
         end if;
      end Check_Interrupts;

      procedure Read32 (Off : Off_T; Res : out Unsigned_32) is
      begin
         case Off is
         when Reg_DR =>
            --  Now empty.
            if S.Rx_Len = 0 then
               --  Humm, no character
               Res := 0;
            elsif S.Rx_Len = 1 then
               --  Only one character
               S.Rx_Len := 0;
               S.FR := S.FR or FR_RXFE;
               S.RIS := S.RIS and not (MASK_RX or MASK_RT);
               Check_Interrupts;
               Res := S.Rx_Fifo (1);
            else
               Res := S.Rx_Fifo (1);
               S.Rx_Fifo (1 .. 14) := S.Rx_Fifo (2 .. 15);
               S.Rx_Len := S.Rx_Len - 1;
            end if;
         when Reg_FR =>
            Res := S.FR;
         when Reg_CR =>
            Res := S.CR;
         when Reg_IMSC =>
            Res := S.IMSC;
         when Reg_IFLS =>
            Res := S.IFLS;
         when Reg_IBRD =>
            Res := S.IBRD;
         when Reg_FBRD =>
            Res := S.FBRD;
         when Reg_RIS =>
            Res := S.RIS;

         when Reg_PeriphID0 =>
            Res := 16#11#;
         when Reg_PeriphID1 =>
            Res := 16#10#;
         when Reg_PeriphID2 =>
            Res := 16#14#;
         when Reg_PeriphID3 =>
            Res := 16#00#;
         when Reg_CellID0 =>
            Res := 16#0d#;
         when Reg_CellID1 =>
            Res := 16#f0#;
         when Reg_CellID2 =>
            Res := 16#05#;
         when Reg_CellID3 =>
            Res := 16#b1#;

         when others =>
            Put ("uart.read ");
            Disp_Reg_Name (Off);
            New_Line;
            raise Program_Error;
         end case;
      end Read32;

      procedure Write32_Mask
        (Off : Off_T; Val : Unsigned_32; Mask : Unsigned_32) is
      begin
         case Off is
         when Reg_DR =>
            if (S.CR and (CR_UARTEN or CR_TXE)) = (CR_UARTEN or CR_TXE) then
               Put (Character'Val (Val and 16#ff#));
               S.RIS := S.RIS or MASK_TX;
               Check_Interrupts;
            end if;
         when Reg_CR =>
            Update (S.CR, Val and 16#ffff#, Mask);
         when Reg_IMSC =>
            Update (S.IMSC, Val and 16#3ff#, Mask);
            Check_Interrupts;
         when Reg_ICR =>
            Set_Disable (S.RIS, Val);
            Check_Interrupts;
         when Reg_LCR =>
            Update (S.LCR, Val and 16#ff#, Mask);
         when Reg_IFLS =>
            Update (S.IFLS, Val and 16#3f#, Mask);
         when Reg_IBRD =>
            Update (S.IBRD, Val and 16#ffff#, Mask);
         when Reg_FBRD =>
            Update (S.FBRD, Val and 16#3f#, Mask);
         when others =>
            Put ("uart.write ");
            Disp_Reg_Name (Off);
            New_Line;
         end case;
      end Write32_Mask;

      procedure Init (IT_Dev : Interrupt_Dev_Acc; IT_Id : Natural;
                      Debug : Debug_Dev_Acc) is
      begin
         S := (Debug => Debug,
               IT_Dev => IT_Dev,
               IT_Id => IT_Id,
               Rx_Fifo => (others => 0),
               Rx_Len => 0,
               FR => FR_RXFE or FR_TXFE,
               CR => CR_RXE or CR_TXE or CR_UARTEN,
               RIS => MASK_TX or MASK_RT,
               LCR => 0,
               IFLS => 0,
               IMSC => 0,
               IBRD => 16,  -- non 0
               FBRD => 0);
      end Init;

      procedure Receive_Cb (C : Unsigned_32) is
         Max : Natural;
      begin
         if C = 9 then
            --  C-i
            Debug (S.Debug.all);
            return;
         end if;
         if (S.CR and (CR_UARTEN or CR_RXE)) = (CR_UARTEN or CR_RXE) then
            --  Uart is enabled.
            if Fifo_En then
               Max := S.Rx_Fifo'Last;
            else
               Max := 1;
            end if;
            if S.Rx_Len = Max then
               --  Overrun.
               return;
            end if;
            S.Rx_Len := S.Rx_Len + 1;
            if C = Break then
               S.Rx_Fifo (S.Rx_Len) := 16#400#;
               S.RIS := S.RIS or MASK_BE;
            elsif C < 256 then
               S.Rx_Fifo (S.Rx_Len) := C;
            else
               raise Program_Error;
            end if;
            if S.Rx_Len = 1 then
               S.FR := S.FR and not FR_RXFE;
               --  FIXME: handle overrun
               S.RIS := S.RIS or MASK_RX or MASK_RT;
               Check_Interrupts;
            end if;
         end if;
      end Receive_Cb;

      procedure Dump is
      begin
         Put ("UART: FR:");
         Put_Hex4 (S.FR);
         Put (" CR:");
         Put_Hex4 (S.CR);
         Put (" RIS:");
         Put_Hex4 (S.RIS);
         Put (" MSC:");
         Put_Hex4 (S.IMSC);
         New_Line;
      end Dump;
   end PL011_Prot;

   function Read32 (Dev : in out PL011_Uart_Dev; Off : Off_T)
                    return Unsigned_32
   is
      Res : Unsigned_32;
   begin
      Dev.Prot.Read32 (Off, Res);
      return Res;
   end Read32;

   procedure Write32_Mask
     (Dev : in out PL011_Uart_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32) is
   begin
      Dev.Prot.Write32_Mask (Off, Val, Mask);
   end Write32_Mask;

   procedure Put (Dev : in out PL011_Uart_Emu; C : Unsigned_32) is
   begin
      Dev.Parent.Prot.Receive_Cb (C);
   end Put;

   procedure Init (Dev : access PL011_Uart_Dev;
                   IT_Dev : Interrupt_Dev_Acc; IT_Id : Natural;
                   Debug : Debug_Dev_Acc) is
   begin
      Dev.Prot.Init (IT_Dev, IT_Id, Debug);

      Dev.Emu.Parent := Dev.all'Unrestricted_Access;
      Uart.Register_Client (Dev.Emu'Unrestricted_Access);
   end Init;

   procedure Dump (Dev : in out PL011_Uart_Dev) is
   begin
      Dev.Prot.Dump;
   end Dump;

end Emu_PL011;
