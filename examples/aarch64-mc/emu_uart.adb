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

with Ada.Text_IO; use Ada.Text_IO;

package body Emu_Uart is
   --  Registers
   Reg_Control           : constant := 16#00#;
   Reg_Mode              : constant := 16#04#;
   Reg_Intrpt_En         : constant := 16#08#;
   Reg_Intrpt_Dis        : constant := 16#0c#;
   Reg_Intrpt_Mask       : constant := 16#10#;
   Reg_Chnl_Int_Sts      : constant := 16#14#;
   Reg_Baud_Rate_Gen     : constant := 16#18#;
   Reg_Rcvr_Timeout      : constant := 16#1c#;
   Reg_Rcvr_Fifo_Trigger : constant := 16#20#;
   Reg_Modem_Ctrl        : constant := 16#24#;
   Reg_Modem_Sts         : constant := 16#28#;
   Reg_Channel_Sts       : constant := 16#2c#;
   Reg_Tx_Rx_Fifo        : constant := 16#30#;
   Reg_Baud_Rate_Divider : constant := 16#34#;
   Reg_Flow_Delay        : constant := 16#38#;
   Reg_Tx_Fifo_Trigger   : constant := 16#44#;

   --  CR bits
   CR_RX_EN  : constant := 16#00000004#;
   CR_RX_DIS : constant := 16#00000008#;
   CR_TX_EN  : constant := 16#00000010#;
   CR_TX_DIS : constant := 16#00000020#;

   procedure Disp_Reg_Name (Off : Off_T) is
   begin
      case Off is
         when Reg_Control =>
            Put ("Control");
         when Reg_Mode =>
            Put ("Mode");
         when Reg_Intrpt_En =>
            Put ("Intrpt_En");
         when Reg_Intrpt_Dis =>
            Put ("Intrpt_Dis");
         when Reg_Intrpt_Mask =>
            Put ("Intrpt_Mask");
         when Reg_Chnl_Int_Sts =>
            Put ("Chnl_Int_Sts");
         when Reg_Baud_Rate_Gen =>
            Put ("Baud_Rate_Gen");
         when Reg_Rcvr_Timeout =>
            Put ("Rcvr_Timeout");
         when Reg_Rcvr_Fifo_Trigger =>
            Put ("Rcvr_Fifo_Trigger");
         when Reg_Modem_Ctrl =>
            Put ("Modem_Ctrl");
         when Reg_Modem_Sts =>
            Put ("Modem_Sts");
         when Reg_Channel_Sts =>
            Put ("Channel_Sts");
         when Reg_Tx_Rx_Fifo =>
            Put ("Tx_Rx_Fifo");
         when Reg_Baud_Rate_Divider =>
            Put ("Baud_Rate_Divider");
         when Reg_Flow_Delay =>
            Put ("Flow_Delay");
         when Reg_Tx_Fifo_Trigger =>
            Put ("Tx_Fifo_Trigger");
         when others =>
            Put ("???");
      end case;
   end Disp_Reg_Name;

   function Read32 (Dev : in out Uart_Dev; Off : Off_T) return Unsigned_32 is
   begin
      case Off is
         when Reg_Control =>
            return Dev.CR;
         when Reg_Channel_Sts =>
            --  Tx and Rx empty
            return 2#0_1010#;
         when others =>
            Put ("uart.read ");
            Disp_Reg_Name (Off);
            New_Line;
            return 0;
      end case;
   end Read32;

   procedure Write32_Mask
     (Dev : in out Uart_Dev;
      Off : Off_T;
      Val : Unsigned_32;
      Mask : Unsigned_32) is
   begin
      case Off is
         when Reg_Control =>
            Dev.CR := (Dev.CR and not Mask) or (Val and Mask);
         when Reg_Tx_Rx_Fifo =>
            if (Mask and 16#ff#) /= 0
              and then (Dev.Cr and (CR_TX_EN or CR_TX_DIS)) = CR_TX_EN
            then
               Put (Character'Val (Val and 16#ff#));
            end if;
         when others =>
            Put ("uart.write ");
            Disp_Reg_Name (Off);
            New_Line;
      end case;
   end Write32_Mask;
end Emu_Uart;
