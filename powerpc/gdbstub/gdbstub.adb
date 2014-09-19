------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Gdbstub_Io; use Gdbstub_Io;
with Gdbstub.CPU;
with Interfaces; use Interfaces;

package body Gdbstub is

   Flag_Debug : constant Boolean := False;

   Exit_Stub_Loop : Boolean;

   procedure Debug_Put (C : Character);
   procedure Debug_Put (Msg : String);
   procedure Debug_Newline;
   procedure Debug_Put_Line (Msg : String);
   --  Output on the debug port.

   procedure Put_Line (Msg : String);
   pragma Unreferenced (Put_Line);
   --  Output on the serial port.

   function To_Hex (C : Character) return Unsigned_8;
   --  Convert a character to its value.
   --  Invalid characters return 16#ff#.

   procedure Read_Data (Start : Address;
                        Len : Storage_Count;
                        Success : in out Boolean);
   --  Append data to the current packet.

   procedure Write_Data (Start : Address;
                         Len : Storage_Count;
                         Success : in out Boolean);

   procedure Handle_Packet;
   procedure Handle_Query_Packet;
   procedure Handle_Breakpoint_Packet;
   procedure Handle_Cont_Packet;
   procedure Handle_Step_Packet;
   procedure Handle_Mem_Read_Packet;
   procedure Handle_Mem_Write_Packet;
   procedure Handle_Mem_Write_Binary_Packet;
   procedure Handle_All_Regs_Read_Packet;
   procedure Handle_All_Regs_Write_Packet;
   procedure Handle_Reg_Write_Packet;
   --  Handle incoming packet.

   procedure Reply_Status;
   procedure Reply_Ok;
   procedure Reply_Error (Err : Unsigned_8);
   --  Create a reply packet.

   procedure Append_Hex (V : Unsigned_8; Success : in out Boolean);
   --  Append a byte (in hex) to the current packet.

   procedure Send_Packet;
   --  Send the current packet.

   procedure Get_Packet;
   function Check_Packet_Checksum return Boolean;
   function Get_Packet_Body return Boolean;
   procedure Wait_Packet_Start;
   --  Packet input procedures.

   procedure Check_Hex_Digit (Success : in out Boolean);
   --  Check that the next character in the packet is an hex-digit character.
   --  Set Success to false in case of error.

   procedure Extract_Byte (Res : out Unsigned_8; Success : in out Boolean);
   procedure Extract_Address (Res : out Address; Success : in out Boolean);
   procedure Extract_Char (C : Character; Success : in out Boolean);
   procedure Extract_Len (Res : out Storage_Count; Success : in out Boolean);
   --  Extract a field from the current packet.  Set success to false in case
   --  of error.  Update Packet_Idx.

   function Is_EOP return Boolean;

   procedure Check_EOP (Success : in out Boolean);
   --  Check for end of packet.

   procedure Debug_Put (C : Character) is
   begin
      Write_Debug (C);
   end Debug_Put;

   procedure Debug_Put (Msg : String) is
   begin
      for I in Msg'Range loop
         Debug_Put (Msg (I));
      end loop;
   end Debug_Put;

   procedure Debug_Newline is
   begin
      Write_Debug (ASCII.CR);
      Write_Debug (ASCII.LF);
   end Debug_Newline;

   procedure Debug_Put_Line (Msg : String) is
   begin
      Debug_Put (Msg);
      Debug_Newline;
   end Debug_Put_Line;

   procedure Put_Line (Msg : String) is
   begin
      for I in Msg'Range loop
         Write_Byte (Msg (I));
      end loop;
      Write_Byte (ASCII.CR);
      Write_Byte (ASCII.LF);
   end Put_Line;

   Packet : String (1 .. 1024);
   --  Current packet.

   Packet_Len : Natural;
   --  Length of Packet.

   Packet_Idx : Natural;
   --  Current index while parsing the packet.

   Packet_Checksum : Unsigned_8;

   To_Char : constant array (Unsigned_8 range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   function To_Hex (C : Character) return Unsigned_8 is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when others =>
            return 255;
      end case;
   end To_Hex;

   procedure Wait_Packet_Start is
      C : Character;
   begin
      --  Wait for the start character.
      loop
         C := Read_Byte;
         exit when C = '$';
      end loop;
   end Wait_Packet_Start;

   function Get_Packet_Body return Boolean is
      C : Character;
   begin
      loop
         Packet_Len := 0;
         Packet_Checksum := 0;

         loop
            if Packet_Len = Packet'Last then
               if Flag_Debug then
                  Debug_Put_Line ("Packet too long");
                  return False;
               end if;
            end if;
            C := Read_Byte;
            exit when C = '$';
            if C = '#' then
               return True;
            end if;
            Packet_Checksum := Packet_Checksum + Character'Pos (C);
            Packet_Len := Packet_Len + 1;
            Packet (Packet_Len) := C;
         end loop;
         if Flag_Debug then
            Debug_Put_Line ("start in the middle");
         end if;
      end loop;
   end Get_Packet_Body;

   function Check_Packet_Checksum return Boolean is
      C : Character;
      V : array (1 .. 2) of Unsigned_8;
   begin
      for I in V'Range loop
         C := Read_Byte;
         V (I) := To_Hex (C);
         if V (I) > 15 then
            return False;
         end if;
      end loop;
      return V (1) * 16 + V (2) = Packet_Checksum;
   end Check_Packet_Checksum;

   procedure Get_Packet is
      --  Wait for the sequence $<data>#<checksum>
   begin
      loop
         Wait_Packet_Start;

         if Get_Packet_Body then
            if not Check_Packet_Checksum then
               --  NAK
               Write_Byte ('-');
               Debug_Put_Line ("Bad checksum");
            else
               --  ACK
               Write_Byte ('+');

               return;
            end if;
         end if;
      end loop;
   end Get_Packet;

   Last_Signal : Unsigned_8;

   procedure Append_Hex (V : Unsigned_8; Success : in out Boolean) is
   begin
      if not Success then
         return;
      end if;
      if Packet_Len + 2 > Packet'Last then
         Success := False;
         return;
      end if;
      Packet (Packet_Len + 1) := To_Char (V / 16);
      Packet (Packet_Len + 2) := To_Char (V mod 16);
      Packet_Len := Packet_Len + 2;
   end Append_Hex;

   procedure Reply_Status is
      Succ : Boolean := True;
   begin
      Packet (1) := 'S';
      Packet_Len := 1;
      Append_Hex (Last_Signal, Succ);
      pragma Assert (Succ);
   end Reply_Status;

   procedure Reply_Error (Err : Unsigned_8) is
      Succ : Boolean := True;
   begin
      Packet (1) := 'E';
      Packet_Len := 1;
      Append_Hex (Err, Succ);
      pragma Assert (Succ);
   end Reply_Error;

   procedure Reply_Ok is
   begin
      Packet (1) := 'O';
      Packet (2) := 'K';
      Packet_Len := 2;
   end Reply_Ok;

   procedure Read_Data (Start : Address;
                        Len : Storage_Count;
                        Success : in out Boolean) is
   begin
      for Off in 0 .. Len - 1 loop
         exit when not Success;
         declare
            B : Unsigned_8;
            for B'Address use Start + Off;
         begin
            Append_Hex (B, Success);
         end;
      end loop;
   end Read_Data;

   procedure Write_Data (Start : Address;
                         Len : Storage_Count;
                         Success : in out Boolean) is
   begin
      for Off in 0 .. Len - 1 loop
         exit when not Success;
         declare
            B : Unsigned_8;
            for B'Address use Start + Off;
         begin
            Extract_Byte (B, Success);
         end;
      end loop;
   end Write_Data;

   --  Handle 'q' packets.
   procedure Handle_Query_Packet is
   begin
      if Flag_Debug then
         Debug_Put ("query packet: ");
         Debug_Put_Line (Packet (1 .. Packet_Len));
      end if;
      if Packet_Len >= 10 and then Packet (1 .. 10) = "qSupported" then
         --  No extra features supported.
         Packet_Len := 0;
      elsif Packet_Len = 2 and then Packet (1 .. 2) = "qC" then
         --  Current thread.
         Packet (3) := '0';
         Packet_Len := 3;
      elsif Packet_Len = 9 and then Packet (1 .. 9) = "qSymbol::" then
         --  We don't query symbols.
         Reply_Ok;
      else
         --  Ignored.
         Debug_Put ("ignored query packet: ");
         Debug_Put_Line (Packet (1 .. Packet_Len));
         Packet_Len := 0;
      end if;
   end Handle_Query_Packet;

   --  Handle 'z'/'Z' packets.
   procedure Handle_Breakpoint_Packet is
   begin
      if Flag_Debug then
         Debug_Put ("breakpoint packet: ");
         Debug_Put_Line (Packet (1 .. Packet_Len));
      end if;
      if True then
         --  Ignored.
         Debug_Put ("ignored break packet: ");
         Debug_Put_Line (Packet (1 .. Packet_Len));
         Packet_Len := 0;
      end if;
   end Handle_Breakpoint_Packet;

   procedure Check_Hex_Digit (Success : in out Boolean) is
   begin
      if not Success then
         return;
      end if;
      if Packet_Idx > Packet_Len then
         Success := False;
         return;
      end if;
      if To_Hex (Packet (Packet_Idx)) > 15 then
         Success := False;
         return;
      end if;
   end Check_Hex_Digit;

   procedure Extract_Byte (Res : out Unsigned_8; Success : in out Boolean) is
      Vh, Vl : Unsigned_8;
   begin
      Res := 0;
      if not Success then
         return;
      end if;
      if Packet_Idx + 1 > Packet_Len then
         Success := False;
         return;
      end if;
      Vh := To_Hex (Packet (Packet_Idx + 0));
      Vl := To_Hex (Packet (Packet_Idx + 1));
      Packet_Idx := Packet_Idx + 2;
      if Vh > 15 or else Vl > 15 then
         Success := False;
         return;
      end if;
      Res := Vh * 16 + Vl;
   end Extract_Byte;

   procedure Extract_Address (Res : out Address; Success : in out Boolean) is
      V : Unsigned_8;
      Resint : Integer_Address;
   begin
      Check_Hex_Digit (Success);
      if not Success then
         return;
      end if;
      V := To_Hex (Packet (Packet_Idx));
      Resint := 0;
      loop
         Resint := Resint * 16 + Integer_Address (V);
         Packet_Idx := Packet_Idx + 1;
         exit when Packet_Idx > Packet_Len;
         V := To_Hex (Packet (Packet_Idx));
         exit when V > 15;
      end loop;
      Res := To_Address (Resint);
   end Extract_Address;

   procedure Extract_Char (C : Character; Success : in out Boolean) is
   begin
      if not Success then
         return;
      end if;

      if Packet_Idx > Packet_Len then
         Success := False;
         return;
      end if;
      if Packet (Packet_Idx) /= C then
         Success := False;
         return;
      end if;
      Packet_Idx := Packet_Idx + 1;
   end Extract_Char;

   procedure Extract_Len (Res : out Storage_Count; Success : in out Boolean) is
      V : Unsigned_8;
      Resint : Storage_Count;
   begin
      Check_Hex_Digit (Success);
      if not Success then
         Res := 0;
         return;
      end if;
      V := To_Hex (Packet (Packet_Idx));
      Resint := 0;
      loop
         Resint := Resint * 16 + Storage_Count (V);
         Packet_Idx := Packet_Idx + 1;
         exit when Packet_Idx > Packet_Len;
         V := To_Hex (Packet (Packet_Idx));
         exit when V > 15;
      end loop;
      Res := Resint;
   end Extract_Len;

   function Is_EOP return Boolean is
   begin
      return Packet_Idx = Packet_Len + 1;
   end Is_EOP;

   procedure Check_EOP (Success : in out Boolean) is
   begin
      if not Is_EOP then
         Success := False;
      end if;
   end Check_EOP;

   procedure Handle_Mem_Read_Packet is
      Addr : Address;
      Len : Storage_Count;
      Success : Boolean;
   begin
      Packet_Idx := 2;
      Success := True;
      Extract_Address (Addr, Success);
      Extract_Char (',', Success);
      Extract_Len (Len, Success);
      Check_EOP (Success);
      if not Success then
         Reply_Error (1);
         return;
      end if;
      Packet_Len := 0;
      Read_Data (Addr, Len, Success);
      if not Success then
         Reply_Error (2);
      end if;
   end Handle_Mem_Read_Packet;

   procedure Handle_Mem_Write_Packet is
      Addr : Address;
      Len : Storage_Count;
      Success : Boolean;
   begin
      Packet_Idx := 2;
      Success := True;
      Extract_Address (Addr, Success);
      Extract_Char (',', Success);
      Extract_Len (Len, Success);
      Extract_Char (':', Success);
      if not Success then
         Reply_Error (1);
         return;
      end if;
      Write_Data (Addr, Len, Success);
      if Success then
         CPU.Invalidate_Icache (Addr, Len);
         Reply_Ok;
      else
         Reply_Error (1);
      end if;
   end Handle_Mem_Write_Packet;

   procedure Handle_Mem_Write_Binary_Packet is
      Addr : Address;
      Len : Storage_Count;
      Success : Boolean;
      B : Unsigned_8;
   begin
      Packet_Idx := 2;
      Success := True;
      Extract_Address (Addr, Success);
      Extract_Char (',', Success);
      Extract_Len (Len, Success);
      Extract_Char (':', Success);
      if not Success then
         Reply_Error (1);
         return;
      end if;
      for I in 0 .. Len - 1 loop
         if Packet_Idx > Packet_Len then
            Success := False;
            exit;
         end if;
         B := Character'Pos (Packet (Packet_Idx));
         Packet_Idx := Packet_Idx + 1;
         if B = 16#7d# then
            --  Escape character.
            if Packet_Idx > Packet_Len then
               Success := False;
               exit;
            end if;
            B := Character'Pos (Packet (Packet_Idx)) xor 16#20#;
            Packet_Idx := Packet_Idx + 1;
         end if;
         declare
            M : Unsigned_8;
            for M'Address use Addr + I;
         begin
            M := B;
         end;
      end loop;
      Check_EOP (Success);
      if Success then
         CPU.Invalidate_Icache (Addr, Len);
         Reply_Ok;
      else
         Reply_Error (1);
      end if;
   end Handle_Mem_Write_Binary_Packet;

   procedure Handle_All_Regs_Write_Packet is
      Success : Boolean;
   begin
      Packet_Idx := 2;
      Success := True;
      Write_Data (Registers_Area, Registers_Size, Success);
      if not Success then
         Reply_Error (2);
      elsif not Is_EOP then
         Reply_Error (1);
      else
         Reply_Ok;
      end if;
   end Handle_All_Regs_Write_Packet;

   procedure Handle_All_Regs_Read_Packet is
      Success : Boolean;
   begin
      Packet_Len := 0;
      Success := True;
      Read_Data (Registers_Area, Registers_Size, Success);
      if not Success then
         Reply_Error (2);
      end if;
   end Handle_All_Regs_Read_Packet;

   procedure Handle_Reg_Write_Packet is
      Success : Boolean;
      Len : Storage_Count;
      Addr : Address;
      Size : Storage_Count;
   begin
      Packet_Idx := 2;
      Success := True;
      Extract_Len (Len, Success);
      Extract_Char ('=', Success);
      if not Success
        or else Len > Natural'Pos (Natural'Last)
      then
         Reply_Error (1);
         return;
      end if;
      CPU.Get_Register_Area (Natural (Len), Addr, Size);
      if Size = 0 then
         Reply_Error (1);
         return;
      end if;
      Write_Data (Addr, Size, Success);
      if not Success then
         Reply_Error (2);
      elsif not Is_EOP then
         Reply_Error (1);
      else
         Reply_Ok;
      end if;
   end Handle_Reg_Write_Packet;

   procedure Handle_Cont_Packet is
   begin
      Packet_Idx := 2;
      if Is_EOP then
         CPU.Set_Trace_Flag (False);
         Exit_Stub_Loop := True;
      else
         Reply_Error (1);
      end if;
   end Handle_Cont_Packet;

   procedure Handle_Step_Packet is
   begin
      Packet_Idx := 2;
      if Is_EOP then
         CPU.Set_Trace_Flag (True);
         Exit_Stub_Loop := True;
      else
         Reply_Error (1);
      end if;
   end Handle_Step_Packet;

   procedure Handle_Packet is
   begin
      if Flag_Debug then
         Debug_Put ("packet: ");
         Debug_Put (Packet (1));
         Debug_Newline;
      end if;

      case Packet (1) is
         when '?' =>
            --  Reason
            Reply_Status;
         when 'c' =>
            Handle_Cont_Packet;
         when 'g' =>
            --  Get registers.
            Handle_All_Regs_Read_Packet;
         when 'G' =>
            --  Write registers
            Handle_All_Regs_Write_Packet;
         when 'H' =>
            --  Thread selection packet.
            --  Ignore it.
            Packet_Len := 0;
         when 'k' =>
            Packet_Len := 0;
            Kill;
         when 'm' =>
            --  Memory read.
            Handle_Mem_Read_Packet;
         when 'M' =>
            --  Memory write.
            Handle_Mem_Write_Packet;
         when 'P' =>
            Handle_Reg_Write_Packet;
         when 'q' =>
            --  Query packet.
            Handle_Query_Packet;
         when 's' =>
            Handle_Step_Packet;
         when 'X' =>
            --  Memory write in binary.
            Handle_Mem_Write_Binary_Packet;
         when 'z' | 'Z' =>
            Handle_Breakpoint_Packet;
         when others =>
            --  Ignore other commands.
            Debug_Put ("ignored packet: ");
            Debug_Put (Packet (1));
            Debug_Newline;
            Packet_Len := 0;
      end case;
   end Handle_Packet;

   procedure Send_Packet is
      Checksum : Unsigned_8;
   begin
      Write_Byte ('$');
      Checksum := 0;
      for I in 1 .. Packet_Len loop
         Checksum := Checksum + Character'Pos (Packet (I));
         Write_Byte (Packet (I));
      end loop;
      Write_Byte ('#');
      Write_Byte (To_Char (Checksum / 16));
      Write_Byte (To_Char (Checksum mod 16));
   end Send_Packet;

   procedure Handle_Exception (Sig : Natural) is
   begin
      --  Send status.
      Last_Signal := Unsigned_8 (Sig);
      Reply_Status;
      Send_Packet;

      Exit_Stub_Loop := False;

      loop
         Get_Packet;
         if Packet_Len /= 0 then
            Handle_Packet;
            exit when Exit_Stub_Loop;
            Send_Packet;
         end if;
      end loop;
   end Handle_Exception;

   procedure Stub is
   begin
      Gdbstub_Io.Initialize;
      --  Put_Line ("AdaCore GdbStub");
      Debug_Put_Line ("AdaCore GdbStub - Debug port");

      CPU.Setup_Handlers;
      CPU.Breakpoint;
   end Stub;
end Gdbstub;
