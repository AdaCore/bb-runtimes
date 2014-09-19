------------------------------------------------------------------------------
--                                                                          --
--                    BAREBOARD EXAMPLE                                     --
--                                                                          --
--            Copyright (C) 2011, AdaCore                                   --
------------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_Io;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;

procedure Info is
   Hex_Digits : array (0 .. 15) of Character := "0123456789abcdef";

   subtype String8 is String (1 .. 8);
   subtype String4 is String (1 .. 4);

   function Image8 (V : Unsigned_32) return String8 is
      Res : String8;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;
      return Res;
   end Image8;

   function Image4 (V : Unsigned_32) return String4 is
      Res : String4;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (4 - I)) and 15));
      end loop;
      return Res;
   end Image4;

   function Image1 (V : Unsigned_32) return Character is
   begin
      return  Hex_Digits (Natural (V and 15));
   end Image1;

   generic
      Spr : Natural;
   function Get_Spr return Unsigned_32;

   function Get_Spr return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mfspr %0,%1",
           Inputs => Natural'Asm_Input ("K", Spr),
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_Spr;

   generic
      Spr : Natural;
   procedure Set_Spr (V : Unsigned_32);

   procedure Set_Spr (V : Unsigned_32) is
   begin
      Asm ("mtspr %0,%1",
           Inputs => (Natural'Asm_Input ("K", Spr),
                      Unsigned_32'Asm_Input ("r", V)));
   end Set_Spr;

   function Get_Pir is new Get_Spr (286);
   function Get_Pvr is new Get_Spr (287);
   function Get_Svr is new Get_Spr (1023);
   function Get_Mmucfg is new Get_Spr (1015);

   function Get_Pid0 is new Get_Spr (48);
   function Get_Pid1 is new Get_Spr (633);
   function Get_Pid2 is new Get_Spr (634);

   function Get_Tlb0cfg is new Get_Spr (688);
   function Get_Tlb1cfg is new Get_Spr (689);

   procedure Set_Mas0 is new Set_Spr (624);
   function Get_Mas1 is new Get_Spr (625);
   function Get_Mas2 is new Get_Spr (626);
   function Get_Mas3 is new Get_Spr (627);

   function Field (V : Unsigned_32; F, L : Natural) return Unsigned_32 is
   begin
      return Shift_Right (V, 63 - L)
        and Shift_Right (16#ffff_ffff#, 32 - (L - F + 1));
   end Field;

   procedure Put_Bit (V : Unsigned_32; F : Natural; Name : Character) is
   begin
      if Field (V, F, F) = 1 then
         Put (Name);
      else
         Put ('-');
      end if;
   end Put_Bit;

   procedure Os_Exit;
   pragma Import (C, Os_Exit, "exit");

   Val : Unsigned_32;
   Npids : Unsigned_32;
   Ntlbs : Unsigned_32;
   Nent : Unsigned_32;
begin
   Put_Line ("PIR: " & Image8 (Get_Pir));
   Put_Line ("PVR: " & Image8 (Get_Pvr));
   Put_Line ("SVR: " & Image8 (Get_Svr));

   Val := Get_Mmucfg;
   Put_Line ("MMUCFG: " & Image8 (Val));
   Npids := Field (Val, 49, 52);
   Ntlbs := 1 + Field (Val, 60, 61);
   Put ("NPIDS: " & Image8 (Npids));
   Put (", PIDSIZE: " & Image8 (1 + Field (Val, 53, 57)));
   Put (", NTLBS: " & Image8 (Ntlbs));
   Put (", MAVN: " & Image8 (1 + Field (Val, 62, 63)));
   New_Line;

   Put ("PID0: " & Image8 (Get_Pid0));
   if Npids > 1 then
      Put (", PID1: " & Image8 (Get_Pid1));
      if Npids > 2 then
         Put (", PID2: " & Image8 (Get_Pid2));
      end if;
   end if;
   New_Line;

   for I in 0 .. Ntlbs - 1 loop
      case I is
         when 0 => Val := Get_Tlb0cfg;
         when 1 => Val := Get_Tlb1cfg;
         when others => exit;
      end case;
      Put_Line ("TLB" & Character'Val (48 + I) & "CFG: " & Image8 (Val));
      Put ("  Assoc: " & Image4 (Field (Val, 32, 39)));
      Put (", MinSz: " & Image1 (Field (Val, 40, 43)));
      Put (", MaxSz: " & Image1 (Field (Val, 44, 47)));
      Put (", Iprot: " & Image1 (Field (Val, 48, 48)));
      Put (", Avail: " & Image1 (Field (Val, 49, 49)));
      Put (", Nentry: " & Image4 (Field (Val, 52, 63)));
      New_Line;
   end loop;

   New_Line;
   Put_Line ("TLB1:");
   Val := Get_Tlb1cfg;
   Nent := Field (Val, 52, 63);
   Put_Line ("   #: P Pid  S VA                Flags   PA      U  USUSUS");
   for I in 0 .. Nent - 1 loop
      Set_Mas0 (2 ** 28 + I * 2 ** 16);
      Asm ("tlbre", Volatile => True);
      Val := Get_Mas1;
      if Field (Val, 32, 32) = 1 then
         declare
            Sz : Unsigned_32;
            Mask : Unsigned_32;
         begin
            --  Valid
            Put (Image4 (I) & ": ");
            Put_Bit (Val, 33, 'P');
            Put (' ');
            Put (Image4 (Field (Val, 34, 47)));
            Put (' ');
            Put (Image1 (Field (Val, 51, 51)));
            Put (' ');
            Sz := Field (Val, 52, 55);
            Mask := Shift_Right (16#ffff_ffff#, Natural (32 - (10 + 2 * Sz)));

            Val := Get_Mas2;
            Put (Image8 (Val and not Mask));
            Put ('-');
            Put (Image8 (Val or Mask));
            Put (' ');
            Put (Image1 (Field (Val, 56, 57)));
            Put_Bit (Val, 58, 'V');
            Put_Bit (Val, 59, 'W');
            Put_Bit (Val, 60, 'I');
            Put_Bit (Val, 61, 'M');
            Put_Bit (Val, 62, 'G');
            Put_Bit (Val, 63, 'E');
            Put (' ');
            Val := Get_Mas3;
            Put (Image8 (Val and not Mask));
            Put (' ');
            Put (Image1 (Field (Val, 54, 57)));
            Put (' ');
            Put_Bit (Val, 58, 'x');
            Put_Bit (Val, 59, 'X');
            Put_Bit (Val, 60, 'w');
            Put_Bit (Val, 61, 'W');
            Put_Bit (Val, 62, 'r');
            Put_Bit (Val, 63, 'R');
            New_Line;
         end;
      end if;
   end loop;

   New_Line;

   Os_Exit;
end Info;
