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

with Interfaces; use Interfaces;
with Commands; use Commands;
with Console; use Console;
with Term; use Term;
with Dumps; use Dumps;
with System.Machine_Code; use System.Machine_Code;
pragma Warnings (Off);
with System.Text_IO;
pragma Warnings (On);
with Srec;

package body Mpc55xx is
   procedure Disable_Watchdog is
   begin
      Set_Tcr (Get_Tcr and not 16#c01e_0000#);
   end Disable_Watchdog;

   function Field (V : Unsigned_32; F, L : Natural) return Unsigned_32 is
   begin
      return Shift_Right (V, 63 - L)
        and Shift_Right (16#ffff_ffff#, 32 - (L - F + 1));
   end Field;

   procedure Put_Bit (V : Unsigned_32; F : Natural; Name : Character) is
   begin
      Put_Bit (Field (V, F, F) = 1, Name);
   end Put_Bit;

   procedure Proc_Cr is
   begin
      Put_Line ("PIR: " & Image8 (Get_Pir));
      Put_Line ("PVR: " & Image8 (Get_Pvr));
      Put_Line ("SVR: " & Image8 (Get_Svr));
      Put_Line ("MSR: " & Image8 (Get_Msr));
      New_Line;
      Put_Line ("HID0: " & Image8 (Get_Hid0));
      Put_Line ("HID1: " & Image8 (Get_Hid1));
   end Proc_Cr;

   procedure Set_L1csr0 (V : Unsigned_32) is
   begin
      Asm ("msync" & ASCII.LF & ASCII.HT &
           "isync" & ASCII.LF & ASCII.HT &
           "mtspr 1010,%0",
           Inputs => Unsigned_32'Asm_Input ("r", V),
           Volatile => True);
   end Set_L1csr0;

   procedure Proc_Cache is
   begin
      Next_Word;
      if Line (Pos .. End_Pos) = "on" then
         Put_Line ("Invalidating and enabling cache");
         Set_L1csr0 (2#10#);
         loop
            exit when (Get_L1csr0 and 2#10#) = 0;
         end loop;
         if (Get_L1csr0 and 2#100#) /= 0 then
            Put_Line ("cache operation aborted");
            return;
         end if;
         Put_Line ("Enabling");
         Set_L1csr0 (2#1#);
      else
         Put_Register
           ("L1CFG0", Get_L1cfg0,
            "2:carch,cwpa,cfaha,cfiswa,2,2:cbsize,2:crepl," &
              "cla,cpa,8:cnway,11:csize");
         Put_Register
           ("L1CSR0", Get_L1csr0,
            "4:wid,4:wdd,awid,awdd,1,cwm,dpb,dsb," &
            "dstrm,cpe,5,cul,clo,clfr,5,cabt,cfi,ce");
      end if;
   end Proc_Cache;

   procedure Proc_Timer is
   begin
      Put_Line ("TCR: " & Image8 (Get_Tcr));
      Put_Line ("TSR: " & Image8 (Get_Tsr));
      Put_Line ("TBU: " & Image8 (Get_Tbu) & " TBL: " & Image8 (Get_Tbl));
   end Proc_Timer;

   procedure Proc_Pll is
      FMPLL_SYNCR : Unsigned_32;
      for FMPLL_SYNCR'Address use System'To_Address (16#c3f8_0000#);
      pragma Import (Ada, FMPLL_SYNCR);
      pragma Volatile (FMPLL_SYNCR);

      FMPLL_SYNSR : Unsigned_32;
      for FMPLL_SYNSR'Address use System'To_Address (16#c3f8_0004#);
      pragma Import (Ada, FMPLL_SYNSR);
      pragma Volatile (FMPLL_SYNSR);

   begin
      Put_Line ("SYNCR: " & Image8 (FMPLL_SYNCR));
      Put_Line ("SYNSR: " & Image8 (FMPLL_SYNSR));
   end Proc_Pll;

   procedure Proc_Tlb is
      Val : Unsigned_32;
      Npids : Unsigned_32;
      Ntlbs : Unsigned_32;
      Nent : Unsigned_32;
   begin
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
      Put_Line ("   #: P Pid  S VA                Flags   PA       U USUSUS");
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
               Put_Bit (Val, 33, 'P'); --  Protected
               Put (' ');
               Put (Image4 (Field (Val, 34, 47))); --  PID
               Put (' ');
               Put (Image1 (Field (Val, 51, 51))); --  Space
               Put (' ');
               Sz := Field (Val, 52, 55);
               Mask := Shift_Right (16#ffff_ffff#,
                                    Natural (32 - (10 + 2 * Sz)));

               Val := Get_Mas2;
               Put (Image8 (Val and not Mask)); --  VA
               Put ('-');
               Put (Image8 (Val or Mask));
               Put (' ');
               Put (Image1 (Field (Val, 56, 57)));
               Put_Bit (Val, 58, 'V'); --  VLE
               Put_Bit (Val, 59, 'W'); --  Write through
               Put_Bit (Val, 60, 'I'); --  Cache inhibited
               Put_Bit (Val, 61, 'M'); --  Memory coherence
               Put_Bit (Val, 62, 'G'); --  Guarded
               Put_Bit (Val, 63, 'E'); --  Endian
               Put (' ');
               Val := Get_Mas3;
               Put (Image8 (Val and not Mask));
               Put (' ');
               Put (Image1 (Field (Val, 54, 57))); --  User flags
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
   end Proc_Tlb;

   SIU_PCR : array (0 .. 230) of Unsigned_16;
   for SIU_PCR'Address use System'To_Address (16#c3f9_0040#);
   pragma Import (Ada, SIU_PCR);
   pragma Volatile (SIU_PCR);

   EBI_BR0 : Unsigned_32;
   for EBI_BR0'Address use System'To_Address (16#c3f8_4010#);
   pragma Import (Ada, EBI_BR0);
   pragma Volatile (EBI_BR0);

   EBI_OR0 : Unsigned_32;
   for EBI_OR0'Address use System'To_Address (16#c3f8_4014#);
   pragma Import (Ada, EBI_OR0);
   pragma Volatile (EBI_OR0);

   procedure Extsram is
   begin
      --  SIU
      --  0x440: primary assignment, 10pF drive strength
      --  0x443: likewise + weak pull-up

      --  PCR 4-27 (Addresses) = 0x440
      --  PCR 28-43 (Data) = 0x440
      for I in 4 .. 43 loop
         SIU_PCR (I) := 16#440#;
      end loop;

      --  PCR 62,63 (RD/WR, BDIP) = 0x440
      for I in 62 .. 63 loop
         SIU_PCR (I) := 16#440#;
      end loop;

      --  PCR 64-65 (W[0-1]) = 0x443
      --  PCR 68-69 (OE, TS) = 0x443
      for I in 64 .. 69 loop
         SIU_PCR (I) := 16#443#;
      end loop;

      --  PCR 0-3 (CS[0-3]) = 0x443
      for I in 0 .. 3 loop
         SIU_PCR (I) := 16#443#;
      end loop;

      --  EBI CS
      --  EBI_CR0 =
      --  EBI_OR0 =
      --  Note that burst cannot be enabled, as burst length is at least
      --  4*4 bytes, while SRAM burst is only 4*2 bytes.
      EBI_BR0 := 16#0000_0843#; -- BA=0,PS=1,BL=1,WEBS=0,TBDIP=0,BI=1,V=1
      EBI_OR0 := 16#fff8_0000#; -- AM=fff8 (512KB),SCY=0,BSCY=0

      --  MMU
      --  use tlb#2, Enable cache
      Set_Mas0 (2 ** 28 + 2 * 2 ** 16);
      Set_Mas1 (2 ** 31 + 2 ** 30 + 7 * 2 ** 8); -- TSIZE=16MB
      Set_Mas2 (16#2000_0000#); -- Cachable
      Set_Mas3 (16#2000_003f#); -- RWX
      Asm ("tlbwe", Volatile => True);
   end Extsram;

   ESCI_CR1 : Unsigned_32;
   for ESCI_CR1'Address use System'To_Address (16#Fffb_0000# + 0);
   pragma Import (Ada, ESCI_CR1);
   pragma Volatile (ESCI_CR1);

   procedure Serial_Br (Baud : Unsigned_32) is
   begin
      --  8 bits, no parity, Tx & Rx enabled, br = Fsys/(16 * baud)
      ESCI_CR1 := 16#000c# + (Fsys / (16 * Baud)) * 2 ** 16;
   end Serial_Br;

   procedure Proc_Br is
      Speed : Unsigned_32;
      Ok : Boolean;
   begin
      Next_Word;
      if End_Pos <= Line_Len then
         Parse_Unsigned32 (Speed, Ok);
         if not Ok then
            return;
         end if;
         Serial_Br (Speed);
      else
         Put ("Speed: ");
         Put (Natural (Fsys / (16 * (ESCI_CR1 / 2 ** 16))));
         New_Line;
      end if;
   end Proc_Br;

   procedure Sync is
   begin
      Asm ("msync", Volatile => True);
      Asm ("isync", Volatile => True);
   end Sync;

   procedure Proc_Load is
   begin
      Put_Line ("Waiting for srec.");
      Srec.Read_Srec;
      Sync;
   end Proc_Load;

   Commands : aliased Command_List :=
     (7,
      ((new String'("tlb - Display TLB configuration and content"),
        Proc_Tlb'Access),
       (new String'("cache [on] - Display cache registers or enable cache"),
        Proc_Cache'Access),
       (new String'("cr - Display some config registers"),
        Proc_Cr'Access),
       (new String'("timer - Display time registers"),
        Proc_Timer'Access),
       (new String'("pll - Display PLL registers"),
        Proc_Pll'Access),
       (new String'("br [SPEED] - Display or set uart baud rate"),
        Proc_Br'Access),
       (new String'("load - S-Record loader"),
        Proc_Load'Access)),
      null);
begin
   --  Set speed
   Serial_Br (57600);

   --  Disable watchdog (in case of running from BAM)
   Disable_Watchdog;

   --  Enable external ram
   Extsram;

   Register_Commands (Commands'Access);
end Mpc55xx;
