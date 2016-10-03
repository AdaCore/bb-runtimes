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
pragma Warnings (Off);
with System.Machine_Reset;
pragma Warnings (On);
with Interfaces; use Interfaces;
with System;
with Commands; use Commands;
with Console; use Console;
with Dumps; use Dumps;
with Term; use Term;

package body Hdk is
   function Get_SCTLR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c1, c0, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_SCTLR;

   procedure Set_SCTLR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c1, c0, 0",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_SCTLR;

   function Get_ACTLR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c1, c0, 1",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ACTLR;

   procedure Set_ACTLR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c1, c0, 1",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_ACTLR;

   function Get_MPUIR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c0, c0, 4",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_MPUIR;

   procedure Set_RGNR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c6, c2, 0",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_RGNR;

   procedure Set_DRBAR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c6, c1, 0",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_DRBAR;

   function Get_DRBAR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c6, c1, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DRBAR;

   procedure Set_DRSR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c6, c1, 2",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_DRSR;

   function Get_DRSR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c6, c1, 2",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DRSR;

   DRSR_E : constant := 1;  -- Enable

   procedure Set_DRACR (Val : Unsigned_32) is
   begin
      Asm ("mcr p15, 0, %0, c6, c1, 4",
           Inputs => Unsigned_32'Asm_Input ("r", Val),
           Volatile => True);
   end Set_DRACR;

   function Get_DRACR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c6, c1, 4",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DRACR;

   AP_NO_NO : constant := 16#0_00#;
   AP_RW_NO : constant := 16#1_00#;
   AP_RW_RO : constant := 16#2_00#;
   AP_RW_RW : constant := 16#3_00#;
   AP_RO_NO : constant := 16#5_00#;
   AP_RO_RO : constant := 16#6_00#;

   DRACR_XN : constant := 16#1000#; --  Execute never

   function Get_DFSR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c5, c0, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DFSR;

   function Get_DFAR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c6, c0, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_DFAR;

   function Get_ADFSR return Unsigned_32 is
      Res : Unsigned_32;
   begin
      Asm ("mrc p15, 0, %0, c5, c1, 0",
           Outputs => Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return Res;
   end Get_ADFSR;

   procedure Dsb is
   begin
      Asm ("dsb", Volatile => True, Clobber => "memory");
   end Dsb;

   procedure Isb is
   begin
      Asm ("isb", Volatile => True, Clobber => "memory");
   end Isb;

   procedure Dmb is
   begin
      Asm ("dmb", Volatile => True, Clobber => "memory");
   end Dmb;

   procedure Enable_Btcm_Ecc is
      Val : Unsigned_32;
   begin
      Val := Get_ACTLR;
      Val := Val or 16#0C00_0000#;
      Dmb;
      Set_ACTLR (Val);
      Isb;
   end Enable_Btcm_Ecc;

   procedure Disable_Btcm_Ecc is
      Val : Unsigned_32;
   begin
      Val := Get_ACTLR;
      Val := Val and not 16#0C00_0000#;
      Dmb;
      Set_ACTLR (Val);
      Isb;
   end Disable_Btcm_Ecc;

   procedure Disable_Mpu is
      V : Unsigned_32;
   begin
      V := Get_SCTLR;
      Dsb;
      V := V and not 1;
      Set_SCTLR (V);
      Isb;
   end Disable_Mpu;

   procedure Enable_Mpu is
      V : Unsigned_32;
   begin
      V := Get_SCTLR;
      Dsb;
      V := V or 1;
      Set_SCTLR (V);
      Isb;
   end Enable_Mpu;

   procedure Disable_Background_Region is
      V : Unsigned_32;
      Br : constant := 16#2_0000#;
   begin
      V := Get_SCTLR;
      V := V and not Br;
      Set_SCTLR (V);
   end Disable_Background_Region;

   procedure Enable_Background_Region is
      V : Unsigned_32;
      Br : constant := 16#2_0000#;
   begin
      V := Get_SCTLR;
      V := V or Br;
      Set_SCTLR (V);
   end Enable_Background_Region;

   procedure Mpu_Setup_Region (Num : Unsigned_32;
                               Base : Unsigned_32;
                               Acc : Unsigned_32;
                               Sz : Unsigned_32) is
   begin
      Set_RGNR (Num);
      Set_DRBAR (Base);
      Set_DRACR (Acc);
      Set_DRSR (Sz);
   end Mpu_Setup_Region;

   procedure Init_Mpu is
      SZ_512B : constant := 2 * 16#08#;
      SZ_4GB : constant := 2 * 16#1f#;
      SZ_256KB : constant := 2 * 16#11#;
      SZ_4MB : constant := 2 * 16#15#;
      SZ_8MB : constant := 2 * 16#16#;
      SZ_16MB : constant := 2 * 16#17#;
      SZ_64MB  : constant := 2 * 16#19#;
      SZ_128MB  : constant := 2 * 16#1a#;

      B : constant := 1;
      C : constant := 2;
      S : constant := 4;
      TEX_NC   : constant := 2#001_0_00#;
      pragma Unreferenced (B, C, S);
      pragma Unreferenced (SZ_64MB, SZ_128MB, TEX_NC);
   begin
      Disable_Mpu;
      Disable_Background_Region;
      Mpu_Setup_Region (0,  16#0000_0000#, 16#1008#, 16#ff_01# + SZ_4GB);
      Mpu_Setup_Region (1,  16#0000_0000#, 16#608#, 16#ff_01# + SZ_4MB);
      Mpu_Setup_Region (2,  16#0800_0000#, 16#308#, 16#ff_01# + SZ_256KB);
      Mpu_Setup_Region (3,  16#0840_0000#, 16#308#, 16#ff_01# + SZ_256KB);
      Mpu_Setup_Region (5,  16#8000_0000#, 16#300#, 16#ff_01# + SZ_8MB);
      Mpu_Setup_Region (6,  16#f000_0000#, 16#1208#, 16#ff_01# + SZ_8MB);
      Mpu_Setup_Region (7,  16#fc00_0000#, 16#1310#, 16#ff_01# + SZ_16MB);
      Mpu_Setup_Region (8,  16#fe00_0000#, 16#1310#, 16#ff_01# + SZ_512B);
      Mpu_Setup_Region (9,  16#ff00_0000#, 16#1310#, 16#ff_2f# + SZ_16MB);
      Enable_Background_Region;
      Enable_Mpu;
   end Init_Mpu;

   procedure Init_Iomm is
      KICK_REG0 : Unsigned_32;
      for KICK_REG0'Address use System'To_Address (16#ffff_ea38#);
      pragma Volatile (KICK_REG0);
      pragma Import (Ada, KICK_REG0);

      KICK_REG1 : Unsigned_32;
      for KICK_REG1'Address use System'To_Address (16#ffff_ea3c#);
      pragma Volatile (KICK_REG1);
      pragma Import (Ada, KICK_REG1);

      PINMMR : array (0 .. 31) of Unsigned_32;
      for PINMMR'Address use System'To_Address (16#ffff_eb10#);
      pragma Volatile_Components (PINMMR);
      pragma Import (Ada, PINMMR);

      GPREG1 : Unsigned_32;
      for GPREG1'Address use System'To_Address (16#ffff_ffa0#);
      pragma Volatile (GPREG1);
      pragma Import (Ada, GPREG1);
   begin
      KICK_REG0 := 16#83E7_0B13#;
      KICK_REG1 := 16#95A4_F1E0#;

      PINMMR (1)  := 2**17 + 2**25;  --  DATA4, DATA5
      PINMMR (2)  := 2**9;           --  DATA6
      PINMMR (3)  := 2**1 + 2**25;   --  DATA7, DATA8
      PINMMR (4)  := 2**9;           --  DATA9
      PINMMR (5)  := 2**25;          --  DATA10
      PINMMR (6)  := 2**9 + 2**25;   --  DATA11, DATA12
      PINMMR (7)  := 2**1 + 2**25;   --  DATA13, DATA14
      PINMMR (8)  := 2**25;          --  DATA15
      null;
      PINMMR (10) := 2**9 + 2**16 + 2**25;         --  DATA3, CS0, DATA2
      PINMMR (11) := 2**0 + 2**8 + 2**17;          --  CS3, CS4, DATA1
      PINMMR (12) := 2**9;                         --  DATA0
      PINMMR (14) := 2**16 + 2**24;                --  nWE, BA1
      PINMMR (15) := 2**0 + 2**8 + 2**16 + 2**24;  --  A21, A20, A19, A18
      PINMMR (16) := 2**1 + 2**8 + 2**16;          --  BA0, A17, A16
      PINMMR (17) := 2**9 + 2**25;                 --  nDQM1, A5
      PINMMR (18) := 2**0 + 2**16;                 --  A15, A14
      PINMMR (19) := 2**1 + 2**17 + 2**25;         --  A4, nDQM0, A3
      PINMMR (20) := 2**0 + 2**8 + 2**24;          --  A13, A12, A11
      PINMMR (21) := 2**0 + 2**16 + 2**24;         --  A1, A10, A9
      PINMMR (22) := 2**0 + 2**8 + 2**16 + 2**25;  --  A0, A7, A6, A2
      PINMMR (23) := 2**0;                         --  A8
      PINMMR (29) := 0; -- 2**8;                         --  CLK_SEL
      KICK_REG0 := 0;
      KICK_REG1 := 0;

      GPREG1 := GPREG1 or 2**31;
   end Init_Iomm;

   procedure Init_Emif is
      SDCR : Unsigned_32;
      for SDCR'Address use System'To_Address (16#fcff_e808#);
      pragma Volatile (SDCR);
      pragma Import (Ada, SDCR);

      SDRCR : Unsigned_32;
      for SDRCR'Address use System'To_Address (16#fcff_e80c#);
      pragma Volatile (SDRCR);
      pragma Import (Ada, SDRCR);

      SDTIMR : Unsigned_32;
      for SDTIMR'Address use System'To_Address (16#fcff_e820#);
      pragma Volatile (SDTIMR);
      pragma Import (Ada, SDTIMR);

      SDSRETR : Unsigned_32;
      for SDSRETR'Address use System'To_Address (16#fcff_e83c#);
      pragma Volatile (SDSRETR);
      pragma Import (Ada, SDSRETR);
   begin
      --  EMIF CLK = VCLK3 = HCLK/2 90Mhz (ie ~12ns)
      --  IS42S16400F: 4 Banks, 16Bits

      --  Procedure B of 17.2.5.5 is followed

      --  1. Configure the desired EMIF_CLK
      --  Already done.

      --  2. Program SDTIMR and SDSRETR to satisdy the timing requirement

      --  Formula                            DataSheet     Value
      --  T_RFC >= (tRFC × fEMIF_CLK) - 1    tRC = 63ns     5
      --  T_RP >= (tRP × fEMIF_CLK) - 1      tRP = 20ns     1
      --  T_RCD >= (tRCD × fEMIF_CLK) - 1    tRCD = 20ns    1
      --  T_WR >= (tWR × fEMIF_CLK) - 1      tRDL = 2CLK    1
      --  T_RAS >= (tRAS × fEMIF_CLK) - 1    tRAS = 42ns    3
      --  T_RC >= (tRC × fEMIF_CLK) - 1      tRC = 63ns     5
      --  T_RRD >= (tRRD × fEMIF_CLK) - 1    tRRD = 14ns    1
      SDTIMR := 2#00101_001_0_001_0_001_0011_0101_0_001_0000#;

      --  Self refresh
      --  T_XS >= (tXSR × fEMIF_CLK) - 1     tRC = 63ns     5
      SDSRETR := 5;

      --  3. Program the RR field of SDRCR such as:
      --  (RR * 8) / fEMIF_CLK > 200us
      --  -> RR = 2084
      SDRCR := 2084;

      --  4. Program SDCR to match the characteristics of the SDRAM device

      --  SDRAM configuration
      --  SR = 0, NM = 1, CL = 3, BIT11_9LOCK = 1, IBANK=2, PAGESIZE=0
      SDCR := 2#0_0000000_00000000_0_1_0_0_011_1_0_010_0_000#;

      --  5. Perform a read from the SDRAM
      declare
         Tmp : Unsigned_32;
         for Tmp'Address use System'To_Address (16#8000_0000#);
         pragma Import (Ada, Tmp);
         pragma Volatile (Tmp);

         V : Unsigned_32;
         pragma Unreferenced (V);
      begin
         V := Tmp;
      end;

      --  6. Finally program the RR field to match that of the attached device

      --  Refresh control
      --  RR <= fEMIF_CLK × tRefresh Period / ncycles
      --  tREF = 64ms, ncycles = 4096 -> RR = 1302
      SDRCR := 1302;
   end Init_Emif;

   procedure Proc_Mpu is
      MPUIR : constant Unsigned_32 := Get_MPUIR;
      Nbr_DReg : constant Unsigned_32 := (MPUIR / 2**8) and 16#ff#;
   begin
      Next_Word;
      if End_Pos > Line_Len then
         Put_Line ("MPUIR: " & Image8 (MPUIR));
         Put_Line ("Rg  Base     Sz");
         if Nbr_DReg > 0 then
            for I in 0 .. Nbr_DReg - 1 loop
               Set_RGNR (I);
               declare
                  DRSR : constant Unsigned_32 := Get_DRSR;
                  Sz : Unsigned_32;
                  Base : constant Unsigned_32 := Get_DRBAR;
                  DRACR : constant Unsigned_32 := Get_DRACR;
               begin
                  if (DRSR and DRSR_E) /= 0 then
                     Put (Image2 (I));
                     Put (": ");
                     Put (Image8 (Base));
                     Put ('-');
                     Sz := Shift_Left (1, Natural ((DRSR / 2) and 16#1f#)) - 1;
                     Sz := 2 * Sz + 1;
                     Put (Image8 (Base + Sz));
                     Put (' ');
                     Put (Image8 (DRSR));
                     Put (' ');
                     Put (Image8 (DRACR));
                     Put (' ');
                     Put_Bit ((DRACR and DRACR_XN) = 0, 'X');
                     Put (' ');
                     case DRACR and 16#7_00# is
                        when AP_NO_NO => Put ("--/--");
                        when AP_RW_NO => Put ("RW/--");
                        when AP_RW_RO => Put ("RW/RO");
                        when AP_RW_RW => Put ("RW/RW");
                        when AP_RO_NO => Put ("RO/--");
                        when AP_RO_RO => Put ("RO/RO");
                        when others   => Put ("??/??");
                     end case;
                     Put (' ');
                     if (DRACR and 2#100_0_0_0#) /= 0 then
                        Put ("Outer ");
                        case DRACR and 2#011_0_0_0# is
                           when 2#000_0_0_0# => Put ("NC");
                           when 2#001_0_0_0# => Put ("WB+WA");
                           when 2#010_0_0_0# => Put ("WT+noWA");
                           when 2#011_0_0_0# => Put ("WB+noWA");
                           when others       => Put ("???");
                        end case;
                        Put (", Inner ");
                        case DRACR and 2#011_0_0_0# is
                           when 2#000_0_0_0# => Put ("NC");
                           when 2#000_0_0_1# => Put ("WB+WA");
                           when 2#000_0_1_0# => Put ("WT+noWA");
                           when 2#000_0_1_1# => Put ("WB+noWA");
                           when others       => Put ("???");
                        end case;
                     else
                        case DRACR and 2#011_0_1_1# is --  TEX + C + B
                           when 2#000_0_0_0# => Put ("Strongly ordered");
                           when 2#000_0_0_1# => Put ("Shareable device");
                           when 2#000_0_1_0# => Put ("WT, no WA");
                           when 2#000_0_1_1# => Put ("WB, no WA");
                           when 2#001_0_0_0# => Put ("Non-cacheable");
                           when 2#001_0_0_1# => Put ("Reserved");
                           when 2#001_0_1_0# => Put ("Implementation defined");
                           when 2#001_0_1_1# => Put ("WB, WA");
                           when 2#010_0_0_0# => Put ("Non-shareable device");
                           when 2#010_0_0_1#
                             |  2#010_0_1_0#
                             |  2#010_0_1_1# => Put ("Reserved");
                           when 2#011_0_0_0#
                             |  2#011_0_0_1#
                             |  2#011_0_1_0#
                             |  2#011_0_1_1# => Put ("Shareable device");
                           when others       => Put ("???");
                        end case;
                     end if;
                     New_Line;
                  end if;
               end;
            end loop;
         end if;
      elsif Line (Pos .. End_Pos) = "init" then
         Init_Mpu;
      else
         Put_Line ("unknown mpu command");
      end if;
   end Proc_Mpu;

   procedure Proc_Sysesr is
      SYS_SYSESR : Unsigned_32;
      for SYS_SYSESR'Address use System'To_Address (16#ffff_ffe4#);
      pragma Volatile (SYS_SYSESR);
      pragma Import (Ada, SYS_SYSESR);

      PORST : constant := 2**15;
      OSCRST : constant := 2**14;
      WDRST : constant := 2**13;
      CPURST : constant := 2**5;
      SWRST : constant := 2**4;
      EXTRST : constant := 2**3;

      SYSESR : constant Unsigned_32 := SYS_SYSESR;
   begin
      Put ("Reset status:");
      if (SYSESR and PORST) /= 0 then
         Put (" Power-On");
      end if;
      if (SYSESR and OSCRST) /= 0 then
         Put (" Oscillator");
      end if;
      if (SYSESR and WDRST) /= 0 then
         Put (" Watchdog");
      end if;
      if (SYSESR and CPURST) /= 0 then
         Put (" CPU");
      end if;
      if (SYSESR and SWRST) /= 0 then
         Put (" Software");
      end if;
      if (SYSESR and EXTRST) /= 0 then
         Put (" External");
      end if;
      New_Line;
      SYS_SYSESR := SYSESR;
   end Proc_Sysesr;

   type Handler_Context is record
      R0  : Unsigned_32;
      R1  : Unsigned_32;
      R2  : Unsigned_32;
      R3  : Unsigned_32;
      R4  : Unsigned_32;
      R5  : Unsigned_32;
      R6  : Unsigned_32;
      R7  : Unsigned_32;
      R8  : Unsigned_32;
      R9  : Unsigned_32;
      R10 : Unsigned_32;
      R11 : Unsigned_32;
      R12 : Unsigned_32;
      LR  : Unsigned_32;
   end record;
   pragma Convention (C, Handler_Context);

   type Handler_Context_Acc is access all Handler_Context;
   pragma Convention (C, Handler_Context_Acc);

   procedure Data_Abort_Handler (Ctxt : Handler_Context_Acc);
   pragma Export (C, Data_Abort_Handler);

   pragma Warnings (Off);
   procedure Force_Dabt_Handler;
   pragma Import (Asm, Force_Dabt_Handler);
   Reference_To_Force_Dabt_Handler : System.Address :=
     Force_Dabt_Handler'Address;
   pragma Volatile (Reference_To_Force_Dabt_Handler);
   pragma Unreferenced (Reference_To_Force_Dabt_Handler);
   pragma Warnings (On);

   procedure Data_Abort_Handler (Ctxt : Handler_Context_Acc) is
      DFSR : constant Unsigned_32 := Get_DFSR;
   begin
      New_Line;
      Put_Line ("*** DATA Abort ***");
      Put ("R0: ");
      Put (Image8 (Ctxt.R0));
      Put ("  R1: ");
      Put (Image8 (Ctxt.R1));
      Put ("  R2: ");
      Put (Image8 (Ctxt.R2));
      Put ("  R3: ");
      Put (Image8 (Ctxt.R3));
      New_Line;
      Put ("R4: ");
      Put (Image8 (Ctxt.R4));
      Put ("  R5: ");
      Put (Image8 (Ctxt.R5));
      Put ("  R6: ");
      Put (Image8 (Ctxt.R6));
      Put ("  R7: ");
      Put (Image8 (Ctxt.R7));
      New_Line;
      Put ("R8: ");
      Put (Image8 (Ctxt.R8));
      Put ("  R9: ");
      Put (Image8 (Ctxt.R9));
      Put (" R10: ");
      Put (Image8 (Ctxt.R10));
      Put (" R11: ");
      Put (Image8 (Ctxt.R11));
      New_Line;
      Put ("PC: ");
      Put (Image8 (Ctxt.LR));
      New_Line;
      Put ("DFSR: ");
      Put (Image8 (DFSR));
      Put (' ');
      case (DFSR and 16#0f#) or Shift_Right (DFSR and 16#400#, 6) is
         when 2#00001# => Put ("alignment");
         when 2#00000# => Put ("background");
         when 2#01101# => Put ("permission");
         when 2#01000# => Put ("sync external abort");
         when 2#10110# => Put ("async external abort");
         when 2#11001# => Put ("sync parity/ECC error");
         when 2#11000# => Put ("async parity/ECC error");
         when 2#00010# => Put ("debug");
         when others => Put ("?unknown?");
      end case;
      Put ("  ADFSR: ");
      Put (Image8 (Get_ADFSR));
      Put ("  DFAR: ");
      Put (Image8 (Get_DFAR));
      New_Line;
      New_Line;
      System.Machine_Reset.Stop;
   end Data_Abort_Handler;

   procedure Proc_Tcram is
      TCRAM1_RAMCTRL : Unsigned_32;
      for TCRAM1_RAMCTRL'Address use System'To_Address (16#ffff_f800#);
      pragma Volatile (TCRAM1_RAMCTRL);
      pragma Import (Ada, TCRAM1_RAMCTRL);

      TCRAM2_RAMCTRL : Unsigned_32;
      for TCRAM2_RAMCTRL'Address use System'To_Address (16#ffff_f900#);
      pragma Volatile (TCRAM2_RAMCTRL);
      pragma Import (Ada, TCRAM2_RAMCTRL);
   begin
      Next_Word;
      if End_Pos > Line_Len then
         Put_Line ("tcram");
      elsif Line (Pos .. End_Pos) = "ecc" then
         Enable_Btcm_Ecc;
      elsif Line (Pos .. End_Pos) = "noecc" then
         Disable_Btcm_Ecc;
      elsif Line (Pos .. End_Pos) = "roecc" then
         --  Disable write to ECC RAM.
         TCRAM1_RAMCTRL := 16#005_000a#;
         TCRAM2_RAMCTRL := 16#005_000a#;
      elsif Line (Pos .. End_Pos) = "rwecc" then
         --  Enable write to ECC RAM.
         TCRAM1_RAMCTRL := 16#005_010a#;
         TCRAM2_RAMCTRL := 16#005_010a#;
      else
         Put_Line ("Unknown subcommand");
      end if;
   end Proc_Tcram;

   procedure Put_Bit (Val : Unsigned_32; M_0 : String; M_1 : String) is
   begin
      if Val = 0 then
         Put ("0: ");
         Put (M_0);
      else
         Put ("1: ");
         Put (M_1);
      end if;
   end Put_Bit;

   procedure Proc_SCTLR is
      SCTLR : constant Unsigned_32 := Get_SCTLR;
   begin
      Put ("SCTLR: ");
      Put_Line (Image8 (SCTLR));

      Put ("IE: endian: ");
      Put_Bit (SCTLR and 2**31, "little", "big");
      New_Line;
      Put ("TE: exception generation: ");
      Put_Bit (SCTLR and 2**30, "arm", "thumb");
      New_Line;
      Put ("AFE: access flag enable: ");
      Put_Bit (SCTLR and 2**29, "", "");
      New_Line;
      Put ("TRE: TEX remap enable: ");
      Put_Bit (SCTLR and 2**28, "", "");
      New_Line;
      Put ("NMFI: non-maskable FIRQ: ");
      Put_Bit (SCTLR and 2**27, "yes", "no");
      New_Line;
      Put ("EE: CPSR E bit set to: ");
      Put_Bit (SCTLR and 2**25, "", "");
      New_Line;
      Put ("VE: exception vector address: ");
      Put_Bit (SCTLR and 2**24, "at fixed address", "provided by VIC");
      New_Line;
      Put ("FI: fast interrupt enable: ");
      Put_Bit (SCTLR and 2**21, "no", "yes");
      New_Line;
      Put ("DZ: divide by zero: ");
      Put_Bit (SCTLR and 2**19, "no exception", "undefined exception");
      New_Line;
      Put ("BR: MPU background region: ");
      Put_Bit (SCTLR and 2**17, "no", "yes");
      New_Line;
      Put ("RR: replacement strategy for cache: ");
      Put_Bit (SCTLR and 2**14, "random", "round-robin");
      New_Line;
      Put ("V: exception vector: ");
      Put_Bit (SCTLR and 2**13, "0x00000000", "0xffff0000");
      New_Line;
      Put ("I: L1 I-cache: ");
      Put_Bit (SCTLR and 2**12, "disabled", "enabled");
      New_Line;
      Put ("Z: branch prediction: ");
      Put_Bit (SCTLR and 2**11, "disabled", "enabled");
      New_Line;
      Put ("C: L1 D-cache: ");
      Put_Bit (SCTLR and 2**2, "disabled", "enabled");
      New_Line;
      Put ("A: struct alignment fault checking: ");
      Put_Bit (SCTLR and 2**1, "disabled", "enabled");
      New_Line;
      Put ("M: MPU: ");
      Put_Bit (SCTLR and 2**1, "disabled", "enabled");
      New_Line;
   end Proc_SCTLR;

   procedure Proc_ACTLR is
      ACTLR : constant Unsigned_32 := Get_ACTLR;
   begin
      Put ("ACTLR: ");
      Put_Line (Image8 (ACTLR));

      Put ("DICDI: case C dual issue: ");
      Put_Bit (ACTLR and 2**31, "enable", "disable");
      New_Line;
      Put ("DIB2DI: case B2 dual issue: ");
      Put_Bit (ACTLR and 2**30, "enable", "disable");
      New_Line;
      Put ("DIB1DI: case B1 dual issue: ");
      Put_Bit (ACTLR and 2**29, "enable", "disable");
      New_Line;
      Put ("DIADI: case A dual issue: ");
      Put_Bit (ACTLR and 2**28, "enable", "disable");
      New_Line;
      Put ("B1TCMPCEN: B1TCM parity/ECC check: ");
      Put_Bit (ACTLR and 2**27, "disabled", "enabled");
      New_Line;
      Put ("B0TCMPCEN: B0TCM parity/ECC check: ");
      Put_Bit (ACTLR and 2**26, "disabled", "enabled");
      New_Line;
      Put ("ATCMPCEN: ATCM parity/ECC check: ");
      Put_Bit (ACTLR and 2**25, "disabled", "enabled");
      New_Line;
      Put ("AXISCEN: AXI slave cache RAM access: ");
      Put_Bit (ACTLR and 2**24, "disabled", "enabled");
      New_Line;
      Put ("AXISCUEN: AXI slave cache RAM user access: ");
      Put_Bit (ACTLR and 2**23, "disabled", "enabled");
      New_Line;
      Put ("DILSM: low interrupt latency on load/store multiple: ");
      Put_Bit (ACTLR and 2**22, "enable", "disable");
      New_Line;
      Put ("DEOLP: end of loop prediction: ");
      Put_Bit (ACTLR and 2**21, "enable", "disable");
      New_Line;
      Put ("DBHE: branch history: ");
      Put_Bit (ACTLR and 2**20, "enable", "disable");
      New_Line;
      Put ("FRCDIS: fetch rate control: ");
      Put_Bit (ACTLR and 2**19, "enable", "disable");
      New_Line;
      Put ("RSDIS: return stack: ");
      Put_Bit (ACTLR and 2**17, "enable", "disable");
      New_Line;
      --  ??? BP
      Put ("DBWR: write burst in the AXI master: ");
      Put_Bit (ACTLR and 2**14, "enable", "disable");
      New_Line;
      Put ("DLFO: linefill optimization in the AXI master: ");
      Put_Bit (ACTLR and 2**13, "enable", "disable");
      New_Line;
      Put ("ERPEG: random parity error generation: ");
      Put_Bit (ACTLR and 2**12, "disabled", "enabled");
      New_Line;
      Put ("DNCH: data forwarding for NC accesses in the AXI master: ");
      Put_Bit (ACTLR and 2**11, "enable", "disable");
      New_Line;
      Put ("FORA: force outer read allocate for OWA regions: ");
      Put_Bit (ACTLR and 2**10, "disabled", "enabled");
      New_Line;
      Put ("FWT: force write-throught for write-back regions: ");
      Put_Bit (ACTLR and 2**9, "disabled", "enabled");
      New_Line;
      Put ("FDSnS: force data side to not-shared when MPU is off: ");
      Put_Bit (ACTLR and 2**8, "disabled", "enabled");
      New_Line;
      Put ("sMOV: sMOV out of order: ");
      Put_Bit (ACTLR and 2**7, "enabled", "disabled");
      New_Line;
      Put ("DILS: low interrupt latency on all load/store insns: ");
      Put_Bit (ACTLR and 2**6, "enable", "disable");
      New_Line;
      --  ??? CEC
      Put ("B1TCMECEN: B1TCM external error: ");
      Put_Bit (ACTLR and 2**2, "disabled", "enabled");
      New_Line;
      Put ("B0TCMECEN: B0TCM external error: ");
      Put_Bit (ACTLR and 2**1, "disabled", "enabled");
      New_Line;
      Put ("ATCMECEN: ATCM external error: ");
      Put_Bit (ACTLR and 2**0, "disabled", "enabled");
      New_Line;
   end Proc_ACTLR;

   Commands : aliased Command_List :=
    (5,
     ((new String'("mpu [init] - Display or initialize MPU config"),
       Proc_Mpu'Access),
      (new String'("sysesr - Display and clear reset cause"),
       Proc_Sysesr'Access),
      (new String'("sctlr - Display SCTLR"),
       Proc_SCTLR'Access),
      (new String'("actlr - Display ACTLR"),
       Proc_ACTLR'Access),
      (new String'("tcram [ecc | noecc] - Display BTCRAM"),
       Proc_Tcram'Access)),
     null);
begin
   Register_Commands (Commands'Access);

   --  Setup sdram
   Init_Iomm;
   Init_Emif;
end Hdk;
