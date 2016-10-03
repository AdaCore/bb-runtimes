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
with Dumps; use Dumps;
with System.Machine_Code; use System.Machine_Code;
with System; use System;
pragma Warnings (Off);
with System.Machine_Reset;
pragma Warnings (On);
with Term; use Term;

package body Ppc6xx is
   type Core_Type is (Core_Unknown, Core_E300);
   Core : Core_Type := Core_Unknown;

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

   pragma Unreferenced (Put_Bit);

   procedure Disp_Msr is
   begin
      Put_Register ("MSR", Get_Msr,
                    "13,pow,1,ile,ee,pr,fp,me,fe0,se,be," &
                      "fe1,1,ip,ir,dr,2,ri,le");
      New_Line;
   end Disp_Msr;

   procedure Proc_Cr is
      Pvr : constant Unsigned_32 := Get_Pvr;
   begin
      Put ("PVR: " & Image8 (Pvr));
      Put (" ");
      case Shift_Right (Pvr, 16) is
         when 16#8083# => Put ("e300c1");
         when 16#8084# => Put ("e300c2");
         when 16#8085# => Put ("e300c3");
         when others =>   Put ("unknown");
      end case;
      New_Line;

      Disp_Msr;
      Put ("HID0: " & Image8 (Get_Hid0));
      case Core is
         when Core_E300 =>
            Put_Register_Desc
              (Get_Hid0,
               "emcp,ecpe,eba,ebd,sbclk,1,eclk,par,doze,nap,sleep,dpm,2,nhr," &
                 "ice,dce,ilock,dlock,icfi,dcfi,2,ifem,2,fbiob,abe,2,noopti");
         when others =>
            null;
      end case;
      New_Line;
      Put_Line ("HID1: " & Image8 (Get_Hid1));

      if Core = Core_E300 then
         Put_Register
           ("HID2",
            Get_Hid2,
            "4,let,ifeb,1,mesi,ifec,ebqs,ebpx,2,hbe,2," &
            "iwlck:3,icwp,4,dwlck:3,5");
      end if;
   end Proc_Cr;

   procedure Proc_Timer is
   begin
      Put_Line ("TBU: " & Image8 (Get_Tbu) & " TBL: " & Image8 (Get_Tbl));
      Put_Line ("DEC: " & Image8 (Get_Dec));
   end Proc_Timer;

   procedure Show_Bat (L, U : Unsigned_32; Name : String) is
      Bl : constant Unsigned_32 := U and BL_256MB;
   begin
      Put (Name);
      Put ("l: " & Image8 (L));
      Put (" u: " & Image8 (U));
      Put (" len: ");
      case Bl is
         when BL_128KB => Put ("128KB");
         when BL_256KB => Put ("256KB");
         when BL_512KB => Put ("512KB");
         when BL_1MB   => Put ("  1MB");
         when BL_2MB   => Put ("  2MB");
         when BL_4MB   => Put ("  4MB");
         when BL_8MB   => Put ("  8MB");
         when BL_16MB  => Put (" 16MB");
         when BL_32MB  => Put (" 32MB");
         when BL_64MB  => Put (" 64MB");
         when BL_128MB => Put ("128MB");
         when BL_256MB => Put ("256MB");
         when others =>   Put ("  ???");
      end case;
      Put (" mode:");
      Put_Bit ((U and Vs_Mask) /= 0, 'S');
      Put_Bit ((U and Vp_Mask) /= 0, 'U');
      Put (", cache:");
      Put_Bit ((L and WIMG_W) /= 0, 'W');
      Put_Bit ((L and WIMG_I) /= 0, 'I');
      Put_Bit ((L and WIMG_M) /= 0, 'M');
      Put_Bit ((L and WIMG_G) /= 0, 'G');
      Put (" prot:");
      case L and Pp_Mask is
         when Pp_No => Put ("--");
         when Pp_RW => Put ("RW");
         when others => Put ("RO");
      end case;
      New_Line;
   end Show_Bat;

   procedure Proc_Bat is
   begin
      Show_Bat (Get_Dbat0l, Get_Dbat0u, "dbat0");
      Show_Bat (Get_Dbat1l, Get_Dbat1u, "dbat1");
      Show_Bat (Get_Dbat2l, Get_Dbat2u, "dbat2");
      Show_Bat (Get_Dbat3l, Get_Dbat3u, "dbat3");
      if Core = Core_E300 then
         Show_Bat (Get_Dbat4l, Get_Dbat4u, "dbat4");
         Show_Bat (Get_Dbat5l, Get_Dbat5u, "dbat5");
         Show_Bat (Get_Dbat6l, Get_Dbat6u, "dbat6");
         Show_Bat (Get_Dbat7l, Get_Dbat7u, "dbat7");
      end if;
      New_Line;
      Show_Bat (Get_Ibat0l, Get_Ibat0u, "ibat0");
      Show_Bat (Get_Ibat1l, Get_Ibat1u, "ibat1");
      Show_Bat (Get_Ibat2l, Get_Ibat2u, "ibat2");
      Show_Bat (Get_Ibat3l, Get_Ibat3u, "ibat3");
      if Core = Core_E300 then
         Show_Bat (Get_Ibat4l, Get_Ibat4u, "ibat4");
         Show_Bat (Get_Ibat5l, Get_Ibat5u, "ibat5");
         Show_Bat (Get_Ibat6l, Get_Ibat6u, "ibat6");
         Show_Bat (Get_Ibat7l, Get_Ibat7u, "ibat7");
      end if;
   end Proc_Bat;

   --  Set the sc vector.
   procedure Set_Excp_Vector (Addr : Address) is
      Vector : Unsigned_32;
      for Vector'Address use Addr;

      Fault_Entry : Unsigned_32;
      pragma Import (C, Fault_Entry, "fault_entry");
   begin
      Vector := Fault_Entry;
      Asm ("dcbst 0,%0; icbi 0,%0",
           Inputs => System.Address'Asm_Input ("r", Addr),
           Volatile => True);
   end Set_Excp_Vector;

   procedure Fault_Handler (Ip : Unsigned_32; Msr : Unsigned_32);
   pragma Export (C, Fault_Handler);

   procedure Fault_Handler (Ip : Unsigned_32; Msr : Unsigned_32) is
   begin
      New_Line;
      Put_Line ("Fault handler");
      Put ("IP: ");
      Put (Image8 (Ip));
      Put ("  MSR: ");
      Put (Image8 (Msr));
      New_Line;
      Disp_Msr;
      New_Line;
      System.Machine_Reset.Stop;
   end Fault_Handler;

   procedure Proc_Fault is
   begin
      Next_Word;
      if Line (Pos .. End_Pos) = "install" then
         Set_Excp_Vector (System'To_Address (16#0100#)); --  Reset
         Set_Excp_Vector (System'To_Address (16#0200#)); --  Machine check
         Set_Excp_Vector (System'To_Address (16#0300#)); --  DSI
         Set_Excp_Vector (System'To_Address (16#0400#)); --  ISI
         Set_Excp_Vector (System'To_Address (16#0500#)); --  External Interrupt
         Set_Excp_Vector (System'To_Address (16#0600#)); --  Alignment
         Set_Excp_Vector (System'To_Address (16#0700#)); --  Program
         Set_Excp_Vector (System'To_Address (16#0800#)); --  FP unavailable
         Set_Excp_Vector (System'To_Address (16#0900#)); --  Decr
         Set_Excp_Vector (System'To_Address (16#0c00#)); --  System call
         Set_Excp_Vector (System'To_Address (16#1000#)); --  itlb
         Set_Excp_Vector (System'To_Address (16#1100#)); --  dtlb load
         Set_Excp_Vector (System'To_Address (16#1200#)); --  dtlb write
         Set_Excp_Vector (System'To_Address (16#1300#)); --  iabr
      elsif Line (Pos .. End_Pos) = "sc" then
         Asm ("sc", Volatile => True);
      else
         Put_Line ("unknown fault command");
      end if;
   end Proc_Fault;

   Commands : aliased Command_List :=
     (4,
      ((new String'("cr - Display some config registers"),
        Proc_Cr'Access),
       (new String'("timer - Display time registers"),
        Proc_Timer'Access),
       (new String'("bat - Display BAT registers"),
        Proc_Bat'Access),
       (new String'("fault install|sc - Fault handling"),
        Proc_Fault'Access)),
      null);
begin
   Register_Commands (Commands'Access);

   --  Find core type
   case Shift_Right (Get_Pvr, 16) is
      when 16#8083# .. 16#8085# => Core := Core_E300;
      when others => null;
   end case;
end Ppc6xx;
