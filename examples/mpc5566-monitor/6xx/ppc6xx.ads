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

with Ppc; use Ppc;

package Ppc6xx is
   pragma Elaborate_Body;

   function Get_Pir is new Get_Spr (286);
   function Get_Pvr is new Get_Spr (287);

   function Get_Hid0 is new Get_Spr (1008);
   function Get_Hid1 is new Get_Spr (1009);
   function Get_Hid2 is new Get_Spr (1011);

   function Get_Dec is new Get_Spr (22);

   --  BAT bits.
   Bepi_Mask : constant := 16#fffe_0000#;
   BL_128KB : constant := 2#000_0000_0000_00#;
   BL_256KB : constant := 2#000_0000_0001_00#;
   BL_512KB : constant := 2#000_0000_0011_00#;
   BL_1MB   : constant := 2#000_0000_0111_00#;
   BL_2MB   : constant := 2#000_0000_1111_00#;
   BL_4MB   : constant := 2#000_0001_1111_00#;
   BL_8MB   : constant := 2#000_0011_1111_00#;
   BL_16MB  : constant := 2#000_0111_1111_00#;
   BL_32MB  : constant := 2#000_1111_1111_00#;
   BL_64MB  : constant := 2#001_1111_1111_00#;
   BL_128MB : constant := 2#011_1111_1111_00#;
   BL_256MB : constant := 2#111_1111_1111_00#;
   Vs_Mask : constant := 16#02#;
   Vp_Mask : constant := 16#01#;
   WIMG_W : constant := 2#1000_000#;
   WIMG_I : constant := 2#0100_000#;
   WIMG_M : constant := 2#0010_000#;
   WIMG_G : constant := 2#0001_000#;
   WIMG_WB : constant := WIMG_M; -- For data
   WIMG_CE : constant := WIMG_M; -- For code
   WIMG_UC : constant := WIMG_I + WIMG_G; -- For data
   Brpn_Mask : constant := 16#fffe_0000#;
   Pp_Mask : constant := 2#11#;
   Pp_No : constant := 2#00#;
   Pp_RO : constant := 2#01#;
   Pp_RW : constant := 2#10#;

   --  BAT SPR numbers
   Ibat0u : constant := 528;
   Ibat0l : constant := 529;
   Ibat1u : constant := 530;
   Ibat1l : constant := 531;
   Ibat2u : constant := 532;
   Ibat2l : constant := 533;
   Ibat3u : constant := 534;
   Ibat3l : constant := 535;

   Dbat0u : constant := 536;
   Dbat0l : constant := 537;
   Dbat1u : constant := 538;
   Dbat1l : constant := 539;
   Dbat2u : constant := 540;
   Dbat2l : constant := 541;
   Dbat3u : constant := 542;
   Dbat3l : constant := 543;

   function Get_Dbat0u is new Get_Spr (Dbat0u);
   function Get_Dbat0l is new Get_Spr (Dbat0l);
   function Get_Dbat1u is new Get_Spr (Dbat1u);
   function Get_Dbat1l is new Get_Spr (Dbat1l);
   function Get_Dbat2u is new Get_Spr (Dbat2u);
   function Get_Dbat2l is new Get_Spr (Dbat2l);
   function Get_Dbat3u is new Get_Spr (Dbat3u);
   function Get_Dbat3l is new Get_Spr (Dbat3l);
   function Get_Dbat4u is new Get_Spr (568);
   function Get_Dbat4l is new Get_Spr (569);
   function Get_Dbat5u is new Get_Spr (570);
   function Get_Dbat5l is new Get_Spr (571);
   function Get_Dbat6u is new Get_Spr (572);
   function Get_Dbat6l is new Get_Spr (573);
   function Get_Dbat7u is new Get_Spr (574);
   function Get_Dbat7l is new Get_Spr (575);

   function Get_Ibat0u is new Get_Spr (Ibat0u);
   function Get_Ibat0l is new Get_Spr (Ibat0l);
   function Get_Ibat1u is new Get_Spr (Ibat1u);
   function Get_Ibat1l is new Get_Spr (Ibat1l);
   function Get_Ibat2u is new Get_Spr (Ibat2u);
   function Get_Ibat2l is new Get_Spr (Ibat2l);
   function Get_Ibat3u is new Get_Spr (Ibat3u);
   function Get_Ibat3l is new Get_Spr (Ibat3l);
   function Get_Ibat4u is new Get_Spr (560);
   function Get_Ibat4l is new Get_Spr (561);
   function Get_Ibat5u is new Get_Spr (562);
   function Get_Ibat5l is new Get_Spr (563);
   function Get_Ibat6u is new Get_Spr (564);
   function Get_Ibat6l is new Get_Spr (565);
   function Get_Ibat7u is new Get_Spr (566);
   function Get_Ibat7l is new Get_Spr (567);
end Ppc6xx;
