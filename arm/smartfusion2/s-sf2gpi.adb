------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.SF2.GPIO;             use Interfaces.SF2.GPIO;
with Interfaces.SF2.System_Registers; use Interfaces.SF2.System_Registers;

package body System.SF2.GPIO is

   ---------------
   -- GPIO_Init --
   ---------------

   procedure GPIO_Init
   is
      SysReg : System_Registers_Peripheral renames System_Registers_Periph;
   begin
      --  Reset the MSS GPIO Hardware
      SysReg.SOFT_RESET_CR.MSS_GPIO_SOFTRESET := True;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_7_0_SOFT_RESET := True;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_15_8_SOFT_RESET := True;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_23_16_SOFT_RESET := True;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_31_24_SOFT_RESET := True;

      --  Take MSS GPIO hardware out of reset
      SysReg.SOFT_RESET_CR.MSS_GPOUT_7_0_SOFT_RESET := False;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_15_8_SOFT_RESET := False;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_23_16_SOFT_RESET := False;
      SysReg.SOFT_RESET_CR.MSS_GPOUT_31_24_SOFT_RESET := False;
      SysReg.SOFT_RESET_CR.MSS_GPIO_SOFTRESET := False;
   end GPIO_Init;

   -----------------
   -- GPIO_Config --
   -----------------

   procedure GPIO_Config
     (Num  : GPIO_Num;
      Mode : GPIO_Mode)
   is
   begin
      case Mode is
         when Input_Mode =>
            GPIO_Periph.CONFIG (Num) :=
              (EN_IN  => True,
               others => <>);
         when Output_Mode =>
            GPIO_Periph.CONFIG (Num) :=
              (EN_OUT    => True,
               EN_OE_BUF => True,
               others => <>);
         when In_Out_Mode =>
            GPIO_Periph.CONFIG (Num) :=
              (EN_OUT    => True,
               EN_IN     => True,
               others    => <>);
      end case;
   end GPIO_Config;

   ---------------------------
   -- GPIO_Config_Interrupt --
   ---------------------------

   procedure GPIO_Config_Interrupt
     (Num   : GPIO_Num;
      Event : GPIO_Interrupt_Mode;
      Mode  : GPIO_Mode := Input_Mode)
   is
   begin
      GPIO_Config (Num, Mode);

      case Event is
         when IRQ_Level_High =>
            GPIO_Periph.CONFIG (Num).TYPES_INT := Int_Level_High;
         when IRQ_Level_Low =>
            GPIO_Periph.CONFIG (Num).TYPES_INT := Int_Level_Low;
         when IRQ_Edge_Positive =>
            GPIO_Periph.CONFIG (Num).TYPES_INT := Int_Edge_Positive;
         when IRQ_Edge_Negative =>
            GPIO_Periph.CONFIG (Num).TYPES_INT := Int_Edge_Negative;
         when IRQ_Edge_Both =>
            GPIO_Periph.CONFIG (Num).TYPES_INT := Int_Edge_Both;
      end case;
   end GPIO_Config_Interrupt;

   ---------
   -- Set --
   ---------

   procedure Set (Num : GPIO_Num) is
   begin
      GPIO_Periph.GPOUT.Arr (Num) := True;
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (Num : GPIO_Num) is
   begin
      GPIO_Periph.GPOUT.Arr (Num) := False;
   end Clear;

   ---------
   -- Set --
   ---------

   function Set (Num : GPIO_Num) return Boolean
   is
   begin
      return GPIO_Periph.GPIN.Arr (Num);
   end Set;

end System.SF2.GPIO;
