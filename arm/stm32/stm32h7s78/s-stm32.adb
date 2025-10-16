with System;
with Interfaces; use Interfaces;

--  Optional SVD packages (adjust names to your SVD generator)
with STM32_SVD.RCC;
with STM32_SVD.PWR;
with STM32_SVD.FLASH;

package body S.STM32 is
   --  Clock / power bring-up for STM32H7S7L8 @ 600 MHz CPU, HSE=24 MHz.
   --  This is a *stub* with concrete register programming steps.
   --  Verify field names against your SVD (H7R/S differs from F7/H7x3).

   package RCC   renames STM32_SVD.RCC;
   package PWR   renames STM32_SVD.PWR;
   package FLASH renames STM32_SVD.FLASH;

   --  PLL1 plan (24 MHz HSE):
   --    PLLM = 6      → ref = 24/6  = 4 MHz
   --    PLLN = 300    → VCO = 4 * 300 = 1200 MHz
   --    PLLP = 2      → PLL1_P = 1200/2 = 600 MHz  (CPUCLK source)
   --    PLLQ = 4      → PLL1_Q = 1200/4 = 300 MHz  (peripherals / sys_ck candidate)
   --    PLLR = 4      → PLL1_R = 1200/4 = 300 MHz
   --  AHB prescaler (HPRE) = /2 to keep AXI/sys_ck ~= 300 MHz
   --  APBx prescalers often /2 to keep <= 150 MHz (verify per peripheral limits).

   PLLM : constant := 6;
   PLLN : constant := 300;
   PLLP : constant := 2;
   PLLQ : constant := 4;
   PLLR : constant := 4;

   procedure Small_Delay is
      pragma Inline (Small_Delay);
   begin
      for K in 1 .. 1000 loop
         null;
      end loop;
   end Small_Delay;

   procedure Enable_Caches is
      --  Enable I-Cache & D-Cache in FLASH/SCB as appropriate.
      --  Many H7R/S parts gate cache control via FLASH.ACR and SCB.CCR.
      --  Here we set FLASH prefetch and caches; SCB cache enable may be done
      --  in your startup as well (not shown).
   begin
      declare
         ACR : FLASH.ACR_Register := FLASH.ACR;
      begin
         --  LATENCY=7 (8 WS), PRFTEN=1, ICEN=1, DCEN=1, WRHIGHFREQ=0b11
         ACR.LATENCY      := 7;
         ACR.PRFTEN       := 1;
         ACR.ICEN         := 1;
         ACR.DCEN         := 1;
         ACR.WRHIGHFREQ   := 3;   --  0b11
         FLASH.ACR        := ACR;
      end;
   end Enable_Caches;

   procedure Set_VOS_High is
      --  Set voltage scaling to High performance range and wait ready.
      --  H7R/S uses PWR_VOSCR/VOSSR registers (names may differ in your SVD).
   begin
      declare
         V : PWR.VOSCR_Register := PWR.VOSCR;
      begin
         --  VOS = 0b11 → High
         V.VOS := 3;
         PWR.VOSCR := V;
      end;

      --  Wait until voltage scaling ready
      loop
         exit when PWR.VOSSR.VOSRDY = 1;
      end loop;
   end Set_VOS_High;

   procedure Enable_HSE is
   begin
      declare
         CR : RCC.CR_Register := RCC.CR;
      begin
         CR.HSEON := 1;
         RCC.CR   := CR;
      end;

      --  Wait for HSERDY
      loop
         exit when RCC.CR.HSERDY = 1;
      end loop;
   end Enable_HSE;

   procedure Configure_PLL1 is
   begin
      --  Disable PLL1 before configuration
      declare
         CR : RCC.CR_Register := RCC.CR;
      begin
         CR.PLL1ON := 0;
         RCC.CR    := CR;
      end;
      loop
         exit when RCC.CR.PLL1RDY = 0;
      end loop;

      --  Select HSE as PLL source, set DIVM1, DIVN1, DIVPQR, disable FRACN
      declare
         CFGR : RCC.PLLCKSELR_Register := RCC.PLLCKSELR;
      begin
         CFGR.PLLSRC := 3;            --  0: None, 1: HSI, 2: CSI, 3: HSE
         CFGR.DIVM1  := PLLM;
         RCC.PLLCKSELR := CFGR;
      end;

      declare
         PLLCFGR : RCC.PLLCFGR_Register := RCC.PLLCFGR;
      begin
         PLLCFGR.PLL1VCOSEL := 0;     --  Wide VCO range
         PLLCFGR.PLL1FRACEN := 0;     --  Integer mode
         PLLCFGR.DIVP1EN    := 1;
         PLLCFGR.DIVQ1EN    := 1;
         PLLCFGR.DIVR1EN    := 1;
         RCC.PLLCFGR := PLLCFGR;
      end;

      declare
         DIVNR  : RCC.PLL1DIVR_Register := RCC.PLL1DIVR;
      begin
         --  Registers typically encode N-1, P-1, Q-1, R-1.
         DIVNR.DIVN1 := PLLN - 1;
         DIVNR.DIVP1 := PLLP - 1;
         DIVNR.DIVQ1 := PLLQ - 1;
         DIVNR.DIVR1 := PLLR - 1;
         RCC.PLL1DIVR := DIVNR;
      end;

      --  Enable PLL1 and wait ready
      declare
         CR : RCC.CR_Register := RCC.CR;
      begin
         CR.PLL1ON := 1;
         RCC.CR    := CR;
      end;
      loop
         exit when RCC.CR.PLL1RDY = 1;
      end loop;
   end Configure_PLL1;

   procedure Configure_Bus_Prescalers is
   begin
      declare
         D1CFGR : RCC.D1CFGR_Register := RCC.D1CFGR;
      begin
         --  HPRE = /2 → HCLK ~ 300 MHz (sys_ck)
         D1CFGR.HPRE  := 0b1000;  --  divide by 2
         --  D1CPRE: CPU clock = PLL1P (no extra divide here)
         D1CFGR.D1CPRE := 0b0000; --  sysclk not divided for CPU
         RCC.D1CFGR := D1CFGR;
      end;

      declare
         D2CFGR : RCC.D2CFGR_Register := RCC.D2CFGR;
      begin
         --  APB1/APB2 prescalers = /2 → 150 MHz when HCLK=300 MHz
         D2CFGR.D2PPRE1 := 0b100; -- divide by 2
         D2CFGR.D2PPRE2 := 0b100; -- divide by 2
         RCC.D2CFGR := D2CFGR;
      end;

      declare
         D3CFGR : RCC.D3CFGR_Register := RCC.D3CFGR;
      begin
         --  APB4 prescaler = /2
         D3CFGR.D3PPRE  := 0b100;
         RCC.D3CFGR := D3CFGR;
      end;
   end Configure_Bus_Prescalers;

   procedure Switch_System_Clock is
   begin
      declare
         CFGR : RCC.CFGR_Register := RCC.CFGR;
      begin
         CFGR.SW := 0b011;   --  Switch to PLL1P
         RCC.CFGR := CFGR;
      end;
      --  Wait for switch complete
      loop
         exit when RCC.CFGR.SWS = 0b011;
      end loop;
   end Switch_System_Clock;

   procedure System_Init is
   begin
      Set_VOS_High;
      Enable_HSE;

      --  Program FLASH latencies & caches *before* ramping clocks
      Enable_Caches;

      Configure_PLL1;
      Configure_Bus_Prescalers;
      Switch_System_Clock;
   end System_Init;

   --  Example of placing hot functions into ITCM:
   --  pragma Linker_Section (Hot_ISR, ".itcm_text");
   --  procedure Hot_ISR is
   --  begin
   --     null;
   --  end Hot_ISR;

end S.STM32;
