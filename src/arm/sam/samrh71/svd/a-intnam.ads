--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  This spec has been automatically generated from ATSAMRH71F20C.svd

--  This is a version for the Microchip ATSAMRH71F20C Microcontroller MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt             : constant Interrupt_ID := -1;
   SUPC_Interrupt                 : constant Interrupt_ID := 0;
   RSTC_Interrupt                 : constant Interrupt_ID := 1;
   RTC_Interrupt                  : constant Interrupt_ID := 2;
   RTT_Interrupt                  : constant Interrupt_ID := 3;
   WDT_Interrupt                  : constant Interrupt_ID := 4;
   PMC_Interrupt                  : constant Interrupt_ID := 5;
   MATRIX0_Interrupt              : constant Interrupt_ID := 6;
   FLEXCOM0_Interrupt             : constant Interrupt_ID := 7;
   FLEXCOM1_Interrupt             : constant Interrupt_ID := 8;
   NMIC_Interrupt                 : constant Interrupt_ID := 9;
   FLEXCOM2_Interrupt             : constant Interrupt_ID := 13;
   FLEXCOM3_Interrupt             : constant Interrupt_ID := 14;
   FLEXCOM4_Interrupt             : constant Interrupt_ID := 15;
   CCW_Interrupt                  : constant Interrupt_ID := 18;
   CCF_Interrupt                  : constant Interrupt_ID := 19;
   FPU_Interrupt                  : constant Interrupt_ID := 20;
   IXC_Interrupt                  : constant Interrupt_ID := 21;
   FLEXCOM5_Interrupt             : constant Interrupt_ID := 22;
   FLEXCOM6_Interrupt             : constant Interrupt_ID := 23;
   FLEXCOM7_Interrupt             : constant Interrupt_ID := 24;
   TC0_Interrupt                  : constant Interrupt_ID := 25;
   TC1_Interrupt                  : constant Interrupt_ID := 26;
   TC2_Interrupt                  : constant Interrupt_ID := 27;
   TC3_Interrupt                  : constant Interrupt_ID := 28;
   TC4_Interrupt                  : constant Interrupt_ID := 29;
   TC5_Interrupt                  : constant Interrupt_ID := 30;
   PWM0_Interrupt                 : constant Interrupt_ID := 31;
   PWM1_Interrupt                 : constant Interrupt_ID := 32;
   ICM_Interrupt                  : constant Interrupt_ID := 33;
   MCAN0_INT0_Interrupt           : constant Interrupt_ID := 36;
   MCAN0_INT1_Interrupt           : constant Interrupt_ID := 37;
   MCAN1_INT0_Interrupt           : constant Interrupt_ID := 38;
   MCAN1_INT1_Interrupt           : constant Interrupt_ID := 39;
   TCMECC_INTFIX_Interrupt        : constant Interrupt_ID := 40;
   TCMECC_INTNOFIX_Interrupt      : constant Interrupt_ID := 41;
   FLEXRAMECC_INTFIX_Interrupt    : constant Interrupt_ID := 42;
   FLEXRAMECC_INTNOFIX_Interrupt  : constant Interrupt_ID := 43;
   SHA_Interrupt                  : constant Interrupt_ID := 44;
   FLEXCOM8_Interrupt             : constant Interrupt_ID := 45;
   FLEXCOM9_Interrupt             : constant Interrupt_ID := 46;
   RSWDT_Interrupt                : constant Interrupt_ID := 47;
   QSPI_Interrupt                 : constant Interrupt_ID := 49;
   HEFC_INT0_Interrupt            : constant Interrupt_ID := 50;
   HEFC_INTFIX_Interrupt          : constant Interrupt_ID := 51;
   HEFC_INTNOFIX_Interrupt        : constant Interrupt_ID := 52;
   TC6_Interrupt                  : constant Interrupt_ID := 53;
   TC7_Interrupt                  : constant Interrupt_ID := 54;
   TC8_Interrupt                  : constant Interrupt_ID := 55;
   TC9_Interrupt                  : constant Interrupt_ID := 56;
   TC10_Interrupt                 : constant Interrupt_ID := 57;
   TC11_Interrupt                 : constant Interrupt_ID := 58;
   HEMC_INTSDRAMC_Interrupt       : constant Interrupt_ID := 59;
   HEMC_INTFIX_Interrupt          : constant Interrupt_ID := 60;
   HEMC_INTNOFIX_Interrupt        : constant Interrupt_ID := 61;
   SFR_Interrupt                  : constant Interrupt_ID := 62;
   TRNG_Interrupt                 : constant Interrupt_ID := 63;
   XDMAC_Interrupt                : constant Interrupt_ID := 64;
   SPW_Interrupt                  : constant Interrupt_ID := 65;
   IP1553_Interrupt               : constant Interrupt_ID := 68;
   GMAC_Interrupt                 : constant Interrupt_ID := 69;
   GMAC_Q1_Interrupt              : constant Interrupt_ID := 70;
   GMAC_Q2_Interrupt              : constant Interrupt_ID := 71;
   GMAC_Q3_Interrupt              : constant Interrupt_ID := 72;
   GMAC_Q4_Interrupt              : constant Interrupt_ID := 73;
   GMAC_Q5_Interrupt              : constant Interrupt_ID := 74;

end Ada.Interrupts.Names;
