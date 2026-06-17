--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from ATSAMV71Q21.svd

--  This is a version for the Microchip ATSAMV71Q21 Microcontroller MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick              : constant Interrupt_ID := -1;
   SUPC                  : constant Interrupt_ID := 0;
   RSTC                  : constant Interrupt_ID := 1;
   RTC                   : constant Interrupt_ID := 2;
   RTT                   : constant Interrupt_ID := 3;
   WDT                   : constant Interrupt_ID := 4;
   PMC                   : constant Interrupt_ID := 5;
   EFC                   : constant Interrupt_ID := 6;
   UART0                 : constant Interrupt_ID := 7;
   UART1                 : constant Interrupt_ID := 8;
   PIOA                  : constant Interrupt_ID := 10;
   PIOB                  : constant Interrupt_ID := 11;
   PIOC                  : constant Interrupt_ID := 12;
   USART0                : constant Interrupt_ID := 13;
   USART1                : constant Interrupt_ID := 14;
   USART2                : constant Interrupt_ID := 15;
   PIOD                  : constant Interrupt_ID := 16;
   PIOE                  : constant Interrupt_ID := 17;
   HSMCI                 : constant Interrupt_ID := 18;
   TWIHS0                : constant Interrupt_ID := 19;
   TWIHS1                : constant Interrupt_ID := 20;
   SPI0                  : constant Interrupt_ID := 21;
   SSC                   : constant Interrupt_ID := 22;
   TC0                   : constant Interrupt_ID := 23;
   TC1                   : constant Interrupt_ID := 24;
   TC2                   : constant Interrupt_ID := 25;
   TC3                   : constant Interrupt_ID := 26;
   TC4                   : constant Interrupt_ID := 27;
   TC5                   : constant Interrupt_ID := 28;
   AFEC0                 : constant Interrupt_ID := 29;
   DACC                  : constant Interrupt_ID := 30;
   PWM0                  : constant Interrupt_ID := 31;
   ICM                   : constant Interrupt_ID := 32;
   ACC                   : constant Interrupt_ID := 33;
   USBHS                 : constant Interrupt_ID := 34;
   MCAN0_INT0            : constant Interrupt_ID := 35;
   MCAN0_INT1            : constant Interrupt_ID := 36;
   MCAN1_INT0            : constant Interrupt_ID := 37;
   MCAN1_INT1            : constant Interrupt_ID := 38;
   GMAC                  : constant Interrupt_ID := 39;
   AFEC1                 : constant Interrupt_ID := 40;
   TWIHS2                : constant Interrupt_ID := 41;
   SPI1                  : constant Interrupt_ID := 42;
   QSPI                  : constant Interrupt_ID := 43;
   UART2                 : constant Interrupt_ID := 44;
   UART3                 : constant Interrupt_ID := 45;
   UART4                 : constant Interrupt_ID := 46;
   TC6                   : constant Interrupt_ID := 47;
   TC7                   : constant Interrupt_ID := 48;
   TC8                   : constant Interrupt_ID := 49;
   TC9                   : constant Interrupt_ID := 50;
   TC10                  : constant Interrupt_ID := 51;
   TC11                  : constant Interrupt_ID := 52;
   MLB                   : constant Interrupt_ID := 53;
   AES                   : constant Interrupt_ID := 56;
   TRNG                  : constant Interrupt_ID := 57;
   XDMAC                 : constant Interrupt_ID := 58;
   ISI                   : constant Interrupt_ID := 59;
   PWM1                  : constant Interrupt_ID := 60;
   FPU                   : constant Interrupt_ID := 61;
   SDRAMC                : constant Interrupt_ID := 62;
   RSWDT                 : constant Interrupt_ID := 63;
   CCW                   : constant Interrupt_ID := 64;
   CCF                   : constant Interrupt_ID := 65;
   GMAC_Q1               : constant Interrupt_ID := 66;
   GMAC_Q2               : constant Interrupt_ID := 67;
   IXC                   : constant Interrupt_ID := 68;

end Ada.Interrupts.Names;
