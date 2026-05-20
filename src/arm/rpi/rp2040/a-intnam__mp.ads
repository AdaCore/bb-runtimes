--
--  Copyright (C) 2021, AdaCore
--

pragma Style_Checks (Off);

--  Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
--
--  SPDX-License-Identifier: BSD-3-Clause

--  This spec has been automatically generated from rp2040.svd

--  This is a version for the  MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt             : constant Interrupt_ID := -1;

   --  Interrupts for CPU 1 (core0)
   TIMER_IRQ_0_Interrupt_CPU_1    : constant Interrupt_ID := 0;
   TIMER_IRQ_1_Interrupt_CPU_1    : constant Interrupt_ID := 1;
   TIMER_IRQ_2_Interrupt_CPU_1    : constant Interrupt_ID := 2;
   TIMER_IRQ_3_Interrupt_CPU_1    : constant Interrupt_ID := 3;
   PWM_IRQ_WRAP_Interrupt_CPU_1   : constant Interrupt_ID := 4;
   USBCTRL_Interrupt_CPU_1        : constant Interrupt_ID := 5;
   XIP_Interrupt_CPU_1            : constant Interrupt_ID := 6;
   PIO0_IRQ_0_Interrupt_CPU_1     : constant Interrupt_ID := 7;
   PIO0_IRQ_1_Interrupt_CPU_1     : constant Interrupt_ID := 8;
   PIO1_IRQ_0_Interrupt_CPU_1     : constant Interrupt_ID := 9;
   PIO1_IRQ_1_Interrupt_CPU_1     : constant Interrupt_ID := 10;
   DMA_IRQ_0_Interrupt_CPU_1      : constant Interrupt_ID := 11;
   DMA_IRQ_1_Interrupt_CPU_1      : constant Interrupt_ID := 12;
   IO_IRQ_BANK0_Interrupt_CPU_1   : constant Interrupt_ID := 13;
   IO_IRQ_QSPI_Interrupt_CPU_1    : constant Interrupt_ID := 14;
   SIO_IRQ_PROC0_Interrupt_CPU_1  : constant Interrupt_ID := 15;
   SIO_IRQ_PROC1_Interrupt_CPU_1  : constant Interrupt_ID := 16;
   CLOCKS_Interrupt_CPU_1         : constant Interrupt_ID := 17;
   SPI0_Interrupt_CPU_1           : constant Interrupt_ID := 18;
   SPI1_Interrupt_CPU_1           : constant Interrupt_ID := 19;
   UART0_Interrupt_CPU_1          : constant Interrupt_ID := 20;
   UART1_Interrupt_CPU_1          : constant Interrupt_ID := 21;
   ADC_IRQ_FIFO_Interrupt_CPU_1   : constant Interrupt_ID := 22;
   I2C0_Interrupt_CPU_1           : constant Interrupt_ID := 23;
   I2C1_Interrupt_CPU_1           : constant Interrupt_ID := 24;
   RTC_Interrupt_CPU_1            : constant Interrupt_ID := 25;

   --  Interrupts for CPU 2 (core1).
   TIMER_IRQ_0_Interrupt_CPU_2    : constant Interrupt_ID := 32 + 0;
   TIMER_IRQ_1_Interrupt_CPU_2    : constant Interrupt_ID := 32 + 1;
   TIMER_IRQ_2_Interrupt_CPU_2    : constant Interrupt_ID := 32 + 2;
   TIMER_IRQ_3_Interrupt_CPU_2    : constant Interrupt_ID := 32 + 3;
   PWM_IRQ_WRAP_Interrupt_CPU_2   : constant Interrupt_ID := 32 + 4;
   USBCTRL_Interrupt_CPU_2        : constant Interrupt_ID := 32 + 5;
   XIP_Interrupt_CPU_2            : constant Interrupt_ID := 32 + 6;
   PIO0_IRQ_0_Interrupt_CPU_2     : constant Interrupt_ID := 32 + 7;
   PIO0_IRQ_1_Interrupt_CPU_2     : constant Interrupt_ID := 32 + 8;
   PIO1_IRQ_0_Interrupt_CPU_2     : constant Interrupt_ID := 32 + 9;
   PIO1_IRQ_1_Interrupt_CPU_2     : constant Interrupt_ID := 32 + 10;
   DMA_IRQ_0_Interrupt_CPU_2      : constant Interrupt_ID := 32 + 11;
   DMA_IRQ_1_Interrupt_CPU_2      : constant Interrupt_ID := 32 + 12;
   IO_IRQ_BANK0_Interrupt_CPU_2   : constant Interrupt_ID := 32 + 13;
   IO_IRQ_QSPI_Interrupt_CPU_2    : constant Interrupt_ID := 32 + 14;
   SIO_IRQ_PROC0_Interrupt_CPU_2  : constant Interrupt_ID := 32 + 15;
   SIO_IRQ_PROC1_Interrupt_CPU_2  : constant Interrupt_ID := 32 + 16;
   CLOCKS_Interrupt_CPU_2         : constant Interrupt_ID := 32 + 17;
   SPI0_Interrupt_CPU_2           : constant Interrupt_ID := 32 + 18;
   SPI1_Interrupt_CPU_2           : constant Interrupt_ID := 32 + 19;
   UART0_Interrupt_CPU_2          : constant Interrupt_ID := 32 + 20;
   UART1_Interrupt_CPU_2          : constant Interrupt_ID := 32 + 21;
   ADC_IRQ_FIFO_Interrupt_CPU_2   : constant Interrupt_ID := 32 + 22;
   I2C0_Interrupt_CPU_2           : constant Interrupt_ID := 32 + 23;
   I2C1_Interrupt_CPU_2           : constant Interrupt_ID := 32 + 24;
   RTC_Interrupt_CPU_2            : constant Interrupt_ID := 32 + 25;

end Ada.Interrupts.Names;
