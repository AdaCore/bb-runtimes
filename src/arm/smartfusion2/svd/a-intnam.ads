--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from M2Sxxx.svd

--  This is a version for the Microcontroller Subsystem (MSS)
--   - Hard 166 MHz 32-Bit ARM Cortex-M3 Processor (r2p1)
--     Embedded Trace Macrocell (ETM)
--     Memory Protection Unit (MPU)
--     JTAG Debug (4 wires), SW Debug (SWD, 2wires), SW Viewer (SWV)
--   - 64 KB Embedded SRAM (eSRAM)
--   - Up to 512 KB Embedded Nonvolatile Memory (eNVM)
--   - Triple Speed Ethernet (TSE) 10/100/1000 Mbps MAC
--   - USB 2.0 High Speed On-The-Go (OTG) Controller with ULPI Interface
--   - CAN Controller, 2.0B Compliant, Conforms to ISO11898-1
--   - 2 Each: SPI, I2C, Multi-Mode UARTs (MMUART) Peripherals
--   - Hardware Based Watchdog Timer
--   - 1 General Purpose 64-Bit (or two 32-bit) Timer(s)
--   - Real-Time Calendar/Counter (RTC)
--   - DDR Bridge (4 Port Data R/W Buffering Bridge to DDR Memory) with 64-Bit
--  AXI IF
--   - 2 AHB/APB Interfaces to FPGA Fabric (master/slave capable)
--   - 2 DMA Controllers to Offload Data Transactions from the Cortex-M3
--  Processor
--   - 8-Channel Peripheral DMA (PDMA)
--   - High Performance DMA (HPDMA)
--
--  Clocking Resources
--   - Clock Sources
--     Up to 2 High Precision 32 KHz to 20 MHz Main Crystal Oscillator
--     1 MHz Embedded RC Oscillator
--     50 MHz Embedded RC Oscillator
--   - Up to 8 Clock Conditioning Circuits (CCCs)
--     Output Clock with 8 Output Phases
--     Frequency: Input 1 to 200 MHz, Output 20 to 400MHz
--
--  High Speed Serial Interfaces
--   - Up to 16 SERDES Lanes, Each Supporting:
--     XGXS/XAUI Extension (to implement a 10 Gbps (XGMII) Ethernet PHY
--  interface)
--     Native SERDES Interface Facilitates Implementation of Serial RapidIO
--     PCI Express (PCIe) Endpoint Controller
--
--  High Speed Memory Interfaces
--   - Up to 2 High Speed DDRx Memory Controllers
--     MSS DDR (MDDR) and Fabric DDR (FDDR) Controllers
--     Supports LPDDR/DDR2/DDR3
--     Maximum 333 MHz Clock Rate
--     SECDED Enable/Disable Feature
--     Supports Various DRAM Bus Width Modes, x16, x18, x32, x36
--   - SDRAM Support MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick_Interrupt                 : constant Interrupt_ID := -1;
   WDOGWAKEUPINT_Interrupt            : constant Interrupt_ID := 0;
   RTC_WAKEUP_INTR_Interrupt          : constant Interrupt_ID := 1;
   SPIINT0_Interrupt                  : constant Interrupt_ID := 2;
   SPIINT1_Interrupt                  : constant Interrupt_ID := 3;
   I2C_INT0_Interrupt                 : constant Interrupt_ID := 4;
   I2C_SMBALERT0_Interrupt            : constant Interrupt_ID := 5;
   I2C_SMBSUS0_Interrupt              : constant Interrupt_ID := 6;
   I2C_INT1_Interrupt                 : constant Interrupt_ID := 7;
   I2C_SMBALERT1_Interrupt            : constant Interrupt_ID := 8;
   I2C_SMBSUS1_Interrupt              : constant Interrupt_ID := 9;
   MMUART0_INTR_Interrupt             : constant Interrupt_ID := 10;
   MMUART1_INTR_Interrupt             : constant Interrupt_ID := 11;
   MAC_INT_Interrupt                  : constant Interrupt_ID := 12;
   PDMAINTERRUPT_Interrupt            : constant Interrupt_ID := 13;
   TIMER1_INTR_Interrupt              : constant Interrupt_ID := 14;
   TIMER2_INTR_Interrupt              : constant Interrupt_ID := 15;
   CAN_INTR_Interrupt                 : constant Interrupt_ID := 16;
   ENVM_INT0_Interrupt                : constant Interrupt_ID := 17;
   ENVM_INT1_Interrupt                : constant Interrupt_ID := 18;
   COMM_BLK_INTR_Interrupt            : constant Interrupt_ID := 19;
   USB_MC_INT_Interrupt               : constant Interrupt_ID := 20;
   USB_DMA_INT_Interrupt              : constant Interrupt_ID := 21;
   MSSDDR_PLL_LOCK_INT_Interrupt      : constant Interrupt_ID := 22;
   MSSDDR_PLL_LOCKLOST_INT_Interrupt  : constant Interrupt_ID := 23;
   SW_ERRORINTERRUPT_Interrupt        : constant Interrupt_ID := 24;
   CACHE_ERRINTR_Interrupt            : constant Interrupt_ID := 25;
   DDRB_INTR_Interrupt                : constant Interrupt_ID := 26;
   HPD_XFR_CMP_INT_Interrupt          : constant Interrupt_ID := 27;
   HPD_XFR_ERR_INT_Interrupt          : constant Interrupt_ID := 28;
   ECCINTR_Interrupt                  : constant Interrupt_ID := 29;
   MDDR_IO_CALIB_INT_Interrupt        : constant Interrupt_ID := 30;
   FAB_PLL_LOCK_INT_Interrupt         : constant Interrupt_ID := 31;
   FAB_PLL_LOCKLOST_INT_Interrupt     : constant Interrupt_ID := 32;
   FIC64_INT_Interrupt                : constant Interrupt_ID := 33;
   GPIO_INT_0_Interrupt               : constant Interrupt_ID := 50;
   GPIO_INT_1_Interrupt               : constant Interrupt_ID := 51;
   GPIO_INT_2_Interrupt               : constant Interrupt_ID := 52;
   GPIO_INT_3_Interrupt               : constant Interrupt_ID := 53;
   GPIO_INT_4_Interrupt               : constant Interrupt_ID := 54;
   GPIO_INT_5_Interrupt               : constant Interrupt_ID := 55;
   GPIO_INT_6_Interrupt               : constant Interrupt_ID := 56;
   GPIO_INT_7_Interrupt               : constant Interrupt_ID := 57;
   GPIO_INT_8_Interrupt               : constant Interrupt_ID := 58;
   GPIO_INT_9_Interrupt               : constant Interrupt_ID := 59;
   GPIO_INT_10_Interrupt              : constant Interrupt_ID := 60;
   GPIO_INT_11_Interrupt              : constant Interrupt_ID := 61;
   GPIO_INT_12_Interrupt              : constant Interrupt_ID := 62;
   GPIO_INT_13_Interrupt              : constant Interrupt_ID := 63;
   GPIO_INT_14_Interrupt              : constant Interrupt_ID := 64;
   GPIO_INT_15_Interrupt              : constant Interrupt_ID := 65;
   GPIO_INT_16_Interrupt              : constant Interrupt_ID := 66;
   GPIO_INT_17_Interrupt              : constant Interrupt_ID := 67;
   GPIO_INT_18_Interrupt              : constant Interrupt_ID := 68;
   GPIO_INT_19_Interrupt              : constant Interrupt_ID := 69;
   GPIO_INT_20_Interrupt              : constant Interrupt_ID := 70;
   GPIO_INT_21_Interrupt              : constant Interrupt_ID := 71;
   GPIO_INT_22_Interrupt              : constant Interrupt_ID := 72;
   GPIO_INT_23_Interrupt              : constant Interrupt_ID := 73;
   GPIO_INT_24_Interrupt              : constant Interrupt_ID := 74;
   GPIO_INT_25_Interrupt              : constant Interrupt_ID := 75;
   GPIO_INT_26_Interrupt              : constant Interrupt_ID := 76;
   GPIO_INT_27_Interrupt              : constant Interrupt_ID := 77;
   GPIO_INT_28_Interrupt              : constant Interrupt_ID := 78;
   GPIO_INT_29_Interrupt              : constant Interrupt_ID := 79;
   GPIO_INT_30_Interrupt              : constant Interrupt_ID := 80;
   GPIO_INT_31_Interrupt              : constant Interrupt_ID := 81;

end Ada.Interrupts.Names;
