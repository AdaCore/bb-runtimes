--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from M2Sxxx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Microcontroller Subsystem (MSS)
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
--   - SDRAM Support
package Interfaces.SF2 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Base type --
   ---------------

   type UInt32 is new Interfaces.Unsigned_32;
   type UInt16 is new Interfaces.Unsigned_16;
   type Byte is new Interfaces.Unsigned_8;
   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   --------------------
   -- Base addresses --
   --------------------

   System_Registers_Base : constant System.Address :=
     System'To_Address (16#40038000#);
   MDDR_Base : constant System.Address :=
     System'To_Address (16#40020800#);
   SERDES_0_PCIE_Base : constant System.Address :=
     System'To_Address (16#40028000#);
   SERDES_0_LANE_0_Base : constant System.Address :=
     System'To_Address (16#40029000#);
   SERDES_0_LANE_1_Base : constant System.Address :=
     System'To_Address (16#20029400#);
   SERDES_0_LANE_2_Base : constant System.Address :=
     System'To_Address (16#40029800#);
   SERDES_0_LANE_3_Base : constant System.Address :=
     System'To_Address (16#40029C00#);
   SERDES_0_SYS_REG_Base : constant System.Address :=
     System'To_Address (16#4002A000#);
   SERDES_1_PCIE_Base : constant System.Address :=
     System'To_Address (16#4002C000#);
   SERDES_1_LANE_0_Base : constant System.Address :=
     System'To_Address (16#4002D000#);
   SERDES_1_LANE_1_Base : constant System.Address :=
     System'To_Address (16#4002D400#);
   SERDES_1_LANE_2_Base : constant System.Address :=
     System'To_Address (16#4002D800#);
   SERDES_1_LANE_3_Base : constant System.Address :=
     System'To_Address (16#4002DC00#);
   SERDES_1_SYS_REG_Base : constant System.Address :=
     System'To_Address (16#4002E000#);
   MMUART_0_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   MMUART_1_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   GPIO_Base : constant System.Address :=
     System'To_Address (16#40013000#);
   Watchdog_Base : constant System.Address :=
     System'To_Address (16#40005000#);
   SPI_0_Base : constant System.Address :=
     System'To_Address (16#40001000#);
   SPI_1_Base : constant System.Address :=
     System'To_Address (16#40011000#);
   I2C0_Base : constant System.Address :=
     System'To_Address (16#40002000#);
   I2C1_Base : constant System.Address :=
     System'To_Address (16#40012000#);
   HPDMA_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   PDMA_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   COMBLK_Base : constant System.Address :=
     System'To_Address (16#40016000#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#40017000#);
   CAN_Base : constant System.Address :=
     System'To_Address (16#40015000#);
   AHB_to_eNVM_0_Base : constant System.Address :=
     System'To_Address (16#60080000#);
   AHB_to_eNVM_1_Base : constant System.Address :=
     System'To_Address (16#600C0000#);
   Timer_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   FDDR_Base : constant System.Address :=
     System'To_Address (16#40021000#);
   Ethernet_MAC_Base : constant System.Address :=
     System'To_Address (16#40041000#);
   USB_Base : constant System.Address :=
     System'To_Address (16#40043000#);

end Interfaces.SF2;
