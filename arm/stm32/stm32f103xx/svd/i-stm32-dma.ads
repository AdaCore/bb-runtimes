--
--  Copyright (C) 2019, AdaCore
--

--  This spec has been automatically generated from STM32F103xx.svd

pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Interfaces.STM32.DMA is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype ISR_GIF1_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF1_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF1_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF1_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF2_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF2_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF2_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF2_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF3_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF3_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF3_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF3_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF4_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF4_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF4_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF4_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF5_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF5_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF5_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF5_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF6_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF6_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF6_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF6_Field is Interfaces.STM32.Bit;
   subtype ISR_GIF7_Field is Interfaces.STM32.Bit;
   subtype ISR_TCIF7_Field is Interfaces.STM32.Bit;
   subtype ISR_HTIF7_Field is Interfaces.STM32.Bit;
   subtype ISR_TEIF7_Field is Interfaces.STM32.Bit;

   --  DMA interrupt status register (DMA_ISR)
   type ISR_Register is record
      --  Read-only. Channel 1 Global interrupt flag
      GIF1           : ISR_GIF1_Field;
      --  Read-only. Channel 1 Transfer Complete flag
      TCIF1          : ISR_TCIF1_Field;
      --  Read-only. Channel 1 Half Transfer Complete flag
      HTIF1          : ISR_HTIF1_Field;
      --  Read-only. Channel 1 Transfer Error flag
      TEIF1          : ISR_TEIF1_Field;
      --  Read-only. Channel 2 Global interrupt flag
      GIF2           : ISR_GIF2_Field;
      --  Read-only. Channel 2 Transfer Complete flag
      TCIF2          : ISR_TCIF2_Field;
      --  Read-only. Channel 2 Half Transfer Complete flag
      HTIF2          : ISR_HTIF2_Field;
      --  Read-only. Channel 2 Transfer Error flag
      TEIF2          : ISR_TEIF2_Field;
      --  Read-only. Channel 3 Global interrupt flag
      GIF3           : ISR_GIF3_Field;
      --  Read-only. Channel 3 Transfer Complete flag
      TCIF3          : ISR_TCIF3_Field;
      --  Read-only. Channel 3 Half Transfer Complete flag
      HTIF3          : ISR_HTIF3_Field;
      --  Read-only. Channel 3 Transfer Error flag
      TEIF3          : ISR_TEIF3_Field;
      --  Read-only. Channel 4 Global interrupt flag
      GIF4           : ISR_GIF4_Field;
      --  Read-only. Channel 4 Transfer Complete flag
      TCIF4          : ISR_TCIF4_Field;
      --  Read-only. Channel 4 Half Transfer Complete flag
      HTIF4          : ISR_HTIF4_Field;
      --  Read-only. Channel 4 Transfer Error flag
      TEIF4          : ISR_TEIF4_Field;
      --  Read-only. Channel 5 Global interrupt flag
      GIF5           : ISR_GIF5_Field;
      --  Read-only. Channel 5 Transfer Complete flag
      TCIF5          : ISR_TCIF5_Field;
      --  Read-only. Channel 5 Half Transfer Complete flag
      HTIF5          : ISR_HTIF5_Field;
      --  Read-only. Channel 5 Transfer Error flag
      TEIF5          : ISR_TEIF5_Field;
      --  Read-only. Channel 6 Global interrupt flag
      GIF6           : ISR_GIF6_Field;
      --  Read-only. Channel 6 Transfer Complete flag
      TCIF6          : ISR_TCIF6_Field;
      --  Read-only. Channel 6 Half Transfer Complete flag
      HTIF6          : ISR_HTIF6_Field;
      --  Read-only. Channel 6 Transfer Error flag
      TEIF6          : ISR_TEIF6_Field;
      --  Read-only. Channel 7 Global interrupt flag
      GIF7           : ISR_GIF7_Field;
      --  Read-only. Channel 7 Transfer Complete flag
      TCIF7          : ISR_TCIF7_Field;
      --  Read-only. Channel 7 Half Transfer Complete flag
      HTIF7          : ISR_HTIF7_Field;
      --  Read-only. Channel 7 Transfer Error flag
      TEIF7          : ISR_TEIF7_Field;
      --  unspecified
      Reserved_28_31 : Interfaces.STM32.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      GIF1           at 0 range 0 .. 0;
      TCIF1          at 0 range 1 .. 1;
      HTIF1          at 0 range 2 .. 2;
      TEIF1          at 0 range 3 .. 3;
      GIF2           at 0 range 4 .. 4;
      TCIF2          at 0 range 5 .. 5;
      HTIF2          at 0 range 6 .. 6;
      TEIF2          at 0 range 7 .. 7;
      GIF3           at 0 range 8 .. 8;
      TCIF3          at 0 range 9 .. 9;
      HTIF3          at 0 range 10 .. 10;
      TEIF3          at 0 range 11 .. 11;
      GIF4           at 0 range 12 .. 12;
      TCIF4          at 0 range 13 .. 13;
      HTIF4          at 0 range 14 .. 14;
      TEIF4          at 0 range 15 .. 15;
      GIF5           at 0 range 16 .. 16;
      TCIF5          at 0 range 17 .. 17;
      HTIF5          at 0 range 18 .. 18;
      TEIF5          at 0 range 19 .. 19;
      GIF6           at 0 range 20 .. 20;
      TCIF6          at 0 range 21 .. 21;
      HTIF6          at 0 range 22 .. 22;
      TEIF6          at 0 range 23 .. 23;
      GIF7           at 0 range 24 .. 24;
      TCIF7          at 0 range 25 .. 25;
      HTIF7          at 0 range 26 .. 26;
      TEIF7          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype IFCR_CGIF1_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF1_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF1_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF1_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF2_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF2_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF2_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF2_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF3_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF3_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF3_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF3_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF4_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF4_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF4_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF4_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF5_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF5_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF5_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF5_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF6_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF6_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF6_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF6_Field is Interfaces.STM32.Bit;
   subtype IFCR_CGIF7_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTCIF7_Field is Interfaces.STM32.Bit;
   subtype IFCR_CHTIF7_Field is Interfaces.STM32.Bit;
   subtype IFCR_CTEIF7_Field is Interfaces.STM32.Bit;

   --  DMA interrupt flag clear register (DMA_IFCR)
   type IFCR_Register is record
      --  Write-only. Channel 1 Global interrupt clear
      CGIF1          : IFCR_CGIF1_Field := 16#0#;
      --  Write-only. Channel 1 Transfer Complete clear
      CTCIF1         : IFCR_CTCIF1_Field := 16#0#;
      --  Write-only. Channel 1 Half Transfer clear
      CHTIF1         : IFCR_CHTIF1_Field := 16#0#;
      --  Write-only. Channel 1 Transfer Error clear
      CTEIF1         : IFCR_CTEIF1_Field := 16#0#;
      --  Write-only. Channel 2 Global interrupt clear
      CGIF2          : IFCR_CGIF2_Field := 16#0#;
      --  Write-only. Channel 2 Transfer Complete clear
      CTCIF2         : IFCR_CTCIF2_Field := 16#0#;
      --  Write-only. Channel 2 Half Transfer clear
      CHTIF2         : IFCR_CHTIF2_Field := 16#0#;
      --  Write-only. Channel 2 Transfer Error clear
      CTEIF2         : IFCR_CTEIF2_Field := 16#0#;
      --  Write-only. Channel 3 Global interrupt clear
      CGIF3          : IFCR_CGIF3_Field := 16#0#;
      --  Write-only. Channel 3 Transfer Complete clear
      CTCIF3         : IFCR_CTCIF3_Field := 16#0#;
      --  Write-only. Channel 3 Half Transfer clear
      CHTIF3         : IFCR_CHTIF3_Field := 16#0#;
      --  Write-only. Channel 3 Transfer Error clear
      CTEIF3         : IFCR_CTEIF3_Field := 16#0#;
      --  Write-only. Channel 4 Global interrupt clear
      CGIF4          : IFCR_CGIF4_Field := 16#0#;
      --  Write-only. Channel 4 Transfer Complete clear
      CTCIF4         : IFCR_CTCIF4_Field := 16#0#;
      --  Write-only. Channel 4 Half Transfer clear
      CHTIF4         : IFCR_CHTIF4_Field := 16#0#;
      --  Write-only. Channel 4 Transfer Error clear
      CTEIF4         : IFCR_CTEIF4_Field := 16#0#;
      --  Write-only. Channel 5 Global interrupt clear
      CGIF5          : IFCR_CGIF5_Field := 16#0#;
      --  Write-only. Channel 5 Transfer Complete clear
      CTCIF5         : IFCR_CTCIF5_Field := 16#0#;
      --  Write-only. Channel 5 Half Transfer clear
      CHTIF5         : IFCR_CHTIF5_Field := 16#0#;
      --  Write-only. Channel 5 Transfer Error clear
      CTEIF5         : IFCR_CTEIF5_Field := 16#0#;
      --  Write-only. Channel 6 Global interrupt clear
      CGIF6          : IFCR_CGIF6_Field := 16#0#;
      --  Write-only. Channel 6 Transfer Complete clear
      CTCIF6         : IFCR_CTCIF6_Field := 16#0#;
      --  Write-only. Channel 6 Half Transfer clear
      CHTIF6         : IFCR_CHTIF6_Field := 16#0#;
      --  Write-only. Channel 6 Transfer Error clear
      CTEIF6         : IFCR_CTEIF6_Field := 16#0#;
      --  Write-only. Channel 7 Global interrupt clear
      CGIF7          : IFCR_CGIF7_Field := 16#0#;
      --  Write-only. Channel 7 Transfer Complete clear
      CTCIF7         : IFCR_CTCIF7_Field := 16#0#;
      --  Write-only. Channel 7 Half Transfer clear
      CHTIF7         : IFCR_CHTIF7_Field := 16#0#;
      --  Write-only. Channel 7 Transfer Error clear
      CTEIF7         : IFCR_CTEIF7_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : Interfaces.STM32.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      CGIF1          at 0 range 0 .. 0;
      CTCIF1         at 0 range 1 .. 1;
      CHTIF1         at 0 range 2 .. 2;
      CTEIF1         at 0 range 3 .. 3;
      CGIF2          at 0 range 4 .. 4;
      CTCIF2         at 0 range 5 .. 5;
      CHTIF2         at 0 range 6 .. 6;
      CTEIF2         at 0 range 7 .. 7;
      CGIF3          at 0 range 8 .. 8;
      CTCIF3         at 0 range 9 .. 9;
      CHTIF3         at 0 range 10 .. 10;
      CTEIF3         at 0 range 11 .. 11;
      CGIF4          at 0 range 12 .. 12;
      CTCIF4         at 0 range 13 .. 13;
      CHTIF4         at 0 range 14 .. 14;
      CTEIF4         at 0 range 15 .. 15;
      CGIF5          at 0 range 16 .. 16;
      CTCIF5         at 0 range 17 .. 17;
      CHTIF5         at 0 range 18 .. 18;
      CTEIF5         at 0 range 19 .. 19;
      CGIF6          at 0 range 20 .. 20;
      CTCIF6         at 0 range 21 .. 21;
      CHTIF6         at 0 range 22 .. 22;
      CTEIF6         at 0 range 23 .. 23;
      CGIF7          at 0 range 24 .. 24;
      CTCIF7         at 0 range 25 .. 25;
      CHTIF7         at 0 range 26 .. 26;
      CTEIF7         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype CCR_EN_Field is Interfaces.STM32.Bit;
   subtype CCR_TCIE_Field is Interfaces.STM32.Bit;
   subtype CCR_HTIE_Field is Interfaces.STM32.Bit;
   subtype CCR_TEIE_Field is Interfaces.STM32.Bit;
   subtype CCR_DIR_Field is Interfaces.STM32.Bit;
   subtype CCR_CIRC_Field is Interfaces.STM32.Bit;
   subtype CCR_PINC_Field is Interfaces.STM32.Bit;
   subtype CCR_MINC_Field is Interfaces.STM32.Bit;
   subtype CCR_PSIZE_Field is Interfaces.STM32.UInt2;
   subtype CCR_MSIZE_Field is Interfaces.STM32.UInt2;
   subtype CCR_PL_Field is Interfaces.STM32.UInt2;
   subtype CCR_MEM2MEM_Field is Interfaces.STM32.Bit;

   --  DMA channel configuration register (DMA_CCR)
   type CCR_Register is record
      --  Channel enable
      EN             : CCR_EN_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : CCR_TCIE_Field := 16#0#;
      --  Half Transfer interrupt enable
      HTIE           : CCR_HTIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : CCR_TEIE_Field := 16#0#;
      --  Data transfer direction
      DIR            : CCR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : CCR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : CCR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : CCR_MINC_Field := 16#0#;
      --  Peripheral size
      PSIZE          : CCR_PSIZE_Field := 16#0#;
      --  Memory size
      MSIZE          : CCR_MSIZE_Field := 16#0#;
      --  Channel Priority level
      PL             : CCR_PL_Field := 16#0#;
      --  Memory to memory mode
      MEM2MEM        : CCR_MEM2MEM_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : Interfaces.STM32.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      EN             at 0 range 0 .. 0;
      TCIE           at 0 range 1 .. 1;
      HTIE           at 0 range 2 .. 2;
      TEIE           at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CIRC           at 0 range 5 .. 5;
      PINC           at 0 range 6 .. 6;
      MINC           at 0 range 7 .. 7;
      PSIZE          at 0 range 8 .. 9;
      MSIZE          at 0 range 10 .. 11;
      PL             at 0 range 12 .. 13;
      MEM2MEM        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype CNDTR_NDT_Field is Interfaces.STM32.UInt16;

   --  DMA channel 1 number of data register
   type CNDTR_Register is record
      --  Number of data to transfer
      NDT            : CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : Interfaces.STM32.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMA controller
   type DMA_Peripheral is record
      --  DMA interrupt status register (DMA_ISR)
      ISR    : aliased ISR_Register;
      --  DMA interrupt flag clear register (DMA_IFCR)
      IFCR   : aliased IFCR_Register;
      --  DMA channel configuration register (DMA_CCR)
      CCR1   : aliased CCR_Register;
      --  DMA channel 1 number of data register
      CNDTR1 : aliased CNDTR_Register;
      --  DMA channel 1 peripheral address register
      CPAR1  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 1 memory address register
      CMAR1  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR2   : aliased CCR_Register;
      --  DMA channel 2 number of data register
      CNDTR2 : aliased CNDTR_Register;
      --  DMA channel 2 peripheral address register
      CPAR2  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 2 memory address register
      CMAR2  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR3   : aliased CCR_Register;
      --  DMA channel 3 number of data register
      CNDTR3 : aliased CNDTR_Register;
      --  DMA channel 3 peripheral address register
      CPAR3  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 3 memory address register
      CMAR3  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR4   : aliased CCR_Register;
      --  DMA channel 4 number of data register
      CNDTR4 : aliased CNDTR_Register;
      --  DMA channel 4 peripheral address register
      CPAR4  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 4 memory address register
      CMAR4  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR5   : aliased CCR_Register;
      --  DMA channel 5 number of data register
      CNDTR5 : aliased CNDTR_Register;
      --  DMA channel 5 peripheral address register
      CPAR5  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 5 memory address register
      CMAR5  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR6   : aliased CCR_Register;
      --  DMA channel 6 number of data register
      CNDTR6 : aliased CNDTR_Register;
      --  DMA channel 6 peripheral address register
      CPAR6  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 6 memory address register
      CMAR6  : aliased Interfaces.STM32.UInt32;
      --  DMA channel configuration register (DMA_CCR)
      CCR7   : aliased CCR_Register;
      --  DMA channel 7 number of data register
      CNDTR7 : aliased CNDTR_Register;
      --  DMA channel 7 peripheral address register
      CPAR7  : aliased Interfaces.STM32.UInt32;
      --  DMA channel 7 memory address register
      CMAR7  : aliased Interfaces.STM32.UInt32;
   end record
     with Volatile;

   for DMA_Peripheral use record
      ISR    at 16#0# range 0 .. 31;
      IFCR   at 16#4# range 0 .. 31;
      CCR1   at 16#8# range 0 .. 31;
      CNDTR1 at 16#C# range 0 .. 31;
      CPAR1  at 16#10# range 0 .. 31;
      CMAR1  at 16#14# range 0 .. 31;
      CCR2   at 16#1C# range 0 .. 31;
      CNDTR2 at 16#20# range 0 .. 31;
      CPAR2  at 16#24# range 0 .. 31;
      CMAR2  at 16#28# range 0 .. 31;
      CCR3   at 16#30# range 0 .. 31;
      CNDTR3 at 16#34# range 0 .. 31;
      CPAR3  at 16#38# range 0 .. 31;
      CMAR3  at 16#3C# range 0 .. 31;
      CCR4   at 16#44# range 0 .. 31;
      CNDTR4 at 16#48# range 0 .. 31;
      CPAR4  at 16#4C# range 0 .. 31;
      CMAR4  at 16#50# range 0 .. 31;
      CCR5   at 16#58# range 0 .. 31;
      CNDTR5 at 16#5C# range 0 .. 31;
      CPAR5  at 16#60# range 0 .. 31;
      CMAR5  at 16#64# range 0 .. 31;
      CCR6   at 16#6C# range 0 .. 31;
      CNDTR6 at 16#70# range 0 .. 31;
      CPAR6  at 16#74# range 0 .. 31;
      CMAR6  at 16#78# range 0 .. 31;
      CCR7   at 16#80# range 0 .. 31;
      CNDTR7 at 16#84# range 0 .. 31;
      CPAR7  at 16#88# range 0 .. 31;
      CMAR7  at 16#8C# range 0 .. 31;
   end record;

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => DMA1_Base;

   --  DMA controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => DMA2_Base;

end Interfaces.STM32.DMA;
