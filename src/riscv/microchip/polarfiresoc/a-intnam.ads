--
--  Copyright (C) 2020, AdaCore
--

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   L2_Cache_Metadata_Correction    : constant Interrupt_ID := 1;
   L2_Cache_Metadata_uncorrectable : constant Interrupt_ID := 2;
   L2_Cache_Data_Correction        : constant Interrupt_ID := 3;
   L2_Cache_Data_uncorrectable     : constant Interrupt_ID := 4;

   DMA_Channel_0_Done  : constant Interrupt_ID := 5;
   DMA_Channel_0_Error : constant Interrupt_ID := 6;
   DMA_Channel_1_Done  : constant Interrupt_ID := 7;
   DMA_Channel_1_Error : constant Interrupt_ID := 8;
   DMA_Channel_2_Done  : constant Interrupt_ID := 9;
   DMA_Channel_2_Error : constant Interrupt_ID := 10;
   DMA_Channel_3_Done  : constant Interrupt_ID := 11;
   DMA_Channel_3_Error : constant Interrupt_ID := 12;

   Bus_Error_Unit_Hart_0  : constant Interrupt_ID := 182;
   Bus_Error_Unit_Hart_1  : constant Interrupt_ID := 183;
   Bus_Error_Unit_Hart_2  : constant Interrupt_ID := 184;
   Bus_Error_Unit_Hart_3  : constant Interrupt_ID := 185;
   Bus_Error_Unit_Hart_4  : constant Interrupt_ID := 186;

end Ada.Interrupts.Names;
