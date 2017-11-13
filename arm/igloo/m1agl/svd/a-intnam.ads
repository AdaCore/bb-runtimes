--
--  Copyright (C) 2018, AdaCore
--

--  This spec has been automatically generated from M1AGL.svd

--  This is a version for the Cortex-M1 on IGLOO FPGA MCU
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  System tick
   Sys_Tick                        : constant Interrupt_ID := -1;
   Core_Timer_Interrupt            : constant Interrupt_ID := 1;

end Ada.Interrupts.Names;
