--
--  Copyright (C) 2023, AdaCore
--

--  This is a version for the MIV-RV32
package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   ----------------
   -- Interrupts --
   ----------------

   --  Target Specific Names

   Timer_0 : constant Interrupt_ID := 8;
   Timer_1 : constant Interrupt_ID := 12;
   Console : constant Interrupt_ID := 15;
   Timer_2 : constant Interrupt_ID := 20;
   Timer_3 : constant Interrupt_ID := 26;

   --  General IRQ Names

   IRQ1  : constant Interrupt_ID := 1;
   IRQ2  : constant Interrupt_ID := 2;
   IRQ3  : constant Interrupt_ID := 3;
   IRQ4  : constant Interrupt_ID := 4;
   IRQ5  : constant Interrupt_ID := 5;
   IRQ6  : constant Interrupt_ID := 6;
   IRQ7  : constant Interrupt_ID := 7;
   IRQ8  : constant Interrupt_ID := 8;
   IRQ9  : constant Interrupt_ID := 9;
   IRQ10 : constant Interrupt_ID := 10;
   IRQ11 : constant Interrupt_ID := 11;
   IRQ12 : constant Interrupt_ID := 12;
   IRQ13 : constant Interrupt_ID := 13;
   IRQ14 : constant Interrupt_ID := 14;
   IRQ15 : constant Interrupt_ID := 15;
   IRQ16 : constant Interrupt_ID := 16;
   IRQ17 : constant Interrupt_ID := 17;
   IRQ18 : constant Interrupt_ID := 18;
   IRQ19 : constant Interrupt_ID := 19;
   IRQ20 : constant Interrupt_ID := 20;
   IRQ21 : constant Interrupt_ID := 21;
   IRQ22 : constant Interrupt_ID := 22;
   IRQ23 : constant Interrupt_ID := 23;
   IRQ24 : constant Interrupt_ID := 24;
   IRQ25 : constant Interrupt_ID := 25;
   IRQ26 : constant Interrupt_ID := 26;
   IRQ27 : constant Interrupt_ID := 27;
   IRQ28 : constant Interrupt_ID := 28;
   IRQ29 : constant Interrupt_ID := 29;
   IRQ30 : constant Interrupt_ID := 30;
   IRQ31 : constant Interrupt_ID := 31;

end Ada.Interrupts.Names;
