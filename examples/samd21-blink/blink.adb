-- with Interfaces.SAM.PM; use Interfaces.SAM.PM;
with Interfaces.SAM.PORT; use Interfaces.SAM.PORT;
with Interfaces.SAM; use Interfaces.SAM;
with Setup_Pll;

procedure Blink is

   PA15_MASK : constant UInt32 := 2#00000000_00000010_00000000_00000000#;

begin
   --  Setup_Pll;

   PORT_Periph.DIRSET0 := PA15_MASK;

   loop
      for I in 1 .. 500_000 loop
         null;
      end loop;
   PORT_Periph.OUTTGL0 := PA15_MASK;
   end loop;
   -- null;
end Blink;

--  Notes
--  Gonna need PORT - the I/O Pin Controller
--  Each PORT Group (this means PA and PB confirmed with svd)
--  is controlled by the registers in PORT
--  Each PORT pin has 2 relevant bits: 
--  A DIR and a OUT (output value)
--  Also an IN value for when it's an input
--  Sounds like all you need to do is write a 1 to DIRSET for that pin,
--  then toggle with OUTTGL, OUTSET or OUTCLR
--  See 23.6.3.1 for more on configuring a port pin

--  Need to write bit Y of DIR reg to set DIRSET for indiv pin
--  So for pin PA15 I need to write the 16th bit of 
--  these regs to do what I want
--  Mask: 00000000_00000000_10000000_00000000
