------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2016, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;

with System.BB.Parameters;
with System.IOPorts; use System.IOPorts;

package body System.BB.Board_Support is
   use System.BB.Interrupts;

   procedure Initialize_PICs;
   --  Initialize the PICs

   subtype Master_Interrupt_IDs is Interrupt_ID range 1 .. 8;
   pragma Unreferenced (Master_Interrupt_IDs);
   subtype Slave_Interrupt_IDs is Interrupt_ID range 9 .. 16;
   --  There are two PICs.  The second one is a slave of the first one

   Master_Pic_Port : constant Port_Id := 16#20#;
   Slave_Pic_Port  : constant Port_Id := 16#A0#;
   --  PICs port

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      Initialize_PICs;
   end Initialize_Board;

   ---------------------
   -- Initialize_PICs --
   ---------------------

   procedure Initialize_PICs is
   begin
      --  Master PIC

      --  ICW1: ICW4 needed, cascade mode, edge-triggered

      Outb (Master_Pic_Port + 0, 2#0001_0001#);

      --  ICW2: Vector 0-7

      Outb (Master_Pic_Port + 1, 16#00#);

      --  ICW3: (master): slave on int 2

      Outb (Master_Pic_Port + 1, 2#0000_0100#);

      --  ICW4: 8086 mode, normal EOI, buffered mode/master, not special mode

      Outb (Master_Pic_Port + 1, 2#0000_1100#);

      --  Slave PIC

      --  ICW1: ICW4 needed, cascade mode, edge-triggered

      Outb (Slave_Pic_Port + 0, 2#0001_0001#);

      --  ICW2: Vector 8-15

      Outb (Slave_Pic_Port + 1, 16#08#);

      --  ICW3: (slave): slave id 2

      Outb (Slave_Pic_Port + 1, 16#02#);

      --  ICW4: 8086 mode, normal EOI, buffered mode/slave, not special mode

      Outb (Slave_Pic_Port + 1, 2#0000_1000#);

      --  Unmask all interrupts except timer

      Outb (Master_Pic_Port + 1, 1);
      Outb (Slave_Pic_Port + 1, 0);
   end Initialize_PICs;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      --  Frequency of the system clock for the decrementer timer

      return 100_000_000;
   end Ticks_Per_Second;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Nothing to do on standard powerpc

      null;
   end Clear_Alarm_Interrupt;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Specific.Vector_Id) return Interrupt_ID
   is
      pragma Unreferenced (Vector);

      Intack : Unsigned_8;
      for Intack'Address use 16#BFFFFFF0#;
      pragma Volatile (Intack);
      pragma Import (Ada, Intack);
      --  Prep specific address to send an IACK request on the bus and get
      --  the pending interrupt.

   begin
      return System.BB.Interrupts.Interrupt_ID (Intack);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID;
      Prio      : Interrupt_Priority)
   is
      pragma Unreferenced (Interrupt, Prio);
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler, CPU_Specific.External_Interrupt_Excp);
   end Install_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

      return Interrupt_Priority'First;
   end Priority_Of_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
   begin
      if Interrupt in Slave_Interrupt_IDs then
         Outb (Slave_Pic_Port, 2#0010_0000#);
      end if;

      Outb (Master_Pic_Port, 2#0010_0000#);
   end Clear_Interrupt_Request;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Integer) is
   begin
      --  Note that Priority cannot be the last one, as this procedure is
      --  unable to disable the decrementer interrupt.

      pragma Assert (Priority /= Interrupt_Priority'Last);

      null;
   end Set_Current_Priority;

   ----------------
   -- Power_Down --
   ----------------

   procedure Power_Down is
   begin
      null;
   end Power_Down;

end System.BB.Board_Support;
