Microsemi Cortex-M1 Run-times
=============================

Runtimes Supported
------------------

* ZFP
* Ravenscar-SFP

Build instructions
------------------

Inside the `BSPs` directory, use gprbuild to compile the run-time:

$ gprbuild -j0 -P ravenscar_sfp_m1agl.gpr

To install the run-time in your tool-chain use the following command:

$ gprinstall -p -P ravenscar_sfp_m1agl.gpr

Example project: Hello world
----------------------------

An example "Hello world" project is provided with the run-time in the
`hello-m1agl` directory.

Use gprbuild to build the example:

$ gprbuild -P hello.gpr -XLOADER=RAM

An ELF executable (`main`) will be compiled in the directory of the project.

The `-XLOADER=RAM` option specifies that the binary is meant to run with the
RAM mapped to the address 0x0. On the M1AGL board this achieved with dip-switch
SW2 #9 in the OFF position.

Please see Microsemi's documentation to start the OpenOCD debugger. Once the
debugger is ready, start GDB with the ELF binary:

$ arm-eabi-gdb main

If the GDB provided with GNAT does work with the OpenOCD provided by Microsemi,
you can use the GDB provided by Microsemi as a backup:

$ arm-none-eabi-gdb main

In the GDB prompt, connect to the probe with the `target` command, load the
binary with the `load` command and start the execution with the `continue`
command:

(gdb) target remote :3333
Remote debugging using :3333
0xfffffffe in ?? ()
(gdb) load
Loading section .text, size 0x2c98 lma 0x0
Loading section .ARM.exidx, size 0x8 lma 0x2c98
Loading section .rodata, size 0x560 lma 0x2ca0
Loading section .data, size 0x98 lma 0x3200
Start address 0x2628, load size 12952
Transfer rate: 25 KB/sec, 2158 bytes/write.
(gdb) continue
Continuing.

The example will print "Hello World!" with a number every seconds on the board
console (USB serial). It will also trigger a software interrupt, for which a
handler is attached to and the handler also prints a message on the console.

System Clock
------------

The optional SysTick timer of the armv6-m architecture is not implemented in
the Microsemi Cortex-M1. The Ravenscar-SFP run-time uses the CoreTimer device
to implement Ada semantics for time, i.e., delay statements and package
Ada.Real_Time.

The CoreTimer device is therefore not available for the user and its
configuration must not be modified in any way by user code when using the
Ravenscar-SFP run-time.

The CoreTimer frequency is set to half the APB frequency using the prescaler.
It is also configured to trigger an interrupt every milliseconds, this
interrupt is used by the run-times to implement the delay and timing events.

Interrupts
----------

In armv6-m Ravenscar run-times the interrupt handling should be provided by the
ARM Nested Vectored Interrupt Controller (NVIC). However in the Microsemi
Cortex-M1 only one interrupt line of the NVIC is used and the real interrupt
handling is done underneath by the CoreInterrupt device.

The Ravenscar-SFP run-time uses the CoreInterrupt device to implement Ada
semantics for interrupt handling.

The CoreInterrupt device is therefore not available for the user and its
configuration must not be modified in any way by user code when using the
Ravenscar-SFP run-time.

Context Switch
--------------

In armv6-m Ravenscar run-times the context switch should be done with a PendSV
trap and a dedicated handler that performs the switching of contexts. PendSV is
part of the optional features of the armv6-m architecture, the OS extension.

The Microsemi Cortex-M1 doesn't implement PendSV trap so the Ravenscar-SFP
run-time uses one of the IRQ of the CoreInterrupt device instead. The IRQ used
- the #8 - is therefore not available for the user.

Serial Port
-----------

The run-time libraries provide a minimal version of package Ada.Text_IO
supporting character and string-based input and output routines.

The ZFP and Ravenscar-SFP run-times use the CoreUARTapb device to implement
Ada.Text_IO. The device is configured for 57600 baudrate, 8-bit data, 1 stop
bit and no parity.

If Ada.Text_IO is not used, the user is free to use the CoreUARTapb device and
change its configuration.

Resources Used
--------------

 - In Ravenscar-SFP run-time
  - CoreTimer is used to implement Ada timing features
  - CoreInterrupt is used to implement Ada interrupt handling
  - IRQ #8 is used for context switch

 - In Ravenscar-SFP and ZFP run-times
  - CoreUARTapb if Ada.Text_IO is used

Memory Layout
-------------

With SW2 #9 in the ON position:
 - External FLASH 0x00000000
 - External SRAM 0x08000000
With SW2 #9 in the OFF position:
 - External SRAM 0x00000000
 - External FLASH 0x08000000

The rest of the memory map is fixed:
 - Internal SSRAM 0x20000000
 - CoreGPIO 0xA0000000
 - CoreUARTapb 0xA1000000
 - CoreTimer 0xA2000000
 - CoreInterrupt 0xA3000000

Memory sizes
````````````

 - FLASH            size : 4M
 - SRAM  (external) size : 1M
 - SSRAM (internal) size : 2k ???
