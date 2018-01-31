ARM STM32 Runtimes
==================

Runtimes Supported
------------------

* ZFP
* Ravenscar-SFP
* Ravenscar-Full

Targets Supported
-----------------

Cortex-M4 and Cortex-M7 MCUs

System Clocks
-------------

Clocks Configuration
,,,,,,,,,,,,,,,,,,,,

The system clock source is the main phase-locked loop (PLL) driven by an
external crystal. The frequency of the external crystal (HSE) is
specified in package System.BB.Board_Parameters (in the gnat directory as
file s-bbbopa.ads), as is the main clock frequency. For example:

.. code-block:: ada

   Main_Clock_Frequency : constant := 168_000_000;
   HSE_Clock_Frequency : constant := 8_000_000;

Change the values in that package to reflect your specific board, as
necessary. The runtime system uses them to configure the clocks so
changes will take effect automatically. Package System.BB.Parameters
(gnat/s-bbpara.ads) imports those values and re-exports them as constants
used by library procedure Setup_PLL. The shared procedure Setup_PLL
configures the PLL and the derived clocks to achieve that main clock
frequency. Compilation will fail if the requested clock frequency is not
achievable.

Clock Overdriving
,,,,,,,,,,,,,,,,,

Procedure Setup_PLL always attempts to enable clock overdriving by
calling procedure PWR_Overdrive_Enable declared in package
System.BB.MCU_Parameters. However, not all targets allow overdriving so
the corresponding procedure body is null in the sources for those
targets. Change the procedure body for your target accordingly.

Startup Code
------------

The startup code is in multiple assembly language files located in the gnat
subdirectory within the runtime.

There are two assembly language files for the startup code, one each for
executing from RAM or ROM, plus a common file shared by both (e.g., that
starts the FPU). These are files named start-ram.S, start-rom.S, and
common.S, respectively.

The specific startup code is selected by the linker scripts' references to
the unique symbols defined in the assembly files, via the entry point
definitions.

For the ZFP runtime these start-\*.S file is used to initialize the vector
table.

For the Ravenscar runtimes, the vector table is initialized by code in
handler.S. The code in package System.BB.CPU_Primitives (gnat/s-bbcppr.adb)
installs GNAT-specific handlers that raise exceptions for the traps.

Floating-point Co-processor
---------------------------

Package System.BB.Parameters (gnat/s-bbpara.ads) specifies whether a FPU is
present, but this is used in conditional code in the context switch routine,
not to decide whether to enable the FPU. The supported STM32 targets all
have an FPU so the unit is enabled in the common startup code
(start-common.S). If your target does not have an FPU change the assembly
code and the System.BB.Parameters package.

Interrupts
----------

The package Ada.Interrupts.Names (a-intnam.ads) is located in the gnat
directory. This package spec was automatically generated from an SVD file so
you will not need to change it unless an SVD file was unavailable for your
target.

See package System.BB.MCU_Parameters (s-bbmcpa.ads) for the number of
interrupts defined. That number must reflect the contents of the
SVD-generated package Ada.Interrupts.Names.

Memory Layout
-------------

The memory layout is controlled by linker scripts specific to whether the
program is located in RAM or ROM. The scripts are located in the ld
directory and are named common-RAM.ld and common-ROM.ld, respectively.

Script selection is controlled by a scenario variable declared in an XML
file named runtime.xml that is located in the runtime root directory. The
scenario variable is used to specify linker switches.

The memory sections' locations and sizes are specified in memory-map.ld,
also located in the ld directory. The XML also specifies this file as part
of the linker switches.

You can modify all of these scripts as required. Alternatively, these
scripts can be overridden at link time using the LDSCRIPT environment
variable.

Resources Used
--------------

The Ravenscar runtime libraries use the SysTick interrupt to implement Ada
semantics for time, i.e., delay statements and package Ada.Real_Time. The
SysTick interrupt handler runs at highest priority. See procedure
Sys_Tick_Handler in package body System.BB.CPU_Primitives
(gnat/s-bbcppr.adb), which calls through to the handler in the trap vectors
only when necessary for the sake of efficiency.

The runtime libraries provide a minimal version of package Ada.Text_IO
supporting character- and string-based input and output routines. These are
implemented using a board-specific UART. You can change the UART selection
as well as the configuration (e.g., the baud rate). The source files are
located in the gnat directory in a package named System.Text_IO
(gnat/s-textio.adb).
