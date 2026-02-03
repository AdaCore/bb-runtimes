TI AM64x/AM243x Arm Cortex-A53 Runtime
======================================

Runtimes supported
------------------

* Light

Target Supported
----------------

* TI AM64x/AM243x Arm Cortex-A53 Subsystem (A53SS)
    - The runtime runs on the first A53SS CPU.

Using the runtime
-----------------

As a prerequisite, GNAT Pro for Aarch64 ELF needs to be installed. To then
use the runtime provide the full path to the runtime in the project file:

    for Runtime ("Ada") use "/path/to/runtime/light-am64x";

Rebuilding the Runtime
----------------------

If you need to make changes to the runtime's BSP you can rebuild it as follows:

    python build.py

If you need to rebuild the runtime with debug information you can rebuild it with

    python build.py --build-flags=-XBUILD=Debug

Resources used by the Runtime
-----------------------------

- UART0 for `Ada.Text_IO`.

Startup Code
------------

The TI AM64x/AM243x A53SS runtime provides low-level startup code in `start.S`
that contains the the entry point to the application. It performs the low-level
runtime initialization required before high-level Ada code is called. The
startup code performs the following operations:

 * Initialization of the core registers
 * Enabling of the FPU
 * Clearing the BSS section
 * Dropping down to Exception Level 1 from Exception Level 3
 * Initial Configuration of the MMU

The ARMv8-A architecture defines four Exception Levels (EL), 3 to 0. The device will
boot to EL 3. The runtime will perform a basic configuration for EL 3 and EL 2 before
switching to EL 1. In EL 1 it will set up the stack and MMU. If further configuration
in the early boot stages is required the runtime expects the following symbols to be
implemented:

 * `__configure_el3`
 * `__configure_el2`
 * `__configure_el1`

Each of these symbols will be called in the context of the respective EL, allowing
further device configuration and permission management in that context. The EL 3 and 2
versions will be called right before dropping into the next lower EL. The EL 1 version
will be called before the stack and MMU setup. For all of these functions **no stack is set up**.
All registers can be used with the exception of the link register (x30) which is required
to return properly to the calling code. The user is free to configure a more complex
environment at their own responsibility. The runtime will assume that no stack has been
set up after any of these functions return.

The runtime configures the MMU in EL1 with a flat level 1 mapping. This configuration is
partially done in `System.MMU` which also provides types specifying types for translation
tables After the initial configuration the runtime will call the procedure
`__initialize_mmu_post` with the level 1 table as a parameter. This procedure can be used
to customize the MMU configuration. The user is responsible for the correctness of the
modified configuration and for allocating memory for further tables. Within this procedure
only a limited set of Ada features is available within this procedure as the runtime is
not fully initialized yet. In particular elaboration code and the secondary stack must be
avoided.

Text_IO
-------

The runtime provides a minimal version of the `Ada.Text_IO` package supporting
character- and string-based input and output routines for basic I/O needs. It
is recommended to implement your own I/O packages based around your I/O
channel of choice.

The bodies of the `Ada.Text_IO` routines call through to a device-specific I/O
package named `System.Text_IO`. See the package body in the file `s-textio.adb`
in the gnat directory for more details.

`System.Text_IO` on the AM64x/AM263x A53SS runtime is configured by default to
use UART0. UART0 is configured for 115200 baud rate, one stop bit, no parity
using the 48 MHz clock source.

Note: `System.Text_IO` does not provide any synchronised access to UART0 across
R5FSS and A53SS CPUs. If you deploy multiple applications using the runtime,
ensure only one application uses `Ada.Text_IO`. If your other applications
require Text_IO services, have these applications interface directly with
different I/O peripherals, or use the AM64x/AM243x IPC facilities to relay
messages through the application designated to communicate with UART0.
