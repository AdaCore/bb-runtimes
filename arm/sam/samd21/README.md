ARM SAMD21 Runtime
===================

Runtimes Supported
------------------

* ZFP

Targets Supported
-----------------

Cortex-M0+ MCUs

System Clocks
-------------

The system clock source is the main Digital Frequency Locked Loop (DFLL) sourced from the external crytal (XOSC32K). The clocks were configured according to [this example](https://microchipdeveloper.com/32arm:samd21-code-gcc-clock-system-dfll48m-init) from the manufacturer, and are assumed to run at 48MHz.

Startup Code
------------

The startup code is in the assembly language file start-rom.S in the crt0 subdirectory within the runtime

Memory Layout
-------------

The memory layout is controlled by the linker scripts memory-map.ld and common-ROM.ld. You can modify these scripts as required. 

Resources Used
--------------

The runtime provides a minimal version of the package Ada.Text_IO supporting character- and string-based input and output routines. These are implemented using a board-specific UART. You can change the UART selection as well as the configuration. The source file is located in crt0/s-textio.adb

If you want to use UART, the default settings are 115200 baud, 8N1. It runs off an 8MHz clock.

Notes
-----

This runtime was developed using the Arduino Zero as a reference board. See the samd21-blink example to see how GPIO works with this runtime. Check [Ada Drivers Library](https://github.com/AdaCore/Ada_Drivers_Library) for incoming GPIO, timing, I2C, SPI, and other drivers. If you want to develop your own, you can use the reference hardware interface files in this library (in the crt0 directory, anything with i-sam-*.ads), or generate more by downloading the manufacturer SVD file from http://packs.download.atmel.com/ and running that through (svd2Ada)[https://github.com/AdaCore/svd2ada].

Build/Installation
------------------

See repo's main README.md


Authored by Abe Cohen @abeco