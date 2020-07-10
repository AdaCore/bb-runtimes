This is a simple blinky example for SAMD21 boards. On the Arduino Zero, this will blink the onboard LED.

Flashing Instructions:
----------------------

gprbuild -P blink.gpr
openocd -f arduino_zero.cfg

(In another terminal)
arm-eabi-gdb -f obj/blink
target extended-remote localhost:3333
monitor reset halt
load
monitor reset run


I've never used GPS/GNAT Studio to flash, but I assume it's possible as well.

If you want to use UART, the default settings from the runtime are 115200 baud, 8N1. Check (Ada Drivers Library)[https://github.com/AdaCore/Ada_Drivers_Library] for incoming GPIO, timing, I2C, SPI, and other drivers.

Authored by Abe Cohen @abeco