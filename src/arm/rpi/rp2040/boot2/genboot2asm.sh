#!/bin/sh

# Example usage: genboot2asm.sh generic_03 generated/boot2__generic_03.S

# Build the bootloader executable
gprbuild -p -P boot2.gpr -XFLASH_DEVICE=$1

# Convert to binary format
arm-eabi-objcopy -O binary obj/$1/boot2 obj/$1/boot2.bin

# Insert padding & CRC32, and output as an assembly file
python3 genboot2asm.py obj/$1/boot2.bin -o $2
