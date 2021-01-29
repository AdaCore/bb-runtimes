#!/usr/bin/python3

"""Pad and CRC32 a boot2 binary file, and output as an assembly (.S) file."""

import argparse
import crcmod
import binascii
import struct
import sys

# Max. boot2 length without CRC32
MAX_BOOT2_LENGTH = 256 - 4

# The RP2040 uses a slightly unusual CRC configuration
# See Section 2.8.1.3.1 of the RP2040 Datasheet
crc32 = crcmod.mkCrcFun(
    poly=0x104c11db7,
    initCrc=0xFFFFFFFF,
    rev=False,
    xorOut=0
)

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "BINFILE",
        nargs=1,
        help="Binary file to pad, CRC, and convert to asm"
    )
    parser.add_argument(
        "-o", "--output",
        nargs='*',
        default=[],
        help="Output asm source file"
    )

    args = parser.parse_args()

    with open(args.BINFILE[0], 'rb') as f:
        raw_boot2 = f.read()

    raw_boot2_len = len(raw_boot2)

    # Check file size
    print(f"boot2 size is {raw_boot2_len} bytes")
    if raw_boot2_len > MAX_BOOT2_LENGTH:
        print(
            "error: Input file size exceeds maximum .boot2 length"
            f"({raw_boot2_len} > {MAX_BOOT2_LENGTH})",
            file=sys.stderr
        )
        sys.exit(-1)

    # Add padding
    padding_length = MAX_BOOT2_LENGTH - raw_boot2_len
    padded_boot2 = raw_boot2 + bytes(padding_length)

    # Add the CRC32 (little-endian format)
    crc = crc32(padded_boot2)
    padded_boot2_with_crc32 = padded_boot2 + struct.pack('<I', crc)

    # Write the output
    for filename in args.output:
        with open(filename, 'w') as f:
            print(f"Generating output file: {filename}")
            f.write('.section .boot2, "a"\n')
            f.write('.global __boot2\n')
            f.write('__boot2:\n')

            # Write 16 bytes per line
            for i in range(0, len(padded_boot2_with_crc32), 16):
                chunk = padded_boot2_with_crc32[i:i + 16]
                f.write(".byte ")
                f.write(", ".join(f"0x{byte:02x}" for byte in chunk))
                f.write('\n')
