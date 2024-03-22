OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
              "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)

MEMORY
{
  RAM (rwx): ORIGIN = 0, LENGTH = 128M
  ROM (rx) : ORIGIN = 0xfe000000, LENGTH = 32M
  RESET(rw): ORIGIN = 0xfff00100, LENGTH = 4K
}

REGION_ALIAS("ROMABLE_REGION", ROM)
REGION_ALIAS("REGION_DATA", RAM)

ENTRY(_start)

INCLUDE common.ld
