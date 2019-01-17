/*
# From datasheet
# Boot 0x00000000-0x0007ffff (512K)
# flash0 0x00080000-000bffff (256K)
# flash1 0x000c0000-000fffff (256K)
# sram 0x20000000 (96K)
# sram0 0x20000000-2007ffff (64K)
# sram1 0x20080000-200fffff (32K)
*/

MEMORY
{
  flash (rx)  : ORIGIN = 0x00080000, LENGTH = 512K
  sram (rwx)  : ORIGIN = 0x20000000, LENGTH = 96K
}

REGION_ALIAS("sram_tx", sram)
REGION_ALIAS("sram_ro", sram)
REGION_ALIAS("sram_bs", sram)
REGION_ALIAS("sram_da", sram)
