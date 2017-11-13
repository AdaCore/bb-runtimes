MEMORY
{
  flash  (rx) : ORIGIN = 0x00000000, LENGTH = 16M
  /* sram   (rx) : ORIGIN = 0x08000000, LENGTH = 1M */
  sram   (rx) : ORIGIN = 0x00000000, LENGTH = 1M
  ssram (rwx) : ORIGIN = 0x20000000, LENGTH = 2k
}

REGION_ALIAS("sram_tx", sram)
REGION_ALIAS("sram_ro", sram)
REGION_ALIAS("sram_bs", sram)
REGION_ALIAS("sram_da", sram)
