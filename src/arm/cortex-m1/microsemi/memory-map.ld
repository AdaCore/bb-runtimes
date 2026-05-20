MEMORY
{
  fsram (rwx) : ORIGIN = 0x00000000, LENGTH = 128K
  itcm (x)    : ORIGIN = 0x10000000, LENGTH = 64K
  dtcm (rw)   : ORIGIN = 0x20000000, LENGTH = 64K
}

REGION_ALIAS("sram_text", fsram)
REGION_ALIAS("sram_rodata", fsram)
REGION_ALIAS("sram_bss", fsram)
REGION_ALIAS("sram_data", fsram)

REGION_ALIAS("dtcm_rodata", dtcm)
REGION_ALIAS("dtcm_bss", dtcm)
REGION_ALIAS("dtcm_data", dtcm)
