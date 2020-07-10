MEMORY
{
    flash   (rx)  : ORIGIN = 0x00000000, LENGTH = 0x00040000
    ram     (rwx) : ORIGIN = 0x20000000, LENGTH = 0x00008000
}

REGION_ALIAS("sram_tx", ram)
REGION_ALIAS("sram_ro", ram)
REGION_ALIAS("sram_bs", ram)
REGION_ALIAS("sram_da", ram)