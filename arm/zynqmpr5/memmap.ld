MEMORY
{
   psu_ocm_ram_0_MEM_0 : ORIGIN = 0xFFFC0000, LENGTH = 0x40000
   psu_qspi_linear_0_MEM_0 : ORIGIN = 0xC0000000, LENGTH = 0x20000000
   psu_r5_0_atcm_MEM_0 : ORIGIN = 0x0, LENGTH = 0x10000
   psu_r5_0_btcm_MEM_0 : ORIGIN = 0x20000, LENGTH = 0x10000
   /* Configure only 1GB of DDR: should be enough for bare metal applications */
   psu_r5_ddr_0_MEM_0 : ORIGIN = 0x100000, LENGTH = 0x40000000
}
