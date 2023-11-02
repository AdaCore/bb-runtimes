MEMORY
{
   /* TCM is configured for dual core mode (each core has 64KB of TCM) */

   R5F_TCMA : ORIGIN = 0x00000000, LENGTH = 32K
   R5F_TCMB : ORIGIN = 0x41010000, LENGTH = 32K

   /* MSRAM is divided on 8 banks of 256KB each. The banks are separated below
      to allow easy assignment of banks to applications running on different
      cores. Several banks can be assigned to single application by merging
      the required banks below into a single region if required. */

   MSRAM_256K0 : ORIGIN = 0x70000000, LENGTH = 256K
   MSRAM_256K1 : ORIGIN = 0x70040000, LENGTH = 256K
   MSRAM_256K2 : ORIGIN = 0x70080000, LENGTH = 256K
   MSRAM_256K3 : ORIGIN = 0x700C0000, LENGTH = 256K
   MSRAM_256K4 : ORIGIN = 0x70100000, LENGTH = 256K
   MSRAM_256K5 : ORIGIN = 0x70140000, LENGTH = 256K
   MSRAM_256K6 : ORIGIN = 0x70180000, LENGTH = 256K
   MSRAM_256K7 : ORIGIN = 0x701C0000, LENGTH = 256K

   /* FLASH Memory. Ensure it does not overlap with other CPUs if you're
      planning to place executable code and other CPU specific data in this
      region. */

   FLASH : ORIGIN = 0x60100000, LENGTH = 512K

   /* DDR Memory. Ensure it does not overlap with other CPUs if you're
      planning to place executable code and other CPU specific data in this
      region. */

   DDR_RAM : ORIGIN = 0x80000000, LENGTH = 2048M

}
