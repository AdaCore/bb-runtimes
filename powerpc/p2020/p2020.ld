OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
	      "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)

ENTRY(_start_ram)
_DEFAULT_STACK_SIZE = 64 * 1024;

MEMORY
{
  /* To be adjusted: this corresponds to the P2020 RDB */
  /* Start at 32 MB to leave room for the bootloader */
  sdram (RWXA) : ORIGIN = 0x02000000, LENGTH = 1024M - 32M
}

SECTIONS
{
  .text           :
  {
    __text = .;
    *(.interrupt)
    *(.text.unlikely .text.*_unlikely)
    *(.text.exit .text.exit.*)
    *(.text.startup .text.startup.*)
    *(.text.hot .text.hot.*)
    *(.text .stub .text.* .gnu.linkonce.t.*)
    /* .gnu.warning sections are handled specially by elf32.em.  */
    *(.gnu.warning)
    *(.glink)
  } = 0
  .init           :
  {
    KEEP (*(.init))
  } =0
  .fini           :
  {
    KEEP (*(.fini))
  } =0
  .ctors : { KEEP(SORT_BY_NAME(*)(.ctors)) }
  .dtors : { KEEP(SORT_BY_NAME(*)(.dtors)) }

  .rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) }
  .rodata1        : { *(.rodata1) }

  .sdata2         :
  {
    __sdata2_start = .;
    PROVIDE (_SDA2_BASE_ = 32768);
    *(.sdata2 .sdata2.* .gnu.linkonce.s2.*)
    __sdata2_end = .;
  }
  .sbss2          :
  {
    __sbss2_start = .;
    *(.sbss2 .sbss2.* .gnu.linkonce.sb2.*)
    __sbss2_end = .;
  }

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  }
  .eh_frame       : ONLY_IF_RO { KEEP (*(.eh_frame)) }
  .gcc_except_table : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }

  . = ALIGN (8);

  /* Exception handling  */
  .eh_frame       : ONLY_IF_RW { KEEP (*(.eh_frame)) }
  .gcc_except_table   : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }
  .got2           : { *(.got2) }
  .data           :
  {
    __data = .;
    *(.data .data.* .gnu.linkonce.d.*)
    SORT(CONSTRUCTORS)
  }
  .data1          : { *(.data1) }
  /* We want the small data sections together, so single-instruction offsets
     can access them all, and initialized data all before uninitialized, so
     we can shorten the on-disk segment size.  */
  .sdata          :
  {
    __SDATA_START__ = .;
    PROVIDE (_SDA_BASE_ = 32768);
    *(.sdata .sdata.* .gnu.linkonce.s.*)
  }
  _edata = .; PROVIDE (edata = .);
  .sbss :
  {
    __sbss_start = .;
    *(.dynsbss)
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
    __sbss_end = .;
    __SBSS_END__ = .;
  }

  .stacks :
  {
   . = ALIGN(16);
   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   __interrupt_stack_end = .;

   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(16);
   __stack_end = .;
  }

  .bss :
  {
   __bss_start = .;
   *(.dynbss)
   *(.bss .bss.* .gnu.linkonce.b.*)
   *(COMMON)
   . = ALIGN(16);
   __bss_end = .;
   _end = .;
  }

  __heap_start = _end;
  __heap_end = ORIGIN(sdram) + LENGTH(sdram);

  .comment       0 : { *(.comment) }

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* SGI/MIPS DWARF 2 extensions */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
