_DEFAULT_STACK_SIZE = 64 * 1024;
__heap_size = 1024 * 1024;

SECTIONS
{

  /* First instruction is fetched at this address */
  .reset :
  {
   KEEP(*(.reset))
  } > RESET

  /* Leave room for exception vectors.  */
  .vectors (NOLOAD):
  {
    . = 0x2000;
  } > REGION_DATA

  . = ORIGIN(ROMABLE_REGION);
  /* Code sections, merged into text segment: */
  .text :
  {
    __text = .;
    *(.text .text.* .gnu.linkonce.t.*)
    *(.gnu.warning)
    *(.glink)
  } > ROMABLE_REGION = 0

  /* Read-only sections, merged into rodata segment: */
  .rodata :
  {
    . = ALIGN(4);
    *(.rodata .rodata.* .gnu.linkonce.r.*)
    *(.rodata1)
    *(.got2)
    . = ALIGN(4);
  } > ROMABLE_REGION = 0

  /* init, ctors and dtors sections.  Assume we really need them
     when they happen to be generated, so KEEP to prevent garbage
     collection of per-object pieces.  */

  .init  : { KEEP(*(.init)) } > ROMABLE_REGION
  .ctors : { KEEP(SORT_BY_NAME(*)(.ctors)) } > ROMABLE_REGION
  .dtors : { KEEP(SORT_BY_NAME(*)(.dtors)) } > ROMABLE_REGION

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  } > ROMABLE_REGION

  .eh_frame  : { KEEP (*(.eh_frame)) } > ROMABLE_REGION
  .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) } > ROMABLE_REGION

  __sdata2_load = .;
  .sdata2 : AT (__sdata2_load)
  {
    __sdata2_start = .;
    PROVIDE (_SDA2_BASE_ = 32768);
    *(.sdata2 .sdata2.* .gnu.linkonce.s2.*)
    . = ALIGN(4);
    __sdata2_end = .;
  } > REGION_DATA

  .sbss2 (NOLOAD):
  {
    __sbss2_start = .;
    *(.sbss2 .sbss2.* .gnu.linkonce.sb2.*)
    . = ALIGN(4);
    __sbss2_end = .;
  } > REGION_DATA

  /* Ensure __data_load has the same alignment than .data section. */
  __data_load = ALIGN(LOADADDR(.sdata2) + SIZEOF(.sdata2), ALIGNOF(.data));
  .data : AT(__data_load)
  {
    __data = .;
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d.*)
    KEEP (*(.gnu.linkonce.d.*personality*))
    SORT(CONSTRUCTORS)
    *(.data1)
    PROVIDE (_SDA_BASE_ = 32768);
    *(.sdata .sdata.* .gnu.linkonce.s.*)
    . = ALIGN(4);
    __data_end = .;
  } > REGION_DATA

  .sbss (NOLOAD):
  {
    __sbss_start = .;
    *(.dynsbss)
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
    __sbss_end = .;
  } > REGION_DATA

  .stacks (NOLOAD):
  {
   . = ALIGN(8);
   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   __interrupt_stack_end = .;

   /* Separate stack per CPU */
   __stack_start = .;

   __cpu1_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   __cpu1_stack_end = .;

   __cpu0_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   __cpu0_stack_end = .;

   __stack_end = .;
  } > REGION_DATA

  .bss (NOLOAD):
  {
   __bss_start = .;
   *(.dynbss)
   *(.bss .bss.* .gnu.linkonce.b.*)
   *(COMMON)
   . = ALIGN(4);
   __bss_end = .;

   . = ALIGN(8);
   /* One heap */
   __heap_start = .;
   . = . + __heap_size;
   __heap_end = .;

   _end = .;
  } > REGION_DATA

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */

  /* DWARF 1.  */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }

  /* GNU DWARF 1 extensions.  */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }

  /* DWARF 1.1 and DWARF 2.  */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }

  /* DWARF 2.  */
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line .debug_line.* .debug_line_end) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }

  /* SGI/MIPS DWARF 2 extensions.  */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }

  /* DWARF 3.  */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }

  /* DWARF 5.  */
  .debug_addr     0 : { *(.debug_addr) }
  .debug_line_str 0 : { *(.debug_line_str) }
  .debug_loclists 0 : { *(.debug_loclists) }
  .debug_macro    0 : { *(.debug_macro) }
  .debug_names    0 : { *(.debug_names) }
  .debug_rnglists 0 : { *(.debug_rnglists) }
  .debug_str_offsets 0 : { *(.debug_str_offsets) }
  .debug_sup      0 : { *(.debug_sup) }

  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) }
}
