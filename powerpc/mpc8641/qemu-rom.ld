OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
              "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)
ENTRY(_start_rom)

_DEFAULT_STACK_SIZE = 64 * 1024;
__heap_size = 1024 * 1024;

SECTIONS
{

  /* First instruction is fetched at this address */
  . = 0xfff00100;
  .reset : AT (0xfff00100)
  {
   KEEP(*(.reset))
  }

  /* Start of ROM.  */
  . = 0xfe000000;

  /* Code sections, merged into text segment: */
  .text :
  {
    __text = .;
    *(.text .text.* .gnu.linkonce.t.*)
    *(.gnu.warning)
    *(.glink)
  } = 0

  /* Read-only sections, merged into rodata segment: */
  .rodata :
  {
    . = ALIGN(4);
    *(.rodata .rodata.* .gnu.linkonce.r.*)
    *(.rodata1)
    *(.got2)
    . = ALIGN(4);
  } = 0

  /* init, ctors and dtors sections.  Assume we really need them
     when they happen to be generated, so KEEP to prevent garbage
     collection of per-object pieces.  */

  .init  : { KEEP(*(.init)) }
  .ctors : { KEEP(SORT_BY_NAME(*)(.ctors)) }
  .dtors : { KEEP(SORT_BY_NAME(*)(.dtors)) }

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  }

  .eh_frame  : { KEEP (*(.eh_frame)) }
  .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) }

  __rom_end = .;

  /* Leave room for exception vectors.  */
  . = 0x2000;
  __sdata2_load = __rom_end;
  .sdata2 : AT (__sdata2_load)
  {
    __sdata2_start = .;
    PROVIDE (_SDA2_BASE_ = 32768);
    *(.sdata2 .sdata2.* .gnu.linkonce.s2.*)
    . = ALIGN(4);
    __sdata2_end = .;
  }
  .sbss2 :
  {
    __sbss2_start = .;
    *(.sbss2 .sbss2.* .gnu.linkonce.sb2.*)
    . = ALIGN(4);
    __sbss2_end = .;
  }

  __data_load = LOADADDR(.sdata2) + SIZEOF(.sdata2);

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
  }

  .sbss :
  {
    __sbss_start = .;
    *(.dynsbss)
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
    __sbss_end = .;
  }

  .stacks :
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
  }

  .bss :
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
  }

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
  /DISCARD/ : { *(.note.GNU-stack) }
}
