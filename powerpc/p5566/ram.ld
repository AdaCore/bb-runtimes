OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
              "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)
ENTRY(_start_ram)

_DEFAULT_STACK_SIZE = 0x8000; /* Some ACATS need a large amount of stack.  */

MEMORY
{
  extsram (RWXA) : ORIGIN = 0x20000000, LENGTH = 512K
}

SECTIONS
{
  /* Code sections, merged into text segment: */
  .text :
  {
    __text = .;
    KEEP (*(.interrupt)) /* Interrupt vector table, must be within
                            a 64KB window */
    KEEP (*(.init))
    *(.text .text.* .gnu.linkonce.t.*)
    *(.gnu.warning)
  } = 0

  .rodata :
  {
    . = ALIGN(4);
    *(.rodata .rodata.* .gnu.linkonce.r.*)
    *(.rodata1)
    . = ALIGN(4);
  } =0

  .eh_frame_hdr :
  {
    __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  }
  .eh_frame       : { KEEP (*(.eh_frame)) }
  .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) }

  .sdata2 :
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
  .data :
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

  . = ALIGN(8);

  .sbss :
  {
    __sbss_start = .;
    *(.dynsbss)
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
    __sbss_end = .;
  }
  .stacks (NOLOAD) : /* No need to load the stacks. */
  {
   . = ALIGN(8);
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
   . = ALIGN(8);
   __bss_end = .;
   _end = .;
  }

  __heap_start = _end;
  __heap_end = ORIGIN(extsram) + LENGTH(extsram);

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

