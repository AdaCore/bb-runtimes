OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc", "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)
ENTRY(_start_flash)

_DEFAULT_STACK_SIZE = 0x2000;

MEMORY
{
  flash (RX)  : ORIGIN = 0x00000000, LENGTH = 3M
  sram (RWXA) : ORIGIN = 0x40000000, LENGTH = 128K
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
  } > flash = 0

  .rodata :
  {
    . = ALIGN(4);
    *(.rodata .rodata.* .gnu.linkonce.r.*)
    *(.rodata1)
    . = ALIGN(4);
  } > flash =0

  .eh_frame_hdr :
  {
    __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  } > flash
  .eh_frame       : { KEEP (*(.eh_frame)) } > flash
  .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) } > flash

  .sdata2 :
  {
    __sdata2_start = .;
    PROVIDE (_SDA2_BASE_ = 32768);
    *(.sdata2 .sdata2.* .gnu.linkonce.s2.*)
    . = ALIGN(4);
    __sdata2_end = .;
  } > flash
  .sbss2 :
  {
    __sbss2_start = .;
    *(.sbss2 .sbss2.* .gnu.linkonce.sb2.*)
    . = ALIGN(4);
    __sbss2_end = .;
  } > flash
  __rom_end = .;
  __data_load = __rom_end;
  __sram_start = ORIGIN (sram);
  __sram_size_dword = LENGTH (sram) / 8;

  .data : AT (__data_load)
  {
    /* Force alignment of __data_start.  */
    . = ALIGN (4);
    __data_start = .;
    __data = .;

    *(.data .data.* .gnu.linkonce.d.*)
    KEEP (*(.gnu.linkonce.d.*personality*))
    SORT(CONSTRUCTORS)
    *(.data1)
    PROVIDE (_SDA_BASE_ = 32768);
    *(.sdata .sdata.* .gnu.linkonce.s.*)

    /* Force alignment of __data_end.  */
    . = ALIGN(4);
    __data_end = .;
  } > sram

  __data_size_word = (__data_end - __data_start) / 4;

  .sbss :
  {
    __sbss_start = .;
    *(.dynsbss)
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
    __sbss_end = .;
  } > sram
  .stacks (NOLOAD) : /* No need to load the stacks. */
  {
   . = ALIGN(8);
   __interrupt_stack_start = .;
   *(.noinit.interrupt_stacks)
   __interrupt_stack_end = .;

   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(16);
   __stack_end = .;
  } > sram

  .bss :
  {
   __bss_start = .;
   *(.dynbss)
   *(.bss .bss.* .gnu.linkonce.b.*)
   *(COMMON)
   . = ALIGN(8);
   __bss_end = .;
   _end = .;
  } > sram

  __heap_start = _end;
  __heap_end = ORIGIN(sram) + LENGTH(sram);

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

