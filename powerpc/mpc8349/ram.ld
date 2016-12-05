OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
              "elf32-powerpc")
OUTPUT_ARCH(powerpc:common)
ENTRY(_start_ram)

_DEFAULT_STACK_SIZE = 64 * 1024;

SECTIONS
{
  /* Leave room for exception vectors and stub memory  */
  . = 0x100000;

  /* Code sections, merged into text segment: */
  .text :
  {
    *(.text .text.*)
  } = 0
  .rodata :
  {
    . = ALIGN(4);
    *(.rodata .rodata.*)
    . = ALIGN(4);
  } =0

  .data :
  {
    __data_start = .;
    *(.data .data.*)
    SORT(CONSTRUCTORS)
    . = ALIGN(4);
    __data_end = .;
  }
  .stacks :
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
   *(.bss .bss.*)
   *(COMMON)
   . = ALIGN(4);
   __bss_end = .;

   . = ALIGN(8);
   __heap_start = .;
   . = . + 0x100000;
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
  /DISCARD/ : { *(.note.GNU-stack) *(.eh_frame) }
}

