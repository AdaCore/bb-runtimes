/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  A R M                                   *
 *                                                                          *
 *                            Linker Script File                            *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2006 The European Space Agency            *
 *                   Copyright (C) 2003-2023 AdaCore                        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This is the Morello specific version of this file */

/* This script replaces ld's default linker script, providing the
   appropriate memory map and output format. */

_DEFAULT_STACK_SIZE = (8 * 1024 * 1024);

ENTRY(__vectors);

/*
 * The memory map looks like this:
 * +--------------------------------+ <- low memory
 * | .vectors                       |
 * +--------------------------------+
 * | .text                          |
 * |                                |
 * +--------------------------------+
 * | read only data sections        |
 * |                                |
 * +--------------------------------+
 * | .got                           |
 * |                                |
 * +--------------------------------+
 * | .data                          | initialized data goes here
 * |                                |
 * +--------------------------------+
 * | .bss                           |
 * |       __bss_start              | start of bss, cleared by crt0
 * |    uninitialized data          |
 * |       __bss_end                |
 * |                                |
 * |       __stack_start            |
 * |    stack space                 |
 * |       __stack_end              | top of stack
 * |                                |
 * |       __interrupt_stack_start  |
 * |    interrupt stack space       |
 * |       __interrupt_stack_end    |
 * |       _end                     | start of heap, used by sbrk()
 * +--------------------------------+
 * |    heap space                  |
 * |                                |
 * +--------------------------------+ <- high memory
 */

SECTIONS
{
  .text :
  {
    __text = .;
    KEEP (*(.vectors))
    *(.boot)
    *(.text .text.* .gnu.linkonce.t*)
  } > REGION_CODE

  .rodata :
  {
    *(.rodata .rodata.* .gnu.linkonce.r*)
  } > REGION_CODE

  .cap_relocs :
  {
    __cap_relocs_start = .;
    *(__cap_relocs)
    __cap_relocs_end = .;
  } > REGION_CODE

  /* The __eh_frame_hdr and __eh_frame_hdr_limit symbols define the bounds of
     __eh_frame_hdr_cap, which is used by the unwinder to access exception
     table data. All EH sections must be placed between these two symbols. */

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)

    /* Ensure that this section is not empty on runtimes without exception
       tables. Otherwise, references to __eh_frame_hdr fail to link
       due to an out of range relocation error. */
    . = . + 16;
  } > REGION_CODE

  __eh_frame_hdr_start = SIZEOF(.eh_frame_hdr) > 0 ? ADDR(.eh_frame_hdr) : 0;
  __eh_frame_hdr_end = SIZEOF(.eh_frame_hdr) > 0 ? . : 0;

  .eh_frame :
  {
    __eh_frame_start = .;
    KEEP (*(.eh_frame))
    __eh_frame_end = .;
  } > REGION_CODE

  /* LLVM puts a reference to the personality function in this section. We
     place the section here to make sure that it is within the bounds of
     __eh_frame_hdr_cap, so that the exception-handling code can read the
     reference. */

  .data.DW.ref :
  {
    *(.data.DW.ref.*)
  } > REGION_DATA

  /* The following sections are located in the GNU_RELRO segment. The GNU_RELRO
     segment should be contiguous, so do not place non-GNU_RELRO segments in
     between. */

  .gcc_except_table :
  {
    *(.gcc_except_table .gcc_except_table.*)
  } > REGION_CODE

  . = ALIGN(0x1000);

  .data.rel.ro :
  {
    *(.data.rel.ro)
    *(.data.rel.ro.*)
  } > REGION_DATA

  .got :
  {
    *(.got)
  } > REGION_DATA

  /* End of GNU_RELRO segment */

  .rela.dyn :
  {
    *(.rela.dyn)
    *(.rela.dyn.*)
  } > REGION_DATA

  .data :
  {
    __data_load = .;
    __data = .;
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
    . = ALIGN(0x10);
    __data_end = .;
    __data_dwords = (__data_end - __data_start) >> 3;
    __eh_frame_hdr_limit = .;
  } > REGION_DATA

  .bss (NOLOAD): {
   . = ALIGN(0x10);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

   /* Align the stack to 128 bits (SIMD/fpu register size) */
   . = ALIGN(0x10);
   __bss_end = .;

   /* Separate env stack per CPU and EL levels. */
   . = ALIGN(0x1000);

   __stack_start = .;

   __cpu0_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x1000);
   __cpu0_stack_end = .;

   __cpu1_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x1000);
   __cpu1_stack_end = .;

   __cpu2_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x1000);
   __cpu2_stack_end = .;

   __cpu3_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x1000);
   __cpu3_stack_end = .;

   __stack_end = .;

   /* Interrupt stacks: defined in s-bbinte.adb.  */
   __interrupt_stack_start = .;
   *(.noinit.interrupt_stacks)
   . = ALIGN(0x10);
   __interrupt_stack_end = .;

   __end = .;
   __heap_start = ABSOLUTE(.);
   __heap_end = ABSOLUTE(ORIGIN(REGION_DATA) + LENGTH(REGION_DATA));
  } > REGION_DATA
  __bss_dwords = (__bss_end - __bss_start) >> 3;

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

  .ARM.attributes 0 : { KEEP (*(.ARM.attributes)) KEEP (*(.gnu.attributes)) }
  .note.gnu.arm.ident 0 : { KEEP (*(.note.gnu.arm.ident)) }

  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }

}
