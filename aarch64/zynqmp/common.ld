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
 *                   Copyright (C) 2003-2019 AdaCore                        *
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

/* This is a aarch64 specific version of this file */

/* This script replaces ld's default linker script, providing the
   appropriate memory map and output format. */

_DEFAULT_STACK_SIZE = (200 * 1024);

ENTRY(__vectors);

/*
 * The memory map looks like this:
 * +--------------------------------+ <- low memory
 * | .vectors                       |
 * +--------------------------------+
 * | .text                          |
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
 * |       __interrupt_stack_start  |
 * |    interrupt stack space       |
 * |       __interrupt_stack_end    |
 * |                                |
 * |       __stack_start            |
 * |    stack space                 |
 * |       __stack_end              | top of stack
 * |       _end                     | start of heap, used by sbrk()
 * +--------------------------------+
 * |    heap space                  |
 * |                                |
 * +--------------------------------+
 * |  remote monitor                |
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

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  } > REGION_CODE
  .eh_frame         : { KEEP (*(.eh_frame)) } > REGION_CODE
  .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) } > REGION_CODE

  . = ALIGN(0x10);

  __data_load = .;
  .data : AT (__data_load)
  {
    __data = .;
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
    *(.mmu_tbl0)
    *(.mmu_tbl1)
    *(.mmu_tbl2)
    . = ALIGN(0x10);
    __data_end = .;
  } > REGION_DATA
  __data_dwords = (__data_end - __data_start) >> 3;

  .bss (NOLOAD): {
   . = ALIGN(0x10);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

   /* Align the stack to 128 bits (SIMD/fpu register size) */
   . = ALIGN(0x10);
   __bss_end = .;

   /* Separate env stack per CPU and EL levels= */
   __stack_start = .;

   __cpu0_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10);
   __cpu0_stack_end = .;

   __cpu1_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10);
   __cpu1_stack_end = .;

   __cpu2_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10);
   __cpu2_stack_end = .;

   __cpu3_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10);
   __cpu3_stack_end = .;

   __stack_end = .;

   /* Interrupt stacks: defined in s-bbinte.adb.  */
   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   . = ALIGN(0x10);
   __interrupt_stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(REGION_DATA) + LENGTH(REGION_DATA);
  } > REGION_DATA

   __bss_dwords = (__bss_end - __bss_start) >> 3;

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
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
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }

  .ARM.attributes 0 : { KEEP (*(.ARM.attributes)) KEEP (*(.gnu.attributes)) }
  .note.gnu.arm.ident 0 : { KEEP (*(.note.gnu.arm.ident)) }

  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
