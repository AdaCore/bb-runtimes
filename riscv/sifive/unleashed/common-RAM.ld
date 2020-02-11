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

/* This is a RISC-V specific version of this file */

INCLUDE memory-map.ld

SEARCH_DIR(.)
__DYNAMIC  =  0;

_DEFAULT_STACK_SIZE = (40 * 1024);

OUTPUT_ARCH("riscv")

ENTRY(_start_ram);

SECTIONS
{
  .text :
  {
    __text = .;
    KEEP (*(SORT_NONE(.start)))
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
  } > ram

  .init  : { KEEP(*(.init)) } > ram
  .ctors : { KEEP(SORT_BY_NAME(*)(.ctors)) } > ram
  .dtors : { KEEP(SORT_BY_NAME(*)(.dtors)) } > ram

  .rodata :
  {
    *(.rdata)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rom_end = .;
  } > ram

  .eh_frame_hdr :
  {
     __eh_frame_hdr = .;
    *(.eh_frame_hdr)
  } > ram

  .eh_frame  :
  {
    KEEP (*(.eh_frame))
  } > ram

  .gcc_except_table :
  {
    *(.gcc_except_table .gcc_except_table.*)
  } > ram

  .data :
  {
    __data = .;
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
  } > ram

  .srodata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    PROVIDE( __global_pointer$ = . + 0x800 );
    *(.srodata.cst16)
    *(.srodata.cst8)
    *(.srodata.cst4)
    *(.srodata.cst2)
    *(.srodata .srodata.*)
  } > ram

  .sdata ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    *(.sdata .sdata.*)
    *(.gnu.linkonce.s.*)
  } > ram

  .sdata2 ALIGN(4) : ALIGN(4) /* Align both virtual and load addresses */
  {
    *(.sdata2 .sdata2.*)
    *(.gnu.linkonce.s2.*)
  } > ram

  __data_end = .;

  /* Size of all data sections (.data, .srodata, .sdata) in number of 32bit
   * words.
   */
  __data_words = (__data_end - __data_start) >> 2;

  /* Base address of all data sections in ROM. The startup code copies these
   * sections from __data_load (in ROM) to __data_start (in RAM).
   */
  __data_load = LOADADDR(.data);

  .bss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;

   *(.sbss*)
   *(.gnu.linkonce.sb.*)
   *(.bss .bss.*)
   *(.gnu.linkonce.b.*)
   *(COMMON)

   /* Interrupt stacks. Statically allocated in System.BB.Interrupts */
   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
   __interrupt_stack_end = .;


   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
   __stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(ram) + LENGTH(ram);
   __bss_end = .;
  } > ram

   __bss_words = (__bss_end - __bss_start) >> 2;
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
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
