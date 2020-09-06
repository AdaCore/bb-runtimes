/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               R I S C - V                                *
 *                                                                          *
 *                            Linker Script File                            *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2006 The European Space Agency            *
 *                   Copyright (C) 2003-2020 AdaCore                        *
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

/* This script replaces ld's default linker script, providing the
   appropriate memory map and output format. */

INCLUDE memory-map.ld

_DEFAULT_STACK_SIZE = 4*1024;

ENTRY(_start_ram);

SECTIONS
{
  /* The Host/Target Interface is located at address 0 */
  . = 0;
  .host_target_interface :
  {
    KEEP(*(.host_target_interface))
  }

  .text :
  {
    *(.start)
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
    /* Ensure that the begin of .rodata is word aligned.  */
    . = ALIGN(. != 0 ? 0x4 : 0x1);
  } > ram_tx

  .rodata :
  {
    *(.lit)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(. != 0 ? 0x4 : 0x1);
  } > ram_ro

  .srodata :
  {
    *(.srodata.cst16) *(.srodata.cst8) *(.srodata.cst4) *(.srodata.cst2)
    *(.srodata .srodata.*)
    . = ALIGN(. != 0 ? 0x4 : 0x1);
  } > ram_ro

  .data :
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
    /* Ensure that the end of the data section is always word aligned.
       Initial values are stored in 4-bytes blocks so we must guarantee that
       these blocks do not fall out the section (otherwise they are truncated
       and the initial data for the last block are lost).  If the section is
       empty don't align at all since ".data" must not be emited and
       __data_start is expected to be equal to __data_end.  */
    . = ALIGN(. != 0 ? 0x4 : 0x1);
    __data_end = .;
  } > ram_da

  .sdata :
  {
    __global_pointer$ = . + 0x800;
    *(.sdata .sdata.* .gnu.linkonce.s.*)
  } > ram_da

  .sdata2 :
  {
    *(.sdata2 .sdata2.*)
    *(.gnu.linkonce.s2.*)
  } > ram_da

  .sbss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;
   *(.sbss .sbss.*)
  } > ram_bs

  .bss (NOLOAD): {
   *(.bss .bss.*)
   *(COMMON)

   __bss_end = .;

   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x10); /* Align stack to 128 bits as required in the ABI */
   __stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(ram_bs) + LENGTH(ram_bs);
  } > ram_bs

  __bss_words = (__bss_end - __bss_start) >> 2;

  /* Stabs debugging sections.  */
  .stab          0 : { *(.stab) }
  .stabstr       0 : { *(.stabstr) }
  .stab.excl     0 : { *(.stab.excl) }
  .stab.exclstr  0 : { *(.stab.exclstr) }
  .stab.index    0 : { *(.stab.index) }
  .stab.indexstr 0 : { *(.stab.indexstr) }
  .comment       0 : { *(.comment) }
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
  /* SGI/MIPS DWARF 2 extensions */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }
  /* DWARF Extension.  */
  .debug_macro    0 : { *(.debug_macro) }
  .debug_addr     0 : { *(.debug_addr) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
}
