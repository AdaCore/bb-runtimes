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

/* ARM Cortex-M1 TCM version */

/* This script takes advantage of the Cortex-M1 tightly-coupled memory. The
   program is stored in SRAM with the text section loaded into the ITCM
   and the data sections loaded into the DTCM. This script relies on
   the ITCM being remapped to the lower alais of 0x0 to allow the program
   to execute from this memory. Consequently, after the ITCM remapping
   SRAM will no longer be accessable. */

INCLUDE memory-map.ld

_DEFAULT_STACK_SIZE = 4*1024;

ENTRY(_start_tcm);

SECTIONS
{
  .text :
  {
    __text_start = .;
    KEEP (*(.vectors))
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
    . = ALIGN(0x4);
	__text_end = .;
  } > sram_text
  __text_words = (__text_end - __text_start) >> 2;
  __text_dest = ORIGIN (itcm);

  /* The following read-only sections need to be copied to DTCM as they will
     no longer be accessable from SRAM once the ITCM is remapped. DTCM is used
     rather than ITCM due to the performance penalty of accessing ITCM as data.
   */

  __extab_load = __text_end;
  .ARM.extab : AT (__extab_load)
  {
    __extab_start = .;
    *(.ARM.extab* .gnu.linkonce.armextab.*)
    __extab_end = .;
  } > dtcm_rodata
   __extab_words = (__extab_end - __extab_start) >> 2;

  __exidx_load = __extab_load + (__extab_words << 2);
  /* .ARM.exidx is 4-bytes aligned, so __exidx_start needs to be
     aligned too. Note that assigning the location counter also makes
     ld attach the following symbols to the next section (instead of the
     previous section which is the default), so will properly
     consider the location counter of .ARM.exidx for __exidx_start and
      __exidx_end and not the previous section's one. */
  . = ALIGN(0x4);
  PROVIDE_HIDDEN (__exidx_start = .);
  .ARM.exidx : AT (__exidx_load)
  {
     *(.ARM.exidx* .gnu.linkonce.armexidx.*)
  } > dtcm_rodata
  PROVIDE_HIDDEN (__exidx_end = .);
   __exidx_words = (__exidx_end - __exidx_start) >> 2;

  __rodata_load = __exidx_load + __exidx_words << 2;
  .rodata : AT (__rodata_load)
  {
    __rodata_start = .;
    *(.lit)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rodata_end = .;
  } > dtcm_rodata
  __rodata_words = (__rodata_end - __rodata_start) >> 2;

  __data_load = __rodata_load + (__rodata_words << 2);
  .data : AT (__data_load)
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)

    /* Ensure that the end of the data section is always word aligned.
       Initial values are stored in 4-bytes blocks so we must guarantee
       that these blocks do not fall out the section (otherwise they are
       truncated and the initial data for the last block are lost). */

    . = ALIGN(0x4);
    __data_end = .;
  } > dtcm_data
  __data_words = (__data_end - __data_start) >> 2;

  .bss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

   . = ALIGN(0x8);    /* Align the stack to 64 bits */
   __bss_end = .;

   __interrupt_stack_start = .;
   *(.interrupt_stacks)
   . = ALIGN(0x8);
   __interrupt_stack_end = .;

   __stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x8);
   __stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(dtcm_bss) + LENGTH(dtcm_bss);
  } > dtcm_bss

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
