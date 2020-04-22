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
 *                   Copyright (C) 2003-2013 AdaCore                        *
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

/* This is a ARM specific version of this file */

/* This script replaces ld's default linker script, providing the
   appropriate memory map and output format. */

_DEFAULT_STACK_SIZE = (20 * 1024);

_SVC_STACK_SIZE   =  256;
_UNDEF_STACK_SIZE =  256;
_ABORT_STACK_SIZE =  256;
_IRQ_STACK_SIZE   = 2048;
_FIQ_STACK_SIZE   =  256;

ENTRY(__start_ram);

MEMORY
{
  SRAM (rwx) : ORIGIN = 1M, LENGTH = 1022M
  UCSRAM (rw) : ORIGIN = 1023M, LENGTH = 1M
}

REGION_ALIAS("REGION_CODE", SRAM)
REGION_ALIAS("REGION_DATA", SRAM)

/*
 * The memory map looks like this:
 * +--------------------------------+ <- low memory
 * | .traps                         |
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
    KEEP (*(.traps))
    *(.text .text.* .gnu.linkonce.t*)
  } > REGION_CODE

  .rodata :
  {
    *(.rodata .rodata.* .gnu.linkonce.r*)
  } > REGION_CODE

  .ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) } > REGION_CODE
  /* .ARM.exidx is 4-bytes aligned, so __exidx_start needs to be
     aligned too. Note that assigning the location counter also makes
     ld attach the following symbols to the next section (instead of the
     previous section which is the default), so will properly
     consider the location counter of .ARM.exidx for __exidx_start and
      __exidx_end and not the previous section's one. */
  . = ALIGN(0x4);
  PROVIDE_HIDDEN (__exidx_start = .);
  .ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) } > REGION_CODE
  PROVIDE_HIDDEN (__exidx_end = .);

  .data :
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)
    . = ALIGN(0x4);
    __data_end = .;
  } > REGION_DATA
  __data_words = (__data_end - __data_start) >> 2;

  .bss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

   /* Align the stack to 64 bits */

   . = ALIGN(0x8);
   __bss_end = .;

   __interrupt_stack_start = .;

   /* CPU 0 */
   __cpu0_abort_stack_start = .;
   . = ALIGN(. + _ABORT_STACK_SIZE, 0x8);
   __cpu0_abort_stack_end = .;

   __cpu0_undef_stack_start = .;
   . = ALIGN(. + _UNDEF_STACK_SIZE, 0x8);
   __cpu0_undef_stack_end = .;

   __cpu0_svc_stack_start = .;
   . = ALIGN(. + _SVC_STACK_SIZE, 0x8);
   __cpu0_svc_stack_end = .;

   __cpu0_fiq_stack_start = .;
   . = ALIGN(. + _FIQ_STACK_SIZE, 0x8);
   __cpu0_fiq_stack_end = .;

   /* Use the largest of .interrupt_stacks or the default trap stack sizes.
      This allows use with either ZFP or Ravenscar run times. */

   __cpu0_irq_stack_start = .;
   *(.interrupt_stacks)
   . = MAX(., __cpu0_irq_stack_start + _IRQ_STACK_SIZE);
   . = ALIGN(0x8);
   __cpu0_irq_stack_end = .;

   /* CPU 1 */
   __cpu1_abort_stack_start = .;
   . = ALIGN(. + _ABORT_STACK_SIZE, 0x8);
   __cpu1_abort_stack_end = .;

   __cpu1_undef_stack_start = .;
   . = ALIGN(. + _UNDEF_STACK_SIZE, 0x8);
   __cpu1_undef_stack_end = .;

   __cpu1_svc_stack_start = .;
   . = ALIGN(. + _SVC_STACK_SIZE, 0x8);
   __cpu1_svc_stack_end = .;

   __cpu1_fiq_stack_start = .;
   . = ALIGN(. + _FIQ_STACK_SIZE, 0x8);
   __cpu1_fiq_stack_end = .;

   /* Use the largest of .interrupt_stacks or the default trap stack sizes.
      This allows use with either ZFP or Ravenscar run times. */

   __cpu1_irq_stack_start = .;
   . = ALIGN (. + _IRQ_STACK_SIZE, 0x8);
   __cpu1_irq_stack_end = .;

   __interrupt_stack_end = .;

   /* Separate stack per CPU */
   __stack_start = .;

   __cpu1_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x8);
   __cpu1_stack_end = .;

   __cpu0_stack_start = .;
   . += DEFINED (__stack_size) ? __stack_size : _DEFAULT_STACK_SIZE;
   . = ALIGN(0x8);
   __cpu0_stack_end = .;

   __stack_end = .;

   _end = .;
   __heap_start = .;
   __heap_end = ORIGIN(REGION_DATA) + LENGTH(REGION_DATA);
  } > REGION_DATA

   __bss_words = (__bss_end - __bss_start) >> 2;

   .ucache (NOLOAD): {
     . = ALIGN(0x08);
     *(.ucache)
   } > UCSRAM

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

