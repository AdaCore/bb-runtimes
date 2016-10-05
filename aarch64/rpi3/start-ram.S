/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                          Copyright (C) 2013 AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 ****************************************************************************/

/* This file is specific to the Cortex a7 for Raspberry PI 2 board.  */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
	.global __start_ram
	.global	__start_slave_cpu

	.section .traps,"ax"

__start_ram:
	b	__start_ram1

	.text

	.type	__start_ram1, %function
__start_ram1:
	# Boot conditions:
	# CurrentEL: 3
#if 0
	# Enable SMPEN
	mrs	x0, S3_1_C15_C2_1
	orr	x0, x0, #0x40
	msr	S3_1_C15_C2_1, x0
#endif

	# Move slave processors to an infinite loop
	mrs	x0,mpidr_el1
	ubfx	x0,x0,#0,#28
	cmp	x0,#0
	beq	__start_master
0:	wfe
	b	0b

__start_master:
	# Load stack pointer
	adrp	x2,system_stack_base
	ldr	x0,[x2, #:lo12:system_stack_base]
	mov	sp,x0

	# Set NS, IRQ, FIQ, EA of SCR
	mrs	x0, scr_el3
	orr	x0, x0, #0xf
	msr	scr_el3, x0

	# Enable FPU (don't trap accesses)
	msr	cptr_el3, xzr

	/* Clear .bss */
	adrp	x2,bss_segment
	ldr	x0,[x2, #:lo12:bss_segment]
	ldr	x1,[x2, #:lo12:bss_segment + 8]
0:	cbz	x1,1f
	/* FIXME: write multiple words?  */
	str	xzr,[x0],#8
	sub	x1,x1,#1
	cbnz	x1,0b
1:

	/* Call the main routine */
	bl	main
	bl	_exit
0:	b	0b
	.size	__start_ram1, . - __start_ram1

	.p2align	3

bss_segment:
	.dword	__bss_start
	.dword	__bss_dwords

system_stack_base:
	.dword	__cpu0_stack_end
