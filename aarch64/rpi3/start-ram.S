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
	mrs	x0,mpidr_el1
	ubfx	x0,x0,#0,#28
	cmp	x0,#0
	beq	__start_master
0:	wfe
	b	0b

__start_master:
	ldr	x0,system_stack_base
	mov	sp,x0

	/* Clear .bss */
	ldr	x0,=__bss_start
	ldr	x1,=__bss_dwords
	cmp	x1,#0
	beq	1f
0:	/* FIXME: write multiple words?  */
	str	xzr,[x0],#8
	sub	x1,x1,#1
	bne	0b
1:

	/* Call the main routine */
	bl	main
	bl	_exit
0:	b	0b
	.size	__start_ram1, . - __start_ram1

system_stack_base:
	.word	__cpu0_stack_end
