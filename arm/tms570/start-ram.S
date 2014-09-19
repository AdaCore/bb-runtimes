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

/* This file is specific to the Hercules ARM Cortex-R4F Microcontrollers,
   such as the TMS570. */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
	.global start_ram

	.section .traps

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

	.weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
	.weak	__gnat_dabt_trap	/* data abort trap */

/* 0 */	b	start_ram
__gnat_undef_trap:	/* 4 */
	b	__gnat_undef_trap	/* Undefined instruction trap */
__gnat_svc_trap:	/* 8 */
	b	__gnat_svc_trap		/* Supervisor call trap */
__gnat_pabt_trap:	/* c */
	b	__gnat_pabt_trap	/* prefetch abort trap	*/
__gnat_dabt_trap:	/* 10 */
	b	__gnat_dabt_trap	/* data abort trap */
	nop		/* 14 */
	ldr	pc,[pc,#-0x1b0] /* IRQ, jump to IRQVECREG, 16#ffff_fe70# */
	ldr	pc,[pc,#-0x1b0] /* FIQ, jump to FIQVECREG, 16#ffff_fe74# */

	.text
svcSp:		.word __svc_stack_end
abortSp:	.word __abort_stack_end
undefSp:	.word __undef_stack_end
fiqSp:		.word __fiq_stack_end
irqSp:		.word __irq_stack_end
userSp:		.word __stack_end

	.type start_ram,%function
start_ram:
	/* See Application Report SPNA106B - January 2013
	   Initialization of Hercules(TM) ARM(R) Cortex(TM)-R4F
	   Microcontrollers.
	   Refer in particular to the corresponding C file
	   sys_startup_recommended.c */

	/* 2.1 Enable Floating-Point Coprocessor  */
	bl	L_coreEnableVfp

	/* 2.2 Initialize Cortex-R4F registers	*/
	bl	L_coreInitRegisters

	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words /* Must be less than 64K words */
	mov	r2,#0
	cmp	r1,#0
	beq	1f
0:	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:
	/* Call the main routine */
	bl	main

	.size	start_ram, . - start_ram

        /* Fall through in order to halt if main would return ... */

	.global	_exit
_exit:
	/* Write 0x8000 to SYSECR (16#FFFF_FFE0#) */
        mov	r2, #0x8000
	mvn	r3, #0
        str	r2, [r3, #-31]
	b	_exit

	.size _exit, . - _exit

L_coreEnableVfp:
	mrc   p15,     #0x00,	   r0,	     c1, c0, #0x02
	orr   r0,      r0,	   #0xF00000
	mcr   p15,     #0x00,	   r0,	     c1, c0, #0x02
	mov   r0,      #0x40000000
	fmxr  fpexc,   r0
	bx    lr

L_coreInitRegisters:
	/* Start initializing CPU registers, including backed registers for
	   all modes. This is necessary on Cortex R4F to avoid miscompares
	   between the cores running in lockstep, and is a good idea on any
	   system. */

	/* Supervisor mode */
	cps	#19 /* 10011 */
	ldr	sp, svcSp

	/* Abort mode */
	cps	#23  /* 10111 */
	ldr	sp, abortSp

	/* Undefined instruction mode */
	cps	#27  /* 11011 */
	ldr	sp, undefSp

	/* FIQ mode */
	cps	#17  /* 10001 */
	ldr	sp, fiqSp

	/* IRQ mode */
	cps	#18  /* 10010 */
	ldr	sp, irqSp

	/* System mode */
	cps	#31  /* 11111 */
	ldr	sp, userSp	/* SYS mode shared stack with User mode */

	bx	lr
	.size	L_coreInitRegisters, . - L_coreInitRegisters
