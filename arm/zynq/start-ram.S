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

/* This file is specific to the Cortex a9 like the Zynq cores.  */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
	.global __start_ram

	.section .traps
	.p2align 4

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

	.weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
        .weak	__gnat_dabt_trap	/* data abort trap */
        .weak	__gnat_irq_trap		/* Interrupt */
        .weak	__gnat_fiq_trap		/* Fast interrupt */

__traps:
/* 0 */			b	__start_ram
__gnat_undef_trap:	b	__gnat_undef_trap
__gnat_svc_trap:	b	__gnat_svc_trap
__gnat_pabt_trap:	b	__gnat_pabt_trap
__gnat_dabt_trap:	b	__gnat_dabt_trap
			nop	/* 0x14: not used.  */
__gnat_irq_trap:	b	__gnat_irq_trap
__gnat_fiq_trap:	b	__gnat_fiq_trap


	.type	__start_ram, %function
__start_ram:
	/* Enable Floating-Point Coprocessor.  */
	bl	L_coreEnableVfp

	/* Initialize Registers. */
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
	/* Set VBAR (Vector Base Address Register).  */
	adr	r0, __traps
	mcr	p15, #0, r0, c12, c0, #0

	/* Call the main routine */
	bl	main
	bl	_exit
	.size	__start_ram, . - __start_ram

L_coreEnableVfp:
	mrc	p15, #0x00, r0, c1, c0, #0x02
	orr	r0, r0, #0xF00000
	mcr	p15, #0x00, r0, c1, c0, #0x02
	mov	r0, #0x40000000
	fmxr	fpexc, r0
	bx	lr

L_coreInitRegisters:
	/* Start initializing CPU registers, including backed registers for
	   all modes. This is necessary on Cortex R4F to avoid miscompares
	   between the cores running in lockstep, and is a good idea on any
	   system. */

        /* Save lr because changing mode will change lr value */
        mov	r0, lr

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

        /* Return on saved lr */
	bx	r0
	.size	L_coreInitRegisters, . - L_coreInitRegisters

svcSp:		.word __svc_stack_end
abortSp:	.word __abort_stack_end
undefSp:	.word __undef_stack_end
fiqSp:		.word __fiq_stack_end
irqSp:		.word __irq_stack_end
userSp:		.word __stack_end
