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
	#include	"memmap.S"

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

	.p2align 5

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
	/* Set stacks.  */

	/* Supervisor mode */
	cps	#19 /* 10011 */
	ldr	sp, svcSp0

	/* Abort mode */
	cps	#23  /* 10111 */
	ldr	sp, abortSp0

	/* Undefined instruction mode */
	cps	#27  /* 11011 */
	ldr	sp, undefSp0

	/* FIQ mode */
	cps	#17  /* 10001 */
	ldr	sp, fiqSp0

	/* IRQ mode */
	cps	#18  /* 10010 */
	ldr	sp, irqSp0

	/* System mode */
	cps	#31  /* 11111 */
	ldr	sp, userSp0	/* SYS mode shared stack with User mode */

	/* Write 0x020202 to the register at 0xf8000a1c.  */
	movw	r0,#:lower16:0x20202
	movt	r0,#:upper16:0x20202
	movw	r1,#:lower16:0xf8000a1c
	movt	r1,#:upper16:0xf8000a1c
	str	r0,[r1]

	/* Enable SCU.  */
	mov	r0, #0x7
	movw	r1,#:lower16:0xf8f00000
	movt	r1,#:upper16:0xf8f00000
	str	r0,[r1]

	/* Enable VFP, L1-cache, MMU.  */
	bl	L_coreInit

	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words
	movt	r1,#:upper16:__bss_words
	mov	r2,#0
	cmp	r1,#0
	beq	1f
0:	/* FIXME: write multiple words?  */
	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:

	/* Call the main routine */
	bl	main
	bl	_exit
	.size	__start_ram, . - __start_ram

L_coreInit:
	/* Enable VFP */
	mrc	p15,#0,r0,c1,c0,#2	@ CPACR
	orr	r0,r0,#0xF00000		@ Enable CP10, CP11 user & system
	mcr	p15,#0,r0,c1,c0,#2
	mov	r0,#0x40000000
	fmxr	fpexc, r0

	/* Set VBAR (Vector Base Address Register).  */
	adr	r0, __traps
	mcr	p15, #0, r0, c12, c0, #0

	/* Invalidate TLBs  */
	mov	r2,#0
	mcr	p15,0,r2,c8,c7,0

	/* Invalidate I-Cache.  */
	mcr	p15,0,r2,c7,c5,0

	/* Invalidate Branch Predictor Array.  */
	mcr	p15,0,r2,c7,c5,6

	/* Invalidate D-Cache.  */
	mrc	p15,#1,r0,c0,c0,#1	@ Read CLIDR
	ands	r3,r0,#0x7000000
	mov	r3,r3,lsr #23		@ Cache level value
	beq	4f			@ No cache
	mov	r10, #0
1:
	add	r2,r10,r10,lsr #1	@ Work out 3xcachelevel
	mov	r1,r0,lsr r2 @ bottom 3 bits are the Cache type for this level
	and	r1, r1, #7		@ get those 3 bits alone
	cmp	r1, #2
	blt	5f			@ no dcache at this level
	mcr	p15,#2,r10,c0,c0,#0 	@ write cache size selection reg
	isb				@ ISB between sel and read
	mrc	p15,#1,r1,c0,c0,#0	@ reads current cache size ID reg
	and 	r2,r1,#0x7		@ extract line length
	add	r2,r2,#4		@ log2 16
	ldr	r4, =0x3ff		@ r4 is the max number on the way size
	ands	r4,r4,r1, lsr #3	@ r5 is the bit position of the way sz
	clz	r5,r4
	ldr	r7, =0x00007fff		@ r7 is the max nbr of the index size
	ands	r7,r7,r1,lsr #13
2:	mov	r9,r4	 		@ r9 working copy of the max way size
3:	orr	r11,r10,r9,lsl r5 	@ factor in the way num and cache num
	orr	r11,r11,r7,lsl r2 	@ factor in the index number
	mcr	p15,#0,r11,c7,c6,#2	@ invalid by set/way
	subs	r9,r9, #1		@ decrement the way number
	bge	3b
	subs	r7,r7,#1		@ decrement the index
	bge	2b
5:	add	r10,r10,#2		@ increment the cache num
	cmp	r3, r10
	bgt	1b
4:

	/* Enable SMP mode.  */
	mrc	p15,#0,r1,c1,c0,#1	@ ACTLR
	orr	r1,#0x47		@ SMP, L1 & L2 prefetch, FW
	mcr	p15,#0,r1,c1,c0,#1

	/* Initialize MMU.  */
	adrl	r0, __mmu_l0
	orr	r0,r0,#0x5b		@ RGN=0b11, S=1, IRGN=0b11
	mcr	p15,#0,r0,c2,c0,#0	@ set TTBR0
	mov	r1,#0
	mcr	p15,#0,r1,c2,c0,#2	@ set TTBC0
	mcr	p15,#0,r1,c13,c0,#1	@ set CONTEXTIDR
	mov	r1,#1
	mcr	p15,#0,r1,c3,c0,#0	@ set DACR

	/*  Enable MMU, I-Cache and D-Cache.  */
	mrc	p15,#0,r1,c1,c0,#0	@ read sctlr
	movw	r2,#0x1005		@ D-cache, I-cache, MMU
	orr	r1,r1,r2
	bic	r1,r1,#0x30000000	@ clear TRE, AFE
	mcr	p15,0,r1,c1,c0,0	@ write sctlr

	/* Synchronization barriers.  */
	dsb				@ Allows MMU to start
	isb				@ Flushes pre-fetch buffer

#if 0
	/* Write 0x020202 to the register at 0xf8000a1c.  */
	mov	r0,#0x20202
	movw	r1,#:lower16:0xf8000a1c
	movt	r1,#:upper16:0xf8000a1c
	str	r0,[r1]

	/* Invalidate L2.  */
	mov	r0,#0xffff
	movw	r1,#:lower16:0xf8f0277c
	movt	r1,#:upper16:0xf8f0277c
	str	r0,[r1]
#endif

	bx	lr

svcSp0:		.word __cpu0_svc_stack_end
abortSp0:	.word __cpu0_abort_stack_end
undefSp0:	.word __cpu0_undef_stack_end
fiqSp0:		.word __cpu0_fiq_stack_end
irqSp0:		.word __cpu0_irq_stack_end
userSp0:	.word __cpu0_stack_end

	.type	__start_cpu1 %function
__start_cpu1:
	/* Set stacks.  */

	/* Supervisor mode */
	cps	#19 /* 10011 */
	ldr	sp, svcSp1

	/* Abort mode */
	cps	#23  /* 10111 */
	ldr	sp, abortSp1

	/* Undefined instruction mode */
	cps	#27  /* 11011 */
	ldr	sp, undefSp1

	/* FIQ mode */
	cps	#17  /* 10001 */
	ldr	sp, fiqSp1

	/* IRQ mode */
	cps	#18  /* 10010 */
	ldr	sp, irqSp1

	/* System mode */
	cps	#31  /* 11111 */
	ldr	sp, userSp1	/* SYS mode shared stack with User mode */

	/* Enable VFP, L1-cache, MMU.  */
	bl	L_coreInit

	/* Per-CPU device initialization.  */
	bl	__gnat_initialize_cpu_devices

	/* Call the main routine.  */
	mov	r0, #2
	bl	__gnat_initialize_slave

	.size	__start_cpu1, . - __start_cpu1

svcSp1:		.word __cpu1_svc_stack_end
abortSp1:	.word __cpu1_abort_stack_end
undefSp1:	.word __cpu1_undef_stack_end
fiqSp1:		.word __cpu1_fiq_stack_end
irqSp1:		.word __cpu1_irq_stack_end
userSp1:	.word __cpu1_stack_end

	/* These routines are not available in zfp.  */
	.weak	__gnat_initialize_slave
	.weak	__gnat_initialize_cpu_devices


	.globl	__kick_cpu1
	.type	__kick_cpu1 %function
__kick_cpu1:
	mov	r1,#0xfffffff0
	adr	r0,__start_cpu1
	str	r0,[r1]
	sev
	bx	lr
	.size	__kick_cpu1, . - __kick_cpu1
