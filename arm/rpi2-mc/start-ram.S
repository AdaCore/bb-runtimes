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
__start_slave_cpu:
	// Stay in HYP mode
	cpsid	if
	ldr	sp,system_stack_base

	@ Read processor id MPIDR[0:1]
	mrc	p15,#0,r9,c0,c0,#5
	ubfx	r9,r9,#0,#2

	/* Set stacks.  */
	adr	r0,system_stack_base
	ldr	sp,[r0,r9, lsl #2]

//	@ Read corresponding stack address
//	adr	r0,interrupt_stack_base
//	ldr	r0,[r0,r9, lsl #2]

	/* Enable VFP: clear HCPTR.  */
	mov	r0, #0
	mcr	p15,#4,r0,c1,c1,#2	@ HCPTR
	mov	r0,#0x40000000		@ FPEN
	fmxr	fpexc, r0

	/* Set HVBAR (Hyp Vector Base Address Register).  */
	adr	r0, __traps
	mcr	p15, #4, r0, c12, c0, #0

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
	mov	r3,r3,lsr #23		@ Extract LoC (level of coherence) *2
	beq	4f			@ No cache
	mov	r10, #0
1:
	add	r2,r10,r10,lsr #1	@ Work out 3xcachelevel
	mov	r1,r0,lsr r2 @ bottom 3 bits are the Cache type for this level
	and	r1, r1, #7		@ get those 3 bits alone
	cmp	r1, #2
	blt	5f			@ no dcache at this level
	mcr	p15,#2,r10,c0,c0,#0 	@ write CCSSELR
	isb				@ ISB between sel and read
	mrc	p15,#1,r1,c0,c0,#0	@ reads CCSIDR
	and 	r2,r1,#0x7		@ extract line length
	add	r2,r2,#4		@ log2 16
	movw	r4, #0x3ff		@ r4 is the max number on the way size
	ands	r4,r4,r1, lsr #3	@ r5 is the bit position of the way sz
	clz	r5,r4			@ Count leading zero
	movw	r7, #0x00007fff		@ r7 is the max nbr of the index size
	ands	r7,r7,r1,lsr #13
2:	mov	r6,r4	 		@ r6 working copy of the max way size
3:	orr	r11,r10,r6,lsl r5 	@ factor in the way num and cache num
	orr	r11,r11,r7,lsl r2 	@ factor in the index number
	mcr	p15,#0,r11,c7,c6,#2	@ invalid by set/way
	subs	r6,r6, #1		@ decrement the way number
	bge	3b
	subs	r7,r7,#1		@ decrement the index
	bge	2b
5:	add	r10,r10,#2		@ increment the cache num
	cmp	r3, r10
	bgt	1b
4:

#if 0
	/* Enable SMP mode.  */
	mrc	p15,#0,r1,c1,c0,#1	@ ACTLR
	orr	r1,#0x47		@ SMP, L1 & L2 prefetch, FW
	mcr	p15,#0,r1,c1,c0,#1
#endif

	/* Initialize MMU.  */
	adrl	r0, __mmu_l1_000000000
	mov	r1, #0
	mcrr	p15,#4,r0,r1,c2		@ HTTBR
	ldr	r2,mmu_tcr
	mcr	p15,#4,r2,c2,c0,#2	@ HTCR
	mov	r0,#0x00ee
	mcr	p15,#4,r0,c10,c2,0	@ HMAR0
	mcr	p15,#4,r0,c10,c2,1	@ HMAR1
#if 0
	orr	r0,r0,#0x5b		@ RGN=0b11, S=1, IRGN=0b11
	mcr	p15,#0,r0,c2,c0,#0	@ set TTBR0
	mov	r1,#0
	mcr	p15,#0,r1,c2,c0,#2	@ set TTBC0
	mcr	p15,#0,r1,c13,c0,#1	@ set CONTEXTIDR
	mov	r1,#1
	mcr	p15,#0,r1,c3,c0,#0	@ set DACR
#endif

	/*  Enable MMU, I-Cache and D-Cache.  */
	movw	r2,#0x1005		@ D-cache, I-cache, MMU
	mcr	p15,4,r2,c1,c0,0	@ write hsctlr

	/* Synchronization barriers.  */
	dsb				@ Allows MMU to start
	isb				@ Flushes pre-fetch buffer

	cmp	r9,#0
	bne	slave_cpu

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

slave_cpu:
	/* Per-CPU device initialization.  */
        cpsid	if
	bl	__gnat_initialize_cpu_devices

	/* Call the main routine (with CPU_Id as argument).  */
	add	r0,r9,#1
	bl	__gnat_initialize_slave
1:	b	1b
	.size	__start_ram, . - __start_ram

interrupt_stack_base:
	.word	__cpu0_interrupt_stack_start
	.word	__cpu1_interrupt_stack_start
	.word	__cpu2_interrupt_stack_start
	.word	__cpu3_interrupt_stack_start

system_stack_base:
	.word	__cpu0_stack_end
	.word	__cpu1_stack_end
	.word	__cpu2_stack_end
	.word	__cpu3_stack_end

	/* SH0=11 (inner shareable), ORGN0=01 (WB,WA), IRGN0=01 (WB,WA).  */
mmu_tcr:
	.word	__mmu_tcr + (3 << 12) + (1 << 10) + (1 << 8)

	/* These routines are not available in zfp.  */
	.weak	__gnat_initialize_slave
	.weak	__gnat_initialize_cpu_devices

/* Generate memmap with:
 memmap.py --arch=arm-long --el2 /memmap.xml > memmap.S
*/

#include "memmap.S"
