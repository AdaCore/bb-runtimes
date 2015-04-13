/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                      Copyright (C) 2011-2012 AdaCore                     *
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

/* This file is specific to ARM-Xtratum.  */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
#include <xm.h>
#include <xm_inc/arch/asm_offsets.h>

#define PSR_I (1<<7)

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

	.weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
	.weak	__gnat_dabt_trap	/* data abort trap */
	.weak	__irq			/* prefetch abort trap	*/
	.weak	__gnat_fiq_trap		/* data abort trap */

	.section .traps, "ax"
	.align 3

	.global start

start:
	b	reset
	b	__gnat_undef_trap     /* Undefined instruction trap */
	b	__gnat_svc_trap       /* Supervisor call trap */
	b	__gnat_pabt_trap      /* prefetch abort trap  */
	b	__gnat_dabt_trap      /* data abort trap */
	nop
	b	__irq
	b	__gnat_fiq_trap

        .text

reset:
	/* R0 contains the partition control table. It must be preserved
	   until the call to init_libxm.  */

	/* Copy .data */
	movw	r3,#:lower16:__data_start
	movt	r3,#:upper16:__data_start
	movw	r1,#:lower16:__data_words
	movt	r1,#:upper16:__data_words
	movw	r2,#:lower16:__data_load
	movt	r2,#:upper16:__data_load
	cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2],#4
	str	r4,[r3],#4
	subs	r1,r1,#1
	bne	0b
1:

	/* Clear .bss */
	movw	r3,#:lower16:__bss_start
	movt	r3,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words
	movt	r1,#:upper16:__bss_words
	mov	r2,#0
	cmp	r1,#0
	beq	1f
0:	str	r2,[r3],#4
	subs	r1,r1,#1
	bne	0b

1:
        /* After reset, the CPU is in the Supervisor mode (M = 10011) */
        movw	sp, #:lower16:__svc_stack_end
        movt	sp, #:upper16:__svc_stack_end

        bl	init_libxm

        /* FIQ mode */
        mov	r0, #0x11
        __XM_HC(#arm_set_cpsr_nr)
        movw	sp, #:lower16:__fiq_stack_end
        movt	sp, #:upper16:__fiq_stack_end

        /* IRQ mode */
        mov	r0, #0x12
        __XM_HC(#arm_set_cpsr_nr)
        movw	sp, #:lower16:__irq_stack_end
        movt	sp, #:upper16:__irq_stack_end

	/* Abort mode */
        mov	r0, #0x17
        __XM_HC(#arm_set_cpsr_nr)
        movw	sp, #:lower16:__abort_stack_end
        movt	sp, #:upper16:__abort_stack_end

        /* Undefined instruction mode */
        mov	r0, #0x1b
        __XM_HC(#arm_set_cpsr_nr)
        movw	sp, #:lower16:__undef_stack_end
        movt	sp, #:upper16:__undef_stack_end

        /* System mode */
        mov	r0, #(0x1f | PSR_I)
        __XM_HC(#arm_set_cpsr_nr)
        movw	sp, #:lower16:__stack_end
        movt	sp, #:upper16:__stack_end

        bl	main

        movw	r0, #:lower16:libXmParams
        movt	r0, #:upper16:libXmParams
        ldr	r0, [r0]
        ldr	r0, [r0, #_IDPART_OFFSET]
        __XM_HC(halt_partition_nr)
