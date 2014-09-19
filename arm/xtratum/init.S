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
	movw	r1,#:lower16:__bss_words /* Must be less than 64K words */
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
        movw	sp, #:lower16:__svc_stack_end
        movt	sp, #:upper16:__svc_stack_end

        bl	main

        movw	r0, #:lower16:libXmParams
        movt	r0, #:upper16:libXmParams
        ldr	r0, [r0]
        ldr	r0, [r0, #_IDPART_OFFSET]
        __XM_HC(halt_partition_nr)

	.size	reset, . - reset

	.global __gnat_get_irq_nr
	.type __gnat_get_irq_nr, #function
__gnat_get_irq_nr:
	movw	r0, #:lower16:libXmParams
	movt	r0, #:upper16:libXmParams
	ldr	r0, [r0]
	ldr	r0, [r0, #_IRQINDEX_OFFSET]
	bx	lr
	.size	__gnat_get_irq_nr, . - __gnat_get_irq_nr


	.type __irq, #function
__irq:
	sub	r14,r14,#4
        push	{r0-r3,ip,lr}
	vpush	{s0-s15}
	vmrs	r1,fpscr
	push	{r0-r1}	/* Keep stack aligned */

	movw	r1, #:lower16:(__gnat_trap_handlers + 5*4)
        movt	r1, #:upper16:(__gnat_trap_handlers + 5*4)
	ldr	r1,[r1]
	mov	r0,#5
	blx	r1

	bl	context_switch_needed
	cmp	r0,#0
	beq	1f

	/* Save SPSR */
	push	{r0-r1,r6-r7}
        mov	r0, sp /* Override pushed r0 */
        __XM_HC(#arm_get_spsr_nr)

	/* Switch to sys mode */
	add	r0, sp,#4 /* Override pushed r1 */
        __XM_HC(#arm_get_cpsr_nr)

	mov	r6, sp
        ldr	r0, [r6,#4]
        orr	r0, r0, #0x1f
        __XM_HC(#arm_set_cpsr_nr)

	/* Save lr */
	mov	r7,lr

	/* call cswitch */
	bl	__gnat_context_switch

	/* Switch to irq mode */
	mov	lr,r7
	ldr	r0,[r6,#4]
        __XM_HC(#arm_set_cpsr_nr)

	/* Restore SPSR */
	pop	{r0-r1,r6-r7}
        __XM_HC(#arm_set_spsr_nr)
1:
	pop	{r0-r1}
	vmsr	fpscr,r1
	vpop	{s0-s15}
	pop	{r0-r3,ip,lr}
	__XM_HC(#arm_iret_nr)
	.size	__irq, . - __irq

	.section ".bss"
_tmp_cpsr:
        .fill 4, 1, 0

        /* Provide dummy personnality routines.  */
	.global __aeabi_unwind_cpp_pr0
	.global __aeabi_unwind_cpp_pr1
	.global __aeabi_unwind_cpp_pr2
	.thumb_func
__aeabi_unwind_cpp_pr0:
__aeabi_unwind_cpp_pr1:
__aeabi_unwind_cpp_pr2:
hang:   b .
