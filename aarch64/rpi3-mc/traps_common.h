/****************************************************** -*- asm -*- *********
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                       Copyright (C) 2017, AdaCore                        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

	.macro  ehandler label, id, handler=__trap_dump
	.p2align 7
	.type	\label,@function
\label:
	// Save all registers (32 * 8 = 256)
	stp	x0, x1, [sp, #-256]!
	stp	x2, x3, [sp, #16]
	stp	x4, x5, [sp, #32]
	stp	x6, x7, [sp, #48]
	stp	x8, x9, [sp, #64]
	stp	x10, x11, [sp, #80]
	stp	x12, x13, [sp, #96]
	stp	x14, x15, [sp, #112]
	stp	x16, x17, [sp, #128]
	stp	x18, x19, [sp, #144]
	stp	x20, x21, [sp, #160]
	stp	x22, x23, [sp, #176]
	stp	x24, x25, [sp, #192]
	stp	x26, x27, [sp, #208]
	stp	x28, x29, [sp, #224]
	str	x30, [sp, #240]
	//  Call handler with context and number
	mov	x0, sp
	mov	x1, #\id
	bl	\handler
	//  Restore
	ldp	x2, x3, [sp, #16]
	ldp	x4, x5, [sp, #32]
	ldp	x6, x7, [sp, #48]
	ldp	x8, x9, [sp, #64]
	ldp	x10, x11, [sp, #80]
	ldp	x12, x13, [sp, #96]
	ldp	x14, x15, [sp, #112]
	ldp	x16, x17, [sp, #128]
	ldp	x18, x19, [sp, #144]
	ldr	x30, [sp, #240]
	//  No need to restore callee saved registers
	ldp	x0, x1, [sp], #256
	eret
	.size	\label, . - \label
	.endm

	//  Interrupt handle frame size
	//  x0-x18 		(19*8 = 152)
	//  x29-x30 		(16   -> 168)
	//  spsr, elr, cpacr	(24   -> 192)
	//  sp, PAD             (16   -> 208)
#define IFRAME_SIZE 208

	//  interrupt handler
	.macro  ihandler_start label, el
	.p2align 7
	.type	\label, @function
\label:
	//  1) save caller-saved regs
	stp	x0, x1, [sp, #-IFRAME_SIZE]!
	stp	x2, x3, [sp, #16]
	stp	x4, x5, [sp, #32]
	stp	x6, x7, [sp, #48]
	stp	x8, x9, [sp, #64]
	stp	x10, x11, [sp, #80]
	stp	x12, x13, [sp, #96]
	stp	x14, x15, [sp, #112]
	stp	x16, x17, [sp, #128]
	stp	x18, x29, [sp, #144]
	mrs	x4, spsr_\el
	mrs	x5, elr_\el
	mrs	x6, cptr_\el
	stp	x30, x4, [sp, #160]
	stp	x5, x6, [sp, #176]

	// 2) load stack pointer
	adrp	x0,interrupt_stack_\el\()_base
	add	x0, x0, #:lo12:interrupt_stack_\el\()_base
	mrs	x1, mpidr_el1
	and	x1, x1, #3
	ldr	x2, [x0, x1, lsl #3]	//  Load new stack pointer

	// 3) Create a frame, switch to irq stack
	mov	x1, sp
	stp	x29, x30, [x2, #-32]!
	str	x1, [x2, #16]
	mov	sp, x2

	b	\label\()_cont
	.size	\label, . - \label
	.endm

	.macro  ihandler_cont label, handler, el
	.type 	\label\()_cont, @function
\label\()_cont:
	//  4) disable fpu
	mrs	x4, cptr_\el
	orr	x4, x4, #(1 << 10)
	msr	cptr_\el, x4

	//  5) call handler
	bl	\handler

	//  6) Switch back to EL2 stack
	ldr	x1, [sp, #16]
	mov	sp, x1

	//  7) switch context if needed
	bl	__gnat_context_switch_needed
	cbz	x0, 1f

	bl	__gnat_pre_context_switch
	bl	__gnat_context_switch

	//  8) restore registers
1:
	ldp	x5, x6, [sp, #176]
	ldp	x30, x4, [sp, #160]
	msr	spsr_\el, x4
	msr	elr_\el, x5
	msr	cptr_\el, x6
	ldp	x18, x29, [sp, #144]
	ldp	x16, x17, [sp, #128]
	ldp	x14, x15, [sp, #112]
	ldp	x12, x13, [sp, #96]
	ldp	x10, x11, [sp, #80]
	ldp	x8, x9, [sp, #64]
	ldp	x6, x7, [sp, #48]
	ldp	x4, x5, [sp, #32]
	ldp	x2, x3, [sp, #16]
	ldp	x0, x1, [sp], #IFRAME_SIZE

	//  9) return
	eret
	.size	\label\()_cont, . - \label\()_cont
	.endm
