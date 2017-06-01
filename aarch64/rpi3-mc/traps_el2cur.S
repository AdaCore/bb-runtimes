#include "traps_common.h"
	
	.section .vects_el2_cur,"ax"
	.globl  __vectors_el2_cur
__vectors_el2_cur:
	//  EL0 stack
	ehandler __trap_el2_syn_el0, 1
	ehandler __trap_el2_irq_el0, 2
	ehandler __trap_el2_fiq_el0, 3
	ehandler __trap_el2_ser_el0, 4

	//  Current exception level
	.p2align 7
__trap_el2_syn_elx:
	// Save all caller-saved registers (32 * 8 = 256)
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
	str	x30, [sp, #240]
	adr	x30,__vector_restore
	mrs	x0, esr_el2
	ubfx	x1, x0, #26, #6	 // Extract EC
	cmp	x1, #0x07
	bne	1f
	b	__gnat_fpen_trap
1:
	stp	x20, x21, [sp, #160]
	stp	x22, x23, [sp, #176]
	stp	x24, x25, [sp, #192]
	stp	x26, x27, [sp, #208]
	stp	x28, x29, [sp, #224]
	//  Call handler with context and number
	mov	x0, sp
	mov	x1, #5
	b	__trap_dump
	b	__vectors_el2_low	//  To reference that symbol

	ihandler_start __trap_el2_irq_elx, el2
	ehandler __trap_el2_fiq_elx, 7
	ehandler __trap_el2_ser_elx, 8

	.text

	ihandler_cont __trap_el2_irq_elx, __gnat_irq_handler, el2

__vector_restore:
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

	.weak	__gnat_fpen_trap
	.weak	__gnat_context_switch
	.weak	__gnat_pre_context_switch
	.weak	__gnat_context_switch_needed
	.weak	__gnat_irq_handler
