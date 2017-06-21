	.text
	
	.type execute_hull, %function
	.globl execute_hull
execute_hull:
	//  Save host context
	stp	x29, x30, [sp, #-176]!
	stp	x19, x20, [sp, #16]
	stp	x21, x22, [sp, #32]
	stp	x23, x24, [sp, #48]
	stp	x25, x26, [sp, #64]
	stp	x27, x28, [sp, #80]

	//  Save host vfpu
	stp	d8, d9, [sp, #96]
	stp	d10, d11, [sp, #112]
	stp	d12, d13, [sp, #128]
	stp	d14, d15, [sp, #144]

	//  Save x0 (context parameter)
	str	x0, [sp, #160]
	
	//  Load vttbr & vtcr
	ldp	x1, x2, [x0, #312]
	msr	vttbr_el2, x2
	msr	vtcr_el2, x1

	//  Load HCR
	ldr	x3, [x0, #328]
	msr	hcr_el2, x3

	//  Mask interrupts (needed as we are setting spsr_el2 and elr_el2).
	//  They will be unmasked in EL1.
	msr	DAIFset, #3

	//  SP and PC
	ldp	x4, x5, [x0, #248]
	msr	sp_el1, x4
	msr	elr_el2, x5

	//  Spsr and vbar
	ldp	x6, x7, [x0, #264]
	msr	spsr_el2, x6
	// msr	vbar_el1, x7 // vbar is not saved
	
	//  Re-route debug: set MDCR_EL2.TDE
	mrs	x8, mdcr_el2
	orr	x8, x8, #(1<<8)
	msr	mdcr_el2, x8

	//  Enable SS: set MDSCR_EL1.SS(1) and MDSCR_EL1.KDE(13)
	mrs	x9, mdscr_el1
	orr	x9, x9, #1
	orr	x9, x9, #(1 << 13)
	msr	mdscr_el1, x9

//	smc	#0x99

	//  Load registers
	ldp	x2, x3, [x0, #16]
	ldp	x4, x5, [x0, #32]
	ldp	x6, x7, [x0, #48]
	ldp	x8, x9, [x0, #64]
	ldp	x10, x11, [x0, #80]
	ldp	x12, x13, [x0, #96]
	ldp	x14, x15, [x0, #112]
	ldp	x16, x17, [x0, #128]
	ldp	x18, x19, [x0, #144]
	ldp	x20, x21, [x0, #160]
	ldp	x22, x23, [x0, #176]
	ldp	x24, x25, [x0, #192]
	ldp	x26, x27, [x0, #208]
	ldp	x28, x29, [x0, #224]
	ldr	x30, [x0, #240]
	ldp	x0, x1, [x0, #0]
	
	eret
exec_cont:
	//  Save x0 and load context
	str	x0, [sp, #168]
	ldr	x0, [sp, #160]

	//  Save regs.
	stp	x2, x3, [x0, #16]
	stp	x4, x5, [x0, #32]
	stp	x6, x7, [x0, #48]
	stp	x8, x9, [x0, #64]
	stp	x10, x11, [x0, #80]
	stp	x12, x13, [x0, #96]
	stp	x14, x15, [x0, #112]
	stp	x16, x17, [x0, #128]
	stp	x18, x19, [x0, #144]
	stp	x20, x21, [x0, #160]
	stp	x22, x23, [x0, #176]
	stp	x24, x25, [x0, #192]
	stp	x26, x27, [x0, #208]
	stp	x28, x29, [x0, #224]
	str	x30, [x0, #240]
	ldr	x2, [sp, #168]
	stp	x2, x1, [x0, #0]
	
	//  Save PC, SP, SPSR
	mrs	x1, sp_el1
	mrs	x2, elr_el2
	mrs	x3, spsr_el2
	stp	x1, x2, [x0, #248]
	str	x3, [x0, #264]

	//  Save ESR, FAR, HPFAR
	mrs	x4, esr_el2
	mrs	x5, far_el2
	mrs	x6, hpfar_el2
	stp	x4, x5, [x0, #288]
	str	x6, [x0, #304]

	//  Enable interrupts
	msr	DAIFclr, #3

	//  Restore host vfpu
	ldp	d8, d9, [sp, #96]
	ldp	d10, d11, [sp, #112]
	ldp	d12, d13, [sp, #128]
	ldp	d14, d15, [sp, #144]

	//  Restore host context
	ldp	x19, x20, [sp, #16]
	ldp	x21, x22, [sp, #32]
	ldp	x23, x24, [sp, #48]
	ldp	x25, x26, [sp, #64]
	ldp	x27, x28, [sp, #80]
	ldp	x29, x30, [sp], #176

	ret
	.size	execute_hull, . - execute_hull

/* Exception table.  */
	
	.macro  ehandler label, id, handler=__trap_dump
	.p2align 7
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
	.endm

	.type	irq_handler,@function
irq_handler:
	//  Save x0 and load context
	str	x0, [sp, #168]
	ldr	x0, [sp, #160]

	//  Save regs (only volatile ones)
	stp	x2, x3, [x0, #16]
	stp	x4, x5, [x0, #32]
	stp	x6, x7, [x0, #48]
	stp	x8, x9, [x0, #64]
	stp	x10, x11, [x0, #80]
	stp	x12, x13, [x0, #96]
	stp	x14, x15, [x0, #112]
	stp	x16, x17, [x0, #128]
	stp	x18, x19, [x0, #144]
	stp	x29, x30, [x0, #232]
	ldr	x2, [sp, #168]
	stp	x2, x1, [x0, #0]
	
	//  Save PC, SP, SPSR
	mrs	x1, sp_el1
	mrs	x2, elr_el2
	mrs	x3, spsr_el2
	stp	x1, x2, [x0, #248]
	str	x3, [x0, #264]

	//  No need to save ESR, FAR, HPFAR (as this is an interrupt)

	// 2) load stack pointer
	adrp	x0,interrupt_stack_el2_base
	add	x0, x0, #:lo12:interrupt_stack_el2_base
	mrs	x1, mpidr_el1
	and	x1, x1, #3
	ldr	x2, [x0, x1, lsl #3]	//  Load new stack pointer

	// 3) Create a frame, switch to irq stack
	mov	x1, sp
	stp	x29, x30, [x2, #-32]!
	str	x1, [x2, #16]
	mov	sp, x2

	//  4) disable fpu
	mrs	x4, cptr_el2
	orr	x4, x4, #(1 << 10)
	msr	cptr_el2, x4

	//  5) call handler
	bl	__gnat_irq_handler

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
	ldr	x0, [sp, #160]

	//  Load HCR
	ldr	x3, [x0, #328]
	msr	hcr_el2, x3

	//  SP and PC
	ldp	x4, x5, [x0, #248]
	msr	sp_el1, x4
	msr	elr_el2, x5

	//  Spsr and vbar
	ldp	x6, x7, [x0, #264]
	msr	spsr_el2, x6
	// msr	vbar_el1, x7  // vbar is not saved

	//  Load registers
	ldp	x2, x3, [x0, #16]
	ldp	x4, x5, [x0, #32]
	ldp	x6, x7, [x0, #48]
	ldp	x8, x9, [x0, #64]
	ldp	x10, x11, [x0, #80]
	ldp	x12, x13, [x0, #96]
	ldp	x14, x15, [x0, #112]
	ldp	x16, x17, [x0, #128]
	ldp	x18, x19, [x0, #144]
	ldp	x29, x30, [x0, #232]
	ldp	x0, x1, [x0, #0]
	
	//  9) return
	eret
	.size	irq_handler, . - irq_handler
	
	.section .vects_el2_low,"ax"
	.globl  __vectors_el2_low
	.p2align 7
__vectors_el2_low:
	//  Lower level, aarch64
	//ehandler __trap_el2_syn_a64, 9 //, __handler_trap_syn_a64
	b	exec_cont
	.p2align 7
	//ehandler __trap_el2_irq_a64, 10
	b	irq_handler
	ehandler __trap_el2_fiq_a64, 11
	ehandler __trap_el2_ser_a64, 12

	ehandler __trap_el2_syn_a32, 13,
	ehandler __trap_el2_irq_a32, 14
	ehandler __trap_el2_fiq_a32, 15
	ehandler __trap_el2_ser_a32, 16
