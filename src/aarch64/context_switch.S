	.text
	.globl __gnat_context_switch
	.type __gnat_context_switch, %function
__gnat_context_switch:
	//  Inputs: x0: running thread address
	//          x1: first thread address

	//  Get context buffer of the running thread and the next thread
	ldr	x2, [x0]
	ldr	x4, [x1]

	//  Save context
	mov	x3, sp
	stp	x19, x20, [x2, #0]
	stp	x21, x22, [x2, #16]
	stp	x23, x24, [x2, #32]
	stp	x25, x26, [x2, #48]
	stp	x27, x28, [x2, #64]
	stp	x29, x30, [x2, #80]
	str	x3, [x2, #96]

	//  Load new context
	ldp	x19, x20, [x4, #0]
	ldp	x21, x22, [x4, #16]
	ldp	x23, x24, [x4, #32]
	ldp	x25, x26, [x4, #48]
	ldp	x27, x28, [x4, #64]
	ldp	x29, x30, [x4, #80]
	ldr	x3, [x4, #96]
	mov	sp, x3

	//  Set running_thread
	str	x4, [x0]

	ret
	.size __gnat_context_switch, . - __gnat_context_switch

	.globl __gnat_start_thread
	.type  __gnat_start_thread, %function
__gnat_start_thread:
	//  Comes from the context switch
	//  X19: PC
	//  X20: Arg
	mov	x0, x20
	mov	x30, #0	// Clear LR
	mov	x29, #0	// Clear FP
	br	x19

	.size __gnat_start_thread, . - __gnat_start_thread
