	.text
	.globl __gnat_context_switch
	.type __gnat_context_switch, %function
__gnat_context_switch:
	//  Inputs: c0: running thread address
	//          c1: first thread address

	//  Get context buffer of the running thread and the next thread
	ldr	c2, [c0]
	ldr	c4, [c1]

	//  Save context
	mov	c3, csp
	stp	c19, c20, [c2, #0]
	stp	c21, c22, [c2, #32]
	stp	c23, c24, [c2, #64]
	stp	c25, c26, [c2, #96]
	stp	c27, c28, [c2, #128]
	stp	c29, c30, [c2, #160]
	str	c3, [c2, #192]

	//  Load new context
	ldp	c19, c20, [c4, #0]
	ldp	c21, c22, [c4, #32]
	ldp	c23, c24, [c4, #64]
	ldp	c25, c26, [c4, #96]
	ldp	c27, c28, [c4, #128]
	ldp	c29, c30, [c4, #160]
	ldr	c3, [c4, #192]
	mov	csp, c3

	//  Set running_thread
	str	c4, [c0]

	ret
	.size __gnat_context_switch, . - __gnat_context_switch

	.globl __gnat_start_thread
	.type  __gnat_start_thread, %function
__gnat_start_thread:
	//  Comes from the context switch
	//  C19: PCC
	//  X20: Arg
	mov	x0, x20
	mov	x30, #0	// Clear LR
	mov	x29, #0	// Clear FP
	br	c19

	.size __gnat_start_thread, . - __gnat_start_thread
