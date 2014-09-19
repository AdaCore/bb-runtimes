	.text

	.globl __gnat_dabt_trap
	.type __gnat_dabt_trap,%function
__gnat_dabt_trap:
	stmfd	sp!, {r0 - r12, lr}
	mov	r0,sp
	bl	data_abort_handler
	ldmfd	sp!, {r0 - r12, lr}
	subs	pc, lr, #8

	.size __gnat_dabt_trap, . - __gnat_dabt_trap

	.globl force_dabt_handler
	.type force_dabt_handler,%function
force_dabt_handler:
	.size force_dabt_handler, 0

