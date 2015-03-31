	.cpu cortex-r4f
@	.eabi_attribute 27, 3
@	.eabi_attribute 28, 1
@	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 2
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4

#include <xm.h>
#include <xm_inc/arch/asm_offsets.h>

	.text
	.align	2
	.global	__gnat_context_switch
	.type	__gnat_context_switch, %function
__gnat_context_switch:
	@ ip = running thread context
        ldr     r0, Lrun
        ldr     ip, [r0]

        @ Save context
        stmia   ip!, {r4-r11,sp,lr}
	vstmia	ip!, {s16-s31}
	vmrs	r1,fpscr
	str	r1,[ip]

        @ Running thread = first_thread
        ldr     ip, Lfirst
        ldr     ip, [ip]
        str     ip, [r0]

        @ Restore context
        ldmia   ip!, {r4-r11,sp,lr}
	vldmia	ip!, {s16-s31}
	ldr	r1,[ip]
	vmsr	fpscr,r1
        bx	lr

        .align 2
Lrun:
        .word   __gnat_running_thread_table
Lfirst:
        .word   first_thread_table

	.size	__gnat_context_switch, .-__gnat_context_switch

	.globl __gnat_start_thread
__gnat_start_thread:
	mov	r0, r4
	blx	r5
0:	b	0b

	.size __gnat_start_thread, . - __gnat_start_thread

	.global __gnat_get_irq_nr
	.type __gnat_get_irq_nr, #function
__gnat_get_irq_nr:
	movw	r0, #:lower16:libXmParams
	movt	r0, #:upper16:libXmParams
	ldr	r0, [r0]
	ldr	r0, [r0, #_IRQINDEX_OFFSET]
	sub	r0, r0, #1  @ ??? Why
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

