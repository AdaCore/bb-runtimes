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

	//  Save x0 (context parameter), mdscr_el1
    mrs  x1, mdscr_el1
	stp	x0, x1, [sp, #160]
	
	//  Load vttbr & vtcr
	ldp	x1, x2, [x0, #312]
	msr	vttbr_el2, x2
	msr	vtcr_el2, x1

	//  Load HCR
	ldr	x3, [x0, #328]
	msr	hcr_el2, x3

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

	ldnp	q0, q1, [x0, #336]
	ldnp	q2, q3, [x0, #368]
	ldnp	q4, q5, [x0, #400]
	ldnp	q6, q7, [x0, #432]
	ldnp	q8, q9, [x0, #464]
	ldnp	q10, q11, [x0, #496]
	ldnp	q12, q13, [x0, #528]
	ldnp	q14, q15, [x0, #560]
	ldnp	q16, q17, [x0, #592]
	ldnp	q18, q19, [x0, #624]
	ldnp	q20, q21, [x0, #656]
	ldnp	q22, q23, [x0, #688]
	ldnp	q24, q25, [x0, #720]
	ldnp	q26, q27, [x0, #752]
	ldnp	q28, q29, [x0, #784]
	ldnp	q30, q31, [x0, #816]

	ldr	w4, [x0, #272]
	ldr	w5, [x0, #276]
	msr	fpsr, x4
	msr	fpcr, x5
	
	//  Mask interrupts (needed as we are setting spsr_el2 and elr_el2).
	//  They will be unmasked in EL1.
	msr	DAIFset, #3

	//  SP and PC
	ldp	x4, x5, [x0, #248]
	msr	sp_el1, x4
	msr	elr_el2, x5

	//  Spsr
	ldr	w6, [x0, #264]
	msr	spsr_el2, x6

	//  Re-route debug: set MDCR_EL2.TDE
	mrs	x8, mdcr_el2
	orr	x8, x8, #(1<<8)
	msr	mdcr_el2, x8

        //  MDSCR
        ldr     w6, [x0, #268]
        msr     mdscr_el1, x6

	//  Load registers
	ldp	x2, x3, [x0, #16]
	ldp	x4, x5, [x0, #32]
	ldp	x6, x7, [x0, #48]
	ldp	x8, x9, [x0, #64]
	ldp	x0, x1, [x0, #0]
	
	eret

exec_cont:
	//  Save x0 and load context
	str	x0, [sp, #168]
	ldr     x0, [sp, #160]

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
	
	//  Save PC, SP, SPSR, MDSCR
	mrs	x1, sp_el1
	mrs	x2, elr_el2
	mrs	x3, spsr_el2
	mrs     x4, mdscr_el1
	stp	x1, x2, [x0, #248]
	str     w3, [x0, #264]
	str     w4, [x0, #268]

	//  Save ESR, FAR, HPFAR
	mrs	x4, esr_el2
	mrs	x5, far_el2
	mrs	x6, hpfar_el2
	stp	x4, x5, [x0, #288]
	str	x6, [x0, #304]

	//  Enable interrupts
	msr	DAIFclr, #3

	stnp	q0, q1, [x0, #336]
	stnp	q2, q3, [x0, #368]
	stnp	q4, q5, [x0, #400]
	stnp	q6, q7, [x0, #432]
	stnp	q8, q9, [x0, #464]
	stnp	q10, q11, [x0, #496]
	stnp	q12, q13, [x0, #528]
	stnp	q14, q15, [x0, #560]
	stnp	q16, q17, [x0, #592]
	stnp	q18, q19, [x0, #624]
	stnp	q20, q21, [x0, #656]
	stnp	q22, q23, [x0, #688]
	stnp	q24, q25, [x0, #720]
	stnp	q26, q27, [x0, #752]
	stnp	q28, q29, [x0, #784]
	stnp	q30, q31, [x0, #816]

	mrs	x4, fpsr
	mrs	x5, fpcr
	str      w4, [x0, #272]
	str      w5, [x0, #276]

	//  Restore host vfpu
	ldp	d8, d9, [sp, #96]
	ldp	d10, d11, [sp, #112]
	ldp	d12, d13, [sp, #128]
	ldp	d14, d15, [sp, #144]

	//  Restore host context
        ldr      w1, [sp, #168]
        msr     mdscr_el1, x1
	ldp	x19, x20, [sp, #16]
	ldp	x21, x22, [sp, #32]
	ldp	x23, x24, [sp, #48]
	ldp	x25, x26, [sp, #64]
	ldp	x27, x28, [sp, #80]
	ldp	x29, x30, [sp], #176

	ret
	.size	execute_hull, . - execute_hull

	//  FPU registers context
	//  vfp_init_flag     (1           -> 0  .. 3)
	//  ctrl, status      (2*4         -> 4  .. 11)
	//  Q0 - Q31          (32*16 = 512 -> 16 .. 527)
#define IFRAME_FPU_SIZE 528
.section .vectors, "a"
.text

.globl __sync_interrupt_low
__sync_interrupt_low:
        b exec_cont
        .size __sync_interrupt_low, . - __sync_interrupt_low

.globl __irq_interrupt_low
.globl __fiq_interrupt_low        
__fiq_interrupt_low:
__irq_interrupt_low:
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

    adrp    x0,interrupt_stack_table
    add     x0, x0, #:lo12:interrupt_stack_table
    mrs     x1, mpidr_el1
    and     x1, x1, #3
    ldr     x2, [x0, x1, lsl #3]  //  Load new stack pointer

	// 3) Create a frame, switch to irq stack
    mov     x1, sp
    mov     sp, x2
    stp     x29, x30, [sp, #-32]!
    str     x1, [sp, #16]

    //  4) allocate the FPU context on the irq stack
        sub     sp, sp, #IFRAME_FPU_SIZE /* allocate the context */

        //  5) call handler
        //  pre-handler: x0: fpu context address
        mov     x0, sp
        bl      __gnat_irq_pre_handler
        str     x0, [sp, #(IFRAME_FPU_SIZE + 24)]   /* save the prev_ctxt */
        bl      __gnat_irq_handler           /* actually call the handler */
        ldr     x1, [sp, #(IFRAME_FPU_SIZE + 24)]   /* load the prev_ctxt */
	mov     x0, sp	                 /* get the irq fpu context */
        bl      __gnat_irq_post_handler
        add     sp, sp, #IFRAME_FPU_SIZE /* free the irq fpu context */

	//  6) Switch back to previous stack
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

        //  Spsr
        ldr	x6, [x0, #264]
        msr	spsr_el2, x6

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
        .size __irq_interrupt_low, . - __irq_interrupt_low
