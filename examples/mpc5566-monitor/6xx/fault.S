	.text

        # Stub for 'sc'.
	.global fault_entry
fault_entry:
	ba	fault_handler_start

	.lcomm fault_regs,20*4,4

#define FAULT_STACK_SIZE 1024
	.lcomm fault_stack, FAULT_STACK_SIZE, 8
fault_stack_end = fault_stack + FAULT_STACK_SIZE

#ifdef NOFPU
#define FRAME_SIZE 80
#else
#define FRAME_SIZE (80 + 15 * 8)
#endif

        .type fault_handler_start,@function
fault_handler_start:
        # Save r2,r3
        mtsprg3 %r3
	mtsprg2	%r2
	# Clear TGPR
	mfmsr	%r2
	lis	%r3,0x20000@h
	andc	%r3,%r2,%r3
	mtmsr	%r3
	
	#
	lis	%r3,fault_regs@h
	ori	%r3,%r3,fault_regs@l
	# Save regs
	stw	%r0,0(%r3)
	stw	%r1,4(%r3)
	stw	%r2,8(%r3)
#	stw	%r3,12(%r3)
	stw	%r4,16(%r3)
	stw	%r5,20(%r3)
	stw	%r6,24(%r3)
	stw	%r7,28(%r3)
	stw	%r8,32(%r3)
	stw	%r9,36(%r3)
	stw	%r10,40(%r3)
	stw	%r11,44(%r3)
	stw	%r12,48(%r3)
	stw	%r13,52(%r3)
	stw	%r14,56(%r3)
	stw	%r15,60(%r3)
	stw	%r16,64(%r3)
	stw	%r17,68(%r3)
	stw	%r18,72(%r3)
	stw	%r19,76(%r3)
	stw	%r20,80(%r3)
	stw	%r21,84(%r3)
	stw	%r22,88(%r3)
	stw	%r23,92(%r3)
	stw	%r24,96(%r3)
	stw	%r25,100(%r3)
	stw	%r26,104(%r3)
	stw	%r27,108(%r3)
	stw	%r28,112(%r3)
	stw	%r29,116(%r3)
	stw	%r30,120(%r3)
	stw	%r31,124(%r3)

        # Save CR
        mfcr    %r4
	stw	%r4,128(%r3)

        # load the fault stack pointer and allocate a frame.
        lis     %r1,(fault_stack_end - 16)@h
	ori	%r1,%r1,(fault_stack_end - 16)@l

	# Retrieve srr0 (ip) and srr1 (msr)
        mfsrr0  %r3
        mfsrr1  %r4

0:	bl	fault_handler
	b	0b
	
	.size fault_handler_start, . - fault_handler_start
