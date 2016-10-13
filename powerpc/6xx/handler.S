/** -*- asm -*- **************************************************************
*                                                                            *
*                      Copyright (C) 2005-2014, AdaCore                      *
*                                                                            *
*                               Assembly File                                *
*                                                                            *
******************************************************************************/


/* INTERRUPT HANDLER

   When an interrupt occurs, the processor moves the address
   of the instruction that caused the interrupt into register SRR0
   and copies the machine state register (msr) into register SRR1.
   The interrupt enable bit (EE) in the MSR is then set to 0
   thereby disabling all externel interrupts. The processor begins
   then execution of the exception handler in the exception vector
   table at the vector offset determined by the interrupts source.
   To return from the exception handler an rfi instruction has to be
   executed thereby copying the stored register srr1 to msr and
   continuing execution at *SRR0.  */

#ifdef _SOFT_FLOAT
#define FRAME_SIZE 80
#else
#define FRAME_SIZE (80 + 15 * 8)
#endif

/* The handler_start routine is copied into the exception vectors.
   So its size must be less than 0x100 bytes.  */

        .type handler_start,@function
handler_start:
        /* Save r3,r4,r5,r9  */
	mtsprg0 %r3
	mtsprg1 %r4
	mtsprg2 %r5
        mtsprg3 %r9

        /* Restore bits FP, IR and DR of MSR
	   This will renable the FPU and the MMU.  */
        mfmsr	%r3
	mfsrr1	%r5
	li	%r9,0x2030
        and	%r5,%r5,%r9
        or	%r3,%r5,%r3
        mtmsr	%r3

        /* Synchronize context in case the FPU was reenabled.  */
        isync

	/* Use a long branch to branch to the common continuation point,
	   so that runtime can be located anywhere. This must be an
	   absolute value to avoid relocation. Save LR.  */
	mflr	%r4
	lis	%r5,handler_cont@h
	ori	%r5,%r5,handler_cont@l
	mtlr	%r5

.Lset_handler_arg:
        /* Note: the 4 instructions below will be overwritten.  Do not modify
           them!  */
        lis     %r9,0@h		/* handler address  */
        ori     %r9,%r9,0@l
        lis     %r3,0@h		/* handler argument  */
        ori     %r3,%r3,0@l

	/* Jump to common point.  */
	blr

.Lhandler_end:
        .size handler_start, . - handler_start

        .type handler_cont,@function
handler_cont:
	/* Entry:
	        %r9: user handler address
	        %r3: user handler argument
	        %r4: saved lr
		%r5: scratch
	        %lr: scratch
	      sprg0: saved %r3
	      sprg1: saved %r4
	      sprg2: saved %r5
	      sprg3: saved %r9
	*/
	/* Restore LR (this makes %r4 scratch).  */
	mtlr	%r4
	/* Save CR.  */
        mfcr    %r5

	/* Check MSR.PR  */
	mfsrr1  %r4
	andi.	%r4,%r4,0x4000
	bne	.Lswitch_stack

        /* Check wether the stack pointer %r1 is already in the
	   interrupt stack.  */
        lis     %r4,__interrupt_stack_start@h
        ori	%r4,%r4,__interrupt_stack_start@l
        cmpl    %cr7,%r1,%r4
        blt     %cr7,.Lswitch_stack
        lis     %r4,__interrupt_stack_end@h
        ori	%r4,%r4,__interrupt_stack_end@l
        cmpl    %cr7,%r1,%r4
        bge     %cr7,.Lswitch_stack

	/* Yes: just allocate a new frame.  */
        stwu    %r1,-FRAME_SIZE(%r1)
        b       .Lstack_established

.Lswitch_stack:
        /* No: load the interrupt stack pointer and allocate a frame.  */
        lis     %r4,interrupt_stack_table@ha
        lwz     %r4,interrupt_stack_table@l(%r4)
        stwu    %r1,-FRAME_SIZE(%r4)
        mr      %r1,%r4

.Lstack_established:
        /* Save volatile registers (r0, r3-r12).  */
        stw     %r0,8(%r1)
        mfsprg0 %r4
        stw     %r4,12(%r1) 	/* saved %r3 */
	mfsprg1 %r4
        stw     %r4,16(%r1)	/* saved %r4 */
	mfsprg2 %r4
        stw     %r4,20(%r1)	/* saved %r5 */
        stw     %r6,24(%r1)
        stw     %r7,28(%r1)
        stw     %r8,32(%r1)
	mfsprg3 %r4
        stw     %r4,36(%r1)	/* saved %r9 */
        stw     %r10,40(%r1)
        stw     %r11,44(%r1)
        stw     %r12,48(%r1)
        /* .. ctr, xer, lr, cr.  */
        mfxer   %r6
        mflr    %r4
        mfctr   %r7
        stw     %r6,52(%r1)	/* xer */
        stw     %r4,56(%r1)	/* lr */
        stw     %r7,60(%r1)	/* ctr */
        stw     %r5,64(%r1)	/* cr */

	/* Set LR to user handler address.  */
	mtlr	%r9

	/* Clear reservation.  */
	li	%r6,68
	stwcx.	%r12,%r6,%r1

        /* Save srr0 (ip) and srr1 (msr).  */
        mfsrr0  %r7
        mfsrr1  %r4
        stw     %r7,68(%r1)
        stw     %r4,72(%r1)

#ifndef _SOFT_FLOAT
        /* Save volatile FP regs.  */
        stfd    %f0,80(%r1)
        stfd    %f1,88(%r1)
        stfd    %f2,96(%r1)
        stfd    %f3,104(%r1)
        stfd    %f4,112(%r1)
        stfd    %f5,120(%r1)
        stfd    %f6,128(%r1)
        stfd    %f7,136(%r1)
        stfd    %f8,144(%r1)
        stfd    %f9,152(%r1)
        stfd    %f10,160(%r1)
        stfd    %f11,168(%r1)
        stfd    %f12,176(%r1)
        stfd    %f13,184(%r1)

        /* Save FPSCR.  */
        mffs    %f0
        stfd    %f0,192(%r1)
#endif

        /* Call user handler.  */
        blrl

	/* Interrupts must be disabled when user handler returns.  */

	/* Check if context switch is needed.  */
	bl       __gnat_context_switch_needed

	/* context switch needed?  */
	cmpwi   %cr7,%r3,0
	beq-    %cr7,.Lno_con_sw

	/* Yes, we need a context switch.  */

        /* Save interrupt stack pointer (in case of nested interrupts).  */
        lis     %r5,interrupt_stack_table@ha
        stw     %r1,interrupt_stack_table@l(%r5)

	bl      __gnat_pre_context_switch
	bl      __gnat_context_switch

        /* Restore interrupt stack pointer.  */
        addi    %r4,%r1,FRAME_SIZE
        lis     %r5,interrupt_stack_table@ha
        stw     %r4,interrupt_stack_table@l(%r5)

.Lno_con_sw:
#ifndef _SOFT_FLOAT
        /* Restore FPU.  */
	lfd    %f0,192(%r1)
        mtfsf  0xff,%f0
        lfd    %f0,80(%r1)
        lfd    %f1,88(%r1)
        lfd    %f2,96(%r1)
        lfd    %f3,104(%r1)
        lfd    %f4,112(%r1)
        lfd    %f5,120(%r1)
        lfd    %f6,128(%r1)
        lfd    %f7,136(%r1)
        lfd    %f8,144(%r1)
        lfd    %f9,152(%r1)
        lfd    %f10,160(%r1)
        lfd    %f11,168(%r1)
        lfd    %f12,176(%r1)
        lfd    %f13,184(%r1)
#endif

       	/* Restore srr0 and srr1.  */
        lwz     %r3,68(%r1)
        lwz     %r4,72(%r1)
        mtsrr0  %r3
        mtsrr1  %r4

        /* Restore ctr, xer, lr, cr.  */
        lwz     %r3,52(%r1)
        lwz     %r4,56(%r1)
        lwz     %r5,60(%r1)
        lwz     %r6,64(%r1)
        mtxer   %r3
        mtlr    %r4
        mtctr   %r5
        mtcr    %r6

        /* Restore r0, r3-r12.  */
        lwz     %r0,8(%r1)
        lwz     %r3,12(%r1)
        lwz     %r4,16(%r1)
        lwz     %r5,20(%r1)
        lwz     %r6,24(%r1)
        lwz     %r7,28(%r1)
        lwz     %r8,32(%r1)
        lwz     %r9,36(%r1)
        lwz     %r10,40(%r1)
        lwz     %r11,44(%r1)
        lwz     %r12,48(%r1)

        lwz     %r1,0(%r1)
	rfi

        .size handler_cont, . - handler_cont


/* Copy_Handler

  Copy the exception handler to a given address. The address
  of the handler is given in GPR3 and the destination address
  is passed in GPR4. The function copy handler copies the
  ISR function above between extern_exc_start and extern_exc_stop
  to the address given in GPR4. The ISR first executes some prologue,
  branches to the specific handler at address specified by GPR5 and
  executues the epilogue.
  The specifiec handler is passed an argument that is the interrupt ID
  that is determined by a call to To_Interrupt.
  This argument is passed via GPR3.

  Arguments
         GPR3   Address of Exception Handler Routine
         GPR4   Trap Address
         GPR5   Exception Handler Parameter
*/

	.global copy_handler
	.type   copy_handler, @function
copy_handler:

        /* First copy handler.  */
        subi    %r8,%r4,4
        li      %r7,(.Lhandler_end - handler_start) / 4 /* Nbr of words.  */
        mtctr   %r7
        lis     %r6,(handler_start - 4)@h	/* r6: start of handler.  */
        ori     %r6,%r6,(handler_start - 4)@l
.Lcopy_loop:
        lwzu    %r9,4(%r6)	/* read word from r6+4, and update r6 */
        stwu    %r9,4(%r8)	/* write it to (r8+4), and update r8.  */
        bdnz    .Lcopy_loop

        /* Second, write instructions that set argument and handler.  */
        addi    %r8,%r4,(.Lset_handler_arg - handler_start - 4)

      	srwi    %r6,%r3,16              /* Shift right 16 bits.  */
       	oris    %r6,%r6,0x3d200000@h    /* opcode for 'lis 9,x'  */
        stwu    %r6,4(%r8)

        clrlwi  %r6,%r3,16	        /* clear 16 high-order bits  */
	oris    %r6,%r6,0x61290000@h	/* opcode for 'ori 9,9,x'  */
        stwu    %r6,4(%r8)

        srwi    %r6,%r5,16              /* Shift right 16 bits.  */
        oris    %r6,%r6,0x3c600000@h    /* opcode for 'lis 3,x'  */
        stwu    %r6,4(%r8)

        clrlwi  %r6,%r5,16		/* clear 16 high-order bits  */
       	oris    %r6,%r6,0x60630000@h	/* opcode for 'ori 3,3,x'  */
        stwu    %r6,4(%r8)

        /* Flush data cache and invalidate instruction cache.  */
        mtctr   %r7
.Lflush:
        dcbst   0,%r4		/* Data Cache Block Store.  */
        icbi    0,%r4		/* Invalidate copy of storage.  */
        addi    %r4,%r4,4
        bdnz    .Lflush

        /* Synchronize.  */
        sync

        blr
        .size   copy_handler, . - copy_handler
