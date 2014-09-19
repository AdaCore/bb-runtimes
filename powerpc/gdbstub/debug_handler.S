##### -*- asm -*- ############################################################
#                                                                            #
#                         Copyright (C) 2010, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################


	#####################################
	# Masks for Machine Status Register #
	#####################################

	MSR_EE=0x00008000

	# INTERRUPT HANDLER

	# When an interrupt occurs, the processor moves the address
	# of the instruction that caused the interrupt into register SRR0
	# and copies the machine state register (msr) into register SRR1.
	# The interrupt enable bit (EE) in the MSR is then set to 0
	# thereby disabling all externel interrupts. The processor begins
	# then execution of the exception handler in the exception vector
	# table at the vector offset determined by the interrupts source.
	# To return from the exception handler an rfi instruction has to be
	# executed thereby copying the stored register srr1 to msr and
	# continuing execution at *SRR0.

        .text

#define FRAME_SIZE 424

        .type handler_start,@function
handler_start:
        # Switch to alternate stack.
        mtsprg0 %r1
        lis     %r1, (debug_stack_end - 8 - FRAME_SIZE)@h
        ori     %r1,%r1, (debug_stack_end - 8 - FRAME_SIZE)@l

        # Save registers
        stw     %r0,8(%r1)
        # skip %r1
        stw     %r2,16(%r1)
        stw     %r3,20(%r1)
        stw     %r4,24(%r1)
        stw     %r5,28(%r1)
        stw     %r6,32(%r1)
        stw     %r7,36(%r1)
        stw     %r8,40(%r1)
        stw     %r9,44(%r1)
        stw     %r10,48(%r1)
        stw     %r11,52(%r1)
        stw     %r12,56(%r1)
        stw     %r13,60(%r1)
        stw     %r14,64(%r1)
        stw     %r15,68(%r1)
        stw     %r16,72(%r1)
        stw     %r17,76(%r1)
        stw     %r18,80(%r1)
        stw     %r19,84(%r1)
        stw     %r20,88(%r1)
        stw     %r21,92(%r1)
        stw     %r22,96(%r1)
        stw     %r23,100(%r1)
        stw     %r24,104(%r1)
        stw     %r25,108(%r1)
        stw     %r26,112(%r1)
        stw     %r27,116(%r1)
        stw     %r28,120(%r1)
        stw     %r29,124(%r1)
        stw     %r30,128(%r1)
        stw     %r31,132(%r1)
        # Save old r1
        mfsprg0 %r3
        stw     %r3,12(%r1)

        # Save srr0 (ip) and srr1 (msr)
        mfsrr0  %r3
        stw     %r3,392(%r1)
        mfsrr1  %r4
        stw     %r4,396(%r1)
        # cr, lr, ctr, xer
        mfcr    %r3
        mflr    %r4
        mfctr   %r5
        mfxer   %r6
        stw     %r3,400(1)
        stw     %r4,404(1)
        stw     %r5,408(1)
        stw     %r6,412(1)

        # Set the continuation point.
       	lis      6, handler_cont@h
	ori      6, 6, handler_cont@l
	mtctr    6

set_handler_arg:
        # Note: the 4 instructions below will be overwritten.  Do not modify
        # them!
        lis     9,0@h	# handler address
        ori     9,9,0@l

        lis     3,0@h   # handler argument
        ori     3,3,0@l

        addi    %r4,%r1,8

        bctr

handler_end:
        .size handler_start, . - handler_start

# Common code.
# This is not put into the handler because it would exceed the size
# and because there are two calls to the run-time.

        .type handler_cont, @function
handler_cont:

        mtctr   9

#ifndef _SOFT_FLOAT
        # First, re-enable the fpu.
        mfmsr   %r11
        ori     %r11,%r11,0x2000
        mtmsr   %r11
        isync

        # Save FP regs
        stfd    %f0,136(%r1)
        stfd    %f1,144(%r1)
        stfd    %f2,152(%r1)
        stfd    %f3,160(%r1)
        stfd    %f4,168(%r1)
        stfd    %f5,176(%r1)
        stfd    %f6,184(%r1)
        stfd    %f7,192(%r1)
        stfd    %f8,200(%r1)
        stfd    %f9,208(%r1)
        stfd    %f10,216(%r1)
        stfd    %f11,224(%r1)
        stfd    %f12,232(%r1)
        stfd    %f13,240(%r1)
        stfd    %f14,248(%r1)
        stfd    %f15,256(%r1)
        stfd    %f16,264(%r1)
        stfd    %f17,272(%r1)
        stfd    %f18,280(%r1)
        stfd    %f19,288(%r1)
        stfd    %f20,296(%r1)
        stfd    %f21,304(%r1)
        stfd    %f22,312(%r1)
        stfd    %f23,320(%r1)
        stfd    %f24,328(%r1)
        stfd    %f25,336(%r1)
        stfd    %f26,344(%r1)
        stfd    %f27,352(%r1)
        stfd    %f28,360(%r1)
        stfd    %f29,368(%r1)
        stfd    %f30,376(%r1)
        stfd    %f31,384(%r1)
#endif

#ifndef _SOFT_FLOAT
        # Save FPSCR
        mffs    %f0
        stfd    %f0,416(%r1)
#endif

        # Call user handler
        bctrl

#ifndef _SOFT_FLOAT
        # Restore FPSCR
        lfd     %f0,416(%r1)
        mtfsf  0xff,%f0
#endif

        # Restore srr0 (ip) and srr1 (msr)
        lwz     %r3,392(%r1)
        mtsrr0  %r3
        lwz     %r4,396(%r1)
        mtsrr1  %r4

        # cr, lr, ctr, xer
        lwz     %r3,400(1)
        lwz     %r4,404(1)
        lwz     %r5,408(1)
        lwz     %r6,412(1)
        mtcr    %r3
        mtlr    %r4
        mtctr   %r5
        mtxer   %r6

#ifndef _SOFT_FLOAT
        # Restore FP regs
        lfd    %f0,136(%r1)
        lfd    %f1,144(%r1)
        lfd    %f2,152(%r1)
        lfd    %f3,160(%r1)
        lfd    %f4,168(%r1)
        lfd    %f5,176(%r1)
        lfd    %f6,184(%r1)
        lfd    %f7,192(%r1)
        lfd    %f8,200(%r1)
        lfd    %f9,208(%r1)
        lfd    %f10,216(%r1)
        lfd    %f11,224(%r1)
        lfd    %f12,232(%r1)
        lfd    %f13,240(%r1)
        lfd    %f14,248(%r1)
        lfd    %f15,256(%r1)
        lfd    %f16,264(%r1)
        lfd    %f17,272(%r1)
        lfd    %f18,280(%r1)
        lfd    %f19,288(%r1)
        lfd    %f20,296(%r1)
        lfd    %f21,304(%r1)
        lfd    %f22,312(%r1)
        lfd    %f23,320(%r1)
        lfd    %f24,328(%r1)
        lfd    %f25,336(%r1)
        lfd    %f26,344(%r1)
        lfd    %f27,352(%r1)
        lfd    %f28,360(%r1)
        lfd    %f29,368(%r1)
        lfd    %f30,376(%r1)
        lfd    %f31,384(%r1)
#endif

        # Save volatile registers (r0, r3-r12)
        lwz     %r0,8(%r1)
        lwz     %r3,12(%r1)
        mtsprg0 %r3
        lwz     %r2,16(%r1)
        lwz     %r3,20(%r1)
        lwz     %r4,24(%r1)
        lwz     %r5,28(%r1)
        lwz     %r6,32(%r1)
        lwz     %r7,36(%r1)
        lwz     %r8,40(%r1)
        lwz     %r9,44(%r1)
        lwz     %r10,48(%r1)
        lwz     %r11,52(%r1)
        lwz     %r12,56(%r1)
        lwz     %r13,60(%r1)
        lwz     %r14,64(%r1)
        lwz     %r15,68(%r1)
        lwz     %r16,72(%r1)
        lwz     %r17,76(%r1)
        lwz     %r18,80(%r1)
        lwz     %r19,84(%r1)
        lwz     %r20,88(%r1)
        lwz     %r21,92(%r1)
        lwz     %r22,96(%r1)
        lwz     %r23,100(%r1)
        lwz     %r24,104(%r1)
        lwz     %r25,108(%r1)
        lwz     %r26,112(%r1)
        lwz     %r27,116(%r1)
        lwz     %r28,120(%r1)
        lwz     %r29,124(%r1)
        lwz     %r30,128(%r1)
        lwz     %r31,132(%r1)

        mfsprg0  %r1
	rfi

        .size handler_cont, . - handler_cont


#############################
##                         ##
##       Copy_Handler      ##
##                         ##
#############################
#
# Copy the exception handler to a given address. The address
# of the handler is given in GPR3 and the destination address
# is passed in GPR4. The function copy handler copies the
# ISR function above between extern_exc_start and extern_exc_stop
# to the address given in GPR4. The ISR first executes some prologue,
# branches to the specific handler at address specified by GPR5 and
# executues the epilogue.
# The specifiec handler is passed an argument that is the interrupt ID
# that is determined by a call to To_Interrupt.
# This argument is passed via GPR3.
#
# Arguments
#         GPR3   Address of Exception Handler Routine
#         GPR4   Trap Address
#         GPR5   Exception Handler Parameter

	.global copy_debug_handler
	.type   copy_debug_handler, @function
copy_debug_handler:

        # First copy handler.
        subi    8,4,4
        lis     6,handler_start@h	# r6: start of handler
        ori     6,6,handler_start@l
        lis     7,handler_end@h		# r7: end of handler
        ori     7,7,handler_end@l
.Lcopy_loop:
        cmp     0,0,6,7			# exit loop if r6 = r7
        bge     .Lcopy_end
        lwz     9,0(6)			# read word from r6
        stwu    9,4(8)			# write it to (r8+4)
        addi    6,6,4			# Add 4 to r6
        b       .Lcopy_loop

.Lcopy_end:
        # Second, write instructions that set argument and handler.
        addi    8,4,(set_handler_arg - handler_start - 4)

      	srwi    6,3,16                  #  Shift right 16 bits.
       	oris    6,6,0x3d200000@h        # opcode for 'lis 9,x'
        stwu    6,4(8)

        clrlwi  6,3,16		        # clear 16 high-order bits
	oris    6,6,0x61290000@h	# opcode for 'ori 9,9,x'
        stwu    6,4(8)

        srwi    6,5,16                  #  Shift right 16 bits.
        oris    6,6,0x3c600000@h        # opcode for 'lis 3,x'
        stwu    6,4(8)

        clrlwi  6,5,16		        # clear 16 high-order bits
       	oris    6,6,0x60630000@h	# opcode for 'ori 3,3,x'
        stwu    6,4(8)

        # Flush data cache and invalidate instruction cache.
        addi    8,4,(handler_end-handler_start)
.Lflush:
        cmp     cr7,4,8
        beq     cr7,.Lflush_done
        dcbst   0,4	# Data Cache Block Store
        icbi    0,4	# Invalidate copy of storage
        addi    4,4,4
        b       .Lflush

.Lflush_done:
        sync
        blr
        .size   copy_debug_handler, . - copy_debug_handler

       	.global debug_trap
	.type   debug_trap, @function
debug_trap:
        # Set the LR to the trap instruction so that we could return to it.
        bl      1f
1:      trap
        .size   debug_trap, . - debug_trap

# Stack
        .section .bss,"wa",@nobits
        .type   debug_stack_start,@common
        .align  8
debug_stack_start:
        .space  2048,0
debug_stack_end:
        .size debug_stack_start, debug_stack_end - debug_stack_start
