## -*- asm -*- ###############################################################
#                                                                            #
#                      Copyright (C) 2005-2010, AdaCore                      #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

SPEFSCR = 512

# Function to store and restore the context of a task for the PowerPC + SPE
# This is a leaf function that doesn't allocate a frame

# Switch from running_thread to first_thread.
# r3 and r4 are preserved.

	.globl __gnat_context_switch
	.type  __gnat_context_switch, @function
__gnat_context_switch:
        # Load the context address of the current thread.
  	lis     %r7,__gnat_running_thread_table@h
        ori     %r7,%r7,__gnat_running_thread_table@l

	mfcr    %r6                  # move Condition Reg. to GPR6
        lwz     %r5,0(%r7)	     # Read current_thread
        mflr    %r9

        # Save callee-saved registers.
	evstdd  %r1,0(%r5)           # store GPR1 = Stack_Pointer
	evstdd  %r2,8(%r5)           # store GPR2
	evstdd  %r13,16(%r5)
	evstdd  %r14,24(%r5)
	evstdd  %r15,32(%r5)
	evstdd  %r16,40(%r5)
	evstdd  %r17,48(%r5)
	evstdd  %r18,56(%r5)
	evstdd  %r19,64(%r5)
	evstdd  %r20,72(%r5)
	evstdd  %r21,80(%r5)
	evstdd  %r22,88(%r5)
	evstdd  %r23,96(%r5)
	evstdd  %r24,104(%r5)
	evstdd  %r25,112(%r5)
	evstdd  %r26,120(%r5)
	evstdd  %r27,128(%r5)
	evstdd  %r28,136(%r5)
	evstdd  %r29,144(%r5)
	evstdd  %r30,152(%r5)
	evstdd  %r31,160(%r5)

	stw     %r6, 168(%r5)            # store Cond. Reg.
        stw     %r9, 172(%r5)            # store Link Reg.

        # Save SPEFSCR
        mfspr   %r13,SPEFSCR
        stw     %r13,176(%r5)

        # Load the context address of the next thread.
  	lis     %r8,first_thread_table@h
        ori     %r8,%r8,first_thread_table@l
        lwz     %r5,0(%r8)
        stw     %r5,0(%r7)

        # Load SPEFSCR
        lwz     %r13,176(%r5)
        mtspr   SPEFSCR,%r13

       	lwz     %r6, 168(%r5)            # load Cond. Reg.
       	lwz     %r9, 172(%r5)            # load Link Reg.
        mtlr    %r9

        # Restore callee-saved registers.
	evldd   %r1,0(%r5)           # load GPR1 = Stack_Pointer
	evldd   %r2,8(%r5)           # load GPR2
	evldd   %r13,16(%r5)
	evldd   %r14,24(%r5)
	evldd   %r15,32(%r5)
	evldd   %r16,40(%r5)
	evldd   %r17,48(%r5)
	evldd   %r18,56(%r5)
	evldd   %r19,64(%r5)
	evldd   %r20,72(%r5)
	evldd   %r21,80(%r5)
	evldd   %r22,88(%r5)
	evldd   %r23,96(%r5)
	evldd   %r24,104(%r5)
	evldd   %r25,112(%r5)
	evldd   %r26,120(%r5)
	evldd   %r27,128(%r5)
	evldd   %r28,136(%r5)
	evldd   %r29,144(%r5)
	evldd   %r30,152(%r5)
	evldd   %r31,160(%r5)
        
        mtcr    %r6

	blr
        .size __gnat_context_switch, . - __gnat_context_switch

        .globl __gnat_start_thread
	.type  __gnat_start_thread, @function
__gnat_start_thread:
	# Clear lr to mark end of stack
	li	%r0,0
	mtlr	%r0

	# Copy argument and pc
	mtctr	%r14
	mr	%r3,%r15

	# Jump
	bctr
        .size __gnat_start_thread, . - __gnat_start_thread
