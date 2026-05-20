##############################################################################
#                                                                            #
#                      Copyright (C) 2005-2020, AdaCore                      #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

# Sore and restore the context of a task for the PowerPC Book E with FPU
# support. This is a leaf function that doesn't allocate a frame.

# Switch from running_thread to first_thread.
# r3 and r4 are not modified.

        .globl __gnat_context_switch
        .type  __gnat_context_switch, @function
__gnat_context_switch:

        .globl __gnat_context_switch
        .type  __gnat_context_switch, @function
__gnat_context_switch:
        # Load the context address of the current thread.
        lis     %r7,__gnat_running_thread_table@h
        ori     %r7,%r7,__gnat_running_thread_table@l

        mfcr    %r6                  # move CR to R6 since it will be clobbered by the next instruction
        lwz     %r5,0(%r7)           # Read current_thread
        mflr    %r9

        # Save callee-saved registers.
        stw     %r1,  0(%r5)
        stw     %r2,  4(%r5)
        stw     %r13, 8(%r5)
        stw     %r14, 12(%r5)
        stw     %r15, 16(%r5)
        stw     %r16, 20(%r5)
        stw     %r17, 24(%r5)
        stw     %r18, 28(%r5)
        stw     %r19, 32(%r5)
        stw     %r20, 36(%r5)
        stw     %r21, 40(%r5)
        stw     %r22, 44(%r5)
        stw     %r23, 48(%r5)
        stw     %r24, 52(%r5)
        stw     %r25, 56(%r5)
        stw     %r26, 60(%r5)
        stw     %r27, 64(%r5)
        stw     %r28, 68(%r5)
        stw     %r29, 72(%r5)
        stw     %r30, 76(%r5)
        stw     %r31, 80(%r5)

        stw     %r6, 84(%r5)            # store Cond. Reg.
        stw     %r9, 88(%r5)            # store Link Reg.

        stfd    %f14, 96(%r5)
        stfd    %f15, 104(%r5)
        stfd    %f16, 112(%r5)
        stfd    %f17, 120(%r5)
        stfd    %f18, 128(%r5)
        stfd    %f19, 136(%r5)
        stfd    %f20, 144(%r5)
        stfd    %f21, 152(%r5)
        stfd    %f22, 160(%r5)
        stfd    %f23, 168(%r5)
        stfd    %f24, 176(%r5)
        stfd    %f25, 184(%r5)
        stfd    %f26, 192(%r5)
        stfd    %f27, 200(%r5)
        stfd    %f28, 208(%r5)
        stfd    %f29, 216(%r5)
        stfd    %f30, 224(%r5)
        stfd    %f31, 232(%r5)
        mffs    %f0
        stfd    %f0,240(%r5)

        # Load the context address of the next thread.
        lis     %r8, first_thread_table@h
        ori     %r8, %r8,first_thread_table@l
        lwz     %r5, 0(%r8)
        stw     %r5, 0(%r7)

        lwz     %r6, 84(%r5)            # load Cond. Reg.
        lwz     %r9, 88(%r5)            # load Link Reg.
        mtlr    %r9

        # Restore callee-saved registers.
        lwz     %r1,  0(%r5)
        lwz     %r2,  4(%r5)
        lwz     %r13, 8(%r5)
        lwz     %r14, 12(%r5)
        lwz     %r15, 16(%r5)
        lwz     %r16, 20(%r5)
        lwz     %r17, 24(%r5)
        lwz     %r18, 28(%r5)
        lwz     %r19, 32(%r5)
        lwz     %r20, 36(%r5)
        lwz     %r21, 40(%r5)
        lwz     %r22, 44(%r5)
        lwz     %r23, 48(%r5)
        lwz     %r24, 52(%r5)
        lwz     %r25, 56(%r5)
        lwz     %r26, 60(%r5)
        lwz     %r27, 64(%r5)
        lwz     %r28, 68(%r5)
        lwz     %r29, 72(%r5)
        lwz     %r30, 76(%r5)
        lwz     %r31, 80(%r5)

        mtcr    %r6                   # restore CR

        lfd     %f14, 96(%r5)
        lfd     %f15, 104(%r5)
        lfd     %f16, 112(%r5)
        lfd     %f17, 120(%r5)
        lfd     %f18, 128(%r5)
        lfd     %f19, 136(%r5)
        lfd     %f20, 144(%r5)
        lfd     %f21, 152(%r5)
        lfd     %f22, 160(%r5)
        lfd     %f23, 168(%r5)
        lfd     %f24, 176(%r5)
        lfd     %f25, 184(%r5)
        lfd     %f26, 192(%r5)
        lfd     %f27, 200(%r5)
        lfd     %f28, 208(%r5)
        lfd     %f29, 216(%r5)
        lfd     %f30, 224(%r5)
        lfd     %f31, 232(%r5)
        lfd     %f0,  240(%r5)
        mtfsf   0xff, %f0

        blr
        .size __gnat_context_switch, . - __gnat_context_switch

        .globl __gnat_start_thread
        .type  __gnat_start_thread, @function
__gnat_start_thread:
        # Clear lr to mark end of the call stack
        li      %r0,0
        mtlr    %r0

        # Copy argument and pc provided by calling function in r14 and r15
        # respectively.
        mtctr   %r14
        mr      %r3,%r15

        # Jump
        bctr
        .size __gnat_start_thread, . - __gnat_start_thread
