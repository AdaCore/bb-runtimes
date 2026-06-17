### -*- asm -*- ##############################################################
#                                                                            #
#                      Copyright (C) 2005-2021, AdaCore                      #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

# Interrupt handler routines for PowerPC Book E

# Layout of the caller saved registers:

FRAME_SIZE = 208        # Round to nearest 16-byte to ensure alignment
        # Floating point registers:
        # r1 + 196:     srr0 (interrupted program counter)
        # r1 + 184:     fpscr
        # r1 + 176:     f13
        # ...
        # r1 + 80:      f1
        # r1 + 72:      f0
        # Integer registers
        # r1 + 68:      cr
        # r1 + 64:      ctr
        # r1 + 60:      xer
        # r1 + 56:      lr
        # r1 + 52:      r12
        # ...
        # r1 + 24:      r5
        # r1 + 20:      r4
        # r1 + 16:      r3
        # r1 + 12:      r0
        # r1 + 8:       srr1 (interrupted msr)
        # r1 + 4:       the compiler will use this slot to store the lr during our Ada handler
        # r1:           r1 (interrupted task stack pointer)

        # PowerPC interrupt handlers. An seperate handler is provided per
        # PowerPC Book E interrupt to load the interrupt vector number and jump
        # to our common interrupt handler routines. These handlers need to be
        # 16-byte aligned as required by the Book-E specification.

        .macro handler num type
        .p2align 4
        .type handler_\num,@function
        .globl handler_\num
handler_\num :
        # Save r3 into SPRG3 and store the interrupt number in r3
        mtsprg3 %r3
        li      %r3, 4*\num

        # Continue
        b       interrupt_handler_common_\type

        # Pad to avoid holes in the code.
        nop
        .size   handler_\num, . - handler_\num
        .endm

        # The interrupt vector prefix of IVORs (in IVPR) is aligned on 64KB.
        # Each handler uses 16 bytes, so the table needs 16*15=240 bytes.
        # Note that we don't set alignment on the .interrupt section, the
        # linker script must make sure that that section is within a 64KB
        # area.
        .section ".interrupt","ax"
        handler 0 critical        # Critical Interrupt
        handler 1 machine_check   # Machine Check
        handler 2 regular         # Data Storage
        handler 3 regular         # Instruction Storage
        handler 4 regular         # External Input (runtime reserved)
        handler 5 regular         # Alignment
        handler 6 regular         # Program
        handler 7 regular         # FP Unavailable
        handler 8 regular         # System Call
        handler 9 regular         # AP Unavailable
        handler 10 regular        # Decrementer (runtime reserved)
        handler 11 regular        # Fixed Interval Timer
        handler 12 critical       # Watchdog Timer
        handler 13 regular        # Data TLB Error
        handler 14 regular        # Instruction TLB Error
        handler 15 critical       # Debug

        .text

        # Common Interrupt Handler routines

        # PowerPC provides three types of interrupt: non-critical (called
        # regular here), critical and machine check. All three use different
        # sets of save/restore registers to allow the higher priority interrupt
        # types preempt a lower interrupt priority type. This means the common
        # handler for each interrupt kind is almost the same, yet different due
        # to the different SRR used. For elegance and maintainability, we use a
        # macro to factor out the common code.

        # The flow of the common interrupt handler is a follows:
        #   1. On entry, the contents of r3 has been saved in sprg3 with r3 now
        #      holding the vector number of the interrupt.
        #   2. The processor is switched to using the interrupt stack
        #   3. The CPU state is stored. For all cases the state includes
        #      the caller-saved general purpose registers and the corresponding
        #      srr registers for the interrupt type. For regular interrupts the
        #      FPU caller saved registers are also saved and the FPU is
        #      reenabled.
        #   4. The interrupt handler associated with interrupt is called.
        #   5. If a context switch is now pending as a result of the handler's
        #      actions, the context is switched to that pending task.
        #   6. The CPU state is restored
        #   7. The return from interrupt instruction corresponding to the
        #      interrupt type is performed.

        .macro interrupt_handler_common type
        .type interrupt_handler_common_\type,@function
interrupt_handler_common_\type:
        # Use r4 as our temp register. Save the current value in SPRG4 for safe
        # keeping.
        mtsprg4 %r4
        # Save CR into SPRG2 as we need to do comparisons operations next
        mfcr    %r4
        mtsprg2 %r4

        # Check wether we are already in the interrupt stack
        lis     %r4,__interrupt_stack_start@ha
        addi    %r4,%r4,__interrupt_stack_start@l
        cmpl    %cr7,%r1,%r4
        blt     %cr7,.Lswitch_stack\@
        lis     %r4,__interrupt_stack_end@ha
        addi    %r4,%r4,__interrupt_stack_end@l
        cmpl    %cr7,%r1,%r4
        bge     %cr7,.Lswitch_stack\@

        # Yes: allocate a frame.
        stwu    %r1,-FRAME_SIZE(%r1)
        b       .Lstack_established\@

.Lswitch_stack\@:
        # No: load the interrupt stack pointer and allocate a frame.
        lis     %r4,interrupt_stack_table@ha
        lwz     %r4,interrupt_stack_table@l(%r4)
        stwu    %r1,-FRAME_SIZE(%r4)
        mr      %r1,%r4

.Lstack_established\@:
        # Save register r5
        stw     %r5,24(%r1)

        # Save *srr0 (ip) and *srr1 (msr)
.ifc    \type,critical
        mfcsrr0  %r4
        mfcsrr1  %r5
.endif
.ifc   \type,machine_check
        mfmcsrr0 %r4
        mfmcsrr1 %r5
.endif
.ifc    \type,regular
        mfsrr0   %r4
        mfsrr1   %r5
.endif
        stw     %r4,196(%r1)
        stw     %r5,8(%r1)

.ifc    \type,regular        # Reenable FPU and MMU if they were enabled prior to the interrupt
        # by copying the FP, IS and DS MSR flags to the interrupt MSR.
        li      %r4,0x2030
        and     %r5,%r5,%r4
        mfmsr   %r4
        or      %r4,%r5,%r4
        mtmsr   %r4

        # Synchronize context in case the FPU was reenabled.
        isync
.endif

        # Save caller registers r0,r3-r12
        stw     %r0,  12(%r1)
        mfsprg3 %r4              # %r3 was saved into SPRG3 above
        stw     %r4,  16(%r1)
        mfsprg4 %r4              # %r4 was saved into SPRG4 in the handler entry
        stw     %r4,  20(%r1)
        # r5 was saved above
        stw     %r6,  28(%r1)
        stw     %r7,  32(%r1)
        stw     %r8,  36(%r1)
        stw     %r9,  40(%r1)
        stw     %r10, 44(%r1)
        stw     %r11, 48(%r1)
        stw     %r12, 52(%r1)

        # Save LR, XER, CTR, CR
        mflr    %r5
        mfxer   %r6
        mfctr   %r7
        mfsprg2 %r8              # CR was saved in SPRG2 above
        stw     %r5,  56(%r1)
        stw     %r6,  60(%r1)
        stw     %r7,  64(%r1)
        stw     %r8,  68(%r1)

.ifc    \type,regular
        # Save caller-saved FPU registers
        stfd    %f0,  72(%r1)
        stfd    %f1,  80(%r1)
        stfd    %f2,  88(%r1)
        stfd    %f3,  96(%r1)
        stfd    %f4,  104(%r1)
        stfd    %f5,  112(%r1)
        stfd    %f6,  120(%r1)
        stfd    %f7,  128(%r1)
        stfd    %f8,  136(%r1)
        stfd    %f9,  144(%r1)
        stfd    %f10, 152(%r1)
        stfd    %f11, 160(%r1)
        stfd    %f12, 168(%r1)
        stfd    %f13, 176(%r1)
        mffs    %f0              # save FPSCR
        stfd    %f0,  184(%r1)
.endif

        # Save interrupt stack pointer (in case of nested interrupts)
        lis     %r5,interrupt_stack_table@ha
        stw     %r1,interrupt_stack_table@l(%r5)

        # Load handler address from exception handler table
        lis     %r7,__gnat_powerpc_exception_handlers@ha
        la      %r7,__gnat_powerpc_exception_handlers@l(%r7)
        lwzx    %r7,%r7,%r3
        mtctr   %r7

        # Call handler (shifting %r3 to produce the vector number)
        srwi    %r3,%r3,2
        bctrl

        # Interrupts must be disabled when user handler returns

        # Potentially switch the context.  This cannot be done within an Ada
        #  procedure as the interrupt stack pointer must be correct.

        # Check if context switch is needed
        bl       __gnat_context_switch_needed
        cmpwi   %cr7,%r3,0
        beq-    %cr7,.Lno_con_sw\@

        # Yes, we need a context switch
        bl      __gnat_context_switch

.Lno_con_sw\@:
        # Restore interrupt stack pointer
        addi    %r4,%r1,FRAME_SIZE
        lis     %r5,interrupt_stack_table@ha
        stw     %r4,interrupt_stack_table@l(%r5)

.ifc    \type,regular
        # Restore caller-saved floating-point registers
        lfd     %f0,  184(%r1)  # restore FPSCR
        mtfsf   0xff, %f0
        lfd     %f0,  72(%r1)
        lfd     %f1,  80(%r1)
        lfd     %f2,  88(%r1)
        lfd     %f3,  96(%r1)
        lfd     %f4,  104(%r1)
        lfd     %f5,  112(%r1)
        lfd     %f6,  120(%r1)
        lfd     %f7,  128(%r1)
        lfd     %f8,  136(%r1)
        lfd     %f9,  144(%r1)
        lfd     %f10, 152(%r1)
        lfd     %f11, 160(%r1)
        lfd     %f12, 168(%r1)
        lfd     %f13, 176(%r1)
.endif

        # Restore *srr0 and *srr1
        lwz     %r3,196(%r1)
        lwz     %r4,8(%r1)

.ifc   \type,critical
        mtcsrr0  %r3
        mtcsrr1  %r4
.endif
.ifc    \type,machine_check
        mtmcsrr0 %r3
        mtmcsrr1 %r4
.endif
.ifc    \type,regular
        mtsrr0   %r3
        mtsrr1   %r4
.endif

        # Restore LR, XER, CTR, CR
        lwz     %r5,  56(%r1)
        lwz     %r6,  60(%r1)
        lwz     %r7,  64(%r1)
        lwz     %r8,  68(%r1)
        mtlr    %r5
        mtxer   %r6
        mtctr   %r7
        mtcr    %r8

        # Restore caller saved registers r0, r3-r12
        lwz     %r0,  12(%r1)
        lwz     %r3,  16(%r1)
        lwz     %r4,  20(%r1)
        lwz     %r5,  24(%r1)
        lwz     %r6,  28(%r1)
        lwz     %r7,  32(%r1)
        lwz     %r8,  36(%r1)
        lwz     %r9,  40(%r1)
        lwz     %r10, 44(%r1)
        lwz     %r11, 48(%r1)
        lwz     %r12, 52(%r1)

        # Load the back chain
        lwz     %r1,0(%r1)

.ifc    \type,critical
        rfci
.endif
.ifc    \type,machine_check
        rfmci
.endif
.ifc    \type,regular
        rfi
.endif

        .size interrupt_handler_common_\type, . - interrupt_handler_common_\type
        .endm

        # The common handler routines for the three types of interrupts
        interrupt_handler_common regular
        interrupt_handler_common critical
        interrupt_handler_common machine_check
