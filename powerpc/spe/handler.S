### -*- asm -*- ##############################################################
#                                                                            #
#                      Copyright (C) 2005-2015, AdaCore                      #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

SPEFSCR = 512

FRAME_SIZE = 128
        # Frame:
        # r1 + FRAME_SIZE + 4: ip (srr0)
        # r1 + FRAME_SIZE:
        # r1 + 120:           ACC
        # r1 + 116:           cr
        # r1 + 112:           ctr
        # r1 + 108:           xer
        # r1 + 104:           lr
        # r1 + 96:            r12
        # ...
        # r1 + 32:            r4
        # r1 + 24:            r3
        # r1 + 16:            r0
        # r1 + 12:            spefscr
        # r1 + 8:             msr (srr1)
        # r1 + 4: 	      reserved for lr
        # r1:                 old r1

	.macro handler num
        .p2align 3
        .type handler_\num,@function
        .globl handler_\num
handler_\num :
        # Save r0
        mtsprg0 %r0

        # Load interrupt number
        li      %r0,4*\num

        # Continue
        b       interrupt_handler_common

	# Pad to avoid holes in the code.
	nop
        .size   handler_\num, . - handler_\num
        .endm

        # The interrupt vector prefix of IVORs (in IVPR) is aligned on 64KB.
	# Each handler uses 16 bytes, so the table needs 16*64=1KB
	# Note that we don't set alignment on the .interrupt section, the
	# linker script must make sure that that section is within a 64KB
	# area.
        .section ".interrupt","ax"
        handler 0
        handler 1
        handler 2
        handler 3
        handler 4
        handler 5
        handler 6
        handler 7
        handler 8
        handler 9
        handler 10
        handler 11
        handler 12
        handler 13
        handler 14
        handler 15
        handler 16
        handler 17
        handler 18
        handler 19
        handler 20
        handler 21
        handler 22
        handler 23
        handler 24
        handler 25
        handler 26
        handler 27
        handler 28
        handler 29
        handler 30
        handler 31
        handler 32
        handler 33
        handler 34
        handler 35
        handler 36
        handler 37
        handler 38
        handler 39
        handler 40
        handler 41
        handler 42
        handler 43
        handler 44
        handler 45
        handler 46
        handler 47
        handler 48
        handler 49
        handler 50
        handler 51
        handler 52
        handler 53
        handler 54
        handler 55
        handler 56
        handler 57
        handler 58
        handler 59
        handler 60
        handler 61
        handler 62
        handler 63

        .text

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

        .type interrupt_handler_common,@function
interrupt_handler_common:
	.cfi_startproc
	.cfi_signal_frame
        # Save r3
        mtsprg3 %r3
        # Save CR
        mfcr    %r3
        mtsprg2 %r3

        # Check wether we are already in the interrupt stack
        lis     %r3,__interrupt_stack_start@ha
        addi	%r3,%r3,__interrupt_stack_start@l
        cmpl    %cr7,%r1,%r3
        blt     %cr7,.Lswitch_stack
        lis     %r3,__interrupt_stack_end@ha
        addi	%r3,%r3,__interrupt_stack_end@l
        cmpl    %cr7,%r1,%r3
        bge     %cr7,.Lswitch_stack

	# Yes: allocate a frame.
        stwu    %r1,-FRAME_SIZE(%r1)
        b       .Lstack_established

.Lswitch_stack:
        # No: load the interrupt stack pointer and allocate a frame.
        lis     %r3,interrupt_stack_table@ha
        lwz     %r3,interrupt_stack_table@l(%r3)
        stwu    %r1,-FRAME_SIZE(%r3)
        mr      %r1,%r3

.Lstack_established:
	.cfi_def_cfa	1,0	# Use %r1 as CFA
	.cfi_offset	1,0	# Previous %r1 at %r1
        # Save register r4
        stw     %r4,32(%r1)
	.cfi_offset	4,32	# Reg 4

        # Save srr0 (ip) and srr1 (msr)
        mfsrr0  %r3
        mfsrr1  %r4
        stw     %r3,FRAME_SIZE+4(%r1)
	.cfi_offset	67,FRAME_SIZE+4	# PC
	.cfi_return_column 67
        stw     %r4,8(%r1)

        # Copy SPE, AP, FP, IR and DR bits.
        li      %r3,0x2030
        oris    %r3,%r3,0x0208
        and     %r4,%r4,%r3
        mfmsr   %r3
        or      %r3,%r4,%r3
        mtmsr   %r3

        # Synchronize context in case the SPE/FPU was reenabled.
        isync

        # Save volatile registers r0,r3-r12
        mfsprg3 %r3
        evstdd  %r3,24(%r1)
        lwz     %r4,32(%r1)
        evstdd  %r4,32(%r1)
        mr      %r3,%r0
        mfsprg0 %r0
        evstdd  %r0,16(%r1)
        evstdd  %r5,40(%r1)
        evstdd  %r6,48(%r1)
        evstdd  %r7,56(%r1)
        evstdd  %r8,64(%r1)
        evstdd  %r9,72(%r1)
        evstdd  %r10,80(%r1)
        evstdd  %r11,88(%r1)
        evstdd  %r12,96(%r1)

        # xer, lr, ctr, cr
        mflr    %r5
        mfxer   %r6
        mfctr   %r7
        mfsprg2 %r8
        stw     %r5,104(%r1)
	.cfi_offset	65,104	# XER
        stw     %r6,108(%r1)
	.cfi_offset	76,108	# LR
        stw     %r7,112(%r1)
	.cfi_offset	66,112	# CTR
        stw     %r8,116(%r1)
	.cfi_offset	70,116	# CR

        # Save interrupt stack pointer (in case of nested interrupts)
        lis     %r5,interrupt_stack_table@ha
        stw     %r1,interrupt_stack_table@l(%r5)

        # Load handler address
        lis     %r7,__gnat_powerpc_exception_handlers@ha
        la      %r7,__gnat_powerpc_exception_handlers@l(%r7)
        lwzx    %r7,%r7,%r3
        mtctr   %r7

        # Save ACC
        evxor   %r6,%r6,%r6	# r6 <- 0
        evmwumiaa %r6,%r6,%r6	# r6 <- ACC + 0*0
        evstdd  %r6,120(%r1)

        # Save SPEFSCR
        mfspr   %r5,SPEFSCR
        stw     %r5,12(%r1)

        # Call handler
	srwi	%r3,%r3,2
        bctrl

	# Interrupts must be disabled when user handler returns

        # Potentially switch the context.  This cannot be done within an Ada
        #  procedure as the interrupt stack pointer must be correct.

	# Check if context switch is needed
	bl       __gnat_context_switch_needed

	# context switch needed?
	cmpwi   %cr7,%r3,0
	beq-    %cr7,.Lno_con_sw

	# Yes, we need a context switch
	bl      __gnat_context_switch

.Lno_con_sw:
        # Restore interrupt stack pointer
        addi    %r4,%r1,FRAME_SIZE
        lis     %r5,interrupt_stack_table@ha
        stw     %r4,interrupt_stack_table@l(%r5)

       	# Restore srr0 and srr1
        lwz     %r3,FRAME_SIZE+4(%r1)
        lwz     %r4,8(%r1)
        mtsrr0  %r3
        mtsrr1  %r4

        # Restore ctr, xer, lr, cr
        lwz     %r5,104(%r1)
        lwz     %r6,108(%r1)
        lwz     %r7,112(%r1)
        lwz     %r8,116(%r1)
        mtlr    %r5
        mtxer   %r6
        mtctr   %r7
        mtcr    %r8

        # Restore SPEFSCR
        lwz     %r3,12(%r1)
        mtspr   SPEFSCR,%r3

        # Restore ACC
        evldd   %r3,120(%r1)
        evmra   %r3,%r3

        # Restore r0, r3-r12
        evldd   %r0,16(%r1)
        evldd   %r3,24(%r1)
        evldd   %r4,32(%r1)
        evldd   %r5,40(%r1)
        evldd   %r6,48(%r1)
        evldd   %r7,56(%r1)
        evldd   %r8,64(%r1)
        evldd   %r9,72(%r1)
        evldd   %r10,80(%r1)
        evldd   %r11,88(%r1)
        evldd   %r12,96(%r1)

        # Load the back chain
        lwz     %r1,0(%r1)
	rfi

	.cfi_endproc
        .size interrupt_handler_common, . - interrupt_handler_common
