##############################################################################
#                                                                            #
#                         Copyright (C) 2019, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

#include "riscv_def.h"

        .section .text
        .globl __gnat_trap_entry
        .type __gnat_trap_entry,@function

/*
 * Macro: GET_HART_STACK_TABLE_ADDR
 * Computes the address of the current hart's entry in interrupt_stack_table.
 * Clobbers: t0, t1
 * Result: t0 = &interrupt_stack_table[mhartid - 1]
 *
 * This is a macro rather than a subroutine because using a subroutine would
 * require either clobbering ra or using a non-standard return register.
 *
 * IMPORTANT: This macro clobbers t0 and t1, so it must only be used AFTER
 * the interrupt frame has been saved. For the initial stack lookup before
 * the frame exists, we use inline code with tp as scratch instead.
 */
.macro GET_HART_STACK_TABLE_ADDR
        csrr    t1, mhartid
        addi    t1, t1, -1          /* Convert to 0-based index */
        slli    t1, t1, 3           /* Multiply by 8 (pointer size) */
        la      t0, interrupt_stack_table
        add     t0, t0, t1          /* Point to correct entry for this hart */
.endm

        /* The trap handler must be 4-byte align not to override the
         * mode field of the mtvec register.
         */
        .align 4

__gnat_trap_entry:

        /* In this piece of code the words trap and interrupt have the same
         * meaning. Trap is more correct with regards to the architecture,
         * but interrupt is historically used in the run-time for symbols like
         * __interrupt_stacks_start.
         */

        /* Our goal in this trap handler is to preserve the context of the
         * interrupted context in the interrupt stack of the given core,
         * call the trap handler, and then restore the first context and
         * return to it.
         *
         * The main challenge is that we need to use registers to compute the
         * address of the interrupt stack and save the context, but we can't
         * clobber any register that the interrupted code might be using mid-calculation.
         * Even, The argument registers (a0-7) must be preserved until the call to
         * __gnat_trap_handler because they may contain arguments for system
         * calls.
         *
         * We need two registers for the minimal operations to compute the
         * stack pointer. Mandatory for save_interrupted_context.
         * We will be using t0 in tp (Thread Pointer)
         *  - tp: For tp that we can override it's value here because it
         * is designed for thread local storage (TLS) that we do not provide
         * in Ravenscar run-time. It is not saved & restored.
         *  - t0: We need to preserve it, for this we use the mscratch CSR
         *  as a temporary register.
         */
        csrrw   t0, mscratch, t0    /* Swap t0 <-> mscratch */

        /*
         */

        /* Check if sp is already in an interrupt stack (nested interrupt) */
        la      tp, __interrupt_stack_start
        bgt     tp, sp, not_in_interrupt_stack // SP is below the interrupt stack
        la      tp, __interrupt_stack_end
        blt     tp, sp, not_in_interrupt_stack // SP is above the interrupt stack

        mv      tp, sp
        j       in_interrupt_stack
not_in_interrupt_stack:

        /* Load the interrupt stack pointer.
         * We use tp as scratch here since we're about to overwrite it anyway.
         * This avoids clobbering t1 before we've saved it.
         */

        /* 1. Read physical hart id (1, 2, 3, or 4) */
        csrr    t0, mhartid

        /* 2. Hart 0 should never reach this code. */
        beqz    t0, infinite_hart_parking

        /* 3. Compute interrupt_stack_table[mhartid - 1] using tp as scratch */
        addi    t0, t0, -1          /* Convert to 0-based index */
        slli    t0, t0, 3           /* Multiply by 8 (pointer size) */
        la      tp, interrupt_stack_table
        add     t0, t0, tp
        LREG    tp, 0(t0)           /* tp now contains the Hart's stack top */


in_interrupt_stack:
        /* At this point tp contains the stack pointer for the trap
         * and mscratch contains the original t0 value that we need to
         * save.
         */

save_interrupted_context:

        /* Allocate a frame */
        addi tp, tp, -FRAME_SIZE

        /* Save the context of the interrupted task (Ti) in the interrupt
         * stack.
         */

        SREG x1, 0(tp)
        SREG x2, 1*REGBYTES(tp)
        SREG x3, 2*REGBYTES(tp)
        /* -- Start of special cases for x4 and x5(t0) -- */
        /* x4 is tp which we don't need to save - skip */
        /* x5 (t0) is in mscratch (we had to clobber it to compute the stack
         * pointer), so save the original value here.
         */
        csrr    t1, mscratch
        SREG    t1, 4*REGBYTES(tp)  /* x5 = t0 */
        /* -- End of special cases for x4 and x5 -- */
        SREG x6, 5*REGBYTES(tp)
        SREG x7, 6*REGBYTES(tp)
        SREG x8, 7*REGBYTES(tp)
        SREG x9, 8*REGBYTES(tp)
        SREG x10, 9*REGBYTES(tp)
        SREG x11, 10*REGBYTES(tp)
        SREG x12, 11*REGBYTES(tp)
        SREG x13, 12*REGBYTES(tp)
        SREG x14, 13*REGBYTES(tp)
        SREG x15, 14*REGBYTES(tp)
        SREG x16, 15*REGBYTES(tp)
        SREG x17, 16*REGBYTES(tp)
        SREG x18, 17*REGBYTES(tp)
        SREG x19, 18*REGBYTES(tp)
        SREG x20, 19*REGBYTES(tp)
        SREG x21, 20*REGBYTES(tp)
        SREG x22, 21*REGBYTES(tp)
        SREG x23, 22*REGBYTES(tp)
        SREG x24, 23*REGBYTES(tp)
        SREG x25, 24*REGBYTES(tp)
        SREG x26, 25*REGBYTES(tp)
        SREG x27, 26*REGBYTES(tp)
        SREG x28, 27*REGBYTES(tp)
        SREG x29, 28*REGBYTES(tp)
        SREG x30, 29*REGBYTES(tp)
        SREG x31, 30*REGBYTES(tp)



#ifndef __riscv_float_abi_soft
        FSREG f0, 31*REGBYTES(tp)
        FSREG f1, 32*REGBYTES(tp)
        FSREG f2, 33*REGBYTES(tp)
        FSREG f3, 34*REGBYTES(tp)
        FSREG f4, 35*REGBYTES(tp)
        FSREG f5, 36*REGBYTES(tp)
        FSREG f6, 37*REGBYTES(tp)
        FSREG f7, 38*REGBYTES(tp)
        FSREG f8, 39*REGBYTES(tp)
        FSREG f9, 40*REGBYTES(tp)
        FSREG f10, 41*REGBYTES(tp)
        FSREG f11, 42*REGBYTES(tp)
        FSREG f12, 43*REGBYTES(tp)
        FSREG f13, 44*REGBYTES(tp)
        FSREG f14, 45*REGBYTES(tp)
        FSREG f15, 46*REGBYTES(tp)
        FSREG f16, 47*REGBYTES(tp)
        FSREG f17, 48*REGBYTES(tp)
        FSREG f18, 49*REGBYTES(tp)
        FSREG f19, 50*REGBYTES(tp)
        FSREG f20, 51*REGBYTES(tp)
        FSREG f21, 52*REGBYTES(tp)
        FSREG f22, 53*REGBYTES(tp)
        FSREG f23, 54*REGBYTES(tp)
        FSREG f24, 55*REGBYTES(tp)
        FSREG f25, 56*REGBYTES(tp)
        FSREG f26, 57*REGBYTES(tp)
        FSREG f27, 58*REGBYTES(tp)
        FSREG f28, 59*REGBYTES(tp)
        FSREG f29, 60*REGBYTES(tp)
        FSREG f30, 61*REGBYTES(tp)
        FSREG f31, 62*REGBYTES(tp)
#endif
        /* At this point, we can use all registers and rely on classic calling conventions. */

        /* Set the stack pointer for the trap handler */
        mv sp, tp

        /* Save mepc. mepc will not be preserved through trap handler and
         * context switches call but s1 will.
         */
        csrr s1, mepc

        /* Call the trap handler */
        call __gnat_trap_handler

        /* Disable interrupts */
        li      t0, MSTATUS_MIE
        csrc    mstatus, t0

        /* Check if we need a context switch */
        call __gnat_context_switch_needed
        beqz a0, .Lno_context_switch

        /* Yes, we need a context switch */

        /* The context of the interrupted task (Ti) lives in the interrupt
         * stack because it was saved by the code above.
         */

        /* The call to __gnat_trap_context_switch will save the trap/interrupt
         * context (where we are now) in the context buffer of Ti, load the
         * context of another task, and jump to the code of this other task.
         */

        /* Save interrupt stack pointer in to make sure Ti context is not
         * smashed if a trap occurs when the next task is running.
         */
        GET_HART_STACK_TABLE_ADDR
        SREG    sp, 0(t0)

        /* Context switch from a trap */
        call __gnat_pre_context_switch
        call __gnat_context_switch

        /* When Ti will be scheduled again, the context switch will restore the
         * trap/interrupt context (where we are now). So the execution will
         * resume from here and the following code will restore Ti context
         * from the interrupt stack and resume Ti.
         */

        /* Deallocate the frame on the interrupt stack */
        addi    t2, sp, FRAME_SIZE
        GET_HART_STACK_TABLE_ADDR
        SREG    t2, 0(t0)

.Lno_context_switch:

        /* Remain in M-mode after mret */
        li      t0, MSTATUS_MPP
        csrs    mstatus, t0

        /* Enable interrupt after mret */
        li      t0, MSTATUS_MPIE
        csrs    mstatus, t0

        /* Restore mepc */
        csrw    mepc, s1

        /* Use tp as temporary register again */
        mv      tp, sp

        /* Restore Ti context */

load_interrupted_context:

        LREG x1, 0*REGBYTES(tp)
        LREG x2, 1*REGBYTES(tp)
        LREG x3, 2*REGBYTES(tp)
        /* x4 is tp which we didn't save here */
        LREG x5, 4*REGBYTES(tp)
        LREG x6, 5*REGBYTES(tp)
        LREG x7, 6*REGBYTES(tp)
        LREG x8, 7*REGBYTES(tp)
        LREG x9, 8*REGBYTES(tp)
        LREG x10, 9*REGBYTES(tp)
        LREG x11, 10*REGBYTES(tp)
        LREG x12, 11*REGBYTES(tp)
        LREG x13, 12*REGBYTES(tp)
        LREG x14, 13*REGBYTES(tp)
        LREG x15, 14*REGBYTES(tp)
        LREG x16, 15*REGBYTES(tp)
        LREG x17, 16*REGBYTES(tp)
        LREG x18, 17*REGBYTES(tp)
        LREG x19, 18*REGBYTES(tp)
        LREG x20, 19*REGBYTES(tp)
        LREG x21, 20*REGBYTES(tp)
        LREG x22, 21*REGBYTES(tp)
        LREG x23, 22*REGBYTES(tp)
        LREG x24, 23*REGBYTES(tp)
        LREG x25, 24*REGBYTES(tp)
        LREG x26, 25*REGBYTES(tp)
        LREG x27, 26*REGBYTES(tp)
        LREG x28, 27*REGBYTES(tp)
        LREG x29, 28*REGBYTES(tp)
        LREG x30, 29*REGBYTES(tp)
        LREG x31, 30*REGBYTES(tp)

#ifndef __riscv_float_abi_soft
        FLREG f0, 31*REGBYTES(tp)
        FLREG f1, 32*REGBYTES(tp)
        FLREG f2, 33*REGBYTES(tp)
        FLREG f3, 34*REGBYTES(tp)
        FLREG f4, 35*REGBYTES(tp)
        FLREG f5, 36*REGBYTES(tp)
        FLREG f6, 37*REGBYTES(tp)
        FLREG f7, 38*REGBYTES(tp)
        FLREG f8, 39*REGBYTES(tp)
        FLREG f9, 40*REGBYTES(tp)
        FLREG f10, 41*REGBYTES(tp)
        FLREG f11, 42*REGBYTES(tp)
        FLREG f12, 43*REGBYTES(tp)
        FLREG f13, 44*REGBYTES(tp)
        FLREG f14, 45*REGBYTES(tp)
        FLREG f15, 46*REGBYTES(tp)
        FLREG f16, 47*REGBYTES(tp)
        FLREG f17, 48*REGBYTES(tp)
        FLREG f18, 49*REGBYTES(tp)
        FLREG f19, 50*REGBYTES(tp)
        FLREG f20, 51*REGBYTES(tp)
        FLREG f21, 52*REGBYTES(tp)
        FLREG f22, 53*REGBYTES(tp)
        FLREG f23, 54*REGBYTES(tp)
        FLREG f24, 55*REGBYTES(tp)
        FLREG f25, 56*REGBYTES(tp)
        FLREG f26, 57*REGBYTES(tp)
        FLREG f27, 58*REGBYTES(tp)
        FLREG f28, 59*REGBYTES(tp)
        FLREG f29, 60*REGBYTES(tp)
        FLREG f30, 61*REGBYTES(tp)
        FLREG f31, 62*REGBYTES(tp)
#endif

        /* Return from trap */
        mret
