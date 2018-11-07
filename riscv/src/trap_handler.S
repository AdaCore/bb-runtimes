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

        /* Note that we can erase tp (Thread Pointer) value here because it
         * designed for thread local storage (TLS) that we do not provide in
         * Ravenscar run-time.
         */

        /* Check if sp is already in an interrupt stack (nested interrupt) */
        la      tp, __interrupt_stack_start
        bgt     tp, sp, not_in_interrupt_stack // SP is below the interrupt stack
        la      tp, __interrupt_stack_end
        blt     tp, sp, not_in_interrupt_stack // SP is above the interrupt stack

        mv      tp, sp
        j       in_interrupt_stack

not_in_interrupt_stack:
        /* Load the interrupt stack pointer */
        la      tp, interrupt_stack_table
        LREG    tp, 0(tp)

in_interrupt_stack:

        /* At this point gp contains the stack pointer for the trap */

save_interrupted_context:

        /* Allocate a frame */
        addi tp, tp, -FRAME_SIZE

        /* Save the context of the interrupted task (Ti) in the interrupt
         * stack.
         */

        SREG x1, 0(tp)
        SREG x2, 1*REGBYTES(tp)
        SREG x3, 2*REGBYTES(tp)
        /* x4 is tp which we don't need to save */
        SREG x5, 4*REGBYTES(tp)
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

#ifndef _SOFT_FLOAT
        # FIXME: save FPU context
#endif

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
        la      t0, interrupt_stack_table
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
        addi    t1, sp, FRAME_SIZE
        la      t0, interrupt_stack_table
        SREG    t1, 0(t0)

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
#ifndef _SOFT_FLOAT
        # FIXME: load FPU context
#endif

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

        /* Return from trap */
        mret
