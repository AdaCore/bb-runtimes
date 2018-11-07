##############################################################################
#                                                                            #
#                         Copyright (C) 2019, AdaCore                        #
#                                                                            #
#                               Assembly File                                #
#                                                                            #
##############################################################################

#include "riscv_def.h"

        #  Functions to store and restore the
        #  context of a task for RISC-V.

        # Switch from running_thread (address in a0) to
        # first_thread (address in a1).

        .globl __gnat_context_switch
        .type  __gnat_context_switch, @function
__gnat_context_switch:

        # Save __gnat_running_thread_table pointer in mscratch
        # FIXME: If we only save/restore the callee-saved register, we can use
        #         t0 here instead of mscratch
        csrw    mscratch, a0

        # The context address of the current thread is in a0
        # Read current thread,
        LREG     a0, 0(a0)

        # FIXME: Do we need to save and load all the registers?
        #         Or only the callee-saved ones?

        SREG x1, 0(a0)
        SREG x2, 1*REGBYTES(a0)
        SREG x3, 2*REGBYTES(a0)
        SREG x4, 3*REGBYTES(a0)
        SREG x5, 4*REGBYTES(a0)
        SREG x6, 5*REGBYTES(a0)
        SREG x7, 6*REGBYTES(a0)
        SREG x8, 7*REGBYTES(a0)
        SREG x9, 8*REGBYTES(a0)
        /* Skip a0/x10*/
        /* SREG x10, 9*REGBYTES(a0) */
        SREG x11, 10*REGBYTES(a0)
        SREG x12, 11*REGBYTES(a0)
        SREG x13, 12*REGBYTES(a0)
        SREG x14, 13*REGBYTES(a0)
        SREG x15, 14*REGBYTES(a0)
        SREG x16, 15*REGBYTES(a0)
        SREG x17, 16*REGBYTES(a0)
        SREG x18, 17*REGBYTES(a0)
        SREG x19, 18*REGBYTES(a0)
        SREG x20, 19*REGBYTES(a0)
        SREG x21, 20*REGBYTES(a0)
        SREG x22, 21*REGBYTES(a0)
        SREG x23, 22*REGBYTES(a0)
        SREG x24, 23*REGBYTES(a0)
        SREG x25, 24*REGBYTES(a0)
        SREG x26, 25*REGBYTES(a0)
        SREG x27, 26*REGBYTES(a0)
        SREG x28, 27*REGBYTES(a0)
        SREG x29, 28*REGBYTES(a0)
        SREG x30, 29*REGBYTES(a0)
        SREG x31, 30*REGBYTES(a0)

#ifndef _SOFT_FLOAT
        # FIXME: save FPU context
#endif

        # Load the context address of the next thread
        LREG    a1, 0(a1)

        # Load __gnat_running_thread_table pointer from mscratch (saved above)
        csrr    a0, mscratch

        # Set the new value of Running_Thread
        SREG   a1, 0(a0)

#ifndef _SOFT_FLOAT
        # FIXME: load FPU context
#endif

        LREG x1, 0*REGBYTES(a1)
        LREG x2, 1*REGBYTES(a1)
        LREG x3, 2*REGBYTES(a1)
        LREG x4, 3*REGBYTES(a1)
        LREG x5, 4*REGBYTES(a1)
        LREG x6, 5*REGBYTES(a1)
        LREG x7, 6*REGBYTES(a1)
        LREG x8, 7*REGBYTES(a1)
        LREG x9, 8*REGBYTES(a1)
        LREG x10, 9*REGBYTES(a1)
        /* Skip a1/x11 */
        /* LREG x11, 10*REGBYTES(a1) */
        LREG x12, 11*REGBYTES(a1)
        LREG x13, 12*REGBYTES(a1)
        LREG x14, 13*REGBYTES(a1)
        LREG x15, 14*REGBYTES(a1)
        LREG x16, 15*REGBYTES(a1)
        LREG x17, 16*REGBYTES(a1)
        LREG x18, 17*REGBYTES(a1)
        LREG x19, 18*REGBYTES(a1)
        LREG x20, 19*REGBYTES(a1)
        LREG x21, 20*REGBYTES(a1)
        LREG x22, 21*REGBYTES(a1)
        LREG x23, 22*REGBYTES(a1)
        LREG x24, 23*REGBYTES(a1)
        LREG x25, 24*REGBYTES(a1)
        LREG x26, 25*REGBYTES(a1)
        LREG x27, 26*REGBYTES(a1)
        LREG x28, 27*REGBYTES(a1)
        LREG x29, 28*REGBYTES(a1)
        LREG x30, 29*REGBYTES(a1)
        LREG x31, 30*REGBYTES(a1)

        ret
        .size __gnat_context_switch, . - __gnat_context_switch

        .globl __gnat_start_thread
        .type  __gnat_start_thread, @function
__gnat_start_thread:
        # x9/s0 contains the thread argument, x18/s1 contains the start address
        # (PC)

        # Set the thread argument in a0
        mv a0, x9

        # Jump
        jalr x18
        .size __gnat_start_thread, . - __gnat_start_thread
