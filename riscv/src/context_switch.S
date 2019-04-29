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

        # The context address of the current thread is in a0
        # Read current thread,
        LREG     t0, 0(a0)

        SREG ra, 0*REGBYTES(t0)
        SREG sp, 1*REGBYTES(t0)
        SREG s0, 2*REGBYTES(t0)
        SREG s1, 3*REGBYTES(t0)
        SREG s2, 4*REGBYTES(t0)
        SREG s3, 5*REGBYTES(t0)
        SREG s4, 6*REGBYTES(t0)
        SREG s5, 7*REGBYTES(t0)
        SREG s6, 8*REGBYTES(t0)
        SREG s7, 9*REGBYTES(t0)
        SREG s8, 10*REGBYTES(t0)
        SREG s9, 11*REGBYTES(t0)
        SREG s10, 12*REGBYTES(t0)
        SREG s11, 13*REGBYTES(t0)

#ifndef __riscv_float_abi_soft
        FSREG fs0, 14*REGBYTES(t0)
        FSREG fs1, 15*REGBYTES(t0)
        FSREG fs2, 16*REGBYTES(t0)
        FSREG fs3, 17*REGBYTES(t0)
        FSREG fs4, 18*REGBYTES(t0)
        FSREG fs5, 19*REGBYTES(t0)
        FSREG fs6, 20*REGBYTES(t0)
        FSREG fs7, 21*REGBYTES(t0)
        FSREG fs8, 22*REGBYTES(t0)
        FSREG fs9, 23*REGBYTES(t0)
        FSREG fs10, 24*REGBYTES(t0)
        FSREG fs11, 25*REGBYTES(t0)
#endif

        # Load the context address of the next thread
        LREG   a1, 0(a1)

        # Set the new value of Running_Thread
        SREG   a1, 0(a0)

        LREG ra, 0*REGBYTES(a1)
        LREG sp, 1*REGBYTES(a1)
        LREG s0, 2*REGBYTES(a1)
        LREG s1, 3*REGBYTES(a1)
        LREG s2, 4*REGBYTES(a1)
        LREG s3, 5*REGBYTES(a1)
        LREG s4, 6*REGBYTES(a1)
        LREG s5, 7*REGBYTES(a1)
        LREG s6, 8*REGBYTES(a1)
        LREG s7, 9*REGBYTES(a1)
        LREG s8, 10*REGBYTES(a1)
        LREG s9, 11*REGBYTES(a1)
        LREG s10, 12*REGBYTES(a1)
        LREG s11, 13*REGBYTES(a1)

#ifndef __riscv_float_abi_soft
        FLREG fs0, 14*REGBYTES(t0)
        FLREG fs1, 15*REGBYTES(t0)
        FLREG fs2, 16*REGBYTES(t0)
        FLREG fs3, 17*REGBYTES(t0)
        FLREG fs4, 18*REGBYTES(t0)
        FLREG fs5, 19*REGBYTES(t0)
        FLREG fs6, 20*REGBYTES(t0)
        FLREG fs7, 21*REGBYTES(t0)
        FLREG fs8, 22*REGBYTES(t0)
        FLREG fs9, 23*REGBYTES(t0)
        FLREG fs10, 24*REGBYTES(t0)
        FLREG fs11, 25*REGBYTES(t0)
#endif

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
