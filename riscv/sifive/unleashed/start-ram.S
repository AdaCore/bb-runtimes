#include "riscv_def.h"

        /**************/
        /* _start_ram */
        /**************/

        .section .start
        .globl _start_ram
        .type _start_rom,@function

_start_ram:
.option push
.option norelax
        la gp, __global_pointer$
.option pop
        la sp, __stack_end

        /* The run-time doesn't support SMP yet, so all harts with an ID
         * different than one are put in an infinite loop.
         *
         * We select hart one because it is the first core other than the
         * monitor, and the monitor doesn't have floating point support.
         */
        li   t0, 1 /* hart id that will be allowed to run */
        csrr t1, mhartid
        bne t0, t1, infinite_loop

        /* Disable interrupts */
        li      t0, MSTATUS_MIE
        csrc    mstatus, t0

        /* Clear time, software and external interrupt enable*/
        csrw mie, zero

        /* Clear bss section */
        la a0, __bss_start
        la a1, __bss_end
        bgeu a0, a1, 2f
1:
        sw zero, (a0)
        addi a0, a0, 4
        bltu a0, a1, 1b
2:

#ifndef __riscv_float_abi_soft
        /* Enable the FPU */
        li      t0, MSTATUS_FS
        csrs    mstatus, t0
#endif

        call main
        call __gnat_exit

infinite_loop:
        wfi
        j infinite_loop

        .globl __gnat_exit
        .type __gnat_exit,@function
        .globl _abort
        .type abort,@function
abort:
__gnat_exit:

        /* Write success exit code to the QEMU sifive_u test device */
        li t0, 0x100000
        li t1, 0x5555
        sw t1, (t0)

        j __gnat_exit

        /* Weak alias _exit to __gnat_exit */
        .weak      _exit
        .set _exit,__gnat_exit
