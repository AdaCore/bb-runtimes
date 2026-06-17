#include "riscv_def.h"

        /**************/
        /* _start_ram */
        /**************/

        .section .start
        .globl _start_ram
        .type _start_ram,@function

_start_ram:

        /* The first few instructions are not executed correctly after reset,
           so pad with some nops. */
        nop
        nop
        nop
        nop
        nop

.option push
.option norelax
        la gp, __global_pointer$
        la sp, __stack_end
.option pop

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

        /* Run global constructors */
        call __libc_init_array

        /* Call the main routine */
        call main

        /* Save returned value before calling the global destructors */
        mv s0, a0

        /* Cleanup global destructors */
        call __libc_fini_array

        /* restore exit status code */
        mv a0, s0

        call exit
