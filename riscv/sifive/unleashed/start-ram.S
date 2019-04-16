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

        call main
        call __gnat_exit
2:      j 2b

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
