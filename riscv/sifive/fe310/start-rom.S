        /**************/
        /* _start_rom */
        /**************/

        .section .start
        .globl _start_rom
        .type _start_rogm,@function

_start_rom:
        la gp, __global_pointer
        la sp, __stack_end

        /* Load data section */
        la a0, __data_load
        la a1, __data_start
        la a2, __data_end
        bgeu a1, a2, 2f
1:
        lw t0, (a0)
        sw t0, (a1)
        addi a0, a0, 4
        addi a1, a1, 4
        bltu a1, a2, 1b
2:

        /* Clear bss section */
        la a0, __bss_start
        la a1, __bss_end
        bgeu a0, a1, 2f
1:
        sw zero, (a0)
        addi a0, a0, 4
        bltu a0, a1, 1b
2:

#define MSTATUS_FS          0x00006000

        /* Enable FPU */
        li   a0, MSTATUS_FS  /* Set FPU bit */
        csrs mstatus, a0     /* Move to mstatus register */
        csrr a1, mstatus     /* Move from mstatus register */
        and  a1, a1, a0      /* Check that the bit is actually set */
        beqz a1, 1f
        fssr x0              /* Set rounding mode */
1:

        call main
        call _exit
2:      j 2b

        .globl _exit
        .type _exit,@function
        .globl _abort
        .type abort,@function
abort:
_exit:
        ebreak
        j _exit
