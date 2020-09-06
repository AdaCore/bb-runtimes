        /**************/
        /* _start_rom */
        /**************/

        .section .start
        .globl _start_rom
        .type _start_rom,@function

_start_rom:
.option push
.option norelax
        la gp, __global_pointer$
.option pop
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

        call main
        call __gnat_exit
2:      j 2b

        .globl __gnat_exit
        .type __gnat_exit,@function
        .globl _abort
        .type abort,@function
abort:
__gnat_exit:

        /* Write to the SiFive Test device on QEMU to shutdown */
        li t0, 0x5555
        li t1, 0x100000
        sw t0, (t1)

        j __gnat_exit

        /* Weak alias _exit to __gnat_exit */
        .weak      _exit
        .set _exit,__gnat_exit
