       /* Define the Host/Target Interface symbols:
        * We don't really need them in the Ada code but QEMU support is looking
	* at them to define the address and size of HTIF registers.
        */

        .section .host_target_interface
	.globl tohost
        .size tohost, 8
tohost:
.skip 8

	.globl fromhost
        .size fromhost, 8
fromhost:
.skip 8


        /**************/
        /* _start_ram */
        /**************/

        .section .start
	.globl _start_ram
	.type _start_ram,@function

_start_ram:
	la gp, __global_pointer$
	la sp, __stack_end

	/* Clear bss section */
	la a0, __bss_start
	la a1, __bss_end
	bgeu a0, a1, 2f
1:
	sd zero, (a0)
	addi a0, a0, 8
	bltu a0, a1, 1b
2:

	call main
        call _exit
2:      j 2b
