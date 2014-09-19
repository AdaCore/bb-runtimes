	.cpu cortex-r4f
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 2
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4

	.text
	.align	2
	.global	__gnat_context_switch
	.type	__gnat_context_switch, %function
__gnat_context_switch:
	@ ip = running thread context
        ldr     r0, Lrun
        ldr     ip, [r0]

        @ Save context
        stmia   ip!, {r4-r11,sp,lr}
	vstmia	ip!, {s16-s31}
	vmrs	r1,fpscr
	str	r1,[ip]

        @ Running thread = first_thread
        ldr     ip, Lfirst
        ldr     ip, [ip]
        str     ip, [r0]

        @ Restore context
        ldmia   ip!, {r4-r11,sp,lr}
	vldmia	ip!, {s16-s31}
	ldr	r1,[ip]
	vmsr	fpscr,r1
        bx	lr

        .align 2
Lrun:
        .word   __gnat_running_thread_table
Lfirst:
        .word   first_thread_table

	.size	__gnat_context_switch, .-__gnat_context_switch

	.globl __gnat_start_thread
__gnat_start_thread:
	mov	r0, r4
	blx	r5
0:	b	0b

	.size __gnat_start_thread, . - __gnat_start_thread
