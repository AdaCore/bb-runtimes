	.file "crtB.asm"
	.align 2
	.text
	.globl	_start
	.type	_start, @function
_start:
	/*
	This startup code is for a program being run out
	internal RAM on the 5554 using nothing but internal
        RAM starting a 4000_0000.  This memory is set up in TLB
        3 by the BAM.
	*/



        /* Set the top of the stack at __stack and create initial null
           frame.
        */
        lis     1,    __stack@h
        ori     1, 1, __stack@l
        addi    1, 1, -16
        lis     0, 0
        stw     0, 0(1)
	

	bl	main

	.globl abort
abort:
	.globl _after_main
_after_main:
	trap
	.size _start, .-_start

	# The compiler presumes that the eabi small data areas
	# are being used and generates a call to __eabi to set
	# up the registers for relative addressing. Here we provide
	# an empty function since we are not using the small data
	# areas.
	.global __eabi
__eabi:
	blr

