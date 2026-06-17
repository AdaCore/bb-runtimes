	.text
	.global _start_ram
	.type _start_ram,@function
_start_ram:
	li	%r0,0

        # Clear sbss2
	lis	%r11,(__sbss2_start - 1)@ha
	la	%r11,(__sbss2_start - 1)@l(%r11)
	lis	%r10,(__sbss2_end)@ha
        la	%r10,(__sbss2_end)@l(%r10)
        subf	%r10,%r11,%r10
	mtctr	%r10
	b	1f
0:	stbu	%r0,1(%r11)
1:
	bdnz	0b

        # Clear sbss
	lis	%r11,(__sbss_start - 1)@ha
	la	%r11,(__sbss_start - 1)@l(%r11)
	lis	%r10,(__sbss_end)@ha
        la	%r10,(__sbss_end)@l(%r10)
        subf	%r10,%r11,%r10
	mtctr	%r10
	b	1f
0:	stbu	%r0,1(%r11)
1:
	bdnz	0b

        # Clear bss
	lis	%r11,(__bss_start - 1)@ha
	la	%r11,(__bss_start - 1)@l(%r11)
	lis	%r10,(__bss_end)@ha
        la	%r10,(__bss_end)@l(%r10)
        subf	%r10,%r11,%r10
	mtctr	%r10
	b	1f
0:	stbu	%r0,1(%r11)
1:
	bdnz	0b

        # Load the new stack pointer
        lis	%r11,(__stack_end - 16)@h
        ori	%r11,%r11,(__stack_end - 16)@l
        # back-link to the previous one.
        stw	%r1,0(%r11)
        # Set the stack.
        mr      %r1,%r11
        # Save lr
        mflr	%r0
        stw	%r0,4(%r1)

	# Setup spe, r2, r13
	bl	_setup

#ifdef CALL__init
	# Call constructors
	bl    __init
#endif

        # Run the program
        bl	main

        # Reset the board.
        bl      _exit

	.size _start_ram, . - _start_ram
