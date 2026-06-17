/* This is an -*- asm -*- file.  */
	.section ".text"
        .global _start_ram

_start_ram:
  	# Load the new stack pointer
        lis	%r11,(__stack_end - 16)@h
        ori	%r11,%r11,(__stack_end - 16)@l
        # back-link to the previous one.
        stw	%r1,0(%r11)
        # Set the stack.
        mr      %r1,%r11

        mflr	%r0
        stw	%r0,12(%r1)
        bl	_setup
        bl	main
        lwz	%r0,12(%r1)
        lwz	%r1,0(%r1)
        mtlr	%r0
        blr
	.size _start_ram, . - _start_ram
