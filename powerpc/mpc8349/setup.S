/* This is an -*- asm -*- file.  */
	.section ".text"
        .global _setup

_setup:
	mfmsr	%r3

#ifndef _SOFT_FLOAT
        # Enable FPU (bit FP)
	ori	%r3,%r3,0x2000
#endif

	# Clear prefix (bit IP)
	li	%r4,0x40
	andc	%r3,%r3,%r4

	mtmsr	%r3
	isync

#ifndef _SOFT_FLOAT
	# Disable overflow and underflow exceptions.
	mtfsb0	25
	mtfsb0	26
#endif

	li	%r0, 0

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
	blr
	.size _setup, . - _setup

	.global __eabi
__eabi:
	blr
	.size __eabi, . - __eabi
