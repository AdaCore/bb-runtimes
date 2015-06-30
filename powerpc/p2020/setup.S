/* This is an -*- asm -*- file.  */
SPEFSCR = 512
MSR_SPE = (1 << (63 - 38))

	.section ".text"
	.global _setup
	.type _setup,@function
_setup:
	# Enable SPE
	mfmsr %r3
	oris  %r3,%r3,MSR_SPE@h
	mtmsr %r3
	isync

	# Enable overflow, division by zero, invalid exceptions.
	li	%r0,0x34
	mtspr	SPEFSCR,%r0

	# load r13 with _SDA_BASE_ address
	lis	%r13,_SDA_BASE_@ha
	la	%r13,_SDA_BASE_@l(%r13)

	# load r2 with _SDA2_BASE_ address
	lis     %r2,_SDA2_BASE_@ha
	la      %r2,_SDA2_BASE_@l(%r2)

	# Clear sbss2
	li	%r0,0
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
	blr
	.size _setup, . - _setup

	.global __eabi
__eabi:
	blr
	.size __eabi, . - __eabi
