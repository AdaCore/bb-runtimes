	.text
.Lptr:
	.globl	_start
	.type	_start,@function
_start:
	/*
	This startup code is for a program being run out
	internal RAM on the 5634, and TLB entry 3 is mapping
	that internal RAM. TLB entry 0 may be in use for internal
	flash. The following TLB programming is for a program using
	SCI_A and sets up TLB entries to access the SIU, PLL and eSCI
	units. Cacheing is Inhibited and Guarded.
	*/


	lis	2,   0x1001		/* TLBL=1, ESEL=1 */
	mtspr	624, 2
	lis	2,   0x8000
	ori	2,   2,    0x200	/* Valid, 16Kbytes */
	mtspr  625, 2
	lis	2,   0xc3f9
	ori	2,   2,    0xA		/* SIU Base, cache Inhibited/Guarded */
	mtspr	626, 2
	lis	2,   0xc3f9
	ori     2,   2,    0xf       	/* RPN=EPN, User and Super R/W */
	mtspr   627, 2
	lis	2,   0
	mtspr   630, 2
	tlbwe
	isync

	lis	2,   0x1004		/* TLBL=1, ESEL=4 */
	mtspr	624, 2
	lis	2,   0x8000
	ori	2,   2,    0x200	/* Valid, 16Kbytes */
	mtspr  625, 2
	lis	2,   0xc3f8
	ori	2,   2,    0xA		/* PLL Base, cache Inhibited/Guarded */
	mtspr	626, 2
	lis	2,   0xc3f8
	ori     2,   2,    0xf       	/* RPN=EPN, User and Super R/W */
	mtspr   627, 2
	lis	2,   0
	mtspr   630, 2
	tlbwe
	isync


	lis	2,   0x1002		/* TLBL=1, ESEL=2 */
	mtspr	624, 2
	lis	2,   0x8000
	ori	2,   2,    0x200	/* Valid, 16Kbytes */
	mtspr  625, 2
	lis	2,   0xfffb
	ori	2,   2,    0xA		/* SCI Base, cache Inhibited/Guarded */
	mtspr	626, 2
	lis	2,   0xfffb
	ori     2,   2,    0xf       	/* RPN=EPN, User and Super R/W */
	mtspr   627, 2
	lis	2,   0
	mtspr   630, 2
	tlbwe
	isync

	lis	2,   0x1005		/* TLBL=1, ESEL=5 */
	mtspr	624, 2
	lis	2,   0x8000
	ori	2,   2,    0x200	/* Valid, 16Kbytes */
	mtspr  625, 2
	lis	2,   0xc3fa
	ori	2,   2,    0xA		/* eMIOS Base, cache Inhibit/Guard */
	mtspr	626, 2
	lis	2,   0xc3fa
	ori     2,   2,    0xf       	/* RPN=EPN, User and Super R/W */
	mtspr   627, 2
	lis	2,   0
	mtspr   630, 2
	tlbwe
	isync

	lis	2,   0x1006		/* TLBL=1, ESEL=6 */
	mtspr	624, 2
	lis	2,   0x8000
	ori	2,   2,    0x500	/* Valid, 1Mbyte */
	mtspr  625, 2
	lis	2,   0xfff0
	ori	2,   2,    0xA		/* Platform Base, cache Inhibit/Guard */
	mtspr	626, 2
	lis	2,   0xfff0
	ori     2,   2,    0xf       	/* RPN=EPN, User and Super R/W */
	mtspr   627, 2
	lis	2,   0
	mtspr   630, 2
	tlbwe
	isync

	/* Disable the watchdog timer by clearing SWT_CR [WEN] */
        lis     1, 0xfff3
        ori     1, 1, 0x8000
        lwz     2, 0(1)
        andi.   2, 2, 0xfffe
        stw     2, 0(1)

        /* Set the top of the stack at __stack and create initial null
           frame.
        */
        lis     1,    __stack@h
        ori     1, 1, __stack@l
        addi    1, 1, -16
        lis     0, 0
        stw     0, 0(1)
	

	bl	main

	.globl _after_main
_after_main:
	bl	_exit
	trap

	# The compiler presumes that the eabi small data areas
	# are being used and generates a call to __eabi to set
	# up the registers for relative addressing. Here we provide
	# an empty function since we are not using the small data
	# areas.
	.global __eabi
__eabi:
	blr

