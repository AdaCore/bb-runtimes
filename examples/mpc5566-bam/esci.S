	.text
	.global _start
_start:
	lis	%r5,0xc3f90000@h
	li	%r4,3
	stw	%r4,0(%r5)	/* Divide sysclk by 3+1 for CLKOUT */

	# Initialize PLL to 64Mhz
	# Fsys = Fref * (MFD+4) / ((PREDIV + 1) * 2 ** RFD)
	lis	%r5,0xc3f80000@h
	lis	%r4,0x16080000@h /* PREDIV=1, MFD=12 RFD=1.  */
		                 /* Fsys =  8 * 16 / (2 * 2) = 32.  */
	stw	%r4,0(%r5)	/* 8 MHz xtal: 0x16080000; */
0:	lwz	%r4,4(%r5)	/* Wait for FMPLL to LOCK */
	andi.	%r4,%r4,8
	beq	0b
	lis	%r4,0x16000000@h /* prediv=1, mfd=12 rfd=0.  */
	                         /* Fsys =  8 * 16 / (2 * 1) = 64.  */
	stw	%r4,0(%r5)	/* 8 MHz xtal: 0x16080000; */

	# Init ESCI_A
	lis	%r3,0xfffb0000@h	# Base
	# Module is enabled (default setting )
	li	%r4,0x2000
	sth	%r4,4(%r3) # CR2
	# 9600 baud, 8 bits, no parity, Tx & Rx enabled
#cr1=0x004e000c 8Mhz
cr1=0x01a0000c /* 64Mhz */
	lis	%r4, cr1@h
	ori	%r4,%r4,cr1@l
	stw	%r4,0(%r3) # CR1

	# Configure pad
	lis	%r5,0xc3f90000@h
	li	%r4,0x400
	sth	%r4,0xf2(%r5) # TxDA
	sth	%r4,0xf4(%r5) # RxDA

	li	%r7,64
	sth	%r7,6(%r3)

	# Transmit
	li	%r6,0x4000
0:
	lis	%r5,(msg-1)@h
	ori	%r5,%r5,(msg-1)@l
1:
	lbzu	%r7,1(%r5)	# next message byte
	cmpwi	%r7,0		# end of message
	beq	3f
	# Wait for TxRDY = 1
2:
	lwz	%r4,8(%r3)	# ESCI.SR
	andi.	%r4,%r4,0x4000	# TXRDY
	beq	2b
	# Clear TxRDY flag
	stw	%r6,8(%r3)
	# Write byte
	sth	%r7,6(%r3)
	b	1b

3:
	lwz	%r4,8(%r3)	# ESCI.SR
	andi.	%r4,%r4,0x8000	# RXRDY
	beq	3b
	ori	%r7,%r0,0x8000
	stw	%r7,8(%r3)
	lhz	%r7,6(%r3)
	b	0b
	
msg:	.ascii "Hello world from 5566\r\n\0"
