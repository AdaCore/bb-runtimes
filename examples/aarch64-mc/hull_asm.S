	.text
	
	.type start_hull, %function
	.globl start_hull
start_hull:
	//  r0: mmu table
	//  r1: entry point
	
	//  Enable stage 2
	//  VTCR_EL2: PS=000 (4GB), TG0=00, SH0=10, ORGN0=01, IRGN0=01, SL0=00,
	//            T0SZ=32
	ldr	x2,=((0 << 16) | (0 << 14) | (2 << 12) | (1 << 10) | (1 << 8) \
		   | (0 << 6) | (38 << 0))
	msr	vtcr_el2, x2

	//  VTTBR_EL2
	msr	vttbr_el2, x0
	
	//  HCR_EL2: RW(31), DC(12), IMO(4), FMO(3), VM(0)
	ldr	x3,=((1<<31) | (1 << 12) | (1 << 4) | (1 << 3) | (1 << 0))
//	orr	x3, x3, #(1 << 27) //  TGE (only when EL0!!)
	msr	hcr_el2, x3

	//  VBAR_EL1 (to detect early errors)
	mov	x4, #-0x4000
	msr	vbar_el1, x4

	//  Re-route debug: set MDCR_EL2.TDE
	mrs	x4, mdcr_el2
	orr	x4, x4, #(1<<8)
	msr	mdcr_el2, x4

	//  Enable SS: set MDSCR_EL1.SS(1) and MDSCR_EL1.KDE(13)
	mrs	x4, mdscr_el1
	orr	x4, x4, #1
	orr	x4, x4, #(1 << 13)
	msr	mdscr_el1, x4
	
	//  Clear SP
	mov	x4, #256
	msr	sp_el1, x4
	msr	sp_el0, x4

	//  Mask interrupts (needed as we are setting spsr_el2 and elr_el2).
	//  They will be unmasked in EL1.
	msr	DAIFset, #3

	//  Switch to el1h, AIF set
	mov	x4, #(0x1c0 + 5)  // el1h
//	mov	x4, #(0x1c0 + 0)  // el0
//	orr	x4, x4, #(1 << 21)   //  Set SS
	msr	spsr_el2, x4
	msr	elr_el2, x1
	isb

	//  TODO: clear all registers to avoid leaks
	
	eret
	.size	start_hull, . - start_hull

