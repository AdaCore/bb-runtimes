        top_of_stack   = 0x200000

        .section ".text"
        .global _start

_start:
	/* Clear bss.  */
        addis	1,0,(__bss_start-4)@h
        ori	1,1,(__bss_start-4)@l
        addis	2,0,__end@h
        ori	2,2,__end@l
	li	3,0

.Lbssloop:
	stwu	%r3,4(%r1)
        cmpw	%r1,%r2
	blt	.Lbssloop

	/* Init stack pointer.  */
        addis	1,0,top_of_stack@h
        ori	1,1,top_of_stack@l

	/* Call main.  */
        bl      main

	.globl exit
exit:
        li	3,0
	li	10,0x63
        sc

	.globl putchar
putchar:
	li	10,0x20
	sc
	blr
