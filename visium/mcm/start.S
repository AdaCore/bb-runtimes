        .text
        
        .align  4
        .global __start
__start:

        /* Initialize the stack pointer. */
        moviu   r1,%u _estack
        movil   r1,%l _estack
	move.l	r23,r1

	/* Terminate the frame chain.  */
        move.l  r22,r0

        /* Call main call main (0, 0)  */

        moviu   r21,%u main
        movil   r21,%l main     ! address of main
	move.l	r1,r0
        bra     tr,r21,r21
        move.l  r2,r0

	/* Halt if the main program comes back here */
        
	moviu	r2,%u exit
	movil	r2,%l exit
	bra	tr,r2,r21
        move.l  r1, r0
        
	nop
	nop
        .end

