/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                          T R A P _ H A N D L E R                         *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                    Copyright (C) 2003-2019 AdaCore                       *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a SPARC specific implementation */

#include "sparc.h"

/*
   This is the service routine shared by all user handlers (for synchronous
   and asynchronous traps). It prepares the machine for safe execution of
   handlers by means of saving the state of the interrupted task, setting up
   the register windows, and enabling traps among other things.
   We enter this handler from the four instructions loaded in the trap table
   by the install handler routines. Apart from jumping to the user handler,
   these four instructions move the value of the PSR to l0 and the trap
   type (plus bit number 8 set to 1 for synchronous traps) to l3. When
   acknowledging a trap, the SPARC processor automatically copies the
   interrupted PC and nPC into l1 and l2 respectively. Hence, when starting
   the common handler the following values are in the local registers:
	 l0 <- PSR (when the trap handler started)
	 l1 <- PC (trapped pc)
	 l2 <- nPC
	 l3 <- Trap Type (TT) + synchronous bit
  */

	.text
	.align 4
	.global common_handler
	.type   common_handler,@function
common_handler:
	/* Switch to the interrupt stack, only if not already on it.  */
	mov	%fp, %l6
	set	__interrupt_stack_start, %l4
	cmp	%l6, %l4
	bl	.Ldo_switch_stack
	 set	__interrupt_stack_end, %l4
	cmp	%l6, %l4
	bl	.Lno_switch_stack
	 nop

.Ldo_switch_stack:
	CURRENT_CPU(%l7)
        sll     %l7, 2, %l5     /* offset in the threads table */
	set	interrupt_stack_table, %l4
	ld      [%l4 + %l5], %l6	! Load stack pointer (per-cpu)

.Lno_switch_stack:
	mov	%g4, %l4	! save the global registers this block uses
	mov	%g5, %l5
				! and %l6 contains the new stack pointer

	/* When the SPARC executes a trap handler, it automatically decrements
	   the CWP (like a save). We must check whether this new window is an
	   invalid one and process a window overflow if this is the case.
	   There is only one invalid window, thus only one bit set in WIM. */

	rd	%wim, %g4	! get the invalid window
	srl	%g4, %l0, %g5	! shift the WIM to the right CWP times
				! (shift count and CWP use only the 5
				! least significant bits)
	cmp	%g5, 1		! are we in the invalid window?
	bne	.Lno_window_ov	! no, skip window overflow on this window

	/* Window saving. This way we ensure that the trap handler will have,
	   at least, one window to use. */

	 srl	 %g4, 1, %g5
	sll	%g4, NUMBER_OF_REGISTER_WINDOWS - 1, %g4
	or	%g4, %g5, %g4	! rotate WIM register one bit to the right
	save			! decrement CWP

	wr	%g4, %wim	! next three instructions following wr
				! must not be restore, save nor rett

	/* Save the state of window next to the one where the trap handler
	   is executing. */

	std	%l0, [%sp + L0_OFFSET]	! save local registers on the stack
	NOP_FIX_GRLIB_TN_0009
	std	%l2, [%sp + L2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l4, [%sp + L4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l6, [%sp + L6_OFFSET]
	NOP_FIX_GRLIB_TN_0009

	std	%i0, [%sp + I0_OFFSET]	! save in registers on the stack
	NOP_FIX_GRLIB_TN_0009
	std	%i2, [%sp + I2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i4, [%sp + I4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i6, [%sp + I6_OFFSET]

	restore

.Lno_window_ov:
	/* We execute the following restore (and the subsequent save) so that
	   the debugger finds a standard prologue (call_trap_handler) at the
	   beginning of the corresponding local function, and hence it is able
	   to unwind through trap handlers. Otherwise, this function would
	   look like a frameless function, while it is not. Note that this
	   restore will move us to the interrupted frame, which cannot be
	   invalid, so that we do not need to worry about window underflow.
	   The global effect of the restore plus the following save is to
	   have a frame big enough for saving the state of the interrupted
	   task and for window overflow/underflow. */

	restore

	.size common_handler, . -common_handler

	.type   call_trap_handler,@function
call_trap_handler:
	/* Ensure that we have enough space for saving the state of the
	   interrupted task (for later retrieve it when finishing interrupt
	   handling), and space for saving this register window to the stack
	   (in case of a context switch before finishing interrupt handling).
	*/

	/* Make space for window overflow trap and for saving the interrupted
	   context on the stack.  */
	.cfi_startproc
	save	%sp, -INTERRUPT_STACK_FRAME_SIZE, %sp
	sub	%l6, INTERRUPT_STACK_FRAME_SIZE, %sp

	/* Note: return address in r15 (o7).  */
	.cfi_window_save
	.cfi_register	15,17	! o7 saved in l1
	.cfi_def_cfa_register 30

.Lsave_isf:
	/* Save the state of the interrupted task registers (PSR, PC, nPC, Y,
	   global, and input registers) in the Interrupt Stack Frame (ISF).
	   Note that the ISF includes a regular minimum stack frame which will
	   be used, if needed, by register window overflow and underflow
	   handlers. Note that global registers g4 and g5 are still saved in
	   l4 and l5 respectively. */

	rd	%y, %g4
	st	%g4, [%sp + ISF_Y_OFFSET]	! save Y
	! No NOP_FIX_GRLIB_TN_0009, as the first store size is <= 4
	std	%l0, [%sp + ISF_PSR_OFFSET]	! save PSR and PC
	NOP_FIX_GRLIB_TN_0009
	std	%l2, [%sp + ISF_NPC_OFFSET]	! save nPC and TT

	! skip g0
	NOP_FIX_GRLIB_TN_0009
	st	%g1, [%sp + ISF_G1_OFFSET]	! save g1
	! No NOP_FIX_GRLIB_TN_0009, as the first store size is <= 4
	std	%g2, [%sp + ISF_G2_OFFSET]	! save g2 and g3
	NOP_FIX_GRLIB_TN_0009
	std	%l4, [%sp + ISF_G4_OFFSET]	! save g4 and g5 (from copy)
	NOP_FIX_GRLIB_TN_0009
	std	%g6, [%sp + ISF_G6_OFFSET]	! save g6 and g7

	NOP_FIX_GRLIB_TN_0009
	std	%i0, [%sp + ISF_I0_OFFSET]	! save i0 and i1
	NOP_FIX_GRLIB_TN_0009
	std	%i2, [%sp + ISF_I2_OFFSET]	! save i2 and i3
	NOP_FIX_GRLIB_TN_0009
	std	%i4, [%sp + ISF_I4_OFFSET]	! save i4 and i5
	NOP_FIX_GRLIB_TN_0009
	std	%i6, [%sp + ISF_I6_OFFSET]	! save i6 and i7

.Lincrease_nesting_level:
	/* Increase the nesting level for the task handling the interrupt */

	/* Get the running task */

        CURRENT_CPU(%l4)

        set     __gnat_running_thread_table, %l5
        sll     %l4, 2, %l4     /* offset in the threads table */
        ld      [%l5 + %l4], %l5

	/* Increase the nesting level.  */
	ld	[%l5 + INT_NESTING_OFFSET], %l6
	add	%l6, 1, %l6
	st	%l6, [%l5 + INT_NESTING_OFFSET]

	mov	%l3, %g3			! make a copy of Trap Type
						! it will be passed as param
						! for the trap handler

	andn	 %g3, SYNCHRONOUS_TRAP_BIT_MASK, %g3	! clear sync bit

	/* If this is an external interrupt (TT between 0x11 and 0x1f), two
	   things must be done: 1) set the PIL to mask off interrupts and
	   2) change the stack. Note that the original PSR stored in l0 is
	   not modified since it will be restored when the interrupt handler
	   returns. */

	cmp	%g3, 0x11			! g3 contains the trap type
	bl	.Lexecute_handler
	 cmp     %g3, 0x1f
	bg      .Lexecute_handler
	 nop

.Lfix_pil:
	or	%l0, PSR_PIL_MASK, %l0		! mask all interrupts
	/* Note: We use two wr instructions to change the PIL and the ET flag
	   in the PSR, as specified in the SPARC v7.0 Instruction Set
	   manual. */

	wr	%l0, %psr			! set the appropriate PIL

.Lexecute_handler:
	/* When being here the following values are in global registers:
	    g1 <- SP to use
	    l0 <- PSR (with the appropriate PIL and traps disabled)
	    g3 <- Trap Type (TT)
	 */

	/* Get the address of the user's handler from the User_Vector_Table */

	sll	%g3, 2, %g4		! use TT to calculate offset in the
					! user vector table. It must be in
					! a global register because we are
					! moving CWP

	set	user_vector_table, %g2
	ld	[%g2 + %g4], %g2	! g2 <- user's handler address

	wr	%l0, PSR_ET_MASK, %psr	! ----- Enable traps. -----
	nop                             ! But keep all interrupts masked
	nop
	nop

	/* Jump to the trap wrapper that will later call the user's handler */

	call	%g2				! call wrapper procedure
	  mov	 %g3, %o0			! pass the trap type as
						! parameter to the handler

.Lcheck_trap_type:
	/* Retrieve TT of the trap that we are handling for checking whether
	   it is an external interrupt. */

	ld	[%sp + ISF_TT_OFFSET], %l3	    ! retrieve trap type
	andn	%l3, SYNCHRONOUS_TRAP_BIT_MASK, %l3 ! without the sync bit

	/* If this is an external interrupt (TT between 0x11 and 0x1f), we
	    must tell the executive that interrupt handling is finishing. */

	cmp	%l3, 0x11
	bl	.Lrestore_from_isf
	 cmp	 %l3, 0x1f
	bg	.Lrestore_from_isf
	 nop

.Lcheck_context_switch:
	/* We need to check whether there is any scheduling event to handle */

	call	__gnat_context_switch_needed
	 nop

	cmp	%o0, 0
	be	.Lrestore_from_isf		! no context switch needed
	 nop

.Ldo_context_switching:
	/* When being here traps are enabled, but interrupts are disabled
	   (PIL=15). */

	/* Save interrupt stack pointer.  */
	set	interrupt_stack_table, %l6
	st	%sp, [%l6 + %l4]

	call	__gnat_context_switch
	 nop

	/* Save the previous value of the stack pointer.  */
	add	%sp, INTERRUPT_STACK_FRAME_SIZE, %l0
	st	%l0, [%l6 + %l4]

.Lrestore_from_isf:
	ld	[%sp + ISF_Y_OFFSET], %g2
	wr	%g2, %y				! restore Y register
	rd	%psr, %l0

	set	(PSR_EF_MASK | PSR_CWP_MASK), %g3
	and	%l0, %g3, %l6			! get current EF and CWP

	ldd	[%sp + ISF_PSR_OFFSET], %l0	! retrieve PSR and PC
	ldd	[%sp + ISF_NPC_OFFSET], %l2	! retrieve nPC and TT

	andn	%l0, %g3, %l0			! take everything but the EF
						! bit and the CWP from the
						! saved PSR
	or	%l6, %l0, %l0			! install PSR later ...
						! keep traps disabled

	!skip g0
	ld	[%sp + ISF_G1_OFFSET], %g1	! restore global registers
	ldd	[%sp + ISF_G2_OFFSET], %g2
	ldd	[%sp + ISF_G4_OFFSET], %g4
	ldd	[%sp + ISF_G6_OFFSET], %g6

	ldd	[%sp + ISF_I0_OFFSET], %i0	! restore input registers
	ldd	[%sp + ISF_I2_OFFSET], %i2
	ldd	[%sp + ISF_I4_OFFSET], %i4
	ldd	[%sp + ISF_I6_OFFSET], %i6

	/* If (CWP + 1) is an invalid window, we need to reload it. */

	rd	%wim, %l4
	add	%l0, 1, %l6			! CWP + 1
	and	%l6, NUMBER_OF_REGISTER_WINDOWS - 1, %l6 ! do modulo on it
	srl	%l4, %l6, %l7			! l7 = WIM >> (CWP + 1)
	cmp	%l7, 1				! is the window invalid?
	bne	.Lvalid_window			! no, do not restore window

	/* Rotate wim one bit to the left since the window that is currently
	   invalid will be restored. Note that traps are disabled and we
	   cannot use the standard window underflow mechanism.*/

	 sll	 %l4, 1, %l7
	srl	%l4, NUMBER_OF_REGISTER_WINDOWS - 1, %l4
	wr	%l4, %l7, %wim
	nop
	nop
	nop

	/* Restore the window the same way as in a window underflow trap */

	restore					! now into the previous window

	ldd	[%sp + L0_OFFSET], %l0		! restore the local registers
	ldd	[%sp + L2_OFFSET], %l2
	ldd	[%sp + L4_OFFSET], %l4
	ldd	[%sp + L6_OFFSET], %l6

	ldd	[%sp + I0_OFFSET], %i0		! restore the output registers
	ldd	[%sp + I2_OFFSET], %i2
	ldd	[%sp + I4_OFFSET], %i4
	ldd	[%sp + I6_OFFSET], %i6

	save

.Lvalid_window:

.Ldecrease_nesting_level:
	/* Decrease the nesting level for the task handling the interrupt */

	/* Get the running task */

	/* Decrease the stored value of the nesting level */
	ld	[%l5 + INT_NESTING_OFFSET], %l6
	add	%l6, -1, %l6
	st	%l6, [%l5 + INT_NESTING_OFFSET]

	/* Before leaving, restore the base state of the Cache Control
	   Register if we are at the outermost interrupt. */

	cmp	%l6, 0				! is outermost interrupt?

	bne	.Lreturn_from_trap

	/* Get the stored value of the base Cache Control Register. This is
	   the value that is not affected by any interrupt handling. */

	 ld	[%l5 + BASE_CCR_OFFSET], %l6

.Lset_base_ccr:
# if defined (LEON2)
	sethi	%hi(0x80000000), %l5
	st	%l6, [%l5 + 0x14]		! set CCR
# elif defined (LEON3)
	sta	%l6, [%g0] 2			! set CCR
# endif

.Lreturn_from_trap:

	/* Set the PSR as it was before the trap and return from trap */

	/* Adjust return address for synchronous traps. The procedure which
	   installs the trap handler will force bit 8 to 1 in case of
	   synchronous traps (those that want to return to the instruction
	   following the trap in order to prevent the trap from happening
	   again and again). Previously we did this adjustment in the
	   beginning of this routine, but the problem was that when
	   propagating exceptions after a trap (Ravenscar Full run time
	   mapping traps to Ada exceptions) the exception propagation
	   machinery was disrupted because the return address had been
	   modified on the fly.

	   Note that it is important to restore the PSR *after* the andcc
	   instruction, because andcc modifies the ICC, and when returning
	   from the trap we want to restore the ICC as it was before the trap.
	 */

	andcc	%l3, SYNCHRONOUS_TRAP_BIT_MASK, %g0	! test if synch trap
	be	.Lreturn_to_interrupted			! if not, return to
							! interrupted pc
	 wr	%l0, %psr				! disable traps. They
	 						! will be enabled
	 						! later by rett
.Lreturn_to_next:
	nop						! delay slots after
	nop						! wrpsr (before rett)
        mov %l2, %l1
        add %l2, 4, %l2

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

.Lreturn_to_interrupted:
	nop						! delay slots after
	nop						! wrpsr (before rett)

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop
	.cfi_endproc

	.size call_trap_handler, . - call_trap_handler
