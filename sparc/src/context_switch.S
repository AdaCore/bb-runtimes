/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                      C O N T E X T _ S W I T C H                         *
 *                                                                          *
 *                              Assembly File                               *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2019 AdaCore                      *
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
 ****************************************************************************/

/* This file is a SPARC (V8) specific implementation */

#include "sparc.h"

/* procedure context_switch.
   This routine performs a context switch between the two tasks that are
   specified in __gnat_running_thread_table (CPU_Id) and
   first_thread_table (CPU_Id).
 */

/*
   The algorithm for context switch uses simple save and restore
   instructions (without using traps).
   Interrupts are always disabled when entering this procedure. Hence, we
   do not need to disable traps within this function in order to execute
   atomically.

   Save the context:
   1. Store the in and local registers of the current register window on
      the stack.
   2. Save global, output, PSR, Y, and WIM registers in the thread
      descriptor. Save also the Cache Control Register for LEON.
   3. Check condition on WIM/CWP. If all used windows have been saved, then
      go to step 5, else continue.
   4. Execute a restore and save the current window to the stack. Then go
      back to step 3.
   5. Save the number of restores executed in the thread descriptor.

   Restore the context:
   1. Update new running task.
   2. Restore the stack pointer.
   3. Restore the Cache Control Register for LEON.
   4. Retrieve the number of register windows to be restored.
   5. Restore the in and local registers with the data from the stack.
   6. Check whether all the register windows that were used by the task are
      restored. If not, execute a restore and repeat step 4.
   7. Retrieve PSR, Y, global, and output registers from the thread
      descriptor. The WIM register was restored in step 3.
   8. Resume execution by simply returning from leaf function. It jumps to
      the next instruction to execute in the currently running thread, which
      corresponds to the calling address (o7) plus 8 (skip over the call
      instruction and the instruction in the delay slot of the call).
  */

	.text
	.align 4

	.global __gnat_context_switch
	.type __gnat_context_switch,@function
__gnat_context_switch:
.Lsave_running_task_status:
	/* No traps can happen saving the task status */

	/* Save the current window on the stack */

	std	%l0, [%sp + L0_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l2, [%sp + L2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l4, [%sp + L4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l6, [%sp + L6_OFFSET]

	NOP_FIX_GRLIB_TN_0009
	std	%i0, [%sp + I0_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i2, [%sp + I2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i4, [%sp + I4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i6, [%sp + I6_OFFSET]

	/* Get the address of the task descriptor of the currently running
	   task for saving its state. */

	CURRENT_CPU(%l1)

	set	__gnat_running_thread_table, %l0
	sll	%l1, 2, %l1			! offset in the threads table
	ld	[%l0 + %l1], %l1		! get pointer to the running
						! thread.

.Lstore_ccr:
#if defined (LEON2)
	set	0x80000000, %l0
	ld	[%l0 + 0x14], %l0
#elif defined (LEON3)
	lda	[%g0] 2, %l0
#endif
	st	%l0, [%l1 + CCR_OFFSET]

	/* Save global and output registers in the task descriptor area. */

	! skip g0
	st	%g1, [%l1 + G1_OFFSET]		! save the global registers
	! No NOP_FIX_GRLIB_TN_0009, as the first store size is <= 4
	std	%g2, [%l1 + G2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%g4, [%l1 + G4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%g6, [%l1 + G6_OFFSET]

	NOP_FIX_GRLIB_TN_0009
	std	%o0, [%l1 + O0_OFFSET]		! save the output registers
	NOP_FIX_GRLIB_TN_0009
	std	%o2, [%l1 + O2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%o4, [%l1 + O4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%o6, [%l1 + O6_OFFSET]		! o6 contains the SP and o7
						! the return address

	/* Save PSR, WIM, and Y registers */

	rd	%psr, %g6
	st	%g6, [%l1 + PSR_OFFSET]		! save status register

	rd	%y, %g2
	NOP_FIX_GRLIB_TN_0009
	st	%g2, [%l1 + Y_OFFSET]		! save Y register

	rd	%wim, %g2
	NOP_FIX_GRLIB_TN_0009
	st	%g2, [%l1 + WIM_OFFSET]		! save WIM register

	/* Flush all windows with valid contents (the current one has already
	   been flushed). Do not flush windows that have not been used. */

	/* g6 contains the PSR and g2 the WIM */

	mov	1, %g3
	sll	%g3, %g6, %g3			! g3 = WIM for CW invalid.
						! we do not need to apply any
						! mask to the psr for extracting
						! the CWP because the shift only
						! takes the 5 lsb (exactly those
						! containing the CWP)

	mov	%l1, %g1			! make a copy of the pointer to
						! the thread descriptor so that
						! we can later store the number
						! of windows to be restored

	mov	-1, %g4				! g4 = counter containing the
						! number of windows flushed

.Lsave_frame_loop:
	sll	%g3, 1, %g5			! rotate the CW mask left 1
	srl	%g3, NUMBER_OF_REGISTER_WINDOWS - 1, %g3
	or	%g3, %g5, %g3			! g3 = CW mask if we do one
						! restore

       /* If we have already arrived to the invalid window then stop.
	  Otherwise, continue flushing windows. */

	andcc	%g3,%g2,%g0			! any windows to flush?
	bne	.Ldone_flushing			! no, then continue
	 add	 %g4, 1, %g4			! using the delay slot for
						! increasing the number of
						! windows flushed

	restore					! back one window

	/* Save the current window on the stack */

	std	%l0, [%sp + L0_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l2, [%sp + L2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l4, [%sp + L4_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%l6, [%sp + L6_OFFSET]

	! i6 and i7 are saved in the next delay slot
	NOP_FIX_GRLIB_TN_0009
	std	%i0, [%sp + I0_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i2, [%sp + I2_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%i4, [%sp + I4_OFFSET]

#ifdef FIX_UT699
	! Even if there is no store at the target of the branch, avoid a
	! warning from the leon3ftststscan GRLIB-TN-0009 detection script.
	NOP_FIX_GRLIB_TN_0009
	std	 %i6, [%sp + I6_OFFSET]		! using the delay slot
	ba	.Lsave_frame_loop
	 nop
#else
	ba	.Lsave_frame_loop
	 std	 %i6, [%sp + I6_OFFSET]		! using the delay slot
#endif

.Ldone_flushing:
	/* We store the number of extra windows (apart from the one that was
	   being used by the task) that have been saved on the stack. */

	st	%g4, [%g1 + NWIN_OFFSET]

.Lupdate_running_task:
	/* Update the value of the new running thread */
	CURRENT_CPU(%g3)
	sll	%g3, 2, %g3     /* offset in the threads tables */

	set	first_thread_table, %g1
	ld	[%g1 + %g3], %g1
	set	__gnat_running_thread_table, %g4
	st	%g1, [%g4 + %g3]

	/* The status of the registers when entering here is:
	     g1 = task to restore
	     g6 = PSR
	     g2 = WIM
	 */

.Lrestore_running_task_status:
	/* g1 contains the address of the task descriptor of the currently
	   running tasks for restoring its state. Note that the CWP may be
	   different from the one that was saved (and that we will restore)
	   so that all the data manipulation executed from here until the end
	   must be done using global registers. */

	ld	[%g1 + PSR_OFFSET], %g4		! g4 = saved PSR
	set	PSR_EF_MASK, %g7
	andn	%g4, %g7, %g4			! g4 = saved PSR with FP disabled

	wr	%g4, %psr			! restore saved PSR but disabling
	nop					! FP since we do not know the state
						! of the FP unit. The FP trap will
						! do what is necessary when needed
						! note that PIL is always equal to
						! 15 within the context switch
						! routine, so that interrupts are
						! disabled

	ld	[%g1 + NWIN_OFFSET], %g2	! retrieve the number of
						! windows to be restored

	ld	[%g1 + WIM_OFFSET], %g3		! retrieve the WIM register
	wr	%g3, %wim

	ld	[%g1 + O6_OFFSET], %sp		! retrieve the saved SP

.Lrestore_ccr:
	ld	[%g1 + CCR_OFFSET], %g5
#if defined (LEON2)
	set	0x80000000, %g3
	st	%g5, [%g3 + 0x14]
#elif defined (LEON3)
	sta	%g5, [%g0] 2
#endif

.Lrestore_frame_loop:
	/* Restore the current window from the stack */

	ldd	[%sp + L0_OFFSET], %l0
	ldd	[%sp + L2_OFFSET], %l2
	ldd	[%sp + L4_OFFSET], %l4
	ldd	[%sp + L6_OFFSET], %l6

	ldd	[%sp + I0_OFFSET], %i0
	ldd	[%sp + I2_OFFSET], %i2
	ldd	[%sp + I4_OFFSET], %i4
	ldd	[%sp + I6_OFFSET], %i6

	cmp	%g2, 0
	be	.Ldone_restoring
	 add	 %g2, -1, %g2

	ba	.Lrestore_frame_loop
	 restore				! use the delay slot

.Ldone_restoring:
	/* Come back to the right window by restoring the original PSR (still
	   saved in g4). */

	wr	%g4, %psr

	ld	[%g1 + Y_OFFSET], %g2
	wr	%g2, %y 			! restore the Y register

	! skip g0; g1 will be restored later
	ldd	[%g1 + G2_OFFSET], %g2		! restore the global registers
	ldd	[%g1 + G4_OFFSET], %g4		! do g1 later to avoid
	ldd	[%g1 + G6_OFFSET], %g6		! overwritting the pointer.

	ldd	[%g1 + O0_OFFSET], %o0		! restore the output registers
	ldd	[%g1 + O2_OFFSET], %o2
	ldd	[%g1 + O4_OFFSET], %o4
	ldd	[%g1 + O6_OFFSET], %o6

	retl					! return from leaf function
	 ld	 [%g1 + G1_OFFSET], %g1		! restore g1 at the end

	.size __gnat_context_switch, . - __gnat_context_switch

/* procedure set_ccr.
   This routine sets the value passed as argument (through o0) in the Cache
   Control Register. */

	.global set_ccr
	.type   set_ccr,@function
set_ccr:
#if defined (LEON2)
	set	0x80000000, %o1
	st	%o0, [%o1 + 0x14]
#elif defined (LEON3)
	sta	%o0, [%g0] 2
#endif

	retl					! return from leaf subroutine
	 nop

	.size set_ccr, . - set_ccr

/* procedure get_ccr.
   This routine returns the value of the Cache Control Register (in o0). */

	.global get_ccr
	.type   get_ccr,@function
get_ccr:
#if defined (LEON2)
	set	0x80000000, %o1
	ld	[%o1 + 0x14], %o0
#elif defined (LEON3)
	lda	[%g0] 2, %o0
#endif

	retl					! return from leaf subroutine
	 nop
	.size get_ccr, . - get_ccr

	.global fp_context_switch
	.type   fp_context_switch,@function

	/* Trap handler that saves/restores the floating point context in the
	   appropriate thread descriptors. Permits deferred context switching
	   of the floating point registers. */

fp_context_switch:
	/* First we need to enable the use of the floating point unit. Note
	   that l0 contains the PSR. */
	set	PSR_EF_MASK, %l3
	or	%l0, %l3, %l0
	wr	%l0, %psr			! enable FP

	/* Check whether we need to save/restore the floating point
	   state. If the currently executing thread is the one that
	   was the last using the floating point unit then nothing
	   needs to be done. */

        CURRENT_CPU(%l7)

	set	float_latest_user_table, %l3
        set     __gnat_running_thread_table, %l5
        sll     %l7, 2, %l7      /* offset in the threads tables */
	ld	[%l3 + %l7], %l4 /* latest user */
        ld      [%l5 + %l7], %l6 /* running thread */


        /* Don't switch context if there is no latest user. However, we
           need to set the latest user to indicate that there is someone
           using the floating point now. */
	tst	%l4
	bz	.Lupdate_latest_user
	 nop

        /* Don't switch context the latest user is the running thread */
	cmp	%l4, %l6
	be	.Lfp_context_switch_done
	 nop

	/* Save the old floating point state in the task descriptor area */

	st	%fsr, [%l4 + FSR_OFFSET]
	! No NOP_FIX_GRLIB_TN_0009, as the first store size is <= 4
	std	%f0,  [%l4 + F0_F1_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f2,  [%l4 + F2_F3_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f4,  [%l4 + F4_F5_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f6,  [%l4 + F6_F7_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f8,  [%l4 + F8_F9_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f10, [%l4 + F10_F11_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f12, [%l4 + F12_F13_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f14, [%l4 + F14_F15_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f16, [%l4 + F16_F17_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f18, [%l4 + F18_F19_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f20, [%l4 + F20_F21_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f22, [%l4 + F22_F23_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f24, [%l4 + F24_F25_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f26, [%l4 + F26_F27_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f28, [%l4 + F28_F29_OFFSET]
	NOP_FIX_GRLIB_TN_0009
	std	%f30, [%l4 + F30_F31_OFFSET]

	/* Restore the new floating point state from the task descriptor area */

	ldd	[%l6 + F0_F1_OFFSET],   %f0
	ldd	[%l6 + F2_F3_OFFSET],   %f2
	ldd	[%l6 + F4_F5_OFFSET],   %f4
	ldd	[%l6 + F6_F7_OFFSET],   %f6
	ldd	[%l6 + F8_F9_OFFSET],   %f8
	ldd	[%l6 + F10_F11_OFFSET], %f10
	ldd	[%l6 + F12_F13_OFFSET], %f12
	ldd	[%l6 + F14_F15_OFFSET], %f14
	ldd	[%l6 + F16_F17_OFFSET], %f16
	ldd	[%l6 + F18_F19_OFFSET], %f18
	ldd	[%l6 + F20_F21_OFFSET], %f20
	ldd	[%l6 + F22_F23_OFFSET], %f22
	ldd	[%l6 + F24_F25_OFFSET], %f24
	ldd	[%l6 + F26_F27_OFFSET], %f26
	ldd	[%l6 + F28_F29_OFFSET], %f28
	ldd	[%l6 + F30_F31_OFFSET], %f30
	ld	[%l6 + FSR_OFFSET],     %fsr

.Lupdate_latest_user:
	/* Write down the thread that is currently using the floating
	   point unit. */

	st	%l6, [%l3 + %l7]

.Lfp_context_switch_done:
	/* Restore the original PSR */

	wr	%l0, %psr
	nop
	nop
	nop

.Lreturn_from_trap:
        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

	.size fp_context_switch, . - fp_context_switch
