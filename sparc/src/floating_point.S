/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                         F L O A T I N G _ P O I N T                      *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2017 AdaCore                      *
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

/* This file is a SPARC (V7/V8) specific implementation */

#include "sparc.h"

#ifdef XTRATUM
/* Include the para-virtualized hypervisor services */
#include <xm.h>
#endif

	.text
	.align 4
	.global initialize_floating_point
	.type   initialize_floating_point,@function

/* Procedure that installs the trap handler in charge of executing floating
   point context switches. Only for XtratuM*/

initialize_floating_point:
#ifndef XTRATUM
        /* Nothing to do in bare board (no XtratuM) configuration. The
           fp_disabled trap is installed statically in the crt0. */
        retl	! return from leaf subroutine
         nop
#else
        /* Create a prologue since this is not a leaf routine with XtratuM */
        save  %sp, -96, %sp

	/* First we need to obtain the address (within the trap table)
	   where the trap handler needs to be installed. */

	sethi	%hi(trap_table), %l1
	add	%l1, %lo(trap_table), %l1		! trap table address

	add	%l1, 4*4*4, %l1				! position of
							! fp_disabled in the
							! trap table

	/* Now, install the first four instructions of the trap handler in
	   the trap table. We need to flush the instruction cache since we
           are modifying the memory area corresponding to instructions (trap
           table). */

	/* sethi %hi(fp_context_switch), %l0 */

	sethi	%hi(0x21000000), %l2
	sethi	%hi(fp_context_switch), %l3
	srl	%l3, 10, %l3
	add	%l2, %l3, %l2
	st	%l2, [%l1 + 0*4]

	/* jmpl %l0 + %lo(fp_context_switch), %g0 */

	sethi	%hi(0x81c42000), %l2
	add	%l2, %lo(fp_context_switch), %l2
	st	%l2, [%l1 + 1*4]

	/* We cannot read the psr directly here on XtratuM. the
           fp_context_switch routine will have to read it. */

	/* nop */

	set	0x01000000, %l2
	st	%l2, [%l1 + 2*4]

	/* nop */

	set	0x01000000, %l2
	st	%l2, [%l1 + 3*4]

	/* Flush data cache */
	call    XM_flush_cache
	mov     1, %o0
	ret                                     ! return from subroutine
	restore
#endif
	.size initialize_floating_point, . - initialize_floating_point

	/* Trap handler that saves/restores the floating point context in the
	   appropriate thread descriptors. */

	.global fp_context_switch
	.type   fp_context_switch,@function
fp_context_switch:
	/* First we need to enable the use of the floating point unit. Note
	   that l0 contains the PSR. */

#ifdef XTRATUM
	set 	sparc_get_psr_nr, %o0           ! Get PSR
	__XM_AHC
	set	PSR_EF_MASK, %l3
	or	%o0, %l3, %o1
	mov     %o1, %l0                        ! store the PSR in l0 for later
	set	sparc_set_psr_nr, %o0		! enable FP
	__XM_AHC
#else
	set	PSR_EF_MASK, %l3
	or	%l0, %l3, %l0
	wr	%l0, %psr			! enable FP
#endif

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

#ifdef XTRATUM
	mov	%l0, %o1			! Use o1 to pass the saved PSR
	set	sparc_set_psr_nr, %o0		! as argument to the hypercall
	__XM_AHC

	restore
	save

	.Lreturn_from_trap:
	set	sparc_iret_nr, %o0
	__XM_AHC
#else
	wr	%l0, %psr
	nop
	nop
	nop

.Lreturn_from_trap:
	jmp	%l1
	 rett	 %l2			! return from trap
#endif

	.size fp_context_switch, . - fp_context_switch
