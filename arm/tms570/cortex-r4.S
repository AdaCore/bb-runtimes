/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                      Copyright (C) 2011-2017 AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 ****************************************************************************/

        .text

        /*********************************************************************
         * __cpu_errata:                                                     *
         *                                                                   *
         * Installs workaround for known CPU hardware bugs.                  *
         * This is the Cortex-R4 version of this routine                     *
         *********************************************************************/
        .globl  __cpu_errata
        .type   __cpu_errata, %function
__cpu_errata:
        /* Cortex-R4 errata workarounds: check the cpu version */
        mrc     p15, #0, r0, c0, c0, #0 /* MIDR */
        mov     r1, #0xFFF0             /* Primary part num. mask */
        and     r0, r0, r1
        mov     r1, #0xC140             /* Cortex-r4 identifier */
        cmp     r0, r1
        bxne    lr /* skip if cortex-r5 */

        push {lr}
        /* Errata DEVICE#140 (TMS570LS Rev A silicon) */
        ldr r0, DEVID
        ldr r1, [r0]
        ldr r0, DEVID_REVA
        cmp r0, r1
        bleq  _esmCcmErrorsClear_

        /* Errata CORTEX-R4#66:
         * Description:
         *
         * Register Corruption during a Load-Multiple Instruction at an
         * exception vector.
         *
         * Workaround:
         *
         * Disable out-of-order completion for divide instructions in
         * Auxiliary Control register
         */
        mrc     p15, #0, r0, c1, c0, #1
        orr     r0, r0, #0x80
        mcr     p15, #0, r0, c1, c0, #1

        /* Errata CORTEX-R4#57
         * Description:
         *
         * Conditional VMRS APSR_Nvcz, FPSCR may evaluate incorrect flags.
         *
         * Workaround:
         *
         * Disable out-of-order single-precision floating point
         * multiply-accumulate instruction completion
         */
        mrc     p15, #0, r0, c15, c0, #0
        orr     r0, r0, #0x10000
        mcr     p15, #0, r0, c15, c0, #0

        pop     {lr}
        bx      lr

DEVID:                  .word 0xfffffff0
DEVID_REVA:             .word 0x802aad05
	.size __cpu_errata, . - __cpu_errata

        /*********************************************************************
         * _esmCcmErrorsClear_:                                              *
         *                                                                   *
         * Clear ESM Group2 Channel 2 error in ESMSR2 and Compare error in   *
         * CCMSR register.                                                   *
         *********************************************************************/
_esmCcmErrorsClear_:
        ldr   r0, ESMSR1_REG     // load the ESMSR1 status register address
        ldr   r2, ESMSR1_ERR_CLR
        str       r2, [r0]       // clear the ESMSR1 register

        ldr   r0, ESMSR2_REG     // load the ESMSR2 status register address
        ldr   r2, ESMSR2_ERR_CLR
        str       r2, [r0]       // clear the ESMSR2 register

        ldr   r0, ESMSSR2_REG    // load the ESMSSR2 status register address
        ldr   r2, ESMSSR2_ERR_CLR
        str       r2, [r0]       // clear the ESMSSR2 register

        ldr   r0, ESMKEY_REG     // load the ESMKEY register address
        mov       r2, #0x5
        str       r2, [r0]       // clear the ESMKEY register

        ldr   r0, VIM_INTREQ     // load the INTREQ register address
        ldr   r2, VIM_INT_CLR
        str       r2, [r0]       // clear the INTREQ register

        ldr   r0, CCMR4_STAT_REG // load the CCMR4 status register address
        ldr   r2, CCMR4_ERR_CLR
        str       r2, [r0]       // clear the CCMR4 status register
        bx    lr

ESMSR1_REG:      .word  0xFFFFF518
ESMSR2_REG:      .word  0xFFFFF51C
ESMSR3_REG:      .word  0xFFFFF520
ESMKEY_REG:      .word  0xFFFFF538
ESMSSR2_REG:     .word  0xFFFFF53C
CCMR4_STAT_REG:  .word  0xFFFFF600
ERR_CLR_WRD:     .word  0xFFFFFFFF
CCMR4_ERR_CLR:   .word  0x00010000
ESMSR1_ERR_CLR:  .word  0x80000000
ESMSR2_ERR_CLR:  .word  0x00000004
ESMSSR2_ERR_CLR: .word  0x00000004
VIM_INT_CLR:     .word  0x00000001
VIM_INTREQ:      .word  0xFFFFFE20
        .size _esmCcmErrorsClear_, . - _esmCcmErrorsClear_
