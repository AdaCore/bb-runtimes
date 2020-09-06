/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                      Copyright (C) 2011-2019 AdaCore                     *
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

/* This file is specific to the Hercules ARM Cortex-R4F Microcontrollers,
   such as the TMS570. */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */

        .section .traps_vector

/*0*/	.word	__gnat_init
/*4*/	.word	__gnat_undef_trap	/* Undefined instruction trap */
/*8*/	.word	__gnat_svc_trap		/* Supervisor call trap */
/*c*/	.word	__gnat_pabt_trap	/* prefetch abort trap	*/
/*10*/	.word	__gnat_dabt_trap	/* data abort trap */
/*14*/	.word	0
/*18*/	.word	__gnat_irq_trap
/*1c*/	.word	__gnat_fiq_trap

	.section .traps

        .weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
	.weak	__gnat_dabt_trap	/* data abort trap */
	.weak	__gnat_irq_trap 	/* IRQ trap */
	.weak	__gnat_fiq_trap	        /* FIQ trap */

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

/* 0 */	b	__gnat_init
__gnat_undef_trap:	/* 4 */
	b	__gnat_undef_trap	/* Undefined instruction trap */
__gnat_svc_trap:	/* 8 */
	b	__gnat_svc_trap		/* Supervisor call trap */
__gnat_pabt_trap:	/* c */
	b	__gnat_pabt_trap	/* prefetch abort trap	*/
__gnat_dabt_trap:	/* 10 */
	b	__gnat_dabt_trap	/* data abort trap */
	nop		/* 14 */
	ldr	pc,[pc,#-0x1b0] /* IRQ, jump to IRQVECREG, 16#ffff_fe70# */
	ldr	pc,[pc,#-0x1b0] /* FIQ, jump to FIQVECREG, 16#ffff_fe74# */

        .text


        /*********************************************************************
         * __gnat_init:                                                      *
         *                                                                   *
         * main entry point.                                                 *
         *********************************************************************/
        .globl  __gnat_init
        .type   __gnat_init, %function
__gnat_init:
        /*********************************************/
        /* 1. Init registers, stacks and FPU         */
        /*********************************************/

        bl      __gnat_init_cpu

        /*********************************************/
        /* 2. Install workarounds for some hw bugs   */
        /*********************************************/

        bl      __cpu_errata

        /*********************************************/
        /* 3. Memory sections init                   */
        /*********************************************/

        /* Clear .bss */
        movw  r0, #:lower16:__bss_start
        movt  r0, #:upper16:__bss_start
        movw  r1, #:lower16:__bss_words
        movt  r1, #:upper16:__bss_words
        movw  r2, #0
	cmp   r1,#0
	beq   1f
0:      str r2,[r0],#4
        subs r1,r1,#1
        bne 0b
1:

        /* Copy .data */
	movw	r0,#:lower16:__data_start
	movt	r0,#:upper16:__data_start
	movw	r2,#:lower16:__data_load
	movt	r2,#:upper16:__data_load
        cmp     r0, r2
        beq     1f /* start == load: Skip data copy */

	movw	r1,#:lower16:__data_words
	movt	r1,#:upper16:__data_words
	cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2],#4
	str	r4,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:

        /*********************************************/
        /* 4. Board initialisation                   */
        /*********************************************/

        /* See board_init.adb */
        bl __gnat_board_init

	/* Call the main routine */
 	bl      main

        /* reboot the board if main would return */
        bl      _exit

DEVID:                  .word 0xfffffff0
DEVID_REVA:             .word 0x802aad05
FEDACCTRL1:             .word 0xfff87008
FEDACCTRL1_INITVAL:     .word 0x000a060A
	.size __gnat_init, . - __gnat_init


        /*********************************************************************
         * __gnat_init_cpu                                                   *
         *                                                                   *
         * Initializes CPU registers, including backed registers for all     *
         * modes. This is necessary on Cortex-R4F to avoid miscompares       *
         * between the cores running in lockstep, and is a good idea on any  *
         * system anyway to ensure reproductible results.                    *
         * During this initialisation, the stacks are setup, and the FPU is  *
         * enabled.                                                          *
         *********************************************************************/
        .type   __gnat_init_cpu, %function
__gnat_init_cpu:
        /* Start initializing CPU registers, including backed registers for
           all modes. This is necessary on Cortex R4F to avoid miscompares
           between the cores running in lockstep, and is a good idea on any
           system. */

        /* After reset, the CPU is in the Supervisor mode (M = 10011) */
        mov     r0, lr /* Save lr in r0: don't touch r0 below ! */
        mov     r1, #0
        mov     r2, #0
        mov     r3, #0
        mov     r4, #0
        mov     r5, #0
        mov     r6, #0
        mov     r7, #0
        mov     r8, #0
        mov     r9, #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0

        /* Set SPSR */
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        ldr     sp, svcSp	/* r13 */

        /* Switch to FIQ mode (M = 10001) */
        cps     #17
        mov     r8, #0
        mov     r9, #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0
        ldr     sp, fiqSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* IRQ mode */
        cps     #18  /* 10010 */
	ldr     sp, irqSp
	mov     lr, #0
	mrs     r1, CPSR
	msr     SPSR_fsxc, r1

        /* Abort mode */
        cps     #23  /* 10111 */
        ldr     sp, abortSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Undefined instruction mode */
        cps     #27  /* 11011 */
        ldr     sp, undefSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* System mode */
        cps     #31  /* 11111 */
        ldr     sp, userSp	/* SYS mode shared stack with User mode */
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Stay in system mode.  */

        /* Enable FPU on the CPACR/FPEXC registers */
        mrc     p15, #0, r2, c1, c0, #2 /* CPACR */
        orr     r2, r2, #0xF00000       /* Enable CP10, CP11 user & system */
        mcr     p15, #0, r2, c1, c0, #2 /* set */
        mov     r2, #0x40000000         /* Set the FPEXC.EN bit */
        fmxr    fpexc, r2

        vmov    d0, r1, r1
        vmov    d1, r1, r1
        vmov    d2, r1, r1
        vmov    d3, r1, r1
        vmov    d4, r1, r1
        vmov    d5, r1, r1
        vmov    d6, r1, r1
        vmov    d7, r1, r1
        vmov    d8, r1, r1
        vmov    d9, r1, r1
        vmov    d10, r1, r1
        vmov    d11, r1, r1
        vmov    d12, r1, r1
        vmov    d13, r1, r1
        vmov    d14, r1, r1
        vmov    d15, r1, r1

        /* Synchronize 4-entry call-return stack */
        bl      0f
0:      bl      1f
1:      bl      2f
2:      bl      3f
3:      bx      r0

svcSp:	        .word __svc_stack_end
abortSp:        .word __abort_stack_end
undefSp:        .word __undef_stack_end
fiqSp:          .word __fiq_stack_end
irqSp:          .word __irq_stack_end
userSp:         .word __stack_end
        .size   __gnat_init_cpu, . - __gnat_init_cpu

/* provide a weak variant of CPU errata: if any, this can then be overloaded */
        .weak  __cpu_errata
        .type  __cpu_errata, %function
__cpu_errata:
        bx lr
        .size  __cpu_errata, . - __cpu_errata
