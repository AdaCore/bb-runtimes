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

/* This file is specific to the TI AM64x and AM263x ARM Cortex-R5 cores. */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */

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

	b       _start
__gnat_undef_trap:
	b       __gnat_undef_trap
__gnat_svc_trap:
	b       __gnat_svc_trap
__gnat_pabt_trap:
	b       __gnat_pabt_trap
__gnat_dabt_trap:
	b       __gnat_dabt_trap
	nop		/* 14 : unused on the R5*/
__gnat_irq_trap:
        b       __gnat_irq_trap
__gnat_fiq_trap:
	b       __gnat_fiq_trap

.section .boot,"axS"

        /*********************************************************************
         * _start:                                                      *
         *                                                                   *
         * main entry point.                                                 *
         *********************************************************************/
        .globl  _start
        .type   _start, %function
_start:
        /*********************************************/
        /* 1. Init registers, stacks and FPU         */
        /*********************************************/

        /* Start initializing CPU registers, including banked registers, for
           all modes. */

        /* init all base registers */
        mov     r0,  #0
        mov     r1,  #0
        mov     r2,  #0
        mov     r3,  #0
        mov     r4,  #0
        mov     r5,  #0
        mov     r6,  #0
        mov     r7,  #0
        mov     r8,  #0
        mov     r9,  #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0

        /* Switch to FIQ mode (M = 10001) */
        cps     #17
        /* r8-r12,lr are banked */
        mov     r8,  #0
        mov     r9,  #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0
        mov     lr,  #0
        /* stack, spsr init */
        ldr     sp, fiqSp
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* IRQ mode (M = 10010) */
        cps     #18
         /* LR is banked */
	mov     lr, #0
        /* stack, spsr init */
	ldr     sp, irqSp
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* SVC mode (M = 10011) */
        cps     #19
         /* LR is banked */
	mov     lr, #0
        /* stack */
	ldr     sp, irqSp /* The IRQ handler switches to SVC to handle IRQs */
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Abort mode (M = 10111) */
        cps     #23
         /* LR is banked */
        mov     lr, #0
        /* stack, spsr init */
        ldr     sp, abortSp
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Undefined instruction mode (M = 11011) */
        cps     #27
         /* LR is banked */
        mov     lr, #0
        /* stack, spsr init */
        ldr     sp, undefSp
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* System mode (M = 11111) */
        cps     #31
         /* LR is banked */
        mov     lr, #0
        /* stack, spsr init */
        ldr     sp, userSp	/* SYS mode shared stack with User mode */
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Stay in system mode.  */

        /* Enable FPU on the CPACR/FPEXC registers */
        mrc     p15, #0, r2, c1, c0, #2 /* CPACR */
        orr     r2, r2, #0xF00000       /* Enable CP10, CP11 user & system */
        mcr     p15, #0, r2, c1, c0, #2 /* set */
        isb
        vmrs    r2, fpexc
        orr     r2, r2, #0x40000000         /* Set the FPEXC.EN bit */
        vmsr    fpexc, r2

        mov     r1, #0
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

        /* Disable MPU and caches */
        mrc     p15, 0, r2, c1, c0, 0 /* CP15 CTRL register */
        bic     r2, r2, #5            /* Disable MPU and D cache */
        bic     r2, r2, #0x1000       /* Disable I cache */
        dsb                           /* Ensure loads/stores are completed */
        mcr     p15, 0, r2, c1, c0, 0
        isb                           /* insns must execute with the new
                                         MPU settings */

        /* Disable branch prediction, TCM ECC checks */
        mrc     p15, 0, r2, c1, c0, 1 /* ACTLR */
        orr     r2, r2, #(0x1 << 17)  /* Enable RSDIS bit 17 to disable the return stack */
        orr     r2, r2, #(0x1 << 16)  /* Clear BP bit 15 and set BP bit 16:*/
        bic     r2, r2, #(0x1 << 15)  /* Branch always not taken and history table updates disabled*/
        orr     r2, r2, #(0x1 << 27)  /* Enable B1TCM ECC check */
        orr     r2, r2, #(0x1 << 26)  /* Enable B0TCM ECC check */
        orr     r2, r2, #(0x1 << 25)  /* Enable ATCM ECC check */
	bic	r2, r2, #(0x1 << 5)   /* Generate abort on parity errors, with [5:3]=b 000*/
	bic 	r2, r2, #(0x1 << 4)
	bic	r2, r2, #(0x1 << 3)
        mcr     p15, 0, r2, c1, c0, 1
        dsb

        /* Invalidate caches */
	mov	r0,#0                  /* r0 = 0  */
	dsb
	mcr	p15, 0, r0, c7, c5, 0  /* invalidate icache */
	mcr 	p15, 0, r0, c15, c5, 0 /* Invalidate entire data cache*/
	isb

        /*********************************************/
        /* 2. MPU initialization                     */
        /*********************************************/

        /* See s-mpu* and s-boapar.ads */
        bl __gnat_mpu_init

        /* Enable branch prediction */
	mrc     p15, 0, r0, c1, c0, 1       /* Read ACTLR*/
        bic     r0, r0, #(0x1 << 17)        /* Clear RSDIS bit 17 to enable return stack*/
        bic     r0, r0, #(0x1 << 16)        /* Clear BP bit 15 and BP bit 16:*/
        bic     r0, r0, #(0x1 << 15)        /* Normal operation, BP is taken from the global history table.*/
        orr	r0, r0, #(0x1 << 14)	    /* Disable DBWR for errata 780125 */
	mcr     p15, 0, r0, c1, c0, 1       /* Write ACTLR*/

        /* Enable d-cache and i-cache */
	mrc 	p15,0,r1,c1,c0,0
	ldr	r0, =0x1005
	orr 	r1,r1,r0
	dsb
	mcr	p15,0,r1,c1,c0,0		/* Enable cache  */
	isb					/* isb	flush prefetch buffer */

        /* enable asynchronous abort exception */
	mrs	r0, cpsr
	bic	r0, r0, #0x100
	msr	cpsr_xsf, r0

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
        /* 4. Run application                        */
        /*********************************************/

	/* Call the main routine */
 	bl      main

        /* reboot the board if main would return */
        bl      _exit

abortSp:        .word __abort_stack_end
undefSp:        .word __undef_stack_end
fiqSp:          .word __fiq_stack_end
irqSp:          .word __irq_stack_end
userSp:         .word __stack_end
	.size _start, . - _start
