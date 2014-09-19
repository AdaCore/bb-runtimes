/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                      Copyright (C) 2011-2012 AdaCore                     *
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
	.global start_flash

	.global __ada_sys_startup_cont

	.section .traps

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

	.weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
	.weak	__gnat_dabt_trap	/* data abort trap */

/* 0 */	b	start_flash
__gnat_undef_trap:	/* 4 */
	b	__gnat_undef_trap	/* Undefined instruction trap */
__gnat_svc_trap:	/* 8 */
	b	__gnat_svc_trap		/* Supervisor call trap */
__gnat_pabt_trap:	/* c */
	b	__gnat_pabt_trap	/* prefetch abort trap	*/
__gnat_dabt_trap:	/* 10 */
	b	L__dabt_trap		/* data abort trap */
	nop		/* 14 */
	ldr	pc,[pc,#-0x1b0] /* IRQ, jump to IRQVECREG, 16#ffff_fe70# */
	ldr	pc,[pc,#-0x1b0] /* FIQ, jump to FIQVECREG, 16#ffff_fe74# */

	.text
svcSp:		.word __svc_stack_end
abortSp:	.word __abort_stack_end
undefSp:	.word __undef_stack_end
fiqSp:		.word __fiq_stack_end
irqSp:		.word __irq_stack_end
userSp:		.word __stack_end

DEVID:		.word 0xfffffff0
DEVID_REVA:	.word 0x802aad05
FEDACCTRL1:	.word 0xfff87008
FEDACCTRL1_INITVAL:	.word 0x000a060A

	.type start_flash,%function
start_flash:
	/* See Application Report SPNA106B - January 2013
	   Initialization of Hercules(TM) ARM(R) Cortex(TM)-R4F
	   Microcontrollers.
	   Refer in particular to the corresponding C file
	   sys_startup_recommended.c */

	/* 2.1 Enable Floating-Point Coprocessor  */
	bl	L_coreEnableVfp

	/* 2.2 Initialize Cortex-R4F registers	*/
	bl	L_coreInitRegisters

	/* Implement work-around for CCM-R4 issue on silicon revision A */
	ldr	r0, DEVID
	ldr	r1, [r0]
	ldr	r0, DEVID_REVA
	cmp	r0, r1
	bleq	L_esmCcmErrorsClear

	/* 2.3 - 2.31 */
	b	__ada_sys_startup

__ada_sys_startup_cont:
	/* Copy .data */
	movw	r0,#:lower16:__data_start
	movt	r0,#:upper16:__data_start
	movw	r1,#:lower16:__data_words
	movt	r1,#:upper16:__data_words
	movw	r2,#:lower16:__data_load
	movt	r2,#:upper16:__data_load
	cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2],#4
	str	r4,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:

	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words /* Must be less than 64K words */
	mov	r2,#0
	cmp	r1,#0
	beq	1f
0:	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b

	/* Set system mode */
1:	cps	#31

	/* Call the main routine */
	bl	main

	.size	start_flash, . - start_flash

        /* Fall through in order to halt if main would return ... */

	.global	_exit
_exit:
	/* Write 0x8000 to SYSECR (16#FFFF_FFE0#) */
        mov	r2, #0x8000
	mvn	r3, #0
        str	r2, [r3, #-31]
	b	_exit

	.size _exit, . - _exit

L_coreEnableVfp:
	mrc   p15,     #0x00,	   r0,	     c1, c0, #0x02
	orr   r0,      r0,	   #0xF00000
	mcr   p15,     #0x00,	   r0,	     c1, c0, #0x02
	mov   r0,      #0x40000000
	fmxr  fpexc,   r0
	bx    lr

L_coreInitRegisters:
	/* Start initializing CPU registers, including backed registers for
	   all modes. This is necessary on Cortex R4F to avoid miscompares
	   between the cores running in lockstep, and is a good idea on any
	   system. */

	/* After reset, the CPU is in the Supervisor mode (M = 10011) */
	mov	r0, lr
	mov	r1, #0x0000
	mov	r2, #0x0000
	mov	r3, #0x0000
	mov	r4, #0x0000
	mov	r5, #0x0000
	mov	r6, #0x0000
	mov	r7, #0x0000
	mov	r8, #0x0000
	mov	r9, #0x0000
	mov	r10, #0x0000
	mov	r11, #0x0000
	mov	r12, #0x0000

	ldr	sp, svcSp

	/* Switch to FIQ mode (M = 10001) */
	cps	#17
	mov	r8, #0x0000
	mov	r9, #0x0000
	mov	r10, #0x0000
	mov	r11, #0x0000
	mov	r12, #0x0000

	/* Abort mode */
	cps	#23  /* 10111 */
	ldr	sp, abortSp
	mov	lr, r0

	/* Undefined instruction mode */
	cps	#27  /* 11011 */
	ldr	sp, undefSp
	mov	lr, r0

	/* FIQ mode */
	cps	#17  /* 10001 */
	ldr	sp, fiqSp
	mov	lr, r0

	/* IRQ mode */
	cps	#18  /* 10010 */
	ldr	sp, irqSp
	mov	lr, r0

	/* System mode */
	cps	#31  /* 11111 */
	ldr	sp, userSp	/* SYS mode shared stack with User mode */
	mov	lr, r0

	/* Switch back to Supervisor Mode */
	cps	#19 /* 10011 */
	fmdrr	d0, r1, r1
	fmdrr	d1, r1, r1
	fmdrr	d2, r1, r1
	fmdrr	d3, r1, r1
	fmdrr	d4, r1, r1
	fmdrr	d5, r1, r1
	fmdrr	d6, r1, r1
	fmdrr	d7, r1, r1
	fmdrr	d8, r1, r1
	fmdrr	d9, r1, r1
	fmdrr	d10, r1, r1
	fmdrr	d11, r1, r1
	fmdrr	d12, r1, r1
	fmdrr	d13, r1, r1
	fmdrr	d14, r1, r1
	fmdrr	d15, r1, r1

	/* Synchronize 4-entry call-return stack */
	bl	0f
0:	bl	1f
1:	bl	2f
2:	bl	3f
3:	bx	r0
	.size	L_coreInitRegisters, . - L_coreInitRegisters

L_esmCcmErrorsClear:
	ldr	r0, ESMSR1_REG	 // load the ESMSR1 status register address
	ldr	r2, ESMSR1_ERR_CLR
	str	r2, [r0]	 // clear the ESMSR1 register

	ldr	r0, ESMSR2_REG	 // load the ESMSR2 status register address
	ldr	r2, ESMSR2_ERR_CLR
	str	r2, [r0]	 // clear the ESMSR2 register

	ldr	r0, ESMSSR2_REG	 // load the ESMSSR2 status register address
	ldr	r2, ESMSSR2_ERR_CLR
	str	r2, [r0]	 // clear the ESMSSR2 register

	ldr	r0, ESMKEY_REG	 // load the ESMKEY register address
	mov	r2, #0x5
	str	r2, [r0]	 // clear the ESMKEY register

	ldr	r0, VIM_INTREQ	 // load the INTREQ register address
	ldr	r2, VIM_INT_CLR
	str	r2, [r0]	 // clear the INTREQ register

	ldr	r0, CCMR4_STAT_REG // load the CCMR4 status register address
	ldr 	r2, CCMR4_ERR_CLR
	str	r2, [r0]	 // clear the CCMR4 status register
	bx	lr
	.size	L_esmCcmErrorsClear, . - L_esmCcmErrorsClear

ESMSR1_REG:	 .word	0xFFFFF518
ESMSR2_REG:	 .word	0xFFFFF51C
ESMSR3_REG:	 .word	0xFFFFF520
ESMKEY_REG:	 .word	0xFFFFF538
ESMSSR2_REG:	 .word	0xFFFFF53C
CCMR4_STAT_REG:	 .word	0xFFFFF600
ERR_CLR_WRD:	 .word	0xFFFFFFFF
CCMR4_ERR_CLR:	 .word	0x00010000
ESMSR1_ERR_CLR:	 .word	0x80000000
ESMSR2_ERR_CLR:	 .word	0x00000004
ESMSSR2_ERR_CLR: .word	0x00000004
VIM_INT_CLR:	 .word	0x00000001
VIM_INTREQ:	 .word	0xFFFFFE20


L__dabt_trap:
	stmfd	sp!, {r0 - r3}

	/* Check for uncorrectable ECC error on B0TCM/B1TCM.  */
        ldr	r3, ESMSR3_REG
        ldr	r0, [r3]
        tst	r0, #0x28
        beq	1f

	/* Yes, check of ECC memory write is enabled.  */
	ldr	r1, tcram1_ramctrl
	ldr	r2, [r1]
	tst	r2, #0x100
	/* Yes: self-checking mode; skip fault.  */
	bne	Ldabt_skip_insn
	b	0f

	/* Check for uncorrectable ECC error on ATCM.  */
1:	tst	r0, #0x80
	bne	Ldabt_flash_error

	/* Normal data abort handler.  */
0:	ldmfd	sp!, {r0 - r3}
	b	__gnat_dabt_trap

Ldabt_skip_insn:
	/* Branch to the next instruction.  */
	ldmfd	sp!, {r0 - r3}
	subs	pc, lr, #4

Ldabt_flash_error:
	/* Read FDIAGCTRL register.  */
	ldr	r1, flashbase
	ldr	r2, [r1, #0x6C]
	lsr	r2, #16
	tst	r2, #5			@ check if bits 19:16 are 5, this indicates diagnostic mode is enabled
	/* If not, normal handler.  */
	beq	0b
	b	Ldabt_skip_insn

tcram1_ramctrl:	.word	0xFFFFF800
tcram2_ramctrl:	.word	0xFFFFF900
flashbase:	.word	0xFFF87000
