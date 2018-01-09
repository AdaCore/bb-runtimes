/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                          Copyright (C) 2013 AdaCore                      *
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

/* This file is specific to the Cortex-A53 for Raspberry PI 3 board.  */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
	.global __start_ram
	.global	__start_slave_cpu

	.section .traps,"ax"

__start_ram:
	b	__start_ram1

	.text

	.type	__start_ram1, %function
__start_ram1:
	# Boot conditions on RPi3
	# CurrentEL: 3

	# Read processor number
	mrs	x7, mpidr_el1
	and	x7, x7, #3

	mrs	x1, currentel
	cmp	x1, #(3<<2)
	b.ne	__start_el2

	# Started in EL3
	# Configure EL3

	# Load EL3 stack pointer (on 32bit)
	adrp	x2,system_stack_el3_base
	add	x2, x2, #:lo12:system_stack_el3_base
	ldr	w0,[x2, x7, lsl #2]
	mov	sp,x0

	# Load vbars
	adrp	x0,__vectors
	add	x0,x0,#:lo12:__vectors
	msr	vbar_el3, x0
	msr	vbar_el2, x0
	msr	vbar_el1, x0

	# Enable SMPEN
	mrs	x0, S3_1_C15_C2_1
	orr	x0, x0, #0x40
	msr	S3_1_C15_C2_1, x0

	# SCR_EL3: TWE=0, TWI=0, ST=0, RW(10)=1, SIF=0, HCE(8)=1, SMD=0,
	#          EA=0, FIQ=0, IRQ=0, NS=1
	mov	x0,#0x501
	msr	scr_el3, x0

	# SCTLR_EL3: EE=0, WXN=0, I=0, SA(3)=1, C=0, A(1)=1, M=0
	mov	x0,#0x6
	msr	sctlr_el3, x0

	# MDCR_EL3: EPMAD=0, EDAD=0, SPME=0, SDD=0, SPD32=0, TDOSA=0, TDA=0,
	#           TPM=0
	msr	mdcr_el3, xzr

	# ACTLR_EL3: let it to 0

	# Enable FPU (don't trap accesses)
	msr	cptr_el3, xzr

	# HCR_EL2: RW(1)
	mov	x0,#(1<<31)
	msr	hcr_el2,x0

	# Switch to el2h, set AIF
	mov	x0, #(0x1c0 + 9)
	msr	spsr_el3, x0
	adr	x0, __start_el2
	msr	elr_el3, x0

	isb
	eret

__start_el2:
	cmp	x1, #(3<<2)
	b.ne	__start_el1

	// Load EL2 stack pointer (on 32bit)
	adrp	x2,system_stack_el2_base
	add	x2, x2, #:lo12:system_stack_el2_base
	ldr	w0,[x2, x7, lsl #2]
	mov	sp,x0

	// Enable FPU (don't trap accesses)
	msr	cptr_el2, xzr

	// Load vbars
	adrp	x0,__vectors
	add	x0,x0,#:lo12:__vectors
	msr	vbar_el2, x0
	msr	vbar_el1, x0

	// Switch to el1h, set AIF
	mov	x0, #(0x1c0 + 5)
	msr	spsr_el2, x0
	adr	x0, __start_el1
	msr	elr_el2, x0

	isb
	eret

__start_el1:
	// Load EL1 stack pointer (on 32bit)
	adrp	x2,system_stack_base
	add	x2, x2, #:lo12:system_stack_base
	ldr	w0,[x2, x7, lsl #2]
	mov	sp,x0

	// Load vbars
	adrp	x0,__vectors
	add	x0,x0,#:lo12:__vectors
	msr	vbar_el1, x0

	/* Enable SIMD.  */
	mov	x0, #(1 << 20)
	msr	cpacr_el1, x0

	/* Enable MMU and cache.  */
	mov	x0, xzr
	tlbi	aside1is, x0
	adrp	x0,__mmu_l2_000000000
	msr	ttbr0_el1, x0
	// TCR_EL1: TBI0=0, AS=0, IPS=000 (4GB),
	//          TG1=10, SH1=10, ORGN1=11, IRGN1=11, EPD1=1, A1=0, T1SZ=32
	//          TG0=00, SH0=10, ORGN0=11, IRGN0=11, EPD0=0,       T0SZ=32
	movz	x0, #((32 << 0) | (1 << 7) | (3 << 8) | (3 << 10) | (2 << 12) \
	              | (2 << 14)), LSL #16
	movk	x0, #((32 << 0) | (0 << 7) | (3 << 8) | (3 << 10) | (2 << 12) \
	              | (0 << 14))
	msr	tcr_el1, x0
	movz	x0,#0x00ee
	msr	mair_el1,x0
	isb
	mrs	x0, sctlr_el1
	movz	x1, #((1 << 12) + (1 << 2) + (1 << 0))  //  Bit I, C, M
	orr	x0, x0, x1
	bic	x0, x0, #(1 << 19)	//  Clear WXN
	msr	sctlr_el1, x0
	isb

	# Move slave processors to an infinite loop
	cbz	x7, __start_master
	movz	x1, #0x4000, LSL #16	//  Core 0 Mailbox 3
	movk	x1, #0x00cc
	adds	x1, x1, x7, LSL #4	//  Core N Mailbox 3
0:	ldr	w0, [x1]		//  Read mailbox
	cbnz	w0, 1f			//  Break if not 0
	wfe				//  Busy loop
	b	0b
1:	str	w0, [x1]		//  Clear mailbox
	br	x0			//  Jump


__start_master:

	/* CPU0: Clear .bss, assuming in the first 4GB */
	ldr	w0,bss_segment + 0
	ldr	w1,bss_segment + 4
0:	cbz	x1,1f
	/* FIXME: write multiple words?  */
	str	xzr,[x0],#8
	sub	x1,x1,#1
	cbnz	x1,0b
1:

	/* Call the main routine */
	bl	main
	bl	_exit
0:	b	0b
	.size	__start_ram1, . - __start_ram1

	.globl	__start_slave_cpu
	.type __start_slave_cpu, %function
__start_slave_cpu:
	mov	x29, xzr
	mov	x19, x7
	// Initialize devices
	bl	__gnat_initialize_cpu_devices

	/* Call the main routine (with CPU_Id as argument).  */
	add	x0,x19,#1
	bl	__gnat_initialize_slave
1:	b	1b
	.size __start_slave_cpu, . - __start_slave_cpu


	.p2align	3

	# BSS base and size
bss_segment:
	.word	__bss_start
	.word	__bss_dwords

system_stack_base:
	.word	__cpu0_stack_end
	.word	__cpu1_stack_end
	.word	__cpu2_stack_end
	.word	__cpu3_stack_end

system_stack_el3_base:
	.word	__cpu0_stack_el3_end
	.word	__cpu1_stack_el3_end
	.word	__cpu2_stack_el3_end
	.word	__cpu3_stack_el3_end

system_stack_el2_base:
	.word	__cpu0_stack_el2_end
	.word	__cpu1_stack_el2_end
	.word	__cpu2_stack_el2_end
	.word	__cpu3_stack_el2_end

interrupt_stack_start_lit:
	.dword	__interrupt_stack_start
interrupt_stack_end_lit:
	.dword	__interrupt_stack_end

	.data
interrupt_stack_el1_base:
	.dword	__cpu0_interrupt_stack_end
	.dword	__cpu1_interrupt_stack_end
	.dword	__cpu2_interrupt_stack_end
	.dword	__cpu3_interrupt_stack_end

	.text

	.macro  ehandler label, id
	.p2align 7
\label:
	// Save all registers (32 * 8 = 256)
	stp	x0, x1, [sp, #-256]!
	stp	x2, x3, [sp, #16]
	stp	x4, x5, [sp, #32]
	stp	x6, x7, [sp, #48]
	stp	x8, x9, [sp, #64]
	stp	x10, x11, [sp, #80]
	stp	x12, x13, [sp, #96]
	stp	x14, x15, [sp, #112]
	stp	x16, x17, [sp, #128]
	stp	x18, x19, [sp, #144]
	stp	x20, x21, [sp, #160]
	stp	x22, x23, [sp, #176]
	stp	x24, x25, [sp, #192]
	stp	x26, x27, [sp, #208]
	stp	x28, x29, [sp, #224]
	str	x30, [sp, #240]
	//  Call handler with context and number
	mov	x0, sp
	mov	x1, #\id
	bl	__trap_dump
	//  Restore
	ldp	x2, x3, [sp, #16]
	ldp	x4, x5, [sp, #32]
	ldp	x6, x7, [sp, #48]
	ldp	x8, x9, [sp, #64]
	ldp	x10, x11, [sp, #80]
	ldp	x12, x13, [sp, #96]
	ldp	x14, x15, [sp, #112]
	ldp	x16, x17, [sp, #128]
	ldp	x18, x19, [sp, #144]
	ldr	x30, [sp, #240]
	//  No need to restore callee saved registers
	ldp	x0, x1, [sp], #256
	eret
	.endm

	//  Interrupt handle frame size
	//  x0-x18 		(19*8 = 152)
	//  x29-x30 		(16   -> 168)
	//  spsr, elr, cpacr	(24   -> 192)
	//  sp, PAD             (16   -> 208)
#define IFRAME_SIZE 208

	//  interrupt handler
	.macro  ihandler_start label
	.p2align 7
\label:
	//  1) save caller-saved regs
	stp	x0, x1, [sp, #-IFRAME_SIZE]!
	stp	x2, x3, [sp, #16]
	stp	x4, x5, [sp, #32]
	stp	x6, x7, [sp, #48]
	stp	x8, x9, [sp, #64]
	stp	x10, x11, [sp, #80]
	stp	x12, x13, [sp, #96]
	stp	x14, x15, [sp, #112]
	stp	x16, x17, [sp, #128]
	stp	x18, x29, [sp, #144]
	mrs	x4, spsr_el1
	mrs	x5, elr_el1
	mrs	x6, cpacr_el1
	stp	x30, x4, [sp, #160]
	stp	x5, x6, [sp, #176]

	// 2) load stack pointer
	adrp	x0,interrupt_stack_el1_base
	add	x0, x0, #:lo12:interrupt_stack_el1_base
	mrs	x1, mpidr_el1
	and	x1, x1, #3
	ldr	x2, [x0, x1, lsl #3]	//  Load new stack pointer

	// 3) Create a frame, switch to irq stack
	mov	x1, sp
	stp	x29, x30, [x2, #-32]!
	str	x1, [x2, #16]
	mov	sp, x2

	b	\label\()_cont
	.endm

	.macro  ihandler_cont label, handler
\label\()_cont:
	//  4) disable fpu
	mrs	x4, cpacr_el1
	bic	x4, x4, #(1 << 20)
	msr	cpacr_el1, x4

	//  5) call handler
	bl	\handler

	//  6) Switch back to EL1 stack
	ldr	x1, [sp, #16]
	mov	sp, x1

	//  7) switch context if needed
	bl	__gnat_context_switch_needed
	cbz	x0, 1f

	bl	__gnat_pre_context_switch
	bl	__gnat_context_switch

	//  8) restore registers
1:
	ldp	x5, x6, [sp, #176]
	ldp	x30, x4, [sp, #160]
	msr	spsr_el1, x4
	msr	elr_el1, x5
	msr	cpacr_el1, x6
	ldp	x18, x29, [sp, #144]
	ldp	x16, x17, [sp, #128]
	ldp	x14, x15, [sp, #112]
	ldp	x12, x13, [sp, #96]
	ldp	x10, x11, [sp, #80]
	ldp	x8, x9, [sp, #64]
	ldp	x6, x7, [sp, #48]
	ldp	x4, x5, [sp, #32]
	ldp	x2, x3, [sp, #16]
	ldp	x0, x1, [sp], #IFRAME_SIZE

	//  9) return
	eret
	.endm

__vector_restore:
	//  Restore
	ldp	x2, x3, [sp, #16]
	ldp	x4, x5, [sp, #32]
	ldp	x6, x7, [sp, #48]
	ldp	x8, x9, [sp, #64]
	ldp	x10, x11, [sp, #80]
	ldp	x12, x13, [sp, #96]
	ldp	x14, x15, [sp, #112]
	ldp	x16, x17, [sp, #128]
	ldp	x18, x19, [sp, #144]
	ldr	x30, [sp, #240]
	//  No need to restore callee saved registers
	ldp	x0, x1, [sp], #256
	eret

	# Vector table
	.p2align 11
__vectors:
	//  EL0 stack
	ehandler __trap_syn_el0, 1
	ehandler __trap_irq_el0, 2
	ehandler __trap_fiq_el0, 3
	ehandler __trap_ser_el0, 4

	//  Current exception level
	.p2align 7
__trap_syn_elx:
	// Save all caller-saved registers (32 * 8 = 256)
	stp	x0, x1, [sp, #-256]!
	stp	x2, x3, [sp, #16]
	stp	x4, x5, [sp, #32]
	stp	x6, x7, [sp, #48]
	stp	x8, x9, [sp, #64]
	stp	x10, x11, [sp, #80]
	stp	x12, x13, [sp, #96]
	stp	x14, x15, [sp, #112]
	stp	x16, x17, [sp, #128]
	stp	x18, x19, [sp, #144]
	str	x30, [sp, #240]
	adr	x30,__vector_restore
	mrs	x0, esr_el1
	ubfx	x1, x0, #26, #6
	cmp	x1, #0x07
	bne	1f
	b	__gnat_fpen_trap
1:
	stp	x20, x21, [sp, #160]
	stp	x22, x23, [sp, #176]
	stp	x24, x25, [sp, #192]
	stp	x26, x27, [sp, #208]
	stp	x28, x29, [sp, #224]
	//  Call handler with context and number
	mov	x0, sp
	mov	x1, #5
	b	__trap_dump

	ihandler_start __trap_irq_elx
	ehandler __trap_fiq_elx, 7
	ehandler __trap_ser_elx, 8

	//  Lower level, aarch64
	ehandler __trap_syn_a64, 9
	ehandler __trap_irq_a64, 10
	ehandler __trap_fiq_a64, 11
	ehandler __trap_ser_a64, 12
	ehandler __trap_syn_a32, 13
	ehandler __trap_irq_a32, 14
	ehandler __trap_fiq_a32, 15
	ehandler __trap_ser_a32, 16

	ihandler_cont __trap_irq_elx,__gnat_irq_handler

	.weak	__gnat_irq_handler
	.weak	__gnat_context_switch_needed
	.weak	__gnat_pre_context_switch
	.weak	__gnat_context_switch
	.weak	__gnat_initialize_cpu_devices
	.weak	__gnat_initialize_slave
	.weak	__gnat_fpen_trap

#include "memmap.S"
