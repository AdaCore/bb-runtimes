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

	mrs	x1, currentel
	cmp	x1, #(3<<2)
	b.ne	__start_ram1	// failed

	# Started in EL3
	# Configure EL3

	# Set timer frequency (just an indicator for software).
	ldr	x2, =19200000
	msr	cntfrq_el0, x2

	# Read processor number
	mrs	x7, mpidr_el1
	and	x7, x7, #3

	# Load EL3 stack pointer (on 32bit)
	adrp	x2,system_stack_el3_base
	add	x2, x2, #:lo12:system_stack_el3_base
	ldr	w0,[x2, x7, lsl #2]
	mov	sp,x0

	# Load EL2 stack pointer (on 32bit)
	adrp	x2,system_stack_el2_base
	add	x2, x2, #:lo12:system_stack_el2_base
	ldr	w0,[x2, x7, lsl #2]
	msr	sp_el2,x0

	# Load vbars
	adrp	x0,__vectors_el3_cur
	add	x0,x0,#:lo12:__vectors_el3_cur
	msr	vbar_el3, x0
	adrp	x0,__vectors_el2_cur
	add	x0,x0,#:lo12:__vectors_el2_cur
	msr	vbar_el2, x0

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

	# HCR_EL2: RW(1), DC, IMO(4), FMO(3), AMO(5)
	# For RPi3, redirect IRQ/FIQ/ABT to EL2.
	ldr	x0,=((1<<31) | (1<<12) | (1<<5) | (1<<4) | (1<<3))
	msr	hcr_el2,x0

	# Switch to el2h, set AIF
	mov	x0, #(0x1c0 + 9)
	msr	spsr_el3, x0
	adr	x0, __start_el2
	msr	elr_el3, x0

	isb
	eret

__start_el2:
	# Enable FPU (don't trap accesses)
	msr	cptr_el2, xzr

	/* Enable MMU and cache.  */
	mov	x0, xzr
	tlbi	alle2
	adrp	x0,__mmu_l1_000000000
	msr	ttbr0_el2, x0
	ldr	x0, mmu_tcr
	msr	tcr_el2, x0
	movz	x0,#0x00ee
	msr	mair_el2,x0
	isb
	mrs	x0, sctlr_el2
	movz	x1, #((1 << 12) + (1 << 2) + (1 << 0))  //  Bit I, C, M
	orr	x0, x0, x1
	bic	x0, x0, #(1 << 19)	//  Clear WXN
	msr	sctlr_el2, x0
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

	// TCR_EL2: SH0=11, ORGN0=01, IRGN1=01
mmu_tcr:
	.dword	__mmu_tcr + (1 << 8) | (1 << 10) | (3 << 12)

system_stack_el2_base:
	.word	__cpu0_stack_end
	.word	__cpu1_stack_end
	.word	__cpu2_stack_end
	.word	__cpu3_stack_end

system_stack_el3_base:
	.word	__cpu0_stack_el3_end
	.word	__cpu1_stack_el3_end
	.word	__cpu2_stack_el3_end
	.word	__cpu3_stack_el3_end

interrupt_stack_start_lit:
	.dword	__interrupt_stack_start
interrupt_stack_end_lit:
	.dword	__interrupt_stack_end

	.data
	.globl  interrupt_stack_el2_base
interrupt_stack_el2_base:
	.dword	__cpu0_interrupt_stack_end
	.dword	__cpu1_interrupt_stack_end
	.dword	__cpu2_interrupt_stack_end
	.dword	__cpu3_interrupt_stack_end

	.weak	__gnat_initialize_cpu_devices
	.weak	__gnat_initialize_slave

#include "memmap.S"
