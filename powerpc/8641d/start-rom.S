## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2014 AdaCore                      #
#                                                                           #
#  GNAT is free software;  you can  redistribute it  and/or modify it under #
#  terms of the  GNU General Public License as published  by the Free Soft- #
#  ware  Foundation;  either version 3,  or (at your option) any later ver- #
#  sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
#  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
#  or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                           #
#  As a special exception under Section 7 of GPL version 3, you are granted #
#  additional permissions described in the GCC Runtime Library Exception,   #
#  version 3.1, as published by the Free Software Foundation.               #
#                                                                           #
#  You should have received a copy of the GNU General Public License and    #
#  a copy of the GCC Runtime Library Exception along with this program;     #
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
#  <http://www.gnu.org/licenses/>.                                          #
#                                                                           #
#############################################################################

	.section ".text"
        .global _start_rom

#define MSSCR0	1014
#define PIR	1023
#define	_PPC_MSSCR0_BIT_ID	26

/* Put CPU Id (from MSSCR ID field) to REG.  */
#define GET_CPU_ID(reg) \
	mfspr	reg, MSSCR0;  \
	rlwinm.	reg, reg, _PPC_MSSCR0_BIT_ID + 1, 31, 31

_start_rom:
	/* Init FPU.  */
        bl	_setup_cpu

	/* Initialize PIR register with the CPU ID.  */
	GET_CPU_ID(%r3)
	mtspr	PIR, %r3
	
	/* Different code paths per CPU.  */
	beq	_cpu0_start

_cpu1_start:
	/* Check if slave_stack_table is linked (sfp).  */
	lis	%r5, slave_stack_table@h
	ori	%r5, %r5, slave_stack_table@l
	cmplwi	%r5, 0
	
	/* If not, we are in zfp.  */
	beq	_cpu1_zfp_mode

	/* Load the stack from slave_stack_table. This is generic code and
	   over-engineered for our case (8641d has only one slave).  */
	GET_CPU_ID(%r3)
	addi	%r4,%r3,-1	/* Offset 0 is for first slave.  */
	slwi	%r4,%r4,2
	lwzx	%r1,%r5,%r4

        # Allocate a cleared frame
        li	%r0, 0
	stwu	%r0, -16(%r1)
        stw	%r0, 12(%r1)

	/* Initialize_Slave takes a 1-based CPU ID as an argument */
	addi	%r3,%r3,1
	bl	__gnat_initialize_slave

_cpu1_exit:
	b	_cpu1_exit

_cpu1_zfp_mode:
  	# Load the stack from the __cpu1_stack_end symbol */
        lis	%r1,__cpu1_stack_end@h
        ori	%r1,%r1,__cpu1_stack_end@l

        # Allocate a cleared frame
        li	%r0,0
	stwu	%r0,-16(1)
        stw	%r0,12(1)

	bl	main
1:	bl	_exit
	b	1b

_cpu0_start:
	# Load the stack.
        lis	%r1,__cpu0_stack_end@h
        ori	%r1,%r1,__cpu0_stack_end@l

        # Allocate a cleared frame
        li	%r0,0
	stwu	%r0,-16(1)
        stw	%r0,12(1)

        # Copy sdata2
	lis	%r11,(__sdata2_start - 1)@ha
	la	%r11,(__sdata2_start - 1)@l(%r11)
	lis	%r10,(__sdata2_end)@ha
        la	%r10,(__sdata2_end)@l(%r10)
        subf	%r10,%r11,%r10
	lis	%r8,(__sdata2_load - 1)@ha
	la	%r8,(__sdata2_load - 1)@l(%r8)
	mtctr	%r10
	b	1f
0:      lbzu	%r4,1(%r8)
	stbu	%r4,1(%r11)
1:
	bdnz	0b

        # Copy data
	lis	%r11,(__data_start - 1)@ha
	la	%r11,(__data_start - 1)@l(%r11)
	lis	%r10,(__data_end)@ha
        la	%r10,(__data_end)@l(%r10)
        subf	%r10,%r11,%r10
	lis	%r8,(__data_load - 1)@ha
	la	%r8,(__data_load - 1)@l(%r8)
	mtctr	%r10
	b	1f
0:      lbzu	%r4,1(%r8)
	stbu	%r4,1(%r11)
1:
	bdnz	0b

        bl	_setup

        # Run the program
	bl	main

        # Try to stop the machine
        bl	_exit

	.size _start_rom, . - _start_rom

	.section ".reset","ax"
_reset:
	b	_start_rom
	.size _reset, . - _reset

	.weak	__gnat_initialize_slave
	.weak   slave_stack_table
