## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                             S T A R T - B A M                             #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2013 AdaCore                      #
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

# mpc5566 Entry point when booting from internal sram, using BAM

	.section .init,"ax"
	.global _start_bam
	.type _start_bam,@function
_start_bam:
	# Extend watchdog timer period
TCR = 340
#	mfspr	%r10,TCR
#	andis.	%r10,%r10,0x3fe0
#	mtspr	TCR,%r10

	# Initialize pll
	bl	_setup_pll

	# Clear sram, this is required to initialize ECC
	# Must be done using 64 bits store (??? use spe)
	xor	%r31,%r31,%r31
	xor	%r30,%r30,%r30
	lis	%r11,__sram_clear_start@h
	ori	%r11,%r11,__sram_clear_start@l
	li	%r10,__sram_clear_dword@l
	mtctr	%r10
	cmpwi	%r10,0
	beq	1f
0:	stmw	%r30,0(%r11)
	addi	%r11,%r11,8
	bdnz	0b
1:

  	# Load the new stack pointer
        lis	%r11,(__stack_end - 16)@h
        ori	%r11,%r11,(__stack_end - 16)@l
        # back-link to the previous one.
        stw	%r1,0(%r11)
        # Set the stack.
        mr      %r1,%r11
        # Save lr
        mflr	%r0
        stw	%r0,4(%r1)

	# Setup spe, r2, r13
	bl	_setup

        # Run the program
        bl	main

        # Reset the board.
        bl      _exit

	.size _start_bam, . - _start_bam
