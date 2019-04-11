## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                           S T A R T - F L A S H                           #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                         Copyright (C) 2012 AdaCore                        #
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

# mpc5566 Entry point when booting from flash

# TODO:
# Increase PLL to 128 Mhz
# Enable cache
# configure flash, EBI, XBAR

	# SPR definitions
L1CSR0 = 1010

	.section .init,"ax"
	# RCHW (Reset configuration halfword)
	.word	0x025a	# 16bit EBI
	.word	0x025a
	.long 	_start_flash

	.text
	.global _start_flash
	.type _start_flash,@function
_start_flash:
	# Initialize pll
	bl	_setup_pll

	# Clear sram, this is required to initialize ECC
	# Must be done using 64 bits store (??? use spe)
	xor	%r31,%r31,%r31
	xor	%r30,%r30,%r30
	lis	%r11,__sram_start@h
	li	%r10,__sram_size_dword@l
	mtctr	%r10
0:	stmw	%r30,0(%r11)
	addi	%r11,%r11,8
	bdnz	0b

	# Configure flash
	# This register must not be written while executing from flash
	lis	%r11,__sram_start@h
	lis	%r10,_mem_write@h
	ori	%r10,%r10,_mem_write@l
	lmw	%r29,0(%r10)
	stmw	%r29,0(%r11)
	# Prefetching trigered by MCU core
	lis	%r3,0x1
	# Up to 132Mhz: APC=0b010 WWSC=0b01 RWSC=0b011 DPFEN=0b11 IPFEN=0b11
	#               PFLIM=0b110 BFEN=0b1
	ori	%r3,%r3,0x4bfd
	lis	%r4,0xc3f8801c@h
	ori	%r4,%r4,0xc3f8801c@l
	# Cache is not yet enabled, so no need to sync them
	mbar
	mtctr	%r11
	bltr

        # Copy data
	lis   %r11,(__data_start - 4)@ha
	la    %r11,(__data_start - 4)@l(%r11)
	lis   %r10,(__data_end)@ha
        la    %r10,(__data_end)@l(%r10)
        subf  %r10,%r11,%r10
	lis   %r8,(__data_load - 4)@ha
	la    %r8,(__data_load - 4)@l(%r8)
	mtctr %r10
	b     1f
0:      lwzu  %r4,4(%r8)
	stwu  %r4,4(%r11)
1:
	bdnz  0b

	# Invalidate cache (disabled)
0:	li	%r10,2
	msync
	isync
	mtspr	L1CSR0,%r10
	# Wait until operation is completed
1:	mfspr	%r10,L1CSR0
	andi.	%r7,%r10,2
	bne	%cr0,1b
	# Attempt again if operation was aborted
	andi.	%r7,%r10,4
	bne	%cr0,0b
	# Enable cache
	lis	%r10,0x0011 # Copy back + Enable parity checking
	ori	%r10,%r10,0x0001 # Cache enabled
	msync
	isync
	mtspr	L1CSR0,%r10

        # Load the new stack pointer (note that r31=0)
        lis	%r1,(__stack_end - 16)@h
        ori	%r1,%r1,(__stack_end - 16)@l
        # End of frame markers
	xor	%r31,%r31,%r31
        stw	%r31,0(%r1)
	stw	%r31,4(%r1)

	# Initialize timer (could be done in Ada)
	mttbu	%r31
	mttbl	%r31
	mfspr	%r4,1008
	ori	%r4,%r4,0x4000 # TBEN=1 (enable time base)
	mtspr	1008,%r4

	# Setup spe, r2, r13
	bl	_setup

#ifdef CALL__init
	# Call constructors
	bl    __init
#endif

        # Run the program
        bl	main

        # Reset the board.
        bl      _exit

	.size _start_flash, . - _start_flash

	.type _mem_write,@function
_mem_write:
	stw	%r3,0(%r4)
	mbar
	blr
	.size	_mem_write, . - _mem_write
