## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                           B O O T - S E T U P                             #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                         Copyright (C) 2013 AdaCore                        #
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

# mpc5566 setup when powering up (boot from bam or flash)

	.text
	.global _setup_pll
	.type _setup_pll,@function
_setup_pll:
	# External clock control register: CLKOUT=SYSCLK / 2
	lis	%r5,0xc3f90000@h
	li	%r4,0x3f01	# ENGDIV=0x3f, EBDF=1
	stw	%r4,0x984(%r5)

	# Initialize PLL to 128Mhz with FM=2%
	# Fsys = Fref * (MFD+4) / ((PREDIV + 1) * 2 ** RFD)
	# EXP=(MFD+4)*M*P/100  MFD=28 -> M=160, P=2 -> EXP=102
	lis	%r5,0xc3f80000@h
	lis	%r4,0x1e080000@h /* PREDIV=1, MFD=28 RFD=1.  */
	ori	%r4,%r4,0x0066   /* EXP=102 */
	stw	%r4,0(%r5)	/* Fsys =  8 * 32 / (2 * 2) = 64   */
	# Wait for FMPLL to LOCK
	# We need this two stages approach to avoid out of range frequency
0:	lwz	%r3,4(%r5)
	andi.	%r3,%r3,8
	beq	0b
	# Enable FM
	ori	%r4,%r4,0x1800	 /* RATE=1, DEPTH=2 */
	stw	%r4,0(%r5)
	# Wait for PLL to relock
0:	lwz	%r3,4(%r5)
	andi.	%r3,%r3,8
	beq	0b
	# Verify calibration completed and successful
	lwz	%r3,4(%r5)
	andi.	%r3,%r3,3
	cmpli	%cr7,%r3,3
0:	bne	%cr7,0b
	# Modify RFD. No need to lock, as this doesn't touch the pll.
	xoris	%r4,%r4,0x00080000@h /* RFD=0.  */
	                         /* Fsys =  8 * 32 / (2 * 1) = 128.  */
	stw	%r4,0(%r5)

	# Init ESCI_A
	lis	%r3,0xfffb0000@h	# Base
	# 9600 baud (br = Fsys/(16 * baud))
	lis	%r4, 0x0341 	# 9600 baud for a 128Mhz sysclk
	stw	%r4,0(%r3)	# CR1

	# Clear and enable BTB
	li	%r3,0x0201
	mtspr	1013,%r3

	# Branch prediction is enabled by default
	
	blr
	.size _setup_pll, . - _setup_pll
