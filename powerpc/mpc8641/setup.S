## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S E T U P                                 #
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
        .global _setup
	.global _setup_cpu

	# Per-CPU setup (init fpu)
_setup_cpu:
#ifndef NOFPU
        # Enable FPU.
	li    %r3,0x2000
	mtmsr %r3
        isync

        # Disable overflow and underflow exceptions.
        mtfsb0 25
        mtfsb0 26
#else
	# Disable FPU.
	li	%r3,0
	mtmsr	%r3
	isync
#endif
	blr
	.size _setup_cpu, . - _setup

_setup:
        li    %r0,0

        # Clear sbss2
	lis   %r11,(__sbss2_start - 1)@ha
	la    %r11,(__sbss2_start - 1)@l(%r11)
	lis   %r10,(__sbss2_end)@ha
        la    %r10,(__sbss2_end)@l(%r10)
        subf  %r10,%r11,%r10
	mtctr %r10
	b     1f
0:	stbu  %r0,1(%r11)
1:
	bdnz  0b

        # Clear sbss
	lis   %r11,(__sbss_start - 1)@ha
	la    %r11,(__sbss_start - 1)@l(%r11)
	lis   %r10,(__sbss_end)@ha
        la    %r10,(__sbss_end)@l(%r10)
        subf  %r10,%r11,%r10
	mtctr %r10
	b     1f
0:	stbu  %r0,1(%r11)
1:
	bdnz  0b

        # Clear bss
	lis   %r11,(__bss_start - 1)@ha
	la    %r11,(__bss_start - 1)@l(%r11)
	lis   %r10,(__bss_end)@ha
        la    %r10,(__bss_end)@l(%r10)
        subf  %r10,%r11,%r10
	mtctr %r10
	b     1f
0:	stbu  %r0,1(%r11)
1:
	bdnz  0b
        blr
	.size _setup, . - _setup

	.global __eabi
__eabi:
	blr
	.size __eabi, . - __eabi
