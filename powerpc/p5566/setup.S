## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S E T U P                                 #
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

        SPEFSCR = 512
        MSR_SPE = (1 << (63 - 38))

	.text
	.global _setup
	.type _setup,@function
_setup:
        # Enable SPE
	mfmsr	%r3
	oris	%r3,%r3,MSR_SPE@h
	mtmsr	%r3
        isync

	# Enable overflow, division by zero, invalid exceptions.
	li	%r0,0x34
	mtspr	SPEFSCR,%r0

        # load r13 with _SDA_BASE_ address
        lis	%r13,_SDA_BASE_@ha
        la	%r13,_SDA_BASE_@l(%r13)

        # load r2 with _SDA2_BASE_ address
        lis     %r2,_SDA2_BASE_@ha
        la      %r2,_SDA2_BASE_@l(%r2)

	blr
	.size _setup, . - _setup

	.global __eabi
	.type __eabi,@function
__eabi:
	blr
	.size __eabi, . - __eabi
