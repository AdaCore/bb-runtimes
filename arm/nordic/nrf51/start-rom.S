## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2018 AdaCore                      #
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

	.syntax unified
	.cpu cortex-m0
	.thumb

	.text

	.thumb_func
	.globl _start_rom
_start_rom:
        /* Copy .data */
	ldr	r0,=__data_start
	ldr	r1,=__data_words
	ldr	r2,=__data_load
        cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds	r0,#4
        adds	r2,#4
	subs	r1,r1,#1
	bne	0b
1:

	/* Clear .bss */
	ldr	r0,=__bss_start
	ldr	r1,=__bss_words
	movs	r2,#0
        cmp	r1,#0
	beq	1f
0:	str	r2,[r0]
        adds	r0,#4
	subs	r1,r1,#1
	bne	0b

1:
        /* Load stack pointer from the first entry of the vector table */
	ldr	r0,=__vectors
        ldr     r0, [r0]
        mov     sp, r0

        bl	main

	bl	_exit

hang:   b .
