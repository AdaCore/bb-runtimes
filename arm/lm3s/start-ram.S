## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
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

	.syntax unified
	.cpu cortex-m3
	.thumb

	.text

	.thumb_func
	.globl _start_ram
	.type _start_ram, %function
_start_ram:
	/* Init stack */
	ldr	sp,.LinitSp

	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words
	mov	r2,#0
	cbz	r1,1f
0:	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b

1:
	bl	_ada_setup_pll
	bl	main

	bl	_exit

	.global __aeabi_ldiv0
	.type __aeabi_ldiv0, %function

	.global __aeabi_idiv0
	.type __aeabi_idiv0, %function

	.global __aeabi_unwind_cpp_pr0
	.type __aeabi_unwind_cpp_pr0, %function

	.global __aeabi_unwind_cpp_pr1
	.type __aeabi_unwind_cpp_pr1, %function

	.global __aeabi_unwind_cpp_pr2
	.type __aeabi_unwind_cpp_pr2, %function

	.thumb_func
__aeabi_unwind_cpp_pr0:
__aeabi_unwind_cpp_pr1:
__aeabi_unwind_cpp_pr2:
__aeabi_ldiv0:
__aeabi_idiv0:
hang:   b .

.LinitSp:
	.word __stack_end
