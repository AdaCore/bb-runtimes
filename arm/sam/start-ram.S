## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2019 AdaCore                      #
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
	.cpu cortex-m4
	.thumb

	/* This is the initial vector table and also the one that is used by
	   the light runtime. The tasking runtimes install their own table (using
	   VTOR register) defined in handler.S.  */
	.section .vectors,"a"
__vectors0:
	.word   __stack_end     /* stack top address */
	.word   _start_ram      /* 1 Reset */
	.word	hang		/* 2 NMI */
	.word	hang		/* 3 HardFault */
	.word	hang		/* 4 MemManage */
	.word	hang		/* 5 BusFault  */
	.word	hang		/* 6 UsageFault */
	.word	0, 0, 0, 0	/* 7, 8, 9, 10 : Reserved */
	.word	hang		/* 11 SVC_Handler  */
	.word	hang		/* 12 DebugMon */
	.word	0		/* 13 Reserved	*/
	.word	hang		/* 14 PendSV */
	.word	hang		/* 15 SysTick */

	.text

	.thumb_func
	.globl _start_ram
_start_ram:
	/* Set stack */
	movw	r0,#:lower16:__stack_end
	movt	r0,#:upper16:__stack_end
	mov	sp,r0

	/* Enable FPU and cache when appropriate */
	bl _sam_start_common

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

hang:   b .
