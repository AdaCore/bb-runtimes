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
	   the zfp runtime. Ravenscar runtimes install its own table (using
	   VTOR register) defined in handler.S.  */
	.section .vectors,"a"
	.globl	__vectors0
__vectors0:
	b.w	_start_ram	/* stack top address */
	.word	_start_ram	/* 1 Reset */
	.word   hang		/* 2 NMI */
	.word   hang		/* 3 HardFault */
	.word   hang		/* 4 MemManage */
	.word	hang		/* 5 BusFault  */
	.word	hang		/* 6 UsageFault */
	.word	0, 0, 0, 0	/* 7, 8, 9, 10 : Reserved */
	.word	hang		/* 11 SVC_Handler  */
	.word	hang		/* 12 DebugMon */
	.word	0		/* 13 Reserved  */
	.word	hang		/* 14 PendSV */
	.word	SysTick_Handler

	.word	SUPC_IrqHandler		/* 0  Supply Controller */
	.word	RSTC_IrqHandler		/* 1  Reset Controller */
	.word	RTC_IrqHandler		/* 2  Real Time Clock */
	.word	RTT_IrqHandler		/* 3  Real Time Timer */
	.word	WDT_IrqHandler		/* 4  Watchdog Timer */
	.word	PMC_IrqHandler		/* 5  PMC */
	.word	EEFC_IrqHandler		/* 6  EEFC */
	.word	IrqHandlerNotUsed	/* 7  Reserved */
	.word	UART0_IrqHandler	/* 8  UART0 */
	.word	UART1_IrqHandler	/* 9  UART1 */
	.word	SMC_IrqHandler		/* 10 SMC */
	.word	PIOA_IrqHandler		/* 11 Parallel IO Controller A */
	.word	PIOB_IrqHandler		/* 12 Parallel IO Controller B */
	.word	PIOC_IrqHandler		/* 13 Parallel IO Controller C */
	.word	USART0_IrqHandler	/* 14 USART 0 */
	.word	USART1_IrqHandler	/* 15 USART 1 */
	.word	USART2_IrqHandler	/* 16 USART 2 */
	.word	IrqHandlerNotUsed	/* 17 Reserved */
	.word	MCI_IrqHandler		/* 18 MCI */
	.word	TWI0_IrqHandler		/* 19 TWI 0 */
	.word	TWI1_IrqHandler		/* 20 TWI 1 */
	.word	SPI_IrqHandler		/* 21 SPI */
	.word	SSC_IrqHandler		/* 22 SSC */
	.word	TC0_IrqHandler		/* 23 Timer Counter 0 */
	.word	TC1_IrqHandler		/* 24 Timer Counter 1 */
	.word	TC2_IrqHandler		/* 25 Timer Counter 2 */
	.word	TC3_IrqHandler		/* 26 Timer Counter 3 */
	.word	TC4_IrqHandler		/* 27 Timer Counter 4 */
	.word	TC5_IrqHandler		/* 28 Timer Counter 5 */
	.word	ADC_IrqHandler		/* 29 ADC controller */
	.word	DAC_IrqHandler		/* 30 DAC controller */
	.word	PWM_IrqHandler		/* 31 PWM */
	.word	CRCCU_IrqHandler	/* 32 CRC Calculation Unit */
	.word	ACC_IrqHandler		/* 33 Analog Comparator */
	.word	USBD_IrqHandler		/* 34 USB Device Port */
	.word	IrqHandlerNotUsed	/* 35 not used */

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
