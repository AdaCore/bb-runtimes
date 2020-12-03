## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2020 AdaCore                      #
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

	/* This is the interrupt vector table used by all runtimes,
	 * The Cortex-M0 does not support VTOR. */
	.text
	.section .vectors,"a"
	.globl __vectors
	.p2align 8
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end          /* stack top address */
	.word   _start_ram           /* 1 Reset.  */
	.word   fault                /* 2 NMI. */
	.word   fault                /* 3 Hard fault. */
	.word   fault                /* 4 Mem manage. */
	.word   fault                /* 5 Bus fault. */
	.word   fault                /* 6 Usage fault. */
	.word   fault                /* 7 reserved. */
	.word   fault                /* 8 reserved. */
	.word   fault                /* 9 reserved. */
	.word   fault                /* 10 reserved. */
	.word   __gnat_sv_call_trap  /* 11 SVCall. */
	.word   fault                /* 12 reserved. */
	.word   fault                /* 13 reserved. */
	.word   __gnat_pend_sv_trap  /* 14 PendSV. */
	.word   __gnat_sys_tick_trap /* 15 Systick. */
	/* MCU interrupts */
	.word __gnat_irq_trap        /* 16 WWDG */
	.word __gnat_irq_trap        /* 17 PVD */
	.word __gnat_irq_trap        /* 18 RTC */
	.word __gnat_irq_trap        /* 19 FLASH */
	.word __gnat_irq_trap        /* 20 RCC_CRS */
	.word __gnat_irq_trap        /* 21 EXTI0_1 */
	.word __gnat_irq_trap        /* 22 EXTI2_3 */
	.word __gnat_irq_trap        /* 23 EXTI4_15 */
	.word __gnat_irq_trap        /* 24 TSC */
	.word __gnat_irq_trap        /* 25 DMA1_CH1 */
	.word __gnat_irq_trap        /* 26 DMA1_CH2_3_DMA2_CH1_2 */
	.word __gnat_irq_trap        /* 27 DMA1_CH4_5_6_7_DMA2_CH3_4_5 */
	.word __gnat_irq_trap        /* 28 ADC_COMP */
	.word __gnat_irq_trap        /* 29 TIM1_BRK_UP_TRG_COM */
	.word __gnat_irq_trap        /* 30 TIM1_CC */
	.word __gnat_irq_trap        /* 31 TIM2 */
	.word __gnat_irq_trap        /* 32 TIM3 */
	.word __gnat_irq_trap        /* 33 TIM6_DAC */
	.word __gnat_irq_trap        /* 34 TIM7 */
	.word __gnat_irq_trap        /* 35 TIM14 */
	.word __gnat_irq_trap        /* 36 TIM15 */
	.word __gnat_irq_trap        /* 37 TIM16 */
	.word __gnat_irq_trap        /* 38 TIM17 */
	.word __gnat_irq_trap        /* 39 I2C1 */
	.word __gnat_irq_trap        /* 40 I2C2 */
	.word __gnat_irq_trap        /* 41 SPI1 */
	.word __gnat_irq_trap        /* 42 SPI2 */
	.word __gnat_irq_trap        /* 43 USART1 */
	.word __gnat_irq_trap        /* 44 USART2 */
	.word __gnat_irq_trap        /* 45 USART3_4_5_6_7_8 */
	.word __gnat_irq_trap        /* 46 CEC_CAN */
	.word __gnat_irq_trap        /* 47 USB */

	.text
	.thumb_func
	.globl _start_ram
_start_ram:
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
        ldr     r0,=__vectors
        ldr     r0, [r0]
        mov     sp, r0

	bl	_ada_setup_pll
	bl	main

	bl	_exit

hang:   b .

	.text

	.thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

	.thumb_func
.weak __gnat_sv_call_trap
.type __gnat_sv_call_trap, %function
__gnat_sv_call_trap:
0:	b 0b
	.size __gnat_sv_call_trap, . - __gnat_sv_call_trap

	.thumb_func
.weak __gnat_pend_sv_trap
.type __gnat_pend_sv_trap, %function
__gnat_pend_sv_trap:
0:	b 0b
	.size __gnat_pend_sv_trap, . - __gnat_pend_sv_trap

	.thumb_func
.weak __gnat_sys_tick_trap
.type __gnat_sys_tick_trap, %function
__gnat_sys_tick_trap:
0:	b 0b
	.size __gnat_sys_tick_trap, . - __gnat_sys_tick_trap

	.thumb_func
fault:	b fault
