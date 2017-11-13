## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2017 AdaCore                      #
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
        .cpu cortex-m1
        .arch armv6-m
        .thumb

        .section .vectors
        .globl __vectors
        .align 2
__vectors:
        /* Cortex-M core interrupts */
        .word   __stack_end          /* stack top address */
        .word   _start_rom           /* 1 Reset */
        .word   fault                /* 2 NMI. */
        .word   fault                /* 3 Hard fault. */
        .word   fault                /* 4 Mem manage. */
        .word   fault                /* 5 Bus fault. */
        .word   fault                /* 6 Usage fault. */
        .word   fault                /* 7 reserved. */
        .word   fault                /* 8 reserved. */
        .word   fault                /* 9 reserved. */
        .word   fault                /* 10 reserved. */
        .word   fault                /* 11 SVCall. */
        .word   fault                /* 12 Breakpoint. */
        .word   fault                /* 13 reserved. */
        .word   fault                /* 14 PendSV. */
        .word   fault                /* 15 Systick. */
        /* NVIC interrupts:
         * Only one NVIC interrupt line is used on this platform since the
         * interrupts are actually controlled by the CoreInterrupt device.
         * CoreInterrupt uses this first NVIC line to signal an interrupt to
	 * the CPU.
         */
        .word __gnat_irq_trap_without_os_extension

        .text

        .thumb_func
        .globl _start_rom
_start_rom:
        /* Init stack pointer */
        ldr r0,=__stack_end
        mov sp,r0

        /* Disable all NVIC IRQ */
        ldr  r0,=0xE000E180 // Load NVIC Clear Enable register address
        ldr  r1,=0xFFFFFFFF
        str  r1, [r0]

        /* Clear all NVIC IRQ */
        ldr  r0,=0xE000E280 // Load NVIC Clear Pending register address
        ldr  r1,=0xFFFFFFFF
        str  r1, [r0]

        /* Disable all CoreInterrupt IRQ */
        ldr  r0,=0xA3000024 // Load CoreInterrupt Enable Clear register address
        ldr  r1,=0xFFFFFFFF
        str  r1, [r0]

        /* Clear all CoreInterrupt Soft IRQ */
        ldr  r0,=0xA300001C // Load CoreInterrupt Soft Clear register address
        ldr  r1,=0xFFFFFFFF
        str  r1, [r0]

        /* Disable the CoreTimer and clear its interrupts */

        /* We do this to avoid early interrupts if the core was not properly
	 * reset and the timer is still running.
         */

        ldr  r0,=0xA2000008 // Load CoreTimer control register address
        movs r1, 0
        str  r1, [r0]       // Set control register to zero (disable everything)

        ldr  r0,=0xA2000010 // Load CoreTimer interrupt clear register address
        movs r1, 1
        str  r1, [r0]       // Write any value to the interrupt clear register
                            // to clear the pending interrupt (if any).


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
	bl	main
	bl	_exit
        b       fault

	.thumb_func
fault:	b fault
