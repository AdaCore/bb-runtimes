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
        .cpu cortex-m1
        .thumb

        .text
        .globl __vectors
        .section .vectors
        .p2align 8
__vectors:
        /* Cortex-M core interrupts */
        /* Vector table defined here because the Cortex-M1 does not have
           VTOR */
        .word   __stack_end          /* stack top address */
        .word   _start_ram           /* 1 Reset.  */
        .word   fault                /* 2 NMI. */
        .word   fault                /* 3 Hard fault. */
        .word   fault                /* 4 reserved. */
        .word   fault                /* 5 reserved. */
        .word   fault                /* 6 reserved. */
        .word   fault                /* 7 reserved. */
        .word   fault                /* 8 reserved. */
        .word   fault                /* 9 reserved. */
        .word   fault                /* 10 reserved. */
        .word   __gnat_sv_call_trap  /* 11 SVCall. */
        .word   __gnat_bkpt_trap     /* 12 Breakpoint. */
        .word   fault                /* 13 reserved. */
        .word   __gnat_pend_sv_trap  /* 14 PendSV. */
        .word   __gnat_sys_tick_trap /* 15 Systick. */
        /* MCU interrupts */
        .word __gnat_irq_trap        /* 16 IRQ0 */
        .word __gnat_irq_trap        /* 17 IRQ1 */
        .word __gnat_irq_trap        /* 18 IRQ2 */
        .word __gnat_irq_trap        /* 19 IRQ3 */
        .word __gnat_irq_trap        /* 20 IRQ4 */
        .word __gnat_irq_trap        /* 21 IRQ5 */
        .word __gnat_irq_trap        /* 22 IRQ6 */
        .word __gnat_irq_trap        /* 23 IRQ7 */
        .word __gnat_irq_trap        /* 24 IRQ8 */
        .word __gnat_irq_trap        /* 25 IRQ9 */
        .word __gnat_irq_trap        /* 26 IRQ10 */
        .word __gnat_irq_trap        /* 27 IRQ11 */
        .word __gnat_irq_trap        /* 28 IRQ12 */
        .word __gnat_irq_trap        /* 29 IRQ13 */
        .word __gnat_irq_trap        /* 30 IRQ14 */
        .word __gnat_irq_trap        /* 31 IRQ15 */
        .word __gnat_irq_trap        /* 32 IRQ16 */
        .word __gnat_irq_trap        /* 33 IRQ17 */
        .word __gnat_irq_trap        /* 34 IRQ18 */
        .word __gnat_irq_trap        /* 35 IRQ19 */
        .word __gnat_irq_trap        /* 36 IRQ20 */
        .word __gnat_irq_trap        /* 37 IRQ21 */
        .word __gnat_irq_trap        /* 38 IRQ22 */
        .word __gnat_irq_trap        /* 39 IRQ23 */
        .word __gnat_irq_trap        /* 40 IRQ24 */
        .word __gnat_irq_trap        /* 41 IRQ25 */
        .word __gnat_irq_trap        /* 42 IRQ26 */
        .word __gnat_irq_trap        /* 43 IRQ27 */
        .word __gnat_irq_trap        /* 44 IRQ28 */
        .word __gnat_irq_trap        /* 45 IRQ29 */
        .word __gnat_irq_trap        /* 46 IRQ30 */
        .word __gnat_irq_trap        /* 47 IRQ31 */

        .text
        .thumb_func
        .globl _start_ram
_start_ram:
        /* Clear .bss */
        ldr     r0,=__bss_start
        ldr     r1,=__bss_words
        movs    r2,#0
        cmp     r1,#0
        beq     1f
0:      str     r2,[r0]
        adds    r0,#4
        subs    r1,r1,#1
        bne     0b

1:
        /* Load stack pointer from the first entry of the vector table */
        ldr     r0,=__vectors
        ldr     r0, [r0]
        mov     sp, r0

        bl      main

        bl      _exit

hang:   b .

        .thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:      b 0b
        .size __gnat_irq_trap, . - __gnat_irq_trap

        .thumb_func
.weak __gnat_sv_call_trap
.type __gnat_sv_call_trap, %function
__gnat_sv_call_trap:
0:      b 0b
        .size __gnat_sv_call_trap, . - __gnat_sv_call_trap

        .thumb_func
.weak __gnat_pend_sv_trap
.type __gnat_pend_sv_trap, %function
__gnat_pend_sv_trap:
0:      b 0b
        .size __gnat_pend_sv_trap, . - __gnat_pend_sv_trap

        .thumb_func
.weak __gnat_sys_tick_trap
.type __gnat_sys_tick_trap, %function
__gnat_sys_tick_trap:
0:      b 0b
        .size __gnat_sys_tick_trap, . - __gnat_sys_tick_trap

        .thumb_func
fault:  b fault
