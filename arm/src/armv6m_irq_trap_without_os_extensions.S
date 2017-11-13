## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#     G N A T _ I R Q _ T R A P _ W I T H O U T _ O S _ E X T E N S I O N   #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                         Copyright (C) 2017 AdaCore                        #
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

        .thumb_func
        .type __gnat_irq_trap_without_os_extension, %function
	.globl __gnat_irq_trap_without_os_extension
__gnat_irq_trap_without_os_extension:

        /*
         * When armv6-m OS exceptions are not implemented, the PendSV trap
	 * usually used for context switching is not available. Instead, this
	 * handler does (or try to do) the context after each IRQ. It must be
	 * used instead of __gnat_irq_trap on MCUs that do not implement
	 * the OS exceptions.
         */

        mov r3, sp

        /* Switch to the interrupt stack */
        ldr r2, =interrupt_stack_table
        ldr r2, [r2] // load interrupt stack address
        mov sp, r2

        push {r3} /* Save thread SP on the interrupt stack */
        push {lr} /* Save LR on the interrupt stack */

        /* Call the real trap handler */
        bl __gnat_irq_trap

        bl __gnat_context_switch_needed

        pop {r1} /* Load LR from the interrupt stack */
        mov lr, r1

        pop {r1}   /* Load thread SP from the interrupt stack */
        mov sp, r1 /* Go back to the thread stack */

        cmp r0, #0 // Check Context_Switch_Needed() return value
        beq skip_context_switch

        /* Context switch */

        /* Store Context buffer */

        /* The registers R4-R11 and PSP are stored in the "context buffer". The
	 * context buffer is not on the stack.
         */

        ldr  r2,=__gnat_running_thread_table
        ldr  r3, [r2]     // Load address of running context buffer

	stm  r3!, {r4-r7} // Store R4-R7 in context buffer
	mov  r4, r8
	mov  r5, r9
	mov  r6, r10
	mov  r7, r11
	stm  r3!, {r4-r7} // Store R8-R11 in context buffer
	mov  r4, sp
	stm  r3!, {r4}    // Store PSP in context buffer

        /* Do context switch */
        ldr  r3,=first_thread_table
	ldr  r3, [r3]     // Load address of new context buffer
        ldr  r2,=__gnat_running_thread_table
	str  r3, [r2]     // Update value of __gnat_running_thread_table

        /* Load context buffer */
        movs r2, 0x20
        add  r2, r3, r2   // Move R2 where PSP is stored in the context buffer
	ldr  r4, [r2]     // Load PSP from context buffer
	mov  sp, r4       // Set new stack

        movs r2, 0x10
        add  r2, r3, r2   // Move R2 where R8 is stored
        ldm  r2!, {r4-r7} // Load R8-R11 from context buffer
	mov  r8,  r4
	mov  r9,  r5
	mov  r10, r6
	mov  r11, r7

        mov  r2, r3       // Move R2 where R4 is stored in the context buffer
	ldm  r2!, {r4-r7} // Load R4-R7 from context buffer

        ldr r2,=0xA300001C
        ldr r3,=0x80
        str r3, [r2]      // Clear the context switch IRQ

skip_context_switch:

        /* Return from trap */
        ldr r0,=0xFFFFFFFD
        bx  r0
        .size __gnat_irq_trap_without_os_extension, . - __gnat_irq_trap_without_os_extension


        /*********************/
        /* Weak declarations */
        /*********************/

        /* Some of the symbols used in __gnat_irq_trap_without_os_extension
         * may not be defined when the user program does not use Ravenscar
         * tasking. The missing symbols are declared below as weak, so they
         * will be used when the real symbols are not declared by the run-time.
         */

        .text
        .thumb_func
        .weak __gnat_context_switch_needed
        .type __gnat_context_switch_needed, %function
__gnat_context_switch_needed:
        movs r0, #0 // return false
        bx lr
        .size __gnat_context_switch_needed, . - __gnat_context_switch_needed

        .text
        .thumb_func
        .weak __gnat_irq_trap
        .type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

        .data
        .weak interrupt_stack_table
interrupt_stack_table:
        .word 0

        .data
        .weak __gnat_running_thread_table
__gnat_running_thread_table:
        .word 0

        .data
        .weak first_thread_table
first_thread_table:
        .word 0

