/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                           T R A P _ V E C T O R S                        *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                     Copyright (C) 2017-2019 AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 ****************************************************************************/

.org 0
.text

.globl __start
.globl __vectors

.globl __serror_interrupt
.globl __sync_interrupt
.globl __irq_interrupt
.globl __fiq_interrupt
.globl interrupt_stack_table
.globl __serror_interrupt_low
.globl __sync_interrupt_low
.globl __irq_interrupt_low
.globl __fiq_interrupt_low

.org 0
.section .vectors, "a"

/* Below symbols are defined in Ada by the ravenscar runtime. They can be
 * overriden only in case the ZFP runtime is used
 */
.weak   interrupt_stack_table
.weak   __gnat_context_switch_needed
.weak   __gnat_pre_context_switch
.weak   __gnat_context_switch
.weak   __gnat_irq_handler
.weak   __gnat_irq_pre_handler
.weak   __gnat_irq_post_handler
.weak   __gnat_fpen_trap

/* Below symbols are default implementations that can be overriden by users
 * in case interrupts from lower EL level is to be supported.
 */
.weak __sync_interrupt_low
.weak __irq_interrupt_low
.weak __fiq_interrupt_low
.weak __serror_interrupt_low

//  Interrupt handle frame size
//  generic registers
//  x0-x18            (19*8 = 152)
//  x29-x30           (16   -> 168)
//  spsr, elr, cpacr  (24   -> 192)
.set IFRAME_SIZE, 192

//  fpu registers context size:
//  vfp_init_flag     (1           -> 0  .. 3)
//  ctrl, status      (2*4         -> 4  .. 11)
//  Q0 - Q31          (32*16 = 512 -> 16 .. 527)
.set IFRAME_FPU_SIZE, 528

__vectors:
.set vbar, __vectors
.org vbar
        b       __start
.org (vbar + 0x200)
        b       __sync_interrupt
.org (vbar + 0x280)
        b       __irq_interrupt
.org (vbar + 0x300)
        b       __fiq_interrupt
.org (vbar + 0x380)
        b       __serror_interrupt
.org (vbar + 0x400)
        b       __sync_interrupt_low
.org (vbar + 0x480)
        b       __irq_interrupt_low
.org (vbar + 0x500)
        b       __fiq_interrupt_low
.org (vbar + 0x580)
        b       __serror_interrupt_low


.text
__fpu_fault_restore:
        // restore
        ldp     x2, x3, [sp, #16]
        ldp     x4, x5, [sp, #32]
        ldp     x6, x7, [sp, #48]
        ldp     x8, x9, [sp, #64]
        ldp     x10, x11, [sp, #80]
        ldp     x12, x13, [sp, #96]
        ldp     x14, x15, [sp, #112]
        ldp     x16, x17, [sp, #128]
        ldp     x18, x19, [sp, #144]
        ldr     x30, [sp, #240]
        //  No need to restore callee saved registers
        ldp     x0, x1, [sp], #256
        eret

__sync_interrupt_low:
__sync_interrupt:
        // Save all caller-saved registers (32 * 8 = 256)
        stp     x0, x1, [sp, #-256]!
        stp     x2, x3, [sp, #16]
        stp     x4, x5, [sp, #32]
        stp     x6, x7, [sp, #48]
        stp     x8, x9, [sp, #64]
        stp     x10, x11, [sp, #80]
        stp     x12, x13, [sp, #96]
        stp     x14, x15, [sp, #112]
        stp     x16, x17, [sp, #128]
        stp     x18, x19, [sp, #144]
        str     x30, [sp, #240]
        adr     x30,__fpu_fault_restore

        /* Check if the synchronous trap is due to floating point access */
        mrs     x0, esr_el1
        ubfx    x1, x0, #26, #6
        cmp     x1, #0x07
        bne     1f

        /* FPU access trap: save FPU context and enable FPU registers access */
        b       __gnat_fpen_trap
1:
        stp     x20, x21, [sp, #-0x10]!
        stp     x22, x23, [sp, #-0x10]!
        stp     x24, x25, [sp, #-0x10]!
        stp     x26, x27, [sp, #-0x10]!
        stp     x28, x29, [sp, #-0x10]!
        //  Call handler with context and number
        mov     x0, sp
        mov     x1, #5
        b       __trap_dump

__irq_interrupt_low:
__fiq_interrupt_low:
__fiq_interrupt:
__irq_interrupt:
        //  1) save caller-saved regs
        sub     sp, sp, #IFRAME_SIZE
        stp     x0, x1, [sp]
        stp     x2, x3, [sp, #16]
        stp     x4, x5, [sp, #32]
        stp     x6, x7, [sp, #48]
        stp     x8, x9, [sp, #64]
        stp     x10, x11, [sp, #80]
        stp     x12, x13, [sp, #96]
        stp     x14, x15, [sp, #112]
        stp     x16, x17, [sp, #128]
        stp     x18, x29, [sp, #144]
        mrs     x4, spsr_el1
        mrs     x5, elr_el1
        stp     x30, x4, [sp, #160]
        str     x5, [sp, #176]

        // 2) load stack pointer if needed
        // if sp in __interrupt_stack_start .. __interrupt_stack_end, then we
        // are in the nested irq handling case
        adrp    x0,__interrupt_stack_start
        add     x0, x0, #:lo12:__interrupt_stack_start
        cmp     sp, x0
        ble     1f
        adrp    x0,__interrupt_stack_end
        add     x0, x0, #:lo12:__interrupt_stack_end
        cmp     sp, x0
        bge     1f

        //  nested IRQ case: we stay on the current stack
        mov     x2, sp
        b       2f

1:
        //  at this point, we know that SP is in a task stack
        //  let's move to the interrupt stack
        adrp    x0,interrupt_stack_table
        add     x0, x0, #:lo12:interrupt_stack_table
        mrs     x1, mpidr_el1
        and     x1, x1, #3

        ldr     x2, [x0, x1, lsl #3]  //  Load new stack pointer
2:

        // 3) Create a frame, switch to irq stack
        mov     x1, sp
        stp     x29, x30, [x2, #-32]!
        str     x1, [x2, #16]
        mov     sp, x2

        //  4) allocate the FPU context on the irq stack
        sub     sp, sp, #IFRAME_FPU_SIZE /* allocate the context */

        //  6) call handler
        //  pre-handler: x0: fpu context address
        mov     x0, sp
        bl      __gnat_irq_pre_handler
        str     x0, [sp, #-16]!          /* push the prev_ctxt */
        bl      __gnat_irq_handler       /* actually call the handler */
        ldr     x1, [sp], 16             /* pop the prev_ctxt */
        mov     x0, sp                   /* running irq fpu context */
        bl      __gnat_irq_post_handler
        add     sp, sp, #IFRAME_FPU_SIZE /* free the irq fpu context */

        //  7) Switch back to the previous stack
        add     x7, sp, #32              /* x7 <- before the frame created @3 */
        ldr     x1, [sp, #16]
        mov     sp, x1

        /* Save the irq stack pointer:
         *
         * We accomodate here the following scenario:
         * an irq is interrupted by a higher prio. irq, so the handler of the
         * new irq uses the current sp value to execute. When returning from
         * this higher prio irq, we may context switch.
         * In this case, the new running task can be also interrupted at any
         * moment: we need to ensure that the interrupt stack table is updated
         * so that any interruption running there starts from the sp value
         * found initially (so as to not temper the stack of the original irq.
         * Once we resume the original irq handler, we also go through this
         * save phase, so properly restore the stack table to its original base
         * value.
         */
        adrp    x0,interrupt_stack_table
        add     x0, x0, #:lo12:interrupt_stack_table
        mrs     x1, mpidr_el1
        and     x1, x1, #3
        str     x7, [x0, x1, lsl #3]  //  Save stack pointer

        //  8) switch context if needed
        bl      __gnat_context_switch_needed
        cbz     x0, 1f

        //  perform the context switch
        bl      __gnat_pre_context_switch
        bl      __gnat_context_switch

1:
        //  10) restore registers
        ldr     x5, [sp, #176]
        ldp     x30, x4, [sp, #160]
        msr     spsr_el1, x4
        msr     elr_el1, x5
        ldp     x18, x29, [sp, #144]
        ldp     x16, x17, [sp, #128]
        ldp     x14, x15, [sp, #112]
        ldp     x12, x13, [sp, #96]
        ldp     x10, x11, [sp, #80]
        ldp     x8, x9, [sp, #64]
        ldp     x6, x7, [sp, #48]
        ldp     x4, x5, [sp, #32]
        ldp     x2, x3, [sp, #16]
        ldp     x0, x1, [sp], #IFRAME_SIZE

        //  12) return
        eret

__serror_interrupt:
__serror_interrupt_low:
        // Save all registers (32 * 8 = 256)
        stp     x0, x1, [sp, #-256]!
        stp     x2, x3, [sp, #16]
        stp     x4, x5, [sp, #32]
        stp     x6, x7, [sp, #48]
        stp     x8, x9, [sp, #64]
        stp     x10, x11, [sp, #80]
        stp     x12, x13, [sp, #96]
        stp     x14, x15, [sp, #112]
        stp     x16, x17, [sp, #128]
        stp     x18, x19, [sp, #144]
        stp     x20, x21, [sp, #160]
        stp     x22, x23, [sp, #176]
        stp     x24, x25, [sp, #192]
        stp     x26, x27, [sp, #208]
        stp     x28, x29, [sp, #224]
        str     x30, [sp, #240]
        //  Call handler with context and number
        mov     x0, sp
        mov     x1, 8 // Serror fault is num 8
        bl      __trap_dump
        //  Restore
        ldp     x2, x3, [sp, #16]
        ldp     x4, x5, [sp, #32]
        ldp     x6, x7, [sp, #48]
        ldp     x8, x9, [sp, #64]
        ldp     x10, x11, [sp, #80]
        ldp     x12, x13, [sp, #96]
        ldp     x14, x15, [sp, #112]
        ldp     x16, x17, [sp, #128]
        ldp     x18, x19, [sp, #144]
        ldr     x30, [sp, #240]
        //  No need to restore callee saved registers
        ldp     x0, x1, [sp], #256
        eret
