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

/* Below symbols are defined in Ada by the tasking runtime. They can be
 * overriden only in case the non-tasking runtime is used
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
        //  c0-c18            (19*16 = 304)
        //  c29-c30           (32   -> 336)
        //  spsr, celr, cpacr (32   -> 384)
        .set IFRAME_SIZE, 384

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


/* Default task abort handler. This can be overriden only when the non-tasking
   runtime is used */
        .weak   __gnat_get_task_abort_handler
__gnat_get_task_abort_handler:
        mov     x0, #0
        ret


        .text
__fault_restore:
        // restore
        ldp     c2, c3, [csp, #32]
        ldp     c4, c5, [csp, #64]
        ldp     c6, c7, [csp, #96]
        ldp     c8, c9, [csp, #128]
        ldp     c10, c11, [csp, #160]
        ldp     c12, c13, [csp, #192]
        ldp     c14, c15, [csp, #224]
        ldp     c16, c17, [csp, #256]
        ldp     c18, c19, [csp, #288]
        ldr     c30, [csp, #480]
        //  No need to restore callee saved registers
        //  Don't use the post-increment version of ldp as it doesn't preserve
        //  the capability metadata in the CSP register
        ldp     c0, c1, [csp]
        add     csp, csp, #512
        eret


__sync_interrupt_low:
__sync_interrupt:
        // Save all caller-saved registers (32 * 16 = 512)
        stp     c0, c1, [csp, #-512]!
        stp     c2, c3, [csp, #32]
        stp     c4, c5, [csp, #64]
        stp     c6, c7, [csp, #96]
        stp     c8, c9, [csp, #128]
        stp     c10, c11, [csp, #160]
        stp     c12, c13, [csp, #192]
        stp     c14, c15, [csp, #224]
        stp     c16, c17, [csp, #256]
        stp     c18, c19, [csp, #288]
        str     c30, [csp, #480]
        adr     c30, __fault_restore

        /* Check if the synchronous trap is due to floating point access */
        mrs     x0, esr_el1
        ubfx    x1, x0, #26, #6
        cmp     x1, #0x07
        bne     1f

        /* FPU access trap: save FPU context and enable FPU registers access */
        b       __gnat_fpen_trap

1:
        /* Handle other synchronous exceptions through the common Ada handler.
           This function will return the address of an additional handler to be
           executed in the interrupted task's context if required. */
        bl      __gnat_synchronous_exception_handler

        /* Skip if no handler was found */
        cmp     x0, #0
        beq     1f

        /* Return from the exception handler to the local exception handler
           that runs in the fault-causing task's context. */
        msr     celr_el1, c0
        b       __fault_restore

1:
        stp     c20, c21, [csp, #320]
        stp     c22, c23, [csp, #352]
        stp     c24, c25, [csp, #384]
        stp     c26, c27, [csp, #416]
        stp     c28, c29, [csp, #448]
        //  Call handler with context and number
        mov     c0, csp
        scbnds  c0, c0, #512
        mov     x1, #5
        b       __trap_dump

__irq_interrupt_low:
__fiq_interrupt_low:
__fiq_interrupt:
__irq_interrupt:
        //  1) save caller-saved regs
        sub     csp, csp, #IFRAME_SIZE
        stp     c0, c1, [csp]
        stp     c2, c3, [csp, #32]
        stp     c4, c5, [csp, #64]
        stp     c6, c7, [csp, #96]
        stp     c8, c9, [csp, #128]
        stp     c10, c11, [csp, #160]
        stp     c12, c13, [csp, #192]
        stp     c14, c15, [csp, #224]
        stp     c16, c17, [csp, #256]
        stp     c18, c29, [csp, #288]
        str     c30, [csp, #320]
        mrs     x4, spsr_el1
        mrs     c5, celr_el1
        stp     c4, c5, [csp, #336]

        // 2) load stack pointer if needed
        // if sp in __interrupt_stack_start .. __interrupt_stack_end, then we
        // are in the nested irq handling case
        adrp    c0,__interrupt_stack_start
        add     c0, c0, #:lo12:__interrupt_stack_start
        cmp     sp, x0
        ble     1f
        adrp    c0,__interrupt_stack_end
        add     c0, c0, #:lo12:__interrupt_stack_end
        cmp     sp, x0
        bge     1f

        //  nested IRQ case: we stay on the current stack
        mov     c2, csp
        b       2f

1:
        //  at this point, we know that SP is in a task stack
        //  let's move to the interrupt stack
        adrp    c0,interrupt_stack_table
        add     c0, c0, #:lo12:interrupt_stack_table
        mrs     x1, mpidr_el1
        and     x1, x1, #3

        ldr     c2, [c0, x1, lsl #4]  //  Load new stack pointer
2:

        // 3) Create a frame, switch to irq stack
        mov     c1, csp
        stp     c29, c30, [c2, #-64]!
        str     c1, [c2, #16]
        mov     csp, c2

        //  4) allocate the FPU context on the irq stack
        sub     csp, csp, #IFRAME_FPU_SIZE /* allocate the context */

        //  5) call handler
        //  pre-handler: x0: fpu context address
        mov     c0, csp
        bl      __gnat_irq_pre_handler
        str     c0, [csp, #-16]!          /* push the prev_ctxt */
        bl      __gnat_irq_handler       /* actually call the handler */
        ldr     c1, [csp], 16             /* pop the prev_ctxt */
        mov     c0, csp                   /* running irq fpu context */
        bl      __gnat_irq_post_handler
        add     csp, csp, #IFRAME_FPU_SIZE /* free the irq fpu context */

        //  6) Switch back to the previous stack
        add     c7, csp, #64             /* x7 <- before the frame created @3 */
        ldr     c1, [csp, #16]
        mov     csp, c1

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
        adrp    c0,interrupt_stack_table
        add     c0, c0, #:lo12:interrupt_stack_table
        mrs     x1, mpidr_el1
        and     x1, x1, #3
        str     c7, [c0, x1, lsl #4]  //  Save stack pointer

        //  7) switch context if needed
        bl      __gnat_context_switch_needed
        cbz     x0, 1f

        //  perform the context switch
        bl      __gnat_pre_context_switch
        bl      __gnat_context_switch

1:
        //  8) restore registers
        ldp     c4, c5, [csp, #336]
        msr     spsr_el1, x4
        msr     celr_el1, c5
        ldr     c30, [csp, #320]
        ldp     c18, c29, [csp, #288]
        ldp     c16, c17, [csp, #256]
        ldp     c14, c15, [csp, #224]
        ldp     c12, c13, [csp, #192]
        ldp     c10, c11, [csp, #160]
        ldp     c8, c9, [csp, #128]
        ldp     c6, c7, [csp, #96]
        ldp     c4, c5, [csp, #64]
        ldp     c2, c3, [csp, #32]
        ldp     c0, c1, [csp], #IFRAME_SIZE

        //  9) return
        eret

__serror_interrupt:
__serror_interrupt_low:
        // Save all registers (32 * 16 = 512)
        stp     c0, c1, [csp, #-512]!
        stp     c2, c3, [csp, #32]
        stp     c4, c5, [csp, #64]
        stp     c6, c7, [csp, #96]
        stp     c8, c9, [csp, #128]
        stp     c10, c11, [csp, #160]
        stp     c12, c13, [csp, #192]
        stp     c14, c15, [csp, #224]
        stp     c16, c17, [csp, #256]
        stp     c18, c19, [csp, #288]
        stp     c20, c21, [csp, #320]
        stp     c22, c23, [csp, #352]
        stp     c24, c25, [csp, #384]
        stp     c26, c27, [csp, #416]
        stp     c28, c29, [csp, #448]
        str     c30, [csp, #480]
        //  Call handler with context and number
        mov     c0, csp
        scbnds  c0, c0, #512
        mov     x1, 8 // Serror fault is num 8
        bl      __trap_dump
        //  Restore
        ldp     c2, c3, [csp, #32]
        ldp     c4, c5, [csp, #64]
        ldp     c6, c7, [csp, #96]
        ldp     c8, c9, [csp, #128]
        ldp     c10, c11, [csp, #160]
        ldp     c12, c13, [csp, #192]
        ldp     c14, c15, [csp, #224]
        ldp     c16, c17, [csp, #256]
        ldp     c18, c19, [csp, #288]
        ldr     c30, [csp, #480]
        //  No need to restore callee saved registers
        ldp     c0, c1, [csp], #512
        eret
