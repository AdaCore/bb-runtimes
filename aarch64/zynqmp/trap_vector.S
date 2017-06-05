/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                          Copyright (C) 2017 AdaCore                      *
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

.globl __start_ram
.globl __vectors

.globl __fiq_interrupt
.globl __irq_interrupt
.globl __serror_interrupt
.globl __sync_interrupt
.globl interrupt_stack_el1_base

.org 0
.section .vectors, "a"

//  Interrupt handle frame size
//  generic registers
//  x0-x18            (19*8 = 152)
//  x29-x30           (16   -> 168)
//  spsr, elr, cpacr  (24   -> 192)
.set IFRAME_SIZE, 192

//  fpu registers
//  offset                            0
//  vfp_init_flag     (1           -> 0  .. 3)
//  ctrl, status      (2*4         -> 4  .. 11)
//  Q0 - Q31          (32*16 = 512 -> 16 .. 527)
//  old_run           (8           -> 528 .. 535)
//  + padding to align on 16 bytes
.set IFRAME_FPU_SIZE, 544

__vectors:
.set vbar, __vectors
.org vbar

        b       __start_ram
.org (vbar + 0x200)
        b       __sync_interrupt
.org (vbar + 0x280)
        b       __irq_interrupt
.org (vbar + 0x300)
        b       __fiq_interrupt
.org (vbar + 0x380)
        b       __serror_interrupt


.data
interrupt_stack_el1_base:
.dword  __cpu0_interrupt_stack_end
.dword  __cpu1_interrupt_stack_end
.dword  __cpu2_interrupt_stack_end
.dword  __cpu3_interrupt_stack_end

.text
__fpu_fault_restore:
        ldp     x29, x30, [sp], #0x10
        ldp     x2, x3, [sp], #0x10
        ldp     x4, x5, [sp], #0x10
        ldp     x6, x7, [sp], #0x10
        ldp     x8, x9, [sp], #0x10
        ldp     x10, x11, [sp], #0x10
        ldp     x12, x13, [sp], #0x10
        ldp     x14, x15, [sp], #0x10
        ldp     x16, x17, [sp], #0x10
        ldp     x18, x19, [sp], #0x10
        //  No need to restore callee saved registers
        ldp     x0, x1, [sp], #0x10

__sync_interrupt:
        stp     x0, x1, [sp, #-0x10]!
        stp     x2, x3, [sp, #-0x10]!
        stp     x4, x5, [sp, #-0x10]!
        stp     x6, x7, [sp, #-0x10]!
        stp     x8, x9, [sp, #-0x10]!
        stp     x10, x11, [sp, #-0x10]!
        stp     x12, x13, [sp, #-0x10]!
        stp     x14, x15, [sp, #-0x10]!
        stp     x16, x17, [sp, #-0x10]!
        stp     x18, x19, [sp, #-0x10]!
        stp     x29, x30, [sp, #-0x10]!
        adr     x30,__fpu_fault_restore

        /* Check if the synchronous trap is due to floating point access */
        mrs     x0, esr_el1
        ubfx    x1, x0, #26, #6
        cmp     x1, #0x07
        bne     1f

        /* Reserve room for the FPU registers if needed */
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

__irq_interrupt:
__fiq_interrupt:
        //  1) save caller-saved regs
        sub     sp, sp, #(IFRAME_SIZE + IFRAME_FPU_SIZE)
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
        mrs     x6, cpacr_el1
        stp     x30, x4, [sp, #160]
        stp     x5, x6, [sp, #176]
        //  Set v_init flag to 0 (false)
        //  see s-bbcpsp.ads for the FPU context structure
        add     x7, sp, #IFRAME_SIZE     /* x7 <- fp context address */
        strb    wzr, [x7]                /* v_init <- 0 */

        // 2) load stack pointer if needed
        //  the interrupt stacks are above the regular stack: if the current
        //  stack pointer if greater than interrupt_stack_start, don't change
        //  it
        adrp    x0,__interrupt_stack_start
        add     x0, x0, #:lo12:__interrupt_stack_start
        cmp     sp, x0
        b.ge    1f
        //  at this point, we know that SP is in a task stack
        //  let's move to the interrupt stack
        adrp    x0,interrupt_stack_el1_base
        add     x0, x0, #:lo12:interrupt_stack_el1_base
        mrs     x1, mpidr_el1
        and     x1, x1, #3

        ldr     x2, [x0, x1, lsl #3]  //  Load new stack pointer

        // 3) Create a frame, switch to irq stack
        mov     x1, sp
        stp     x29, x30, [x2, #-32]!
        str     x1, [x2, #16]
        mov     sp, x2
1:

        //  4) disable fpu
        mrs     x4, cpacr_el1
        bic     x4, x4, #(1 << 20)
        msr     cpacr_el1, x4

        //  5) call handler
        //  pre-handler: x0: fpu context address
        mov     x0, x7
        bl      __gnat_irq_pre_handler
        bl      __gnat_irq_handler
        bl      __gnat_irq_post_handler

        //  6) Switch back to EL1 stack
        ldr     x1, [sp, #16]
        mov     sp, x1

        //  7) switch context if needed
        bl      __gnat_context_switch_needed
        cbz     x0, 1f

        bl      __gnat_pre_context_switch
        bl      __gnat_context_switch

        //  8) restore registers
1:
        ldp     x5, x6, [sp, #176]
        ldp     x30, x4, [sp, #160]
        msr     spsr_el1, x4
        msr     elr_el1, x5
        msr     cpacr_el1, x6
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
        add     sp, sp, #IFRAME_FPU_SIZE

        //  9) return
        eret

__serror_interrupt:
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

.end
