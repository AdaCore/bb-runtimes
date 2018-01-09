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

/* This file is specific to the Cortex-A53 for UltraZed board.  */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
.global __start_ram
.global __start_slave_cpu

.set    vector_base,    __vectors
.set    RVBAR_BASE,     0xFD5C0040      /* Reset vector */
.set    GICD_IGROUPR,   0xF9010080      /* Irq controller: groups */
.set    GICC_PMR,       0xF9020004      /* Irq controller: priority mask */
.set    SCNTRS_EN,      0xFF260000      /* Global counter */
.set    SCNTRS_FREQ,    0xFF260020

#include "start-config.inc"
#include "memmap.S"

.section .boot,"ax"

.weak   __gnat_initialize_cpu_devices
.weak   __gnat_initialize_slave

__start_ram:
        mov     x0, #0
        mov     x1, #0
        mov     x2, #0
        mov     x3, #0
        mov     x4, #0
        mov     x5, #0
        mov     x6, #0
        mov     x7, #0
        mov     x8, #0
        mov     x9, #0
        mov     x10, #0
        mov     x11, #0
        mov     x12, #0
        mov     x13, #0
        mov     x14, #0
        mov     x15, #0
        mov     x16, #0
        mov     x17, #0
        mov     x18, #0
        mov     x19, #0
        mov     x20, #0
        mov     x21, #0
        mov     x22, #0
        mov     x23, #0
        mov     x24, #0
        mov     x25, #0
        mov     x26, #0
        mov     x27, #0
        mov     x28, #0
        mov     x29, #0
        mov     x30, #0
        b        __start_ram1

        .text

        .type   __start_ram1, %function
__start_ram1:
        # Read processor number
        mrs     x7, mpidr_el1
        and     x7, x7, #3

        # Boot conditions : check the current EL
        mrs     x1, currentel
        cmp     x1, #(3<<2)
        beq     __start_el3

        cmp     x1, #(2<<2)
        beq     __start_el2

        cmp     x1, #(1<<2)
        beq     __start_el1

        # EL0 startup not supported
        b       __error

__error:
        b       __error

__start_el3:
        # Configure EL3

        # Load vector table address
        ldr     x0, =vector_base
        msr     vbar_el3, x0

        # Set the reset vector address
        ldr     w1, =RVBAR_BASE
        mov     w3, #0x8
        mul     w3, w3, w7  /* Offset from rvbar: CPU_num * 8 */
        add     w1, w1, w3
        str     x0, [x1]    /* Store vector_base */

        # Set the timer frequency (indicator for software)
        ldr     x0, =99999000
        msr     cntfrq_el0, x0

        /* SCR_EL3:
         * NS  = 1  EL0 and EL1 are in Non-secure state
         * IRQ = 0  do not take exceptions from EL0-2 at EL3
         * FIQ = 0  same for FIQ
         * EA  = 0  same for external abort and serror
         * SMD = 0  SMC instruction enabled at EL1-3
         * HCE = 1  HVC instruction is enabled at EL1-3
         * SIF = 0  Secure state instruction from non-secure mem are permitted
         * RW  = 1  Next lower level is aarch64
         * ST  = 0  CNTPS_*_EL1 registers accessible only in EL3
         * TWI = 0  WFI instructions are not trapped
         * TWE = 0  WFE instructions are not trapped.
         */
        mov     x0,#0x501
        msr     scr_el3, x0

        /* SCTLR_EL3:
         * M   = 0  Disables EL3 MMU
         * A   = 1  Enables alignment fault checking
         * C   = 1  Enables data and unified caches
         * SA  = 0  Disables stack alignment check.
         * I   = 1  Instruction caches are enabled
         * WXN = 0  Regions with write permissions are not forced XN.
         * EE  = 0  Exception endianness: little endian
         */
        mov     x0,#0x1006
        msr     sctlr_el3, x0

        /* MDCR_EL3: all 0: do not trap debug accesses */
        msr     mdcr_el3, xzr

        /* ACTLR_EL3: enable control at EL2 */
        mov     x0,#0x73
        msr     actlr_el3, x0

        /* Enable FPU (don't trap accesses) */
        msr     cptr_el3, xzr

        /* Running in non-secure mode: all interrupts are group 1 */
        ldr     w2, =GICD_IGROUPR
        mov     w1, #0xFFFFFFFF
        mov     x0, #6
__int_grp1_loop:
        cmp     w0, #0
        beq     1f
        str     w1, [x2]
        add     x2, x2, #4
        sub     x0, x0, #1
        b       __int_grp1_loop
1:
        # Make sure the interrupt priority mask allows non-secure priorities
        ldr     w2, =GICC_PMR
        mov     w1, #0x80
        str     w1, [x2]

        # Switch to el2h, set AIF
        mov     x0, #(0x1c0 + 9)
        msr     spsr_el3, x0
        adr     x0, __start_el2
        msr     elr_el3, x0
        mrs     x0, vbar_el3
        msr     vbar_el2, x0

        isb
        eret

__start_el2:
        # Enable FPU (don't trap accesses)
        msr     cptr_el2, xzr

        # Enable SMPEN
        mrs     x0, S3_1_C15_C2_1
        orr     x0, x0, #0x40
        msr     S3_1_C15_C2_1, x0

        # Make sure the generic timers are initialized
        mrs     x0, cnthctl_el2
        orr     x0, x0, #0x3    /* Enable EL1 access to timers */
        msr     cnthctl_el2, x0
        msr     cntvoff_el2, xzr

        # set vpidr and vmpidr
        mrs     x0, midr_el1
        mrs     x1, mpidr_el1
        msr     vpidr_el2, x0
        msr     vmpidr_el2, x1

.if (RUNTIME_EL == 1)
        # HCR_EL2: RW(1)
        mov     x0,#(1<<31)
        orr     x0, x0, #(1<<29) /* Disable HVC */
        msr     hcr_el2,x0

        # Switch to el1, set AIF
        mov     x0, #(0x1c0 + 5)
        msr     spsr_el2, x0
        adr     x0, __start_el1
        msr     elr_el2, x0

        isb
        eret
.else
        # HCR_EL2: redirect IRQ/FIQ/ABT to EL2.
        ldr     x0,=((1<<31) | (1<<12) | (1<<5) | (1<<4) | (1<<3))
        msr     hcr_el2,x0

        # Load stack pointer (on 32bit)
        adrp    x2, system_stack_base
        add     x2, x2, #:lo12:system_stack_base
        ldr     w0, [x2, x7, lsl #2]
        mov     sp, x0

        # Enable MMU and cache.
        tlbi    alle2
        ldr     x0, =__mmu_l1_000000000
        msr     ttbr0_el2, x0
        // TCR_EL2: TB0=0, PS=000 (4GB),
        //          TG0=00, SH0=11, ORGN0=01, IRGN1=01, T0SZ=32
        ldr     x0, =((32 << 0) | (1 << 8) | (1 << 10) | (3 << 12) \
                | (0 << 14))
        msr     tcr_el2, x0
        # MAIR_EL2:
        # 0: b11101110 Outer write-back cacheable, Inner write-back cacheable
        # others: not used
        movz    x0,#0x00ee
        msr     mair_el2,x0
        isb

        # SCTLR_EL2:
        # I-Cache enabled
        # D-Cache enabled
        # MMU enabled
        mov     x1, xzr
        orr     x1, x1, #(1 << 12)      // Set I bit
        orr     x1, x1, #(1 << 2)       // Set C bit
        orr     x1, x1, #(1 << 0)       // Set M bit
        msr     sctlr_el2, x1
        isb

        b       __start_common
.endif

__start_el1:
        # Enable FPU
        mov     x0, #(1 << 20)
        msr     cpacr_el1, x0

        # Load stack pointer (on 32bit)
        adrp    x2, system_stack_base
        add     x2, x2, #:lo12:system_stack_base
        ldr     w0, [x2, x7, lsl #2]
        mov     sp, x0

        ldr     x0, =vector_base
        msr     vbar_el1, x0

        # Enable MMU and cache.
        tlbi    vmalle1
        ic      iallu   // I-cache invalidate
        mov     x0, #1
        bl      __dcaches_all   // D-caches invalidate
        dsb     sy
        isb

        # TTBR0_EL1:
        ldr     x0, =__mmu_l1_000000000
        msr     ttbr0_el1, x0

        // TCR_EL1: TBI0=0, AS=0, IPS=000 (4GB),
        //          TG1=10, SH1=10, ORGN1=11, IRGN1=11, EPD1=1, A1=0, T1SZ=32
        //          TG0=00, SH0=10, ORGN0=11, IRGN0=11, EPD0=0,       T0SZ=32
        movz    x0, #((32 << 0) | (1 << 7) | (3 << 8) | (3 << 10) | (2 << 12) \
                      | (2 << 14)), LSL #16
	movk    x0, #((32 << 0) | (0 << 7) | (3 << 8) | (3 << 10) | (2 << 12) \
                      | (0 << 14))
	msr     tcr_el1, x0

        # MAIR_EL1:
        # 0: b11101110 Outer write-back cacheable, Inner write-back cacheable
        # others: not used
        movz    x0,#0x00ee
        msr     mair_el1,x0
        isb

        # SCTLR_EL1:
        # WFE: non trapping
        # WFI: non trapping
        # CP15 barrier enabled
        # I-Cache enabled
        # D-Cache enabled
        # MMU enabled
        mov     x1, xzr
        orr     x1, x1, #(1 << 18)      // Set WFE non trapping
        orr     x1, x1, #(1 << 17)      // Set WFI non trapping
        orr     x1, x1, #(1 << 5)       // Set CP15 barrier enabled
        orr     x1, x1, #(1 << 12)      // Set I bit
        orr     x1, x1, #(1 << 2)       // Set C bit
        orr     x1, x1, #(1 << 0)       // Set M bit
        msr     sctlr_el1, x1
        isb

__start_common:
        # CPU 0 starts master, the other are slave cpus
        mrs     x7, mpidr_el1
        and      x7, x7, #3
	cbz	x7, __start_master
        b       __start_slave_cpu

/* __start_master
 */
__start_master:
        /* CPU0: Clear .bss, assuming in the first 4GB */
        ldr      w0,bss_segment + 0
        ldr     w1,bss_segment + 4
0:      cbz     x1,1f
        /* FIXME: write multiple words?  */
        str     xzr,[x0],#8
        sub     x1,x1,#1
        cbnz    x1,0b
1:

        /* Call the main routine */
        bl  main
        bl  _exit
0:      b  0b
        .size  __start_ram1, . - __start_ram1

/* __start_slave_cpu
 */

        .type __start_slave_cpu, %function
__start_slave_cpu:
        mov     x29, xzr
        mov     x19, x7
        // Initialize devices
        bl      __gnat_initialize_cpu_devices

        /* Call the main routine (with CPU_Id as argument).  */
        add     x0,x19,#1
        bl      __gnat_initialize_slave
1:      b       1b
        .size __start_slave_cpu, . - __start_slave_cpu


        .p2align  3

/* __dcaches_all
 *
 * x0: 0 clean & invalidate, 1 invalidate only
 *
 * flush or invalidate all data cache by set/way
 */
__dcaches_all:
        mov     x1, x0
        dsb     sy
        mrs     x10, clidr_el1
        lsr     x11, x10, #24
        and     x11, x11, #0x7  //  clidr_el1.LoC
        cbz     x11, __dcaches_finished   //  LoC == 0 => finish
        mov     x15, x30    // save the link register
        mov     x0, #0          // start flush at cache level 0
        /* x0:  cache level
         * x1:  parameter
         * x10: clidr_el1
         * x11: LoC
         * x15: return address
         */
__dcaches_loop_level:
        lsl     x12, x0, #1
        add     x12, x12, x0    // x12 <- 3 x cache level
        lsr     x12, x10, x12
        and     x12, x12, #7    // x12 <- cache type
        cmp     x12, #2
        b.lt    __dcaches_skip  // skip if no cache or icache
        bl      __dcache_level  // x1 given as parameter
__dcaches_skip:
        add     x0, x0, #1      // Cache Level += 1
        cmp     x11, x0         // Cache_Level <> LoC
        b.gt    __dcaches_loop_level

        mov     x0, #0
        msr     csselr_el1, x0  // restore csselr_el1
        dsb     sy
        isb
        mov  x30, x15
__dcaches_finished:
        ret

/* __dcache_level
 * x0 : cache level
 * x1 : 0 clean & invalidate, 1 invalidate only
 * x2 - x9: clobbered
 */
__dcache_level:
        lsl     x12, x0, #1
        msr     csselr_el1, x12 // select cache level
        isb
        mrs     x6, ccsidr_el1  // read the new cssidr_el1
        and     x2, x6, #7      // x2 <- log2(cache line size) - 4
        add     x2, x2, #4      // x2 <- log2(cache line size)
        mov     x3, #0x3ff
        and     x2, x3, x6, lsr #3      // x3 <- max number of ways
        clz     w5, w3          // bit position of ways
        mov     x4, #0x7fff
        and     x4, x4, x6, lsr #13     // x4 <- max number of sets
__dcache_loop_set:
        mov     x6, x3          // x6 <- copy of ways
__dcache_loop_way:
        lsl     x7, x6, x5
        orr     x9, x12, x7     // map way and level to cisw value
        lsl     x7, x4, x2
        orr     x9, x9, x7      // map set number to cisw value
        tbz     w1, #0, 1f      // invalidate
        dc      isw, x9
        b       2f
1:      dc      cisw, x9        // clean & invalidate
2:      subs    x6, x6, #1      // ways := ways - 1
        b.ge    __dcache_loop_way
        subs    x4, x4, #1      // set := set - 1
        b.ge    __dcache_loop_set
        ret


  # BSS base and size
bss_segment:
        .word   __bss_start
        .word   __bss_dwords

system_stack_base:
        .word   __cpu0_stack_end
        .word   __cpu1_stack_end
        .word   __cpu2_stack_end
        .word   __cpu3_stack_end
