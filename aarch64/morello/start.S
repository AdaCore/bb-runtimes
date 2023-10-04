/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                 S T A R T                                *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                     Copyright (C) 2017-2023 AdaCore                      *
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

/* This file is specific to the Arm Morello SoC.  */

/* This file contains the start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */
        .global __start
        .global __vectors

#define NUM_PE_PER_CPU      1
#define NUM_CPU_PER_CLUSTER 2
#define NUM_PE_PER_CLUSTER  (NUM_PE_PER_CPU * NUM_CPU_PER_CLUSTER)

#define GICD_CTLR    0x30000000
#define GICD_IGROUPR 0x30000080

#define ICC_SRE_EL3 s3_6_c12_c12_5
#define ICC_SRE_EL2 s3_4_c12_c9_5
#define ICC_PMR     s3_0_c4_c6_0

/* Set the type of the heap symbols (defined in common.ld) to allow the linker
 * to derive appropriate capability permissions (read/write) for the symbols.
 * This avoids the linker needing to guess the permissions, which triggers
 * a warning.
 */
        .global __heap_start
        .global __heap_end
        .type __heap_start,%object
        .type __heap_end,%object

#include "memmap.S"

        .section .boot,"ax"
        .arch armv8-a+a64c

        .weak   __gnat_initialize_cpu_devices
        .weak   __gnat_initialize_slave

/* Entry point (starts in A64 mode) */
__start:
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

        /* Read processor number into x7, converting the affinity fields
         * into a number in the range 0 .. 3 */
        mrs     x3, mpidr_el1
        ubfx	x7, x3, #0, #8  /* Aff0 (PE number) */
        ubfx	x0, x3, #8, #8  /* Aff1 (core number) */
        ubfx	x1, x3, #16, #8 /* Aff2 (cluster number) */
        mov     x2, #NUM_PE_PER_CPU
        mov     x3, #NUM_PE_PER_CLUSTER
        madd    x7, x0, x2, x7
        madd    x7, x1, x3, x7

        /* Boot conditions : check the current EL */
        mrs     x1, currentel
        cmp     x1, #(3 << 2)
        beq     __start_el3

        cmp     x1, #(2 << 2)
        beq     __start_el2

        cmp     x1, #(1 << 2)
        beq     __start_el1

        /* EL0 startup not supported */
        b       __error

__error:
        b       __error

/* Entry point for EL3 (starts in A64 mode) */
__start_el3:

        /* Enable Morello architecture features */

        /* EC  = 1  Enable Morello features (don't trap accesses)
         * TFP = 0  Enable SVE, SIMD, and FPU (don't trap accesses)
         */
        mrs     x0, cptr_el3
        orr     x0, x0, #(1 << 9)  /* Set EC */
        bic     x0, x0, #(1 << 10) /* Clear TFP */
        msr     cptr_el3, x0
        isb

        /* SCTLR_EL3:
         * M   = 0  Disables EL3 MMU
         * A   = 1  Enables alignment fault checking
         * C   = 1  Enables data and unified caches
         * SA  = 0  Disables stack alignment check.
         * I   = 1  Instruction caches are enabled
         * WXN = 0  Regions with write permissions are not forced XN.
         * EE  = 0  Exception endianness: little endian
         */
        mov     x0, #0x1006
        msr     sctlr_el3, x0
        isb

        /* SBL     = 1  BL instructions don't seal caps c30
         * PERMVCT = 0  Access to CNTVCT_EL0 at EL3 needs PCC system perms
         * C64E    = 1  On exception entry PSTATE.C64 is set to 1
         * ADRDPB  = 1  ADRDP uses C28 as a base register
         * PCCB0   = 0  Accesses do not add PCC base to the address written
         *              to PC, and do not subtract PCC base from the address
         *              read from PCC.
         * DDCB0   = 0  Accesses do not add or subtract DDC base from the
         *              accessed address.
         * TGEN0   = 0  Generates a fault when loading a valid capability
         *              from memory where the Block and Page descriptor
         *              LC field is 0b11.
         */
        mov     x0, #0xB0
        msr     cctlr_el3, x0
        isb

        /* Load vector table address */
        ldr     x0, =__vectors
        mrs     c1, ddc
        scvalue c0, c1, x0
        msr     cvbar_el3, c0

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
        isb

        /* Disable security to allow non-secure access to some GIC registers */
        mov     x0, #GICD_CTLR
        mov     x1, #(1 << 6) /* set DS=1 */
        str     x1, [x0]

        /* Enable lower ELs to access to the GIC CPU interface registers. */
        mov     x0, #(1 << 3)
        msr     ICC_SRE_EL3, x0

        /* Running in non-secure mode: all interrupts are group 1 */
        ldr     w2, =GICD_IGROUPR
        mov     w1, #0xFFFFFFFF
        mov     x0, #32
__int_grp1_loop:
        cmp     w0, #0
        beq     1f
        str     w1, [x2]
        add     x2, x2, #4
        sub     x0, x0, #1
        b       __int_grp1_loop
1:

        /* Make sure the interrupt priority mask allows non-secure priorities */
        mov     x1, #0x80
        msr     ICC_PMR, x1

        /* MDCR_EL3: all 0: do not trap debug accesses */
        msr     mdcr_el3, xzr

        /* ACTLR_EL3: enable control at EL2 */
        mov     x0,#0x73
        msr     actlr_el3, x0

        /* Switch to EL2, set AIF */
        mov     x0, #(0x1c0 + 9)
        msr     spsr_el3, x0
        ldr     x0, =__start_el2
        mrs     c1, ddc
        scvalue c0, c1, x0
        msr     celr_el3, c0

        isb
        eret
        .size __start_el3, . - __start_el3

/* Entry point for EL2 (starts in A64 mode) */
__start_el2:

        /* TFP = 0  Enable FPU (don't trap accesses)
         * TC  = 0  Enable Morello features (don't trap accesses)
         * bits 0..7 | 12..13 = RES1
         */
        mov     x0, #0x30FF
        msr     cptr_el2, x0
        isb

        /* SBL     = 1  BL instructions don't seal caps c30
         * PERMVCT = 0  Access to CNTVCT_EL0 at EL3 needs PCC system perms
         * C64E    = 1  On exception entry PSTATE.C64 is set to 1
         * ADRDPB  = 1  ADRDP uses C28 as a base register
         * PCCB0   = 0  Accesses do not add PCC base to the address written
         *              to PC, and do not subtract PCC base from the address
         *              read from PCC.
         * DDCB0   = 0  Accesses do not add or subtract DDC base from the
         *              accessed address.
         * TGEN0   = 0  Generates a fault when loading a valid capability
         *              from memory where the Block and Page descriptor
         *              LC field is 0b11.
         */
        mov     x0, #0xB0
        msr     cctlr_el2, x0
        isb

        mrs     c0, ddc

        /* set CVBAR_EL2 */
        ldr     x0, =__vectors
        mrs     c1, ddc
        scvalue c0, c1, x0
        msr     cvbar_el2, c0

        /* Make sure the generic timers are initialized */
        mrs     x0, cnthctl_el2
        orr     x0, x0, #0x3    /* Enable EL1 access to timers */
        msr     cnthctl_el2, x0
        msr     cntvoff_el2, xzr

        /* set vpidr and vmpidr */
        mrs     x0, midr_el1
        mrs     x1, mpidr_el1
        msr     vpidr_el2, x0
        msr     vmpidr_el2, x1

        /* HCR_EL2: RW(1) */
        mov     x0,#(1 << 31)
        orr     x0, x0, #(1 << 29) /* Disable HVC */
        msr     hcr_el2,x0

        /* Enable lower ELs to access to the GIC CPU interface registers. */
        mov     x0, #(1 << 3)
        msr     ICC_SRE_EL2, x0

        /* Switch to C64 mode */
        bx      #4
        .arch armv8-a+c64

        /* Switch to EL1, set AIF */
        mov     x0, #(0x1c0 + 5)
        msr     spsr_el2, x0
        ldr     x0, =__start_el1
        mrs     c1, ddc
        scvalue c0, c1, x0
        msr     celr_el2, c0

        isb
        eret
        .size __start_el2, . - __start_el2

/* Entry point for EL1 (starts in A64 mode) */
        .arch armv8-a+a64c
__start_el1:
        /* FPEN = 0b11  Enable FPU (don't trap accesses)
         * CEN  = 0b11  Enable Morello features (don't trap accesses)
         */
        mov     x0, #0x3C0000
        msr     cpacr_el1, x0

        /* SBL     = 1  BL instructions don't seal caps c30
         * PERMVCT = 0  Access to CNTVCT_EL0 at EL3 needs PCC system perms
         * C64E    = 1  On exception entry PSTATE.C64 is set to 1
         * ADRDPB  = 1  ADRDP uses C28 as a base register
         * PCCB0   = 0  Accesses do not add PCC base to the address written
         *              to PC, and do not subtract PCC base from the address
         *              read from PCC.
         * DDCB0   = 0  Accesses do not add or subtract DDC base from the
         *              accessed address.
         * TGEN0   = 0  Generates a fault when loading a valid capability
         *              from memory where the Block and Page descriptor
         *              LC field is 0b11.
         */
        mov     x0, #0xB0
        msr     cctlr_el1, x0
        isb

        /* set CVBAR_EL1 */
        ldr     x0, =__vectors
        mrs     c1, ddc
        scvalue c0, c1, x0
        msr     cvbar_el1, c0

        /* Switch to C64 mode */
        mrs     c1, ddc
        adr     x0, __start_el1_c64 + 1
        scvalue c0, c1, x0
        mov     x1, #0
        orr     x1, x1, __CHERI_CAP_PERMISSION_PERMIT_STORE__
        orr     x1, x1, __CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__
        clrperm c0, c0, x1
        br      c0
        .size __start_el1, . - __start_el1

/* EL1 startup code in C64 mode */
__start_el1_c64:
        .arch armv8-a+c64

        /* Setup the capability stack pointer (CSP) */

        /* Get address of stack limits for this CPU */
        ldr     x0, =cpu_stacks
        mrs     c1, ddc
        scvalue c0, c1, x0
        mov     x2, 16
        mul     x2, x2, x7
        add     c0, c0, x2 /* c0 = cpu_stacks + (CPU_Num * 16) */

        /* Clear execute permission bit */
        mov     x1, __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__
        clrperm c0, c0, x1

        /* Calculate the bounds and set the CSP */
        ldp     x2, x3, [c0] /* Load stack start/end */
        scvalue c1, c0, x2   /* Point to stack start */
        sub     x2, x3, x2   /* length = end - start */
        scbndse c1, c1, x2   /* Set bounds to the stack length */
        add     c1, c1, x2   /* Point to stack end */
        mov     csp, c1

        bl      __configure_mmu

        /* CPU 0 starts as master, the others are slave CPUs */
        mrs     x7, mpidr_el1
        and     x7, x7, #3
        cbz     x7, __start_master_cpu

        /* Constrain the slave CPU's PCC immediately
         * since the GOT is already initialized
         * by the master CPU */
        adrp    c0, slave_cpu_constrained_pcc
        add     c0, c0, :lo12:slave_cpu_constrained_pcc
        ldr     c0, [c0]
        br      c0
        .size __start_el1_c64, . - __start_el1_c64

/* Entry point for the master CPU (C64 mode, EL1) */
__start_master_cpu:
        /* Copy .data */
        ldr     x0, data_segment + 0
        ldr     x1, data_segment + 8
        ldr     x2, data_segment + 16
        mrs     c4, ddc
        scvalue c0, c4, x0
        scvalue c1, c4, x1
        /* Do nothing if __data_dwords is 0 */
        cbz     x2, 1f
        /* Do nothing if __data_load == __data_start */
        sub     x3, x0, x1
        cbz     x3, 1f
0:      ldr     x3, [c0], #8
        str     x3, [c1], #8
        sub     x2, x2, #1
        cbnz    x2, 0b
1:

        /* Clear .bss */
        ldr     x0, bss_segment + 0
        ldr     x1, bss_segment + 8
        mrs     c2, ddc
        scvalue c0, c2, x0
        ldr     w8, [c0]
        /* Do nothing if __bss_dwords is 0 */
        cbz     x1, 1f
0:      str     xzr, [c0], #8
        sub     x1, x1, #1
        cbnz    x1, 0b
1:

        /* Init global caps (e.g. setup the GOT and process relocations) */
        bl      __gnat_init_morello

        /* Setup the heap */
        bl      __gnat_heap_init

        /* Constrain the PCC */
        adrp    c0, master_cpu_constrained_pcc
        add     c0, c0, :lo12:master_cpu_constrained_pcc
        ldr     c0, [c0]
        br      c0
        .size __start_master_cpu, . - __start_master_cpu

/* Entry point for the master CPU (C64 mode, EL1) with the PCC's bounds
   constrained to the minimum required bounds. */
        .type __start_master_cpu_constrained_pcc, @function
__start_master_cpu_constrained_pcc:

        bl      main
        b       __gnat_stop
0:      b       0b
        .size __start_master_cpu_constrained_pcc, . - __start_master_cpu_constrained_pcc

/* Entry point for the slave CPU (C64 mode, EL1) with the PCC's bounds
   constrained to the minimum required bounds. */
        .type __start_slave_cpu_constrained_pcc, @function
__start_slave_cpu_constrained_pcc:
        mov     x29, xzr
        mov     x19, x7
        /* Initialize cpu-specific devices */
        bl      __gnat_initialize_cpu_devices

        /* Call the rts entry point for the CPU (with CPU_Id as argument). */
        add     x0,x19,#1
        bl      __gnat_initialize_slave
1:      b       1b
        .size __start_slave_cpu_constrained_pcc, . - __start_slave_cpu_constrained_pcc

/* Important note on __configure_mmu, __dcaches_all and __dcache_level:
   we can't use the stack to save the LR, because we may be called in a
   context where the stack is not setup. So to save the Link Register we use:
   * c16 in __configure_mmu
   * c15 in __dcaches_all
   * no need to save anything in __dcache_level because it's a leaf function
 */
        .type __configure_mmu, %function
__configure_mmu:
        /* save the link register */
        mov     c16, c30

        /* Enable MMU and cache. */
        tlbi    vmalle1
        ic      iallu         /* I-cache invalidate */
        mov     x0, #1
        bl      __dcaches_all /* D-caches invalidate */
        dsb     sy
        isb

        /* TTBR0_EL1: */
        ldr     x0, =__mmu_l1_000080000
        msr     ttbr0_el1, x0

        /* Setup/enable the MMU.  */

        /* TCR_EL1 */
        movz    x0, #((0 << 0)  | /* IPS    */ \
                      (0 << 4)  | /* AS     */ \
                      (1 << 5)  | /* TBI0   */ \
                      (1 << 6)  | /* TBI1   */ \
                      (0 << 7)  | /* HA     */ \
                      (0 << 8)  | /* HD     */ \
                      (1 << 9)  | /* HPD0   */ \
                      (0 << 10) | /* HPD1   */ \
                      (1 << 11) | /* HWU059 */ \
                      (1 << 12) | /* HWU060 */ \
                      (1 << 13) | /* HWU061 */ \
                      (1 << 14) | /* HWU062 */ \
                      (0 << 15)), /* HWU159 */ \
                      LSL #32
        movk    x0, #((32 << 0) | /* T1SZ  */ \
                      (0 << 6)  | /* A1    */ \
                      (1 << 7)  | /* EPD1  */ \
                      (3 << 8)  | /* IRGN1 */ \
                      (3 << 10) | /* ORGN1 */ \
                      (2 << 12) | /* SH1   */ \
                      (2 << 14)), /* TG1   */ \
                      LSL #16
        movk    x0, #((32 << 0) | /* T0SZ  */ \
                      (0 << 7)  | /* EPD0  */ \
                      (3 << 8)  | /* IRGN0 */ \
                      (3 << 10) | /* ORGN0 */ \
                      (2 << 12) | /* SH0   */ \
                      (0 << 14))  /* TG0   */
        msr     tcr_el1, x0

        mov     x0, #0xee                       /* Inner/outer cacheable WB */
        msr     mair_el1, x0
        isb

        mrs     x0, sctlr_el1
        ldr     x1, =0x100d                     /* bits I(12) SA(3) C(2) M(0) */
        bic     x0, x0, #(1 << 1)               /* clear bit A(1) */
        bic     x0, x0, #(1 << 19)              /* clear WXN */
        orr     x0, x0, x1                      /* set bits */

        dsb     sy
        msr     sctlr_el1, x0
        isb
        mov     c30, c16 /* restore link register */
        ret

/* __dcaches_all
 *
 * x0: 0 clean & invalidate, 1 invalidate only
 *
 * flush or invalidate all data cache by set/way
 */
        .type __dcaches_all, %function
__dcaches_all:
        mov     x1, x0
        dsb     sy
        mrs     x10, clidr_el1
        lsr     x11, x10, #24
        and     x11, x11, #0x7           /* clidr_el1.LoC */
        cbz     x11, __dcaches_finished  /* LoC == 0 => finish */
        mov     c15, c30                 /* save the link register */
        mov     x0, #0                   /* start flush at cache level 0 */
        /* x0:  cache level
         * x1:  parameter
         * x10: clidr_el1
         * x11: LoC
         * c15: return address
         */
__dcaches_loop_level:
        lsl     x12, x0, #1
        add     x12, x12, x0    /* x12 <- 3 x cache level */
        lsr     x12, x10, x12
        and     x12, x12, #7    /* x12 <- cache type */
        cmp     x12, #2
        b.lt    __dcaches_skip  /* skip if no cache or icache */
        bl      __dcache_level  /* x1 given as parameter */
__dcaches_skip:
        add     x0, x0, #1      /* Cache Level += 1 */
        cmp     x11, x0         /* Cache_Level <> LoC */
        b.gt    __dcaches_loop_level

        mov     x0, #0
        msr     csselr_el1, x0  /* restore csselr_el1 */
        dsb     sy
        isb
        mov  c30, c15
__dcaches_finished:
        ret
        .size __dcaches_all, . - __dcaches_all

/* __dcache_level
 * x0 : cache level
 * x1 : 0 clean & invalidate, 1 invalidate only
 * x2 - x9: clobbered
 */
        .type __dcache_level, %function
__dcache_level:
        lsl     x12, x0, #1
        msr     csselr_el1, x12     /* select cache level */
        isb
        mrs     x6, ccsidr_el1      /* read the new cssidr_el1 */
        and     x2, x6, #7          /* x2 <- log2(cache line size) - 4 */
        add     x2, x2, #4          /* x2 <- log2(cache line size) */
        mov     x3, #0x3ff
        and     x2, x3, x6, lsr #3  /* x3 <- max number of ways */
        clz     w5, w3              /* bit position of ways */
        mov     x4, #0x7fff
        and     x4, x4, x6, lsr #13 /* x4 <- max number of sets */
__dcache_loop_set:
        mov     x6, x3              /* x6 <- copy of ways */
__dcache_loop_way:
        lsl     x7, x6, x5
        orr     x9, x12, x7         /* map way and level to cisw value */
        lsl     x7, x4, x2
        orr     x9, x9, x7          /* map set number to cisw value */
        tbz     w1, #0, 1f          /* invalidate */
        dc      isw, x9
        b       2f
1:      dc      cisw, x9            /* clean & invalidate */
2:      subs    x6, x6, #1          /* ways := ways - 1 */
        b.ge    __dcache_loop_way
        subs    x4, x4, #1          /* set := set - 1 */
        b.ge    __dcache_loop_set
        ret
        .size __dcache_level, . - __dcache_level

        .align 3
cpu_stacks:
        .dword   __cpu0_stack_start
        .dword   __cpu0_stack_end
        .dword   __cpu1_stack_start
        .dword   __cpu1_stack_end
        .dword   __cpu2_stack_start
        .dword   __cpu2_stack_end
        .dword   __cpu3_stack_start
        .dword   __cpu3_stack_end

bss_segment:
        .dword   __bss_start
        .dword   __bss_dwords

data_segment:
        .dword   __data_load
        .dword   __data_start
        .dword   __data_dwords

/* Store function pointer capabilities with constrained bounds.
 * The linker generates entries in the relocation tables with
 * the necessary bounds, and these caps are initialized at
 * startup by __init_global_caps.
 */
        .data
        .align 3
master_cpu_constrained_pcc:
        .chericap __start_master_cpu_constrained_pcc
        .size master_cpu_constrained_pcc, 16
slave_cpu_constrained_pcc:
        .chericap __start_slave_cpu_constrained_pcc
        .size slave_cpu_constrained_pcc, 16
