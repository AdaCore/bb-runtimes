/*******************************************************************************
 * PolarFire SoC SMP Startup for Ada Ravenscar Runtime
 * Target: U54 Application Cores (Harts 1-4)
 * Hart 0 (E51) is parked to isolate the runtime to homogenous U54 cores.
 ******************************************************************************/

#include "riscv_def.h"

        .section .start
        .globl _start_ram
        .type _start_ram,@function


_start_ram:
    /* Disable interrupts and clear pending bits */
    csrc mstatus, MSTATUS_MIE
    csrw mie, zero
    csrw mip, zero

    /* Diverge: Park Hart 0 (E51 Monitor Core) immediately */
    csrr a0, mhartid
    beqz a0, infinite_hart_parking

    /* U54 cores (1-4) must clear delegation registers */
    /* This runtime is designed to run entirely in Machine Mode */
    /* We do not use the Supervisor Mode, so no need for delegation */
    csrw mideleg, zero
    csrw medeleg, zero

    /* Zero all general-purpose registers (x1-x31) */
    li x1, 0;
    li x2, 0;
    li x3, 0;
    li x4, 0;
    li x5, 0;
    li x6, 0;
    li x7, 0;
    li x8, 0;
    li x9, 0;
    li x10,0;
    li x11,0;
    li x12,0;
    li x13,0;
    li x14,0;
    li x15,0;
    li x16,0
    li x17,0;
    li x18,0;
    li x19,0;
    li x20,0;
    li x21,0;
    li x22,0;
    li x23,0;
    li x24,0
    li x25,0;
    li x26,0;
    li x27,0;
    li x28,0;
    li x29,0;
    li x30,0;
    li x31,0

    /* Initialize Global Pointer */
    /* See linker script for __global_pointer$ definition */
    .option push
    .option norelax
    la gp, __global_pointer$
    .option pop

    /*  Hart-Specific Stack Selection */
    csrr a0, mhartid

    li   t0, 1;
    beq a0, t0, .hart1

    li   t0, 2;
    beq a0, t0, .hart2

    li   t0, 3;
    beq a0, t0, .hart3

    li   t0, 4;
    beq a0, t0, .hart4

    /* Invalid hart ID -> Exit. Not expected to happen */
    j infinite_hart_parking


    /* Stack Initialization for each U54 Hart (1-4) */
.hart1:
    la a4, __stack_bottom_h1$;
    la sp, __stack_top_h1$;
    j .init_stack
.hart2:
    la a4, __stack_bottom_h2$;
    la sp, __stack_top_h2$;
    j .init_stack
.hart3:
    la a4, __stack_bottom_h3$;
    la sp, __stack_top_h3$;
    j .init_stack
.hart4:
    la a4, __stack_bottom_h4$;
    la sp, __stack_top_h4$

.init_stack:
    /* Zero entire stack area */
    mv  a5, sp // a5 = stack top
1:  sd  zero, 0(a4) // Clear 8 bytes at a4
    addi a4, a4, 8 // Move to next 8 bytes
    blt  a4, a5, 1b // Loop until entire stack area cleared (a4 < a5)

    /* Diverge: Main Hart (Hart 1) vs Secondary Harts */
    csrr a0, mhartid
    li   t0, 1
    bne  a0, t0, .secondary_hart_wait

    /* ----------------------------------------------------------------------- */
    /* --------------------------- MAIN HART PATH (U54_1) -------------------- */
    /* ----------------------------------------------------------------------- */
    /* Clear hardware memories */
    call .clear_l2lim

    /* Zero BSS (Only Main Hart clears shared memory) */
    la   a0, __bss_start
    la   a1, __bss_end
    bgeu a0, a1, 3f
2:  sw   zero, (a0)
    addi a0, a0, 4
    bltu a0, a1, 2b
3:

#ifndef __riscv_float_abi_soft
    /* Enable Floating Point Unit */
    li   t0, MSTATUS_FS
    csrs mstatus, t0
#endif

    /* Enter Ada Environment: Elaboration and Main procedure */
    /* Note: Elaboration will wake secondary harts via IPIs in Start_All_CPUs */
    call __libc_init_array
    call main

    /* Save returned value before global destructors */
    mv s0, a0

    /* Cleanup global destructors */
    call __libc_fini_array

    /* Restore exit status code and exit */
    mv a0, s0
    call exit
    /* -- END of MAIN HART PATH -- */

.secondary_hart_wait:
    /* ----------------------------------------------------------------------- */
    /* ---------------- SECONDARY HARTS PATH (U54_2 to U54_4) ---------------- */
    /* ----------------------------------------------------------------------- */
    /* Enable only Machine Software Interrupts (MSIP) for first IPI */
    li   a2, MIP_MSIP
    csrw mie, a2

    /* Wait for Interprocess Interrupt (IPI) from Ada main */
.Lwait_ipi:
    /* At this point, mstatus.MIE = 0, (Global interrupt switch disabled)
        and mie.MSIP = 1 (Machine Software Interrupts enabled)
        So the IPI will wake us from WFI without sending to a trap handler */
    wfi

    /* The IPI will set the MSIP bit in the MIP register,
    which we will check after WFI returns  */
    csrr a2, mip
    andi a2, a2, MIP_MSIP
    beqz a2, .Lwait_ipi

    /* Wake confirmed: Disable IPI source the __gnat_initialize_slave
       function will re-enable interrupts properly as needed
    */
    csrw mie, zero

#ifndef __riscv_float_abi_soft
    /* Enable FPU for the secondary U54 core */
    li   t0, MSTATUS_FS
    csrs mstatus, t0
#endif

    /* Initialize secondary hart hardware (timer and interrupts) */
    call init_secondary_hart

    /* Call the Ada RTS entry point for the secondary CPU */
    csrr a0, mhartid // Pass hart ID as first argument a0
    call __gnat_initialize_slave

    .globl infinite_hart_parking
    .type infinite_hart_parking,@function
infinite_hart_parking:
    wfi
    j infinite_hart_parking

.clear_l2lim:
    la a4, 0x08000000          # start of LIM address
    la a5, 0x08200000          # end of LIM address
1:  sd x0, 0(a4)
    addi a4, a4, 8
    blt  a4, a5, 1b
    ret

/* Exit and Abort Code */
        .globl __gnat_exit
        .type __gnat_exit,@function
        .globl _abort
        .type abort,@function
abort:
__gnat_exit:
        /* Reset board by writing 0xDEAD to MSS_RESET_CR */
        li t0, 0x20002018
        li t1, 0xDEAD
        sw t1, (t0)
        j __gnat_exit

        .weak      _exit
        .set _exit,__gnat_exit


.weak   init_secondary_hart
init_secondary_hart:
    j infinite_hart_parking

.weak   __gnat_initialize_slave
__gnat_initialize_slave:
    j infinite_hart_parking

