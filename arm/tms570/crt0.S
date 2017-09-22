/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                      Copyright (C) 2011-2012 AdaCore                     *
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

/* This file is specific to the Hercules ARM Cortex-R4F Microcontrollers,
   such as the TMS570. */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for bareboard targets. */

/* Startup code */

	.section .traps

	/* The following provide default definitions for trap vectors that
	loop back to the trap address, so the system will stop in a safe
	state. The Ravenscar runtimes now can install their handlers by simply
	exporting the relevant symbols. */

	.weak	__gnat_undef_trap	/* Undefined instruction trap */
	.weak	__gnat_svc_trap		/* Supervisor call trap */
	.weak	__gnat_pabt_trap	/* prefetch abort trap	*/
	.weak	__gnat_dabt_trap	/* data abort trap */

/* 0 */	b	__gnat_init
__gnat_undef_trap:	/* 4 */
	b	__gnat_undef_trap	/* Undefined instruction trap */
__gnat_svc_trap:	/* 8 */
	b	__gnat_svc_trap		/* Supervisor call trap */
__gnat_pabt_trap:	/* c */
	b	__gnat_pabt_trap	/* prefetch abort trap	*/
__gnat_dabt_trap:	/* 10 */
	b	__gnat_dabt_trap	/* data abort trap */
	nop		/* 14 */
	ldr	pc,[pc,#-0x1b0] /* IRQ, jump to IRQVECREG, 16#ffff_fe70# */
	ldr	pc,[pc,#-0x1b0] /* FIQ, jump to FIQVECREG, 16#ffff_fe74# */


        .text


        /*********************************************************************
         * __gnat_init:                                                      *
         *                                                                   *
         * main entry point.                                                 *
         *********************************************************************/
        .globl  __gnat_init
        .type   __gnat_init, %function
__gnat_init:
        /*********************************************/
        /* 1. Init registers, stacks and FPU         */
        /*********************************************/

        bl      __gnat_init_cpu

        /*********************************************/
        /* 2. Install workarounds for some hw errata */
        /*********************************************/

        /* Errata DEVICE#140 (TMS570LS Rev A silicon) */
        ldr r0, DEVID
        ldr r1, [r0]
        ldr r0, DEVID_REVA
        cmp r0, r1
        bleq  _esmCcmErrorsClear_

        /* Cortex-R4 errata workarounds: check the cpu version */
        mrc     p15, #0, r0, c0, c0, #0 /* MIDR */
        mov     r1, #0xFFF0             /* Primary part num. mask */
        and     r0, r0, r1
        mov     r1, #0xC140             /* Cortex-r4 identifier */
        cmp     r0, r1
        blne    _skip_r4_errata /* skip if cortex-r5 */

        /* Errata CORTEX-R4#66:
         * Description:
         *
         * Register Corruption during a Load-Multiple Instruction at an
         * exception vector.
         *
         * Workaround:
         *
         * Disable out-of-order completion for divide instructions in
         * Auxiliary Control register
         */
        mrc     p15, #0, r0, c1, c0, #1
        orr     r0, r0, #0x80
        mcr     p15, #0, r0, c1, c0, #1

        /* Errata CORTEX-R4#57
         * Description:
         *
         * Conditional VMRS APSR_Nvcz, FPSCR may evaluate incorrect flags.
         *
         * Workaround:
         *
         * Disable out-of-order single-precision floating point
         * multiply-accumulate instruction completion
         */
        mrc     p15, #0, r0, c15, c0, #0
        orr     r0, r0, #0x10000
        mcr     p15, #0, r0, c15, c0, #0
_skip_r4_errata:

        /*********************************************/
        /* 3. Board initialisation                   */
        /*********************************************/

        /* In case of power-on reset, we need to perform a full board
           power-up, while in case of CPU reset we only need to apply
           CPU initialisation */

        /* Setup clocks, peripherals */
        /* See the corresponding system_<board>.c file */
        bl      __gnat_system_init

        /*************************************/
        /* 3.1. POWER-ON or DEBUG Reset case */
        /*************************************/

        /* Check the reset condition */
        mov     r2, #0xffffffe4   @ SYSESR
        ldr     r3, [r2]
        movw    r1, #0x8800       @ PORST, DBGRST
        tst     r3, r1
        beq     _check_reset_cpu

        str	r1, [r2] /* Clear the reset flag */

        /* Allows the CPU to signal any single-bit or double-bit errors
           detected by its ECC logic for accesses to program flash or data
           RAM */
        bl      __gnat_enable_event_bus_export

        /* Enable response to ECC errors indicated by CPU for accesses
           to flash */
        ldr r0, FEDACCTRL1
        ldr r1, FEDACCTRL1_INITVAL
        str r1, [r0]

        /* Initialize the entire CPU RAM and the corresponding ECC locations */
        bl      __gnat_init_mem

        /* Enable CPU ECC checking for ATCM (flash accesses) */
        bl      __gnat_enable_ram_ecc

        b       _end_board_init

        /*************************************/
        /* 3.2. CPU Reset case               */
        /*************************************/

_check_reset_cpu:
        movw    r1, #0x20       @ CPURST
        tst     r3, r1
        beq     2f @ r3 and r1 == 0?

        str	r1, [r2] /* Clear the reset flag */

        bl      __gnat_enable_event_bus_export

        /*************************************/
        /* 3.3. Finally, setup MPU and cache */
        /*************************************/
        bl      __gnat_mpu_enable
        bl      __gnat_cache_enable

_end_board_init:
        /*********************************************/
        /* 4. Memory sections init                   */
        /*********************************************/

        /* Clear .bss */
        movw  r0, #:lower16:__bss_start
        movt  r0, #:upper16:__bss_start
        movw  r1, #:lower16:__bss_words /* Must be less than 64K words here */
        movw  r2, #0
	cmp   r1,#0
	beq   1f
0:      str r2,[r0],#4
        subs r1,r1,#1
        bne 0b

        /* Copy .data */
	movw	r0,#:lower16:__data_start
	movt	r0,#:upper16:__data_start
	movw	r2,#:lower16:__data_load
	movt	r2,#:upper16:__data_load
        cmp     r0, r2
        beq     1f /* start == load: Skip data copy */

	movw	r1,#:lower16:__data_words
	movt	r1,#:upper16:__data_words
	cmp	r1,#0
	beq	1f
0:	ldr	r4,[r2],#4
	str	r4,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:

	/* Call the main routine */
 	bl      main
        /* reboot the board if main would return */
        bl      __exit

DEVID:                  .word 0xfffffff0
DEVID_REVA:             .word 0x802aad05
FEDACCTRL1:             .word 0xfff87008
FEDACCTRL1_INITVAL:     .word 0x000a060A
	.size __gnat_init, . - __gnat_init


        /*********************************************************************
         * __exit:                                                           *
         *                                                                   *
         * Forces a CPU reset.                                               *
         *********************************************************************/
        .globl  __exit
        .type   __exit, %function
__exit:
	/* Write 0x8000 to SYSECR (16#FFFF_FFE0#) */
        mov	r2, #0x8000
	mvn	r3, #0
        str	r2, [r3, #-31]
        /* Insert a hardware breakpoint */
        bkpt    0
	b	__exit
        .size   __exit, . - __exit


        /*********************************************************************
         * __gnat_init_cpu                                                   *
         *                                                                   *
         * Initializes CPU registers, including backed registers for all     *
         * modes. This is necessary on Cortex-R4F to avoid miscompares       *
         * between the cores running in lockstep, and is a good idea on any  *
         * system anyway to ensure reproductible results.                    *
         * During this initialisation, the stacks are setup, and the FPU is  *
         * enabled.                                                          *
         *********************************************************************/
        .type   __gnat_init_cpu, %function
__gnat_init_cpu:
        /* Start initializing CPU registers, including backed registers for
           all modes. This is necessary on Cortex R4F to avoid miscompares
           between the cores running in lockstep, and is a good idea on any
           system. */

        /* After reset, the CPU is in the Supervisor mode (M = 10011) */
        mov     r0, lr /* Save lr in r0: don't touch r0 below ! */
        mov     r1, #0
        mov     r2, #0
        mov     r3, #0
        mov     r4, #0
        mov     r5, #0
        mov     r6, #0
        mov     r7, #0
        mov     r8, #0
        mov     r9, #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0

        /* Set SPSR */
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        ldr     sp, svcSp	/* r13 */

        /* Switch to FIQ mode (M = 10001) */
        cps     #17
        mov     r8, #0
        mov     r9, #0
        mov     r10, #0
        mov     r11, #0
        mov     r12, #0
        ldr     sp, fiqSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* IRQ mode */
        cps     #18  /* 10010 */
	ldr     sp, irqSp
	mov     lr, #0
	mrs     r1, CPSR
	msr     SPSR_fsxc, r1

        /* Abort mode */
        cps     #23  /* 10111 */
        ldr     sp, abortSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Undefined instruction mode */
        cps     #27  /* 11011 */
        ldr     sp, undefSp
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* System mode */
        cps     #31  /* 11111 */
        ldr     sp, userSp	/* SYS mode shared stack with User mode */
        mov     lr, #0
        mrs     r1, CPSR
        msr     SPSR_fsxc, r1

        /* Stay in system mode.  */

        /* Enable FPU on the CPACR/FPEXC registers */
        mrc     p15, #0, r2, c1, c0, #2 /* CPACR */
        orr     r2, r2, #0xF00000       /* Enable CP10, CP11 user & system */
        mcr     p15, #0, r2, c1, c0, #2 /* set */
        mov     r2, #0x40000000         /* Set the FPEXC.EN bit */
        fmxr    fpexc, r2

        vmov    d0, r1, r1
        vmov    d1, r1, r1
        vmov    d2, r1, r1
        vmov    d3, r1, r1
        vmov    d4, r1, r1
        vmov    d5, r1, r1
        vmov    d6, r1, r1
        vmov    d7, r1, r1
        vmov    d8, r1, r1
        vmov    d9, r1, r1
        vmov    d10, r1, r1
        vmov    d11, r1, r1
        vmov    d12, r1, r1
        vmov    d13, r1, r1
        vmov    d14, r1, r1
        vmov    d15, r1, r1

        /* Synchronize 4-entry call-return stack */
        bl      0f
0:      bl      1f
1:      bl      2f
2:      bl      3f
3:      bx      r0

svcSp:	        .word __svc_stack_end
abortSp:        .word __abort_stack_end
undefSp:        .word __undef_stack_end
fiqSp:          .word __fiq_stack_end
irqSp:          .word __irq_stack_end
userSp:         .word __stack_end
        .size   __gnat_init_cpu, . - __gnat_init_cpu


        /*********************************************************************
         * _esmCcmErrorsClear_:                                              *
         *                                                                   *
         * Clear ESM Group2 Channel 2 error in ESMSR2 and Compare error in   *
         * CCMSR register.                                                   *
         *********************************************************************/
_esmCcmErrorsClear_:
        ldr   r0, ESMSR1_REG     // load the ESMSR1 status register address
        ldr   r2, ESMSR1_ERR_CLR
        str       r2, [r0]       // clear the ESMSR1 register

        ldr   r0, ESMSR2_REG     // load the ESMSR2 status register address
        ldr   r2, ESMSR2_ERR_CLR
        str       r2, [r0]       // clear the ESMSR2 register

        ldr   r0, ESMSSR2_REG    // load the ESMSSR2 status register address
        ldr   r2, ESMSSR2_ERR_CLR
        str       r2, [r0]       // clear the ESMSSR2 register

        ldr   r0, ESMKEY_REG     // load the ESMKEY register address
        mov       r2, #0x5
        str       r2, [r0]       // clear the ESMKEY register

        ldr   r0, VIM_INTREQ     // load the INTREQ register address
        ldr   r2, VIM_INT_CLR
        str       r2, [r0]       // clear the INTREQ register

        ldr   r0, CCMR4_STAT_REG // load the CCMR4 status register address
        ldr   r2, CCMR4_ERR_CLR
        str       r2, [r0]       // clear the CCMR4 status register
        bx    lr

ESMSR1_REG:      .word  0xFFFFF518
ESMSR2_REG:      .word  0xFFFFF51C
ESMSR3_REG:      .word  0xFFFFF520
ESMKEY_REG:      .word  0xFFFFF538
ESMSSR2_REG:     .word  0xFFFFF53C
CCMR4_STAT_REG:  .word  0xFFFFF600
ERR_CLR_WRD:     .word  0xFFFFFFFF
CCMR4_ERR_CLR:   .word  0x00010000
ESMSR1_ERR_CLR:  .word  0x80000000
ESMSR2_ERR_CLR:  .word  0x00000004
ESMSSR2_ERR_CLR: .word  0x00000004
VIM_INT_CLR:     .word  0x00000001
VIM_INTREQ:      .word  0xFFFFFE20
        .size _esmCcmErrorsClear_, . - _esmCcmErrorsClear_


        /*********************************************************************
         * __gnat_enable_event_bus_export:                                   *
         *                                                                   *
         * Enable CPU Event Export. This allows the CPU to signal any        *
         * single-bit or double-bit errors detected by its ECC logic for     *
         * accesses to program flash or data RAM.                            *
         *********************************************************************/
        .type __gnat_enable_event_bus_export, %function
__gnat_enable_event_bus_export:
        mrc	p15, #0, r0, c9, c12, #0
        orr	r0, r0, #0x10
        mcr	p15, #0, r0, c9, c12, #0
        bx	lr
  .size	__gnat_enable_event_bus_export, . - __gnat_enable_event_bus_export


        /*********************************************************************
         * __gnat_init_mem:                                                  *
         *                                                                   *
         * Enable the hardware initialisation for SRAM, preventing ECC       *
         * errors                                                            *
         *********************************************************************/
        .type  __gnat_init_mem, %function
__gnat_init_mem:
        ldr r12, MINITGCR
        mov r4, #0xA
        str r4, [r12]       @ enable global memory hw initialization
        ldr r11, MSIENA
        mov r4, #0x01
        str r4, [r11]       @ Enable auto hardware initialisation for SRAM
1:                          @ loop until hardware init completes
        ldr r5, MSTCGSTAT
        ldr r4, [r5]
        tst r4, #0x100
        beq 1

        mov r4, #5
        str r4, [r12]       @ disable global memory hw initialization
        bx  lr
MINITGCR:        .word 0xFFFFFF5C
MSIENA:          .word 0xFFFFFF60
MSTCGSTAT:       .word 0xFFFFFF68
        .size	__gnat_init_mem, . - __gnat_init_mem


        /*********************************************************************
         * __gnat_enable_ram_ecc                                             *
         *                                                                   *
         * Enable CPU ECC checking for ATCM (flash access)                   *
         *********************************************************************/
        .globl  __gnat_enable_ram_ecc
        .type   __gnat_enable_ram_ecc, %function
__gnat_enable_ram_ecc:
        mrc     p15, #0x00, r0, c1, c0, #0x01
        orr     r0, r0, #0xC000000
        dmb
        mcr     p15, #0x00, r0, c1, c0, #0x01
        isb
        bx  lr
        .size __gnat_enable_ram_ecc, . - __gnat_enable_ram_ecc


        /*********************************************************************
         * __gnat_cache_enable
         *                                                                   *
         * Enables instruction and data cache.
         *********************************************************************/
        .globl __gnat_cache_enable
        .type __gnat_cache_enable, %function
__gnat_cache_enable:
        mrc	p15, #0, r1, c1, c0, #1  @ ACTLR
        bic 	r1, r1, #(1 << 5)        @ Clear bit 5 (enable ECC)
        mcr	p15, #0, r1, c1, c0, #1

        mrc	p15, #0, r1, c1, c0, #0  @ Read SCTLR
        orr	r1, r1, #(1 << 12)       @ I
        orr	r1, r1, #(1 << 2)        @ D
        dsb
        mov	r0, #0
        mcr	p15, #0, r0, c15, c5, #0  @ Invalidate data cache
        dsb
        mcr	p15, #0, r0, c7, c5, #0   @ Invalidate instruction cache
        dsb
        mcr	p15, #0, r1, c1, c0, #0   @ Enabled cache
        isb
        bx	lr
        .size	__gnat_cache_enable, . - __gnat_cache_enable


        /*********************************************************************
         * __gnat_mpu_enable                                                 *
         *                                                                   *
         * Enables MPU and setup the MPU tables.                             *
         * The loaded table depends on __gnat_ram_at_0:                      *
         * 0 : FLASH is expected @ 0x0,       RAM @ 0x8000000                *
         * 1 : FLASH is expected @ 0x8000000, RAM @ 0x0                      *
         *********************************************************************/
        .globl __gnat_mpu_enable
        .type __gnat_mpu_enable, %function
__gnat_mpu_enable:
        @ Disable MPU
        mrc	p15, #0, r0, c1, c0, #0
        bic	r0, r0, #1
        dsb
        mcr	p15, #0, r0, c1, c0, #0
        isb
        @ Disable background region
        mrc     p15, #0, r0, c1, c0, #0
        bic     r0, r0, #0x20000
        mcr	p15, #0, r0, c1, c0, #0

        @ Load region
        ldr     r0, =__gnat_ram_at_0
        tst     r0, #1                   @ RAM and FLASH swapped ?
        adrne   r1, mpu_ram0
        adreq   r1, mpu_table
        mov	r2, #0
1:	ldmia	r1!, {r3, r4, r5}
        cmp	r4, #0
        beq	_clear_unused_regions
        mcr	p15, #0, r2, c6, c2, #0  @ MPU region number
        mcr	p15, #0, r3, c6, c1, #0  @ MPU region base address
        mcr	p15, #0, r4, c6, c1, #2  @ MPU region size and enable
        mcr	p15, #0, r5, c6, c1, #4  @ MPU region access control
        add	r2, r2, #1
        b	1b

        @ Clear unused regions
_clear_unused_regions:
        mov     r3, #0
        mov     r4, #0x8 @ Size = 32bytes (minimal value), EN = 0
        mov     r5, #0x1606

3:      cmp     r2, #16
        beq     _enable_mpu
        mcr	p15, #0, r2, c6, c2, #0  @ MPU region number
        mcr	p15, #0, r3, c6, c1, #0  @ MPU region base address
        mcr	p15, #0, r4, c6, c1, #2  @ MPU region size and enable
        mcr	p15, #0, r5, c6, c1, #4  @ MPU region access control
        add	r2, r2, #1
        b	3b

_enable_mpu:
        @ enable mpu background region
        mrc	p15, #0, r0, c1, c0, #0
        orr	r0, r0, #0x20000
        mcr	p15, #0, r0, c1, c0, #0
        @ enable the mpu itself
        mrc	p15, #0, r0, c1, c0, #0
        orr	r0, r0, #1
        mcr	p15, #0, r0, c1, c0, #0
        isb
        bx	lr

mpu_ram0:
        /* Table used when RAM is mapped at 0x0
         *      Base        Subregions    Sz            En   AP       TEX */
        /* 1: background region: */
        .word   0x00000000, (0xFF << 8) + (0x1F << 1) + (1), 0x1000 + 0x08
        /* 2: 512KB SRAM */
        .word   0x00000000, (0x00 << 8) + (0x12 << 1) + (1), 0x0300 + 0x08
        /* 3: 512KB FLASH */
        .word   0x08000000, (0x00 << 8) + (0x12 << 1) + (1), 0x0600 + 0x08
        /* 4: 64MB  Async RAM */
        .word   0x60000000, (0x00 << 8) + (0x19 << 1) + (1), 0x0300 + 0x00
        /* 5: 128MB SDRAM */
        .word   0x80000000, (0x00 << 8) + (0x1A << 1) + (1), 0x0300 + 0x00
        /* 6: 6MB   Flash OTP, ECC, EEPROM Bank */
        .word   0xF0000000, (0xC0 << 8) + (0x16 << 1) + (1), 0x1200 + 0x08
        /* 7: 16MB  peripheral segment 2 */
        .word   0xFC000000, (0x00 << 8) + (0x17 << 1) + (1), 0x1300 + 0x10
        /* 8: 512B  accessible CRC module */
        .word   0xFE000000, (0x00 << 8) + (0x08 << 1) + (1), 0x1300 + 0x10
        /* 9: 16MB  peripheral segment 3 */
        .word   0xFF000000, (0x00 << 8) + (0x17 << 1) + (1), 0x1300 + 0x10
        .word	0, 0, 0

mpu_table:
        /* Nominal boot: FLASH @0x0, RAM @0x8000000 case
         *      Base        Subregions    Sz            En   AP       TEX */
        /* 1: background region: */
        .word   0x00000000, (0xFF << 8) + (0x1F << 1) + (1), 0x1000 + 0x08
        /* 2: 4MB FLASH */
        .word   0x00000000, (0x00 << 8) + (0x15 << 1) + (1), 0x0600 + 0x08
        /* 3: 512KB SRAM */
        .word   0x08000000, (0x00 << 8) + (0x12 << 1) + (1), 0x0300 + 0x08
        /* 4: 64MB Async RAM */
        .word   0x60000000, (0x00 << 8) + (0x19 << 1) + (1), 0x0300 + 0x00
        /* 5: 128MB SDRAM */
        .word   0x80000000, (0x00 << 8) + (0x1A << 1) + (1), 0x0300 + 0x00
        /* 6: 6MB  Flash OTP, ECC, EEPROM Bank */
        .word   0xF0000000, (0xC0 << 8) + (0x16 << 1) + (1), 0x1200 + 0x08
        /* 7: 16MB peripheral segment 2 */
        .word   0xFC000000, (0x00 << 8) + (0x17 << 1) + (1), 0x1300 + 0x10
        /* 8: 512B accessible CRC module*/
        .word   0xFE000000, (0x00 << 8) + (0x08 << 1) + (1), 0x1300 + 0x10
        /* 9: 16MB peripheral segment 3 */
        .word   0xFF000000, (0x00 << 8) + (0x17 << 1) + (1), 0x1300 + 0x10
        .word	0, 0, 0

        .size __gnat_mpu_enable, . - __gnat_mpu_enable
