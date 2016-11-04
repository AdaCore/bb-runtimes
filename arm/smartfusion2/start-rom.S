## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                         Copyright (C) 2016 AdaCore                        #
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
#  File derived from Microsemi SmartFusion2 CMSIS HAL package:              #
#  CMSIS/startup_gcc/startup_m2sxxx.S                                       #
#  Copyright 2012-2015 Microsemi SoC Products Group.                        #
#                                                                           #
#############################################################################

	.syntax unified
	.cpu cortex-m3
	.thumb

	/* This is the initial vector table and also the one that is used by
	   the zfp runtime. Ravenscar runtimes install its own table (using
	   VTOR register) defined in handler.S.  */
	.section .vectors,"a"
__vectors0:
	.word   __stack_end /* stack top address */
	.word   _start      /* 1 Reset */
	.word   hang        /* 2 NMI */
	.word   hang        /* 3 HardFault */
	.word   hang        /* 4 MemManage */

	.text

	.thumb_func
        .globl _start
        /* .section .boot_code,"ax",%progbits */
        .type  _start, %function
_start:
        /* Check the Error Detection and Correction Controllers status.
           Described in chapter 25 of the SF2 Microcontroller subsystem user
           guide.
           Also in chapter 1 of the SF2 and IGLOO2 High Speed DDR Interfaces
           User Guide (ug0466_v5). */
        /* Result is placed in the r11 register:
           r11[0] if set, eSRAM EDAC enabled
           r11[1] is set, MDDR ECC/SECDED enabled */
        mov r11, #0             /* init to 0 */
        /* Check the MDRR mode: */
        /* DDRC_MODE_CR:
           [1:0]: REG_DDRC_DATA_BUS_WIDTH
           [4:2]: REG_DDRC_MODE
                  - b000: NO SECDED
                  - b101: SECDED enabled
           [5]:   REG_DDRC_TEST_MODE
           [6]:   REG_DDRC_SDRAM
           [7]:   REG_DDRC_MOBILE
           [8]:   REG_DDRC_DDR3
           */
        ldr r0, SF2_DDRC_MODE_CR
        ldr r0, [r0]
        and r0, r0, #0x1C       /* get the REG_DDRC_MODE field */
        cmp r0, #0x14
        bne check_esram_edac
        mov r11, #2             /* M/DDR SECDED enabled */
check_esram_edac:
        /* SF2_EDAC_CR: bit 0 ESRAM0_EDAC_EN, bit 1 ESRAM1_EDAC_EN */
        ldr r0, SF2_EDAC_CR
        ldr r0, [r0]
        and r0, r0, #3
        cmp r0, 0
        beq check_stack_init
        orr r11, r11, #1        /* eSRAM EDAC enabled */
check_stack_init:
        cmp r11, #0             /* if no error crc, then jump to system_init */
        beq system_init
clear_stack:
        /* If error detection is active, we need to initialize the stack RAM content
           to initialize the error detection and correction. */
        ldr r0, =__stack_start
        ldr r1, =__stack_end
        ldr r2, RAM_INIT_PATTERN
        bl fill_memory
        ldr r0, =__interrupt_stack_start
        ldr r1, =__interrupt_stack_end
        ldr r2, RAM_INIT_PATTERN
        bl fill_memory

system_init:
        /* Setup the clocks */
	bl  _ada_setup_pll
        /* Modify MDRR configuration if ECC/SECDED is enabled for MDDR */
        and r10, r11, #0x2
        cmp r10, #0
        beq remap_memory        /* SECDED is disabled */
        /* DDRB_NB_SIZE_CR: [3:0] Size of non-bufferable address region. */
        ldr r0, SF2_DDRB_NB_SIZE_CR
        /* DDRB_CR: bits [0:7] Allows R/W of various masters in DDR bridge
                    bits [8-23]: Address space mapping for the various bus */
        ldr r1, SF2_DDRB_CR
        ldr r2, [r0]
        ldr r3, [r1]
        push {r0, r1, r2, r3}
        mov r2, #0
        mov r3, #0xFF           /* Enable Read/Write, address spaces set to 0 */
        str r2, [r0]
        str r3, [r1]

remap_memory:
        ldr r0, =__sf2_memory_remap
        ldr r2, =0
        ldr r3, =1
        cmp r0, #2
        bne check_esram_remap

        /* __sf2_memory_remap == 2 case: remap external DDR RAM to 0x0 */
        ldr r1, SF2_ESRAM_CR
        str r2, [r1]
        ldr r1, SF2_ENVM_REMAP_CR
        str r2, [r1]
        ldr r1, SF2_DDR_CR
        str r3, [r1]
check_esram_remap:
        cmp r0, #1
        bne copy_data
        ldr r1, SF2_ESRAM_CR
        str r3, [r1]
        ldr r1, SF2_ENVM_REMAP_CR
        str r2, [r1]
        ldr r1, SF2_DDR_CR
        str r2, [r1]
copy_data:
	/* Copy .data */
        ldr r0, =__data_load
        ldr r1, =__data_start
        ldr r2, =__data_end
        bl  block_copy

clear_bss:
        ldr r0, =__bss_start
        ldr r1, =__bss_end
        ldr r2, RAM_INIT_PATTERN
        bl  fill_memory
clear_heap:
        cmp r11, #0
        beq call_main
        ldr r2, HEAP_INIT_PATTERN
        bl  fill_memory

restore_mddr_configuration:
        and r10, r11, 0x2
        cmp r10, #0
        beq call_main
        pop {r0, r1, r2, r3}
        str r2, [r0]
        str r3, [r1]

call_main:
	bl	main
	bl	_exit

/* block_copy:
 * parameters:
 *      r0: source address
 *      r1: target address
 *      r2: target end address
 */
block_copy:
        push   {r3, r4, r5, r6, r7, r8, lr}
        cmp    r0, r1
        beq    block_copy_exit  /* target = source, exit */
        subs.w r2, r2, r1       /* number of bytes to move */
        bpl    block_copy_address_ok /* check the address parameters */
        b .                          /* halt as critical error */
block_copy_address_ok:
        orr.w  r3, r0, r1
        ands.w r3, r3, #3       /* check that both addresses are word-aligned */
        beq    block_copy_continue
block_copy_byte_copy:
        bl     block_copy_byte
        b      block_copy_exit
block_copy_continue:
        mov    r3, #0
        mov    r8, r2           /* Save a copy of the byte count */
        asrs   r2, r2, #4       /* Div by 16: we're moving 4 words */
        beq    block_copy_byte_copy /* need to use byte copy if less than 16 bytes */
block_copy_loop:
        cmp    r2, r3
        itt    ne
        ldmne  r0!, {r4, r5, r6, r7}
        stmne  r1!, {r4, r5, r6, r7}
        add.w  r3, r3, #1       /* use Thumb2- make sure condition code reg not updated */
        bne    block_copy_loop
block_copy_spare_bytes:
        and    r8, #15
        cmp    r8, #0
        beq    block_copy_exit
block_copy_spare_bytes_loop:
        ldrb   r4, [r0]
        strb   r4, [r1]
        add    r0, #1
        add    r1, #1
        subs   r8, r8, #1
        bne    block_copy_spare_bytes_loop
block_copy_exit:
        pop    {r3, r4, r5, r6, r7, r8, pc}


/* block_copy_byte: used for non-aligned memory
 * parameters:
 *      r0: source address
 *      r1: target address
 *      r2: target end address
 */
block_copy_byte:
        push {r3, lr}
        mov  r3, #0
block_copy_byte_loop:
        ldrb r3, [r0]
        strb r3, [r1]
        add  r0, #1
        add  r1, #1
        subs r2, r2, #1
        bne  block_copy_byte_loop
        pop  {r3, pc}

/* fill_memory:
 * parameters:
 *      r0 (in out): start address
 *      r1 (in out) : end address
 *      r2 (in out) : pattern
 *
 * can be called before the stack is initialized, so should not attempt to use it.
 * as a result, this clobbers the following registers:
 *
 * clobbers: r0-r2, r4-r9, lr
 */
fill_memory:
        cmp   r0, r1
        beq   fill_memory_exit    /* end address = start address, exit */
        and.w r6, r0, #3        /* non-aligned? */
        cmp   r6, #0
        beq   fill_memory_end_start /* no spare bytes at start */
        mov   r5, #4
        sub.w r4, r5, r6      /* r4 is set to the number of non-aligned bytes */
        mov   r7, #8
        mul   r8, r7, r6      /* shifts required for pattern for non-aligned bytes */
        mov   r9, r2            /* copy the pattern to r9 */
        ror   r9, r9, r8        /* Rotate right to keep pattern consistent */
fill_memory_spare_bytes_start:  /* r0 is the source address, r1 the destination address */
        cmp   r4, #0
        beq   fill_memory_end_start
        strb  r9, [r0]          /* Fill byte */
        ror.w r9, r9, r7        /* Rotate right by one byte */
        add   r0, r0, #1        /* add 1 to the address */
        subs  r4, r4, #1        /* byte count -= 1 */
        b fill_memory_spare_bytes_start
fill_memory_end_start:
        mov   r6, #0
        mov   r7, r1            /* save end address */
        subs  r1, r1, r0        /* number of bytes to fill */
        asrs  r1, r1, #4        /* divide by 16 to get the number of chucks to move */
        mov   r9, r2            /* copy pattern */
        mov   r4, r2
        mov   r5, r2
        cmp   r1, r6            /* compare to see if all chunks are copied */
        beq   fill_memory_spare_bytes_end
fill_memory_loop:
        it    ne
        stmne r0!, {r2, r4, r5, r9}  /* copy pattern, r0 must be word-aligned */
        add.w r6, r6, #1             /* use Thumb2- make sure condition code reg. not updated */
        cmp   r1, r6            /* compare to see if all chunks copied */
        bne   fill_memory_loop
fill_memory_spare_bytes_end:    /* copy spare bytes at the end if any */
        and.w  r8, r8, #15       /* get spare bytes */
fill_memory_spare_end_loop:
        cmp r8, #0               /* no spare bytes at end- end now */
        beq fill_memory_exit
        strb r2, [r0]
        ror.w  r2, r2, #8        /* Rotate right by 1 byte for the next time */
        add r0, r0, #1           /* add one to address */
        subs r8, r8, #1          /* subtract one from byte count 1 */
        b fill_memory_spare_end_loop
fill_memory_exit:
        bx lr     /* jump to the caller, no stack in use as it may not be available yet. */

hang:   b .

RAM_INIT_PATTERN:       .word   0x00000000
HEAP_INIT_PATTERN:      .word   0xA2A2A2A2

SF2_ESRAM_CR:           .word   0x40038000
SF2_DDR_CR:             .word   0x40038008
SF2_ENVM_REMAP_CR:      .word   0x40038010
SF2_DDRB_NB_SIZE_CR:    .word   0x40038030
SF2_DDRB_CR:            .word   0x40038034
SF2_EDAC_CR:            .word   0x40038038
SF2_DDRC_MODE_CR:       .word   0x40020818
