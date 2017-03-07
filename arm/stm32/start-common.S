## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 C A C H E                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2016 AdaCore                      #
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
	.cpu cortex-m4
	.thumb

	.text
	.thumb_func
	.globl	_stm32_start_common
        .type _stm32_start_common, #function
_stm32_start_common:

        /************************
        * Enable debug Monitor *
        ************************/

        /* Debug register definition */
        .set    DEMCR,  0xE000EDFC
          
        /* Enable the debug monitor so that bkpt instructions are not treated
        * as hard fault when no debugger is attached */
        ldr     r0, =DEMCR
        ldr     r1, [r0]
        orr     r1, r1, #0x10000 /* Set MON_EN in DEMCR */
        str     r1, [r0]

        /**************/
        /* Enable FPU */
        /**************/

        movw     r0,#0xED88
        movt     r0,#0xE000
        ldr      r1,[r0]
        orr      r1,r1,#(0xF << 20)
        str      r1,[r0]

        dsb
        isb

        /*************************
         * Check MCU = Cortex-M7 *
        *************************/

        .set    MCU_ID_REG, 0xE0042000
        ldr     r0, =MCU_ID_REG
        ldr     r1, [r0]
        ldr     r2, =#0xFFF
        and     r1, r1, r2  /* Retrieve the DEV_ID field of the MCU_ID reg */
        ldr     r2, =#0x449
        cmp     r1, r2      /* Compare to STM32F74xxx/75xxx Dev ID */
        beq      init_cache
        ldr     r2, =#0x451
        cmp     r1, r2      /* Compare to STM32F76xxx/77xxx Dev ID */
        bne     end         /* Do not initialize cache on STM32F4 */

init_cache:
        /********************
         * Enable I/D cache *
        ********************/

        /* Register definition for cache handling */
        .set    CCSIDR,  0xE000ED80
        .set    CSSELR,  0xE000ED84
        .set    DCISW,   0xE000EF60
        .set    ICIALLU, 0xE000EF50
        .set    CCR,     0xE000ED14

        /* First invalidate the data cache */
        mov     r0, #0x0
        ldr     r11, =CSSELR
        str     r0, [r11]       /* Select the data cache size */
        dsb

        ldr     r11, =CCSIDR
        ldr     r2, [r11]       /* Cache size identification */
        and     r1, r2, #0x7    /* Number of words in a cache line */
        add     r7, r1, #0x4

        ubfx    r4, r2, #3, #10  /* r4 = number of ways - 1 of data cache */
        ubfx    r2, r2, #13, #15 /* r2 = number of sets - 1 of data cache */
        clz     r6, r4           /* Calculate bit offset for "way" in DCISW */

        ldr     r11, =DCISW

inv_loop1:                       /* For each set */
        mov     r1, r4
        lsls    r8, r2, r7

inv_loop2:                       /* For each way */
        lsls    r3, r1, r6
        orrs    r3, r3, r8

        str     r3, [r11]        /* Invalidate the D-Cache line */
        subs    r1, r1, #1
        bge     inv_loop2
        subs    r2, r2, #1
        bge     inv_loop1

        dsb

        /* Now invalidate the instruction cache */
        mov     r0, #0x0
        ldr     r11, =ICIALLU
        str     r0, [r11]

        dsb
        isb

        /* Finally enable Instruction and Data cache */
        ldr     r11, =CCR
        ldr     r0, [r11]
        orr     r0, r0, #(0x30000) /* Sets the I and D cache enabled fields */
        str     r0, [r11]

        dsb
        isb

        /* Wait for store to complete and reset pipeline with FPU enabled  */
        dsb
        isb

        /*****************************
         * TCM Memory initialisation *
        *****************************/
tcm_init:
        .set    CM7_ITCMCR, 0xE000EF90
        .set    CM7_DTCMCR, 0xE000EF94

        ldr     r0, =CM7_ITCMCR
        ldr     r1, [r0]
        orr     r1, r1, #0x1 /* set the EN field */
        str     r1, [r0]

        ldr     r0, =CM7_DTCMCR
        ldr     r1, [r0]
        orr     r1, r1, #0x1 /* set the EN field */
        str     r1, [r0]

        dsb
        isb

end:
        bx lr
        .size _stm32_start_common, . - _stm32_start_common
