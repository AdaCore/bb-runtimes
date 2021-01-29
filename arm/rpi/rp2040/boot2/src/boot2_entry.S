############################################################################
#                                                                          #
#                         GNAT RUN-TIME COMPONENTS                         #
#                                                                          #
#                      Copyright (C) 2021, AdaCore                         #
#                                                                          #
# GNAT is free software;  you can  redistribute it  and/or modify it under #
# terms of the  GNU General Public License as published  by the Free Soft- #
# ware  Foundation;  either version 3,  or (at your option) any later ver- #
# sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
# OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
# or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                          #
# As a special exception under Section 7 of GPL version 3, you are granted #
# additional permissions described in the GCC Runtime Library Exception,   #
# version 3.1, as published by the Free Software Foundation.               #
#                                                                          #
# You should have received a copy of the GNU General Public License and    #
# a copy of the GCC Runtime Library Exception along with this program;     #
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
# <http://www.gnu.org/licenses/>.                                          #
#                                                                          #
# GNAT was originally developed  by the GNAT team at  New York University. #
# Extensive contributions were provided by Ada Core Technologies Inc.      #
#                                                                          #
############################################################################

.syntax unified
.cpu cortex-m0
.thumb

/* This is the entry point to the stage 2 bootloader.
 * This is a wrapper around the Ada code to correctly
 * handle the LR register and vectoring into the
 * flash vector table. */
.section .text
.global _boot2_entry
_boot2_entry:

    /* Preserve LR during call to the Ada code for comparison later */
    push {lr}
    bl _ada_boot2
    pop {r1}

    /* LR is 0 if boot2 is being called by the bootrom.
     * Otherwise, it is a regular function call. */
    cmp r1, #0
    bne return

    /* Load the vector table address into the Cortex-M0+ VTOR register */
    ldr r0, =0x10000100 /* vector table is located after boot2 in XIP */
    ldr r1, =0xe000ed08 /* M0PLUS_VTOR register */
    str r0, [r1]

    /* Load the stack pointer and reset vector from the vector table */
    ldmia r0, {r0, r1}
    msr msp, r0

return:
    bx r1
