## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                        Copyright (C) 2019 AdaCore                         #
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

/* This is an -*- asm -*- file.  */
        .section .init,"ax"
        # Reset vector
        .long   __stack_end         | Initial Interrupt Stack Pointer
        .long   _start_ram          | Initial Program Counter

        .section ".text"
        .global _start_ram

_start_ram:
        # load stack pointer from initial stack pointer
        movea.l #__stack_end, %sp

        # Clear .bss
        movea.l #__bss_start, %a0
        move.l  #__bss_words, %d0   | move sets the condition codes
        beq.l   1f
0:      clr.l  (%a0)+
        subq.l  #4, %d0
        bpl.l   0b
1:
        bsr.l   main
        bsr.l   _exit

        .size _start_ram, . - _start_ram
