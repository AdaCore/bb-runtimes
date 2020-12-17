## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2020 AdaCore                      #
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

# This start routine is designed to be called from a bootloader that has
# loaded this program into RAM. If the program's main terminates this routine
# will return control back to the bootloader via the blr instruction.
# Consequently, ensure the start routine is called from the bootloader in a way
# that ensures the link and stack registers contain the appropriate return
# information.

        .section ".text"
        .global _start_ram

_start_ram:
        # Load the program stack pointer
        lis     %r11,(__stack_end - 16)@h
        ori     %r11,%r11,(__stack_end - 16)@l
        # Back-link to the caller stack
        stw     %r1,0(%r11)
        # Set the stack pointer to the program stack
        mr      %r1,%r11

        # Call the assembly setup routine and branch to the program's main. If
        # the main returns we return to the bootloader via the address in lr.
        mflr    %r0
        stw     %r0,12(%r1)
        bl      _setup
        bl      main
        lwz     %r0,12(%r1)
        lwz     %r1,0(%r1)
        mtlr    %r0
        blr
        .size _start_ram, . - _start_ram
