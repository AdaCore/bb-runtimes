## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                        Copyright (C) 2021 AdaCore                         #
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

# Multiboot 2 headers for booting from a Multiboot 2 bootloader

        # Multiboot header constants

        MULTIBOOT_MAGIC     = 0xE85250D6
        MULTIBOOT_MAGIC_EAX = 0x36D76289
        ARCHITECTURE        = 0
        HEADER_LENGTH       = multiboot_end - __magic_address2
        HEADER_CHECKSUM     = -(MULTIBOOT_MAGIC + ARCHITECTURE + HEADER_LENGTH)

        # Multiboot header
        # Placed in its own section so that its in the first 8 KiB of the image

        .section .multiboot2
        .global __magic_address2
        .align 4
__magic_address2:
        .long MULTIBOOT_MAGIC
        .long ARCHITECTURE
        .long HEADER_LENGTH
        .long HEADER_CHECKSUM

        # End tag
        .word 0 # type: end
        .word 0 # flags
        .long 8 # size
multiboot_end:
