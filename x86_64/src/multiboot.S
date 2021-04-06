## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                        Copyright (C) 2020 AdaCore                         #
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

# Multiboot 1 headers for booting from a multiboot 1 bootloader

        # Multiboot header constants
        ALIGN          = 1 << 0    # align modules on page boundaries
        MEMINFO        = 0 << 1    # don't provide a memory map
        ADDRESS_FIELDS = 0 << 16   # don't use the address fields

        MULTIBOOT_FLAGS     = ALIGN | MEMINFO | ADDRESS_FIELDS
        MULTIBOOT_MAGIC     = 0x1BADB002
        MULTIBOOT_EAX_MAGIC = 0x2BADB002
        MULTIBOOT_CHECKSUM  = -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

        # Multiboot header
        # Placed in its own section so that its in the first 8 KiB of the image

        .section .multiboot
        .global __magic_address
        .align 4
__magic_address:
        .long MULTIBOOT_MAGIC
        .long MULTIBOOT_FLAGS
        .long MULTIBOOT_CHECKSUM
