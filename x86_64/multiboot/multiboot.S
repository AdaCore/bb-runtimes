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

/* multiboot 1 headers for booting from a multiboot 1 bootloader */

	/* Multiboot header constants */
	.set ALIGN,          1 << 0    # align modules on page boundaries
	.set MEMINFO,        0 << 1    # don't provide a memory map
	.set ADDRESS_FIELDS, 1 << 16   # use the address fields since QEMU
				       # won't load ELF64

	.set MULTIBOOT_FLAGS,     ALIGN | MEMINFO | ADDRESS_FIELDS
	.set MULTIBOOT_MAGIC,     0x1BADB002
	.set MULTIBOOT_EAX_MAGIC, 0x2BADB002
	.set MULTIBOOT_CHECKSUM,  -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

	# Multiboot header
	# Placed in its own section so that its in the first 8 KiB of the image

	.section .multiboot
	.global magic_address
	.align 4
magic_address:
	.long MULTIBOOT_MAGIC
	.long MULTIBOOT_FLAGS
	.long MULTIBOOT_CHECKSUM
	.long magic_address 		# header_addr
	.long __text_start		# load_addr
	.long __data_end                # load_end_addr
	.long __bss_end                 # bss_end_addr
	.long _start                    # entry_addr
