## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                        Copyright (C) 2026 AdaCore                         #
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

# ELF note for booting via the x86/HVM direct boot ABI, also known as PVH.
# This is the mechanism used by QEMU's -kernel option (and by Xen) to load
# an ELF image directly, without a firmware or bootloader.
#
# The loader locates the entry point by reading a PT_NOTE program header
# containing a note in the "Xen" namespace with type XEN_ELFNOTE_PHYS32_ENTRY.
# The note descriptor holds the physical address of the 32-bit entry point.
#
# On entry the CPU is in 32-bit protected mode with paging disabled and flat
# code and data segments, with interrupts disabled. This is the same state the
# multiboot loaders enter in, so we reuse the common __start32 entry point.

        # See xen/include/public/elfnote.h
        XEN_ELFNOTE_PHYS32_ENTRY = 18

        .section .note.Xen, "a", @note
        .balign 4
        .global __pvh_note
__pvh_note:
        .long   2f - 1f                   # n_namesz
        .long   4f - 3f                   # n_descsz
        .long   XEN_ELFNOTE_PHYS32_ENTRY  # n_type
1:      .asciz  "Xen"                     # n_name
2:      .align  4
3:      .long   __start32                 # n_desc: 32-bit entry point
4:      .align  4
