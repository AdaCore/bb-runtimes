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

# LynxSecure Application entry

        # Hardware type we are running on. Keep in sync with
        # System.BB.CPU_Specific.Hardware_TYPE
        LYNXSECURE = 2

        .section .bootlsa,"ax"
        .global __lynx_start
        .code32
        # Entry for a LynxSecure Application (LSA)
        #
        # On entry to __lynx_start, LynxSecure will have set the subject in
        # flat protected mode with segment registers initialised for the memory
        # space.
        #
        # The application image is incorporated into the LynxSecure system
        # image (SRP) and is loaded into the subject's memory on boot at an
        # address of the hypervisor's choosing. The LSA bootloader will
        # relocate the image to the correct load address as recorded in the
        # Multiboot header below, but will oddly jump to the start of orignal
        # image location. Consequently, until we jump into our common 32-bit
        # entry all memory references need to be done in a position independent
        # manner.
__lynx_start:
        # Disable interrupts in case the bootloader had enabled them
        cli

        # Record that we are running on LynxSecure and store the pointer to the
        # subject's RO Page. Only do this on bootstrap (EBP will be non-zero in
        # this case)
        test    %ebp, %ebp
        jz      1f

        movl    $__gnat_host_hardware, %eax
        movl    $LYNXSECURE, (%eax)

        movl    $__gnat_host_info, %edi
        movl    %ebp, (%edi)

        # Jump to our common 32-bit entry routine
1:      movl    $__start32, %eax
        jmp     *%eax


        # LynxSecure requires a Multiboot 1 header with the address fields
        # populated.

        # Multiboot header constants
        ALIGN          = 1 << 0    # align modules on page boundaries
        MEMINFO        = 0 << 1    # don't provide a memory map
        ADDRESS_FIELDS = 1 << 16   # use the address fields

        MULTIBOOT_FLAGS     = ALIGN | MEMINFO | ADDRESS_FIELDS
        MULTIBOOT_MAGIC     = 0x1BADB002
        MULTIBOOT_EAX_MAGIC = 0x2BADB002
        MULTIBOOT_CHECKSUM  = -(MULTIBOOT_MAGIC + MULTIBOOT_FLAGS)

        MULTIBOOT_HEADER_ADDRESS = __magic_address
        MULTIBOOT_LOAD_ADDR      = __text_start
        MULTIBOOT_LOAD_END_ADDR  = 0
        MULTIBOOT_BSS_END_ADDR   = __bss_end
        MULTIBOOT_ENTRY_ADDR     = __lynx_start

        .section .multiboot
        .global __magic_address
        .align 4
__magic_address:
        .long MULTIBOOT_MAGIC
        .long MULTIBOOT_FLAGS
        .long MULTIBOOT_CHECKSUM
        .long MULTIBOOT_HEADER_ADDRESS
        .long MULTIBOOT_LOAD_ADDR
        .long MULTIBOOT_LOAD_END_ADDR
        .long MULTIBOOT_BSS_END_ADDR
        .long MULTIBOOT_ENTRY_ADDR
