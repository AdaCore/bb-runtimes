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

# Start file for x86_64. It contains the following entry points to support
# various bootloaders:
#
#   __qemu_entry
#   __start16
#   __start32
#   __start64

        .section .page_table,"a",@nobits
        .align 4096
        # Allocate space for the page tables. The start code will populate the
        # table as generating a static table for 2,048 entries takes a lot of
        # space.

        PAGE_TABLE_SIZE = 4096
        NUM_PML4_ENTRIES = 1
        NUM_PDP_ENTRIES = 4
        NUM_PDE_ENTRIES = 512
        TOTAL_PAGE_TABLES = 1 + NUM_PML4_ENTRIES + NUM_PDP_ENTRIES
        TOTAL_PAGE_TABLES_SIZE = PAGE_TABLE_SIZE * TOTAL_PAGE_TABLES
page_tables:
        .fill TOTAL_PAGE_TABLES_SIZE, 1, 0

        .section .gdt,"ad"

        # Global Descriptor Table
        # This table describes the memory segments. We keep things simple here
        # to begin with, using a code and data segments.

        # The table entries are pretty messy:

        # BITS   ITEM             DESCRIPTION
        # --------------------------------------------------------------
        # 0-15   Limit 0:15       First 16 bits in the segment limiter
        # 16-31  Base 0:15        First 16 bits in the base address
        # 32-39  Base 16:23       Bits 16-23 in the base address
        # 40-47  Access byte      Segment type and attributes
        #   40    Access          CPU writes 1 when segment is accessed.
        #   41    Read/Write      Code: 1 if can read
        #                         Data: 1 if can write
        #   42    Direction       Data: 0 Segment grows up, 1 down
        #         Conforming      Code: 1 read by lower privilege
        #   43    Executable      Whether the segment can execute
        #   44    Descriptor type 1 for code or data, 0 for everything else
        #  45-46  Privilege Level 0 = Highest privilege (OS),
        #                         3 = Lowest privilege (User applications)
        #   47    Present flag    Set to 1 if segment is present
        # 48-51  Limit 16:19      Bits 16-19 in the segment limiter
        # 52-55  Flags            Depends on segment type
        #   52    Reserved
        #   53    Long mode       Indicates long mode code description
        #   54    Size bit        0 -> 16bit/1 -> 32bit protected mode
        #   55    Granularity     Determine the size of the segment with the
        #                         limiter (0-> byte, 1-> 4KiB)
        # 56-63  Base 24:31       The last 24-31 bits in the base address

        # For now we set both the code and data to map the full 4GB memory
        # range. For 64 bit code:
        #
        #    Base:  0x0
        #    Limit: 0xFFFFF
        #    Acc:   Pres: 1; Priv: 00; Desc: 1; Exc: 1; Conf:0; RW: 1, Acc: 0
        #           => 2#10011010# => 16#9A#
        #    Flags: Gran: 1; Size: 1; Long: 1 => 2#1110# => 16#E#
        #
        #   Table entry: 0xFFFF, 0x0000, 0x9A00, 0x00EF
        #
        # For 64-bit data:
        #
        #    Base:  0x0
        #    Limit: 0xFFFFF
        #    Acc:   Pres: 1; Priv: 00; Desc: 1; Exc: 0; Conf:0; RW: 1, Acc: 0
        #           => 2#10010010# => 16#92#
        #    Flags: Gran: 1; Size: 0; Long: 0 => 2#1000# => 16#8#
        #
        #   Table entry: 0xFFFF, 0x0000, 0x9200, 0x008F
        #
        # For 32 bit code: 0xFFFF, 0x0000, 0x9A00, 0x00CF
        # For 32 bit data: 0xFFFF, 0x0000, 0x9200, 0x00CF
        #
        # If we want to get fancy, the code and data segments should
        # be mapped to the ELF sections.

        .align 8
gdt:
           # BIT 0-15,   16-31, 32-47,  48-63
           .word 0,      0,     0,      0       # 0x00: null descriptor
gdtcode32: .word 0xFFFF, 0,     0x9A00, 0x00CF  # 0x08: 32-bit code
gdtdata32: .word 0xFFFF, 0,     0x9200, 0x00CF  # 0x10: 32-bit data
gdtcode64: .word 0xFFFF, 0,     0x9A00, 0x00AF  # 0x18: 64-bit code
gdtdata64: .word 0xFFFF, 0,     0x9200, 0x00CF  # 0x20: 64-bit data
gdt_pointer:
        .word (gdt_pointer - gdt - 1)
        .long gdt

        CODE32_SEG = gdtcode32 - gdt
        DATA32_SEG = gdtdata32 - gdt
        CODE64_SEG = gdtcode64 - gdt
        DATA64_SEG = gdtdata64 - gdt

        .section .data.argv
argv_str:
        .ascii  "main\0"
        .align 8
argv:
        .quad    argv_str
        .quad    0

        .section .qemu_boot,"ax"
        .global __processor_entry
        .code16
        # A QEMU specific entry since we do not load the BIOS on QEMU. This is
        # done so that we do not have to muck around with BIOS, doing things
        # like creating an image for the BIOS to boot, dealing with the BIOS
        # splash screen, etc. The entry point for a BIOS-less system is simple:
        # a long jump to the program's main entry point. The linker will load
        # this to the top 16 bytes of the real mode's address space.
__processor_entry:
        ljmp    $0, $__start16

        .section .boottext16,"ax"
        .global __start16
        .code16
        # Processor has started in real mode. Switch to 32-bit protected mode
        # with interrupts disabled. Application processors (cores other than
        # the boot core) start here.
__start16:
        cli
        lgdt    gdt_pointer

        # Switch to protected mode, enable cache and cache write-through, and
        # enable the FPU (this enables us to cheat here and just write 1 to
        # CR0). The switch to protected mode is followed by an intrasegment
        # jump to flush the the instruction queue.
        mov     $1, %ax
        movl    %eax, %cr0
        ljmp    $CODE32_SEG, $setup_32mode
        .code32
setup_32mode:
        movw    $DATA32_SEG, %ax
        movw    %ax, %ds
        movw    %ax, %es
        movw    %ax, %ss
        movw    %ax, %fs

        # Jump to our 32-bit entry point. We do this explicitly so the linker
        # doesn't decide to garbage collect __start32 because no-one points to
        # it, and to make sure we don't encounter the situtation where
        # __start32 isn't placed right after this routine.
        jmp     __start32

        .section .boottext32,"ax"
        .global __start32
        .code32
        # Entry when in 32-bit protected mode
__start32:
        .code32
        # Entry when in 32-bit protected mode
        # Load the Global Descriptor Table to configure segment registers with
        # flat-mode descriptors. Also load the IDT with interrupts disabled.

        lgdt    gdt_pointer

        # At some point %ebp should hold a pointer to CPU specific data

        # Disable paging in case the bootloader had enabled it

        CR0_PG_BIT = 31
        movl    %cr0, %eax
        btrl    $CR0_PG_BIT, %eax
        movl    %eax, %cr0

        # Prepare switch to long mode (64-bits) by enabling Physical Address
        # Extension (PAE) and SSE, setting up the page tables and per CPU
        # registers.

        # enable PAE and SSE
        CR4_PAE    =  1 << 5     # CR4.PAE
        CR4_OSFXSR =  1 << 9     # CR4.OSFXSR

        movl    %cr4, %eax
        orl     $(CR4_PAE | CR4_OSFXSR), %eax
        movl    %eax, %cr4
        clts

        # Populate and install the page table. We configure it to identity
        # map the first 4G of RAM using 2M page sizes since not all CPUs
        # support 1G page sizes.

        # Initialize page tables
        leal    page_tables, %edi
        xorl    %eax, %eax
        movl    $TOTAL_PAGE_TABLES_SIZE, %ecx
        rep     stosb

        # Generate PML4 Table
        leal    page_tables, %edi
        leal    0x1003 (%edi), %eax    # Addr to PDP entry, RW, Present
        movl    %eax, 0(%edi)

        # Generate Page Directory Pointer Table
        leal    page_tables + 0x1000, %edi
        leal    0x1007(%edi), %eax     # Addr to PDE entry, RW, Present
        movl    $NUM_PDP_ENTRIES, %ecx # Iterate
1:      movl    %eax, 0x00(%edi)
        addl    $0x00001000, %eax
        addl    $8, %edi
        decl    %ecx
        jnz     1b

        # Generate Page Directory Entry Table
        leal    page_tables + 0x2000, %edi
        movl    $0x00000183, %eax    # G, PS, RW, Present
        movl    $2048, %ecx
1:      movl    %eax, 0(%edi)
        addl    $0x00200000, %eax
        addl    $8, %edi
        decl    %ecx
        jnz     1b

        # Update CR3 with pointer to the new page tables
        leal    page_tables, %eax
        movl    %eax, %cr3

        # enable long mode, no-execute
        EFER_MSR     = 0xC0000080    # Extended feature register
        EFER_MSR_SCE = 0 << 0        # Enable SYSCALL instruction
        EFER_MSR_LME = 1 << 8        # Switch to long mode
        EFER_MSR_NXE = 0 << 11       # Enable NXE

        movl    $EFER_MSR, %ecx
        rdmsr
        orl     $(EFER_MSR_SCE | EFER_MSR_LME | EFER_MSR_NXE), %eax
        wrmsr

        # Enable paging
        CR0_WP = 1 << 16   # Supervisor cannot write to read-only pages
        CR0_PG = 1 << 31   # Enable paging

        movl    %cr0, %eax
        orl     $(CR0_PG | CR0_WP), %eax
        movl    %eax, %cr0

        # switch to long mode and jump to our common 64-bit entry
        jmpl    $CODE64_SEG,$__start64

        .section .boottext64,"ax"
        .global __start64
        .code64
        # Prepare the processor for jumping into our high-level code.
__start64:
        movl    $DATA64_SEG, %eax
        movw    %ax, %ds
        movw    %ax, %es
        movw    %ax, %ss
        movw    %ax, %fs

        # Clear bss section. We zero "bytes" because the underlying hardware
        # (from Ivy Bridge onwards) will perform the operation internally using
        # wider types.
        cld
        xorq    %rax, %rax
        movq    $__bss_start, %rdi
        movq    $__bss_size, %rcx
        rep stosb

        # load stack pointer
        movq    $__stack_end, %rsp

        # GCC expects the direction flag to be cleared
        cld

        # Call main
        call    main

        # Stop the machine
        call    _exit
