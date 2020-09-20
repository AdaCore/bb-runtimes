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

/* Start file for x86_64 */

        .section .bootrodata,"a"
        .align 4096
        .global flat_page_tables

        # Page tables
        # Long mode requires page tables but our runtime doesn't need them
        # so we map the first 4GB with 1GB pages. If you need more memory
        # expand the table.

flat_page_table:
        .long pdp + 0x03                /* 0x03 = R/W, P */
        .long 0
        .fill 4088, 1, 0

pdp:    .long 0x00000083                /* 0x83 = 1GB, R/W, P */
        .long 0
        .long 0x40000083
        .long 0
        .long 0x80000083
        .long 0
        .long 0xC0000083
        .long 0
        .fill 4064, 1, 0

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
gdtcode64: .word 0xFFFF, 0,     0x9A00, 0x00EF  # 0x18: 64-bit code
gdtdata64: .word 0xFFFF, 0,     0x9200, 0x008F  # 0x20: 64-bit data
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

        .section .bios_replacement,"ax"
        .global _qemu_entry
        .code16
_qemu_entry:
        # Seperate entry for QEMU since we have QEMU load this section over the
        # bios so we do not have to much around with having to provide a boot
        # image for the BIOS to boot. See _start16 for a description of this
        # code.
        cli
        lgdt    gdt_pointer
        mov     $1, %ax
        movl    %eax, %cr0
        ljmp    $CODE32_SEG, $_start32

        .section .boottext,"ax"
        .global _start16
        .code16
_start16:
        # Processor has started in real mode. Switch to 32-bit protected mode
        # with interrupts disabled. Application processors (cores other than
        # the boot core) start here.
        cli
        lgdt    gdt_pointer

        # Switch to protected mode, enable cache and cache write through, and
        # enable the FPU (this enables us to cheat here and just write 1 to
        # CR0). The switch to protected mode is followed by an intrasegment
        # jump to flush the the instruction queue.
        mov     $1, %ax
        movl    %eax, %cr0
        ljmp    $CODE32_SEG, $_start32

        .section .boottext,"ax"
        .global _start32
        .code32
_start32:
        movw    $DATA32_SEG, %ax
        movw    %ax, %ds
        movw    %ax, %es
        movw    %ax, %ss
        movw    %ax, %fs

        # Load the Global Descriptor Table to configure segment registers with
        # flat-mode descriptors. Also load the IDT with interrupts disabled.

        lgdt    gdt_pointer
        # lidt idt48

        # At some point %ebp should hold a pointer to CPU specific data

switch_to_long_mode:
        # Prepare switch to long mode (64-bits) by enabling Physical Address
        # Extension (PAE) and SSE, setting up the page tables and per CPU
        # registers.

        # enable PAE and SSE
        .set CR4_PAE,      1 << 5     # CR4.PAE
        .set CR4_OSFXSR,   1 << 9     # CR4.OSFXSR

        movl    %cr4, %eax
        orl     $(CR4_PAE | CR4_OSFXSR), %eax
        movl    %eax, %cr4
        clts

        # setup page table
        movl    $flat_page_table, %eax
        movl    %eax, %cr3

        # enable long mode, no-execute
        .set X86_EFER_MSR, 0xC0000080    # Extended feature register
        .set X86_EFER_MSR_SCE,    1 << 0  # Enable SYSCALL instruction
        .set X86_EFER_MSR_LME,    1 << 8  # Switch to long mode
        .set X86_EFER_MSR_NXE,    1 << 11 # Enable NXE

        movl    $X86_EFER_MSR, %ecx
        rdmsr
        orl     $(X86_EFER_MSR_LME | X86_EFER_MSR_NXE), %eax
        wrmsr

        # Enable paging
        .set CR0_WP,     1 << 16   # Supervisor cannot write to read-only pages
        .set CR0_PG,     1 << 31   # Enable paging

        movl     %cr0, %eax
        orl     $(CR0_PG | CR0_WP), %eax
        movl    %eax, %cr0

        # switch to long mode
        jmpl    $CODE64_SEG,$1f
        .code64
1:      movl    $DATA64_SEG, %eax
        movw    %ax, %ds
        movw    %ax, %es
        movw    %ax, %ss
        movw    %ax, %fs

        # clear bss section
        cld
        xorl    %eax, %eax
        movq    $__bss_start, %rdi
        movq    $__bss_dwords, %rcx
        rep stosq

        # load stack pointer
        movq    $__stack_end, %rsp

        # GCC expects the direction flag to be cleared
        cld

        # Call main
        call    main

        # Stop the machine
        call    _exit

        .size _start32, . - _start32
