/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2006 The European Space Agency            *
 *                      Copyright (C) 2003-2019 AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 ****************************************************************************/

/* This file is a LEON specific implementation */

/* This file contains the trap table and start up code for the multitasking
   executive implemented for LEON targets.
   We assume that all interrupts are masked in the interrupt controller, and
   that cache is enabled if necessary.
   We also assume slave processors will start to execute at the entry point.
 */

#include "sparc.h"

/* Entry for traps which jump to a programmer-specified trap handler */

#define SET_TRAP(Symbol, tt)            \
        mov     %psr, %l0;              \
        sethi   %hi(Symbol), %l4;       \
        jmpl    %l4 + %lo(Symbol), %g0; \
        mov     tt, %l3;

/* Entry for traps which do nothing */

#define DUMMY_TRAP                    \
        mov  %l2, %l1;                \
        add  %l2, 4, %l2;             \
        b    __gnat_return_from_trap; \
        nop;

/* Entry for traps that should not occur. They cause program termination */

#define BAD_TRAP        \
        ta      0;      \
        nop;            \
        nop;            \
        nop;

/* Entry for external interrupt traps. They call a common handler */

#define EXT_INT_TRAP(Vector)                    \
        mov     Vector, %l3;                    \
        sethi   %hi(common_handler), %l4;       \
        jmpl    %l4 + %lo(common_handler), %g0; \
        mov     %psr, %l0;

/* Entry for synchronous traps. They call a common handler and we add 0x100
   to the Vector number (passed via l3) to signal that this is a synchronous
   trap and the trap handler should not reexecute the instruction that
   produced the trap (to avoid trapping again and again). */

#define SYNCH_TRAP(Vector)                      \
        mov     Vector | (1 << 8), %l3;         \
        sethi   %hi(common_handler), %l4;       \
        jmpl    %l4 + %lo(common_handler), %g0; \
        mov     %psr, %l0;

/* This is the trap table */

        .section .traps,"ax"
        .p2align 12
        .global start, trap_table

        .type start,#function
        .type trap_table,#object
start:
trap_table:

/* Hardware Traps */

  /* SET_TRAP(hardreset, 0x00)  ! 00 reset trap */

  ! we must avoid the supervisor instruction
  mov %g0,%l0
  sethi %hi(hardreset),%l4
  jmp %l4+%lo(hardreset)
  mov 0,%l3               ! l3=vector 0

        .size start, . - start

  SYNCH_TRAP(0x01)                              ! 01 instr_access_exception
  SYNCH_TRAP(0x02)                              ! 02 illegal_instruction
  BAD_TRAP                                      ! 03 privileged_instruction

  /* This is the handler for deferred FP context switch */

  SET_TRAP(fp_context_switch, 0x04)             ! 04 fp_disabled

  sethi %hi(overflow), %l4                      ! 05 window_overflow
  jmpl  %l4 + %lo(overflow), %g0
  save
  nop

  rd    %wim, %l5                               ! 06 window_underflow
  sethi %hi(underflow), %l4
  jmpl  %l4 + %lo(underflow), %g0
  sll   %l5, 1, %l6

  SYNCH_TRAP(0x07)                              ! 07 memory_address_not_aligned
  SYNCH_TRAP(0x08)                              ! 08 fp_exception
  SYNCH_TRAP(0x09)                              ! 09 data_access_exception
  BAD_TRAP                                      ! 0A tag_overflow

  BAD_TRAP                                      ! 0B undefined
  BAD_TRAP                                      ! 0C undefined
  BAD_TRAP                                      ! 0D undefined
  BAD_TRAP                                      ! 0E undefined
  BAD_TRAP                                      ! 0F undefined
  BAD_TRAP                                      ! 10 undefined

/* External interrupt entries. These trap handlers are already redirected to
   the common trap handler, that  will in turn call the user-defined interrupt
      handler (if any). */

  EXT_INT_TRAP(0x11)                            ! 11 interrupt level 1
  EXT_INT_TRAP(0x12)                            ! 12 interrupt level 2
  EXT_INT_TRAP(0x13)                            ! 13 interrupt level 3
  EXT_INT_TRAP(0x14)                            ! 14 interrupt level 4
  EXT_INT_TRAP(0x15)                            ! 16 interrupt level 5
  EXT_INT_TRAP(0x16)                            ! 16 interrupt level 6
  EXT_INT_TRAP(0x17)                            ! 17 interrupt level 7
  EXT_INT_TRAP(0x18)                            ! 18 interrupt level 8
  EXT_INT_TRAP(0x19)                            ! 19 interrupt level 9
  EXT_INT_TRAP(0x1a)                            ! 1A interrupt level 10
  EXT_INT_TRAP(0x1b)                            ! 1B interrupt level 11
  EXT_INT_TRAP(0x1c)                            ! 1C interrupt level 12
  EXT_INT_TRAP(0x1d)                            ! 1D interrupt level 13
  EXT_INT_TRAP(0x1e)                            ! 1E interrupt level 14
  EXT_INT_TRAP(0x1f)                            ! 1F interrupt level 15

  BAD_TRAP                                      ! 20 undefined
  SYNCH_TRAP(0x21)                              ! 21 instruction access error
  BAD_TRAP                                      ! 22 undefined
  BAD_TRAP                                      ! 23 undefined
  BAD_TRAP;                                     ! 24 cp_disabled
            BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 25 - 27 undefined
  BAD_TRAP;                                     ! 28 cp_exception
  SYNCH_TRAP(0x29)                              ! 29 data access error
  SYNCH_TRAP(0x2a)                              ! 2A divide by zero
  SYNCH_TRAP(0x2b)                              ! 2B data store error
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 2C - 2F

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 30 - 33
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 34 - 37
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 38 - 3B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 3C - 3F

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 40 - 43
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 44 - 47
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 48 - 4B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 4C - 4F

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 50 - 53
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 54 - 57
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 58 - 5B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 5C - 5F

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 60 - 63
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 64 - 67
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 68 - 6B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 6C - 6F

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 70 - 73
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 74 - 77
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 78 - 7B

  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 7C - 7F

/* Software traps */

  BAD_TRAP                                      ! 80 system reset

#if defined (LEON2)
  /* For LEON2, the breakpoint interrupt can be redirected to the remote
     monitor located at the end of the RAM. */
  sethi %hi(__heap_end), %l4                    ! 81 breakpoint instruction
  or    %l4, %lo(__heap_end), %l4
  jmp   %l4 + (0x81 * 16)
  nop
#else
  BAD_TRAP
#endif

  SYNCH_TRAP(0x82)                              ! 82 division by zero
  SET_TRAP(flush_windows, 0x83)                 ! 83 flush windows
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP        ! 84 - 87
  SET_TRAP(mask_interrupt_level, 0x88)          ! 88 mask interrupt level
  BAD_TRAP; BAD_TRAP; BAD_TRAP;                 ! 89 - 8B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 8C - 8F
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 90 - 93
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 94 - 97
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 98 - 9B
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! 9C - 9F
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! A0 - A3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! A4 - A7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! A8 - AB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! AC - AF
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! B0 - B3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! B4 - B7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! B8 - BB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! BC - BF
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! C0 - C3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! C4 - C7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! C8 - CB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! CC - CF
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! D0 - D3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! D4 - D7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! D8 - DB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! DC - DF
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! E0 - E3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! E4 - E7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! E8 - EB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! EC - EF
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! F0 - F3
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! F4 - F7
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! F8 - FB
  BAD_TRAP; BAD_TRAP; BAD_TRAP; BAD_TRAP;       ! FC - FF

        .size trap_table, . - trap_table

        .text

/* Startup code */

hardreset:

        /* The first priviledged instruction must load TBR from %g1.
           Otherwise, the monitor will fail. */

        clr %g1
        sethi   %hi(trap_table), %g1    ! Initialize TBR
        wr      %g1, %g0, %tbr

        clr     %g1                     ! Wash general registers
        clr     %g2
        clr     %g3
        clr     %g4
        clr     %g5
        clr     %g6
        clr     %g7

        /* Initialize all window registers */

        mov     %g0, %wim
        nop
        nop
        nop
        mov     NUMBER_OF_REGISTER_WINDOWS, %g1
        save

.Lclear_window:
        clr     %i0
        clr     %i1
        clr     %i2
        clr     %i3
        clr     %i4
        clr     %i5
        clr     %i6
        clr     %i7
        clr     %l0
        clr     %l1
        clr     %l2
        clr     %l3
        clr     %l4
        clr     %l5
        clr     %l6
        clr     %l7
        subcc   %g1, 1, %g1
        bnz     .Lclear_window
        save

        /* Initialize PSR, WIM, TBR and Y registers */

        rd      %psr, %g1
        set     PSR_PIL_MASK, %g2
        or      %g1, %g2, %g1
        mov     %g1, %psr               ! Disable interrupts

        mov     %g0,%y

        set     PSR_EF_MASK | PSR_ET_MASK, %g2
        or      %g1, %g2, %g1
        mov     %g1, %psr               ! Enable traps and FP
        nop
        nop
        nop

        /* Set wim so that CWP + 1 is invalid */

        mov     1, %g2
        and     %g1, PSR_CWP_MASK, %g1          ! %g1 <- CWP
        add     %g1, 1, %g1                     ! Add 1 to CWP
        sll     %g2, %g1, %g2                   ! WIM that makes the window
                                                ! preceding CWP invalid.

        srl     %g2, NUMBER_OF_REGISTER_WINDOWS, %g1 ! do modulo
        wr      %g1, %g2, %wim

        /* Check for presence of FPU */
        rd      %psr, %g1
        set     PSR_EF_MASK, %g2
        and     %g1, %g2, %g1
        cmp     %g1, 0
        be      .Lskip_float
        set     double_zero, %g1                ! Use delay slot

.Linit_float:
        /* Initialize floating point unit registers, if FPU is present */


        ldd     [%g1], %f0
        ldd     [%g1], %f2
        ldd     [%g1], %f4
        ldd     [%g1], %f6
        ldd     [%g1], %f8
        ldd     [%g1], %f10
        ldd     [%g1], %f12
        ldd     [%g1], %f14
        ldd     [%g1], %f16
        ldd     [%g1], %f18
        ldd     [%g1], %f20
        ldd     [%g1], %f22
        ldd     [%g1], %f24
        ldd     [%g1], %f26
        ldd     [%g1], %f28
        ldd     [%g1], %f30
        ld      [%g1], %fsr

.Lskip_float:

#if defined (LEON3)
        /* Get CPU id and go to idle loop if current CPU is a slave.
         * Master CPU has id 0, others are slaves.
         */
        CURRENT_CPU(%g2)
        tst     %g2
        bnz     slave_cpu
        nop
#endif

        /* Set the stack for the main program (environment task) */

        sethi   %hi(__stack_end - 0x40), %sp
        or      %sp,%lo(__stack_end - 0x40),%sp  ! set the stack pointer
                                      ! We need to reserve at least 64 bytes
                                      ! for the window overflow routine.
                                      ! Otherwise, the overflow will overwrite
                                      ! the space after the stack (typically
                                      ! the heap).
                                      ! We do not really need to keep this
                                      ! register window (the application will
                                      ! exit upon return from main). We could
                                      ! also mark this current frame as the
                                      ! invalid one, but this could cause
                                      ! problems if there is a trap while
                                      ! executing in this current frame.
                                      ! The callee (main) will create its
                                      ! own frame, moving the stack pointer
                                      ! as much as needed.

        /* Call HW init hooks.  This is done before the first memory access,
           so that the hook can initialize the memory controlers.  */
        call    __gnat_hw_initialize
        clr     %fp                     ! Mark the deepest stack frame by
                                        ! setting the frame pointer to zero.

        /* Clear the bss section */

1:      sethi %hi(__bss_start),%g2
        or    %g2,%lo(__bss_start),%g2  ! start of bss
        sethi %hi(__bss_end),%g3
        or    %g3,%lo(__bss_end),%g3            ! end of bss
        mov   %g0,%g1                   ! so std has two zeros

.Lzerobss:
        std    %g0,[%g2]
        add    %g2,8,%g2
        cmp    %g2,%g3
        bleu,a .Lzerobss
        nop

        /* Call the main routine */

#ifdef CALL__init
        call    _init
        nop
#endif /* !CALL__init */
        call    main
        nop
        .size hardreset, . - hardreset

        .global _exit, _exit
_exit:
_exit:
        mov     1, %g1
        ta      0                       ! Halt if main would return ...
        nop

        .size _exit, . - _exit
        .size _exit, . - _exit

#if defined (LEON3)
/* Slave CPU */

slave_cpu:
        /* Set the slave stack, use the stack of the idle task. */
        set     __gnat_idle_stack_table, %l6
        sll     %g2, 2, %l5     /* offset in the table */
        ld      [%l6 + %l5], %l5
        sub     %l5, 0x40, %sp          ! Set the stack pointer, reserve 64
                                        ! bytes for the save area.
        clr     %fp                     ! Mark the deepest stack frame by
                                        ! setting the frame pointer to zero.

        call    __gnat_initialize_slave
         add    %g2, 1, %o0

        .size slave_cpu, . - slave_cpu
#endif

/* Window overflow trap routine */

        .type overflow,#function
overflow:
!       save    Instruction delayed in the entry. Decrement CWP


        std     %l0, [%sp + L0_OFFSET]  ! Save local registers on the stack
        NOP_FIX_GRLIB_TN_0009
        std     %l2, [%sp + L2_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %l4, [%sp + L4_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %l6, [%sp + L6_OFFSET]

        rd      %wim, %l0               ! Rotate WIM one bit to the right
        sll     %l0, NUMBER_OF_REGISTER_WINDOWS - 1, %l1
        srl     %l0, 1, %l0             ! Next three instructions following wr
        wr      %l0, %l1, %wim          ! must not be restore, save nor rett

        std     %i0, [%sp + I0_OFFSET]  ! Save the "in" registers on the stack
        NOP_FIX_GRLIB_TN_0009
        std     %i2, [%sp + I2_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %i4, [%sp + I4_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %i6, [%sp + I6_OFFSET]

        restore                         ! Increment CWP

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

        .size overflow, . - overflow

/* Window underflow trap routine */
        .type underflow,#function
underflow:
                                        ! Rotate WIM one bit to the left
!       rd      %wim, %l5               ! Instruction in the entry
!       sll     %l5, 1, %l6             ! Instruction delayed in the entry
        srl     %l5, NUMBER_OF_REGISTER_WINDOWS - 1, %l5
        wr      %l5, %l6, %wim
        nop
        nop
        nop

        restore                         ! CWP -> Window saved on the stack
        restore

        ldd     [%sp + I0_OFFSET], %i0  ! Get back saved registers
        ldd     [%sp + I2_OFFSET], %i2
        ldd     [%sp + I4_OFFSET], %i4
        ldd     [%sp + I6_OFFSET], %i6
        ldd     [%sp + L0_OFFSET], %l0
        ldd     [%sp + L2_OFFSET], %l2
        ldd     [%sp + L4_OFFSET], %l4
        ldd     [%sp + L6_OFFSET], %l6

        save                            ! Come back to the trap window
        save                            ! where return address is located

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

        .size underflow, . - underflow
        .type flush_windows,#function
flush_windows:
        /* On entry, %l1 and l2 have %pc and %npc respectively,
           so use l3 .. l6 for saving all the global registers we use. */

        mov     %g1, %l3                ! Preserve globals we'll use
        mov     %psr, %g1               ! Make a copy of the psr
        mov     %g2, %l4
        mov     %g3, %l5
        mov     %g4, %l6

        restore                         ! enter previous frame, the one
                                        ! that called the trap. It cannot
                                        ! trap.

        /* Flush all windows with valid contents (including the current one).
           Do not flush windows that have not been used. */

        mov     %psr, %g2
        and     %g2, PSR_CWP_MASK, %g2          ! g2 = CWP

        rd      %wim, %g3                       ! g3 = wim

        mov     %g0, %wim                       ! We will save windows manually

        mov     1, %g4
        sll     %g4, %g2, %g4                   ! g4 = WIM mask for CW invalid

.Lflush_window:
        /* Invariant: %g2 is free, %g3 = WIM, %g4 = WIM mask for current window

        /* Save the current window on the stack */

        std     %l0, [%sp + L0_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %l2, [%sp + L2_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %l4, [%sp + L4_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %l6, [%sp + L6_OFFSET]

        NOP_FIX_GRLIB_TN_0009
        std     %i0, [%sp + I0_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %i2, [%sp + I2_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %i4, [%sp + I4_OFFSET]
        NOP_FIX_GRLIB_TN_0009
        std     %i6, [%sp + I6_OFFSET]

        sll     %g4, 1, %g2                     ! rotate the CW mask left 1
        srl     %g4, NUMBER_OF_REGISTER_WINDOWS - 1, %g4
        or      %g4, %g2, %g4                   ! g4 = CW mask if we do one
                                                ! restore
        and     %g4, 0xFF, %g4                  ! Take only the least
                                                ! significant byte

        restore                                 ! back one window

        /* If we have already arrived to the invalid window then stop.
           Otherwise, continue flushing windows. */

        cmp     %g3, %g4                        ! Any windows to flush?
        bne     .Lflush_window                  ! No, then continue
         nop

.Lall_flushed:

        mov     %g1, %psr                       ! Restore %psr
        nop                                     ! The three delay slots are not
        nop                                     ! used, as writing even the icc
        nop                                     ! field is not allowed here

        /* Set the appropriate wim, which is the one that invalidates
           the window preceding the trapped window. */

        and     %g1, PSR_CWP_MASK, %g1          ! %g1 <- CWP
        add     %g1, 2, %g1                     ! Add 2 to CWP

        mov     1, %l7
        sll     %l7, %g1, %l7                   ! WIM invalidating the window
                                                ! preceeding the trapped one

        mov     %l3, %g1                        ! Restore global registers
        mov     %l4, %g2
        mov     %l5, %g3
        mov     %l6, %g4

        srl     %l7, NUMBER_OF_REGISTER_WINDOWS, %l6 ! do modulo
        wr      %l6, %l7, %wim
        nop
        nop
        nop

        mov %l2, %l1
        add %l2, 4, %l2

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

        .size flush_windows, . - flush_windows

/* Routine that masks external interrupts that are lower or equal to the
   value specified in %i0. */

mask_interrupt_level:
        sll     %i0, 8, %l4                     ! Put argument in PIL place
        andn    %l0, PSR_PIL_MASK, %l0          ! Erase the PIL field of PSR
        wr      %l0, %l4, %psr                  ! Write PSR with passed PIL
        nop
        nop
        nop

        mov %l2, %l1
        add %l2, 4, %l2

        ! %l1 is PC and %l2 is nPC
        b __gnat_return_from_trap
         nop

        .size mask_interrupt_level, . - mask_interrupt_level


/* Routine to return from trap, potentially with handling of Leon3 errata.
   %l1 must be set to the PC after return, and %l2 to nPC after return. */

        .global __gnat_return_from_trap
        .type __gnat_return_from_trap,#function
__gnat_return_from_trap:

#if defined(LEON3)
/* Comment the #define below to disable the handling of Leon3 errata
   GRLIB-TN-0018 */
#define LEON3_ERRATA_GRLIB_TN_0018
#endif

#if defined (LEON3_ERRATA_GRLIB_TN_0018)

        /* LEON3FT RETT Restart Errata (GRLIB-TN-0018) workaround #1 */
        mov %psr, %l0            ! save condition codes
1:      lda [%g0] 2, %l3         ! read cache control register
        srl %l3, 15, %l4         ! check bit 15 if flush in progress
        andcc %l4, 1, %g0
        bne 1b                   ! loop back if flushing
        ! let following instruction go into the delay slot
        andn %l3, 3, %l4         ! mask out DCS field
        ! align cache line boundary at this point for optimum performance
        sta %l4, [%g0] 2         ! write to disable Icache
        mov %l0, %psr            ! delay + restore condition codes
        or %l1, %l1, %l1         ! delay + catch rf parity error on l1
        or %l2, %l2, %l2         ! delay + catch rf parity error on l2
        sta %l3, [%g0] 2         ! write to re-enable Icache after rett
        nop                      ! delay to ensure first insn after gets cached
        jmp %l1
        rett %l2
#else
        jmpl    %l1, %g0         ! PC
        rett    %l2              ! nPC
#endif
        .size __gnat_return_from_trap, . - __gnat_return_from_trap

        .section ".rodata"
        .align  8
double_zero:
        .double 0.0
        .size double_zero, . - double_zero

        /* There are symbols which are only used when tasking is used
           (fp_context_switch and common_handler) or for multiprocessor targets
           (slave_stack_table and __gnat_initialize_slave). These are
           defined as weak symbols so that they will be replaced by the actual
           variables when using the Ravenscar run times and the multiprocessor
           support. */

        .weak   fp_context_switch
        .weak   common_handler

#if defined (LEON3)
        .weak   __gnat_idle_stack_table
        .weak   __gnat_initialize_slave
#endif
