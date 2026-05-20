/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                               S P A R C                                  *
 *                                                                          *
 *                             C Header File                                *
 *                                                                          *
 *      Copyright (C) 1999-2002, Universidad Politecnica de Madrid          *
 *             Copyright (C) 2003-2005, The European Space Agency           *
 *                    Copyright (C) 2003-2019, AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file defines sizes and offsets that allow saving and restoring the
   hardware status to/from the stack or the thread descriptor. */

/* This file is specific for SPARC (V7/V8) */

/* Get the id of the current cpu (always 0 on single processor) */

#if defined (LEON3)
# define CURRENT_CPU(reg)       \
        rd      %asr17, reg;    \
        srl     reg, 28, reg;
#else
# define CURRENT_CPU(reg)       \
        clr     reg;
#endif

/* Number of register windows that are available */

#define  NUMBER_OF_REGISTER_WINDOWS	8

/* Masks for the PSR */

#define	 PSR_ICC_MASK	 0x00F00000
#define	 PSR_EF_MASK	 0x00001000
#define  PSR_PIL_MASK	 0x00000F00
#define	 PSR_S_MASK	 0x00000080
#define	 PSR_PS_MASK	 0x00000040
#define	 PSR_ET_MASK	 0x00000020
#define	 PSR_CWP_MASK	 0x0000001F

/* Minimum stack frame size as defined by the SPARC ABI */

#define MINIMUM_STACK_FRAME_SIZE          0x60

/* Offsets for accessing to the different fields in the minimum stack
   frame defined by the SPARC ABI.  */

#define L0_OFFSET	0x00
#define	L1_OFFSET	0x04
#define L2_OFFSET	0x08
#define	L3_OFFSET	0x0C
#define L4_OFFSET	0x10
#define	L5_OFFSET	0x14
#define L6_OFFSET	0x18
#define	L7_OFFSET	0x1C

#define I0_OFFSET	0x20
#define	I1_OFFSET	0x24
#define I2_OFFSET	0x28
#define	I3_OFFSET	0x2C
#define I4_OFFSET	0x30
#define	I5_OFFSET	0x34
#define I6_OFFSET	0x38
#define	I7_OFFSET	0x3C

#define STRUCTURE_RETURN_ADDRESS_OFFSET   0x40

#define ARG0_OFFSET     0x44
#define ARG1_OFFSET     0x48
#define ARG2_OFFSET     0x4c
#define ARG3_OFFSET     0x50
#define ARG4_OFFSET     0x54
#define ARG5_OFFSET     0x58
#define PAD0_OFFSET     0x5c

/* Offsets for accessing to the different fields in the thread descriptor */

#define O0_OFFSET	0x00
#define O1_OFFSET	0x04
#define O2_OFFSET	0x08
#define O3_OFFSET	0x0C
#define O4_OFFSET	0x10
#define O5_OFFSET	0x14
#define O6_OFFSET	0x18
#define O7_OFFSET	0x1C

#define	PSR_OFFSET	0x20

#define G1_OFFSET	0x24
#define	G2_OFFSET	0x28
#define G3_OFFSET	0x2C
#define	G4_OFFSET	0x30
#define G5_OFFSET	0x34
#define	G6_OFFSET	0x38
#define G7_OFFSET	0x3C

#define Y_OFFSET        0x40

#define WIM_OFFSET	0x44

#define NWIN_OFFSET     0x48

#define FSR_OFFSET	0x4C

#define F0_F1_OFFSET	0x50
#define F2_F3_OFFSET	0x58
#define F4_F5_OFFSET	0x60
#define F6_F7_OFFSET	0x68
#define F8_F9_OFFSET	0x70
#define F10_F11_OFFSET	0x78
#define F12_F13_OFFSET	0x80
#define F14_F15_OFFSET	0x88
#define F16_F17_OFFSET	0x90
#define F18_F19_OFFSET	0x98
#define F20_F21_OFFSET	0xA0
#define F22_F23_OFFSET	0xA8
#define F24_F25_OFFSET	0xB0
#define F26_F27_OFFSET	0xB8
#define F28_F29_OFFSET	0xC0
#define F30_F31_OFFSET	0xC8

#define INT_NESTING_OFFSET 0xD0

#define BASE_CCR_OFFSET 0xD4
#define CCR_OFFSET      0xD8

/* Minimum stack frame size needed for handling interrupts */

#define	INTERRUPT_STACK_FRAME_SIZE	(0x50 + MINIMUM_STACK_FRAME_SIZE)

/* Offsets for accessing to the different fields in the stack frame defined
   for handling interrupts. */

#define ISF_PSR_OFFSET	0x00 + MINIMUM_STACK_FRAME_SIZE
#define ISF_PC_OFFSET	0x04 + MINIMUM_STACK_FRAME_SIZE
#define ISF_NPC_OFFSET	0x08 + MINIMUM_STACK_FRAME_SIZE
#define ISF_TT_OFFSET   0x0C + MINIMUM_STACK_FRAME_SIZE
#define	ISF_Y_OFFSET	0x10 + MINIMUM_STACK_FRAME_SIZE

#define ISF_G1_OFFSET	0x14 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_G2_OFFSET	0x18 + MINIMUM_STACK_FRAME_SIZE
#define ISF_G3_OFFSET	0x1C + MINIMUM_STACK_FRAME_SIZE
#define	ISF_G4_OFFSET	0x20 + MINIMUM_STACK_FRAME_SIZE
#define ISF_G5_OFFSET	0x24 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_G6_OFFSET	0x28 + MINIMUM_STACK_FRAME_SIZE
#define ISF_G7_OFFSET	0x2C + MINIMUM_STACK_FRAME_SIZE

#define ISF_I0_OFFSET	0x30 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_I1_OFFSET	0x34 + MINIMUM_STACK_FRAME_SIZE
#define ISF_I2_OFFSET	0x38 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_I3_OFFSET	0x3C + MINIMUM_STACK_FRAME_SIZE
#define ISF_I4_OFFSET	0x40 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_I5_OFFSET	0x44 + MINIMUM_STACK_FRAME_SIZE
#define ISF_I6_OFFSET	0x48 + MINIMUM_STACK_FRAME_SIZE
#define	ISF_I7_OFFSET	0x4C + MINIMUM_STACK_FRAME_SIZE

/* Masks for the trap type */

#define SYNCHRONOUS_TRAP_BIT_MASK     0x100

/* Avoid back to back write by inserting a nop.  Workaround to GRLIB TN 0009.
   See http://www.gaisler.com/doc/antn/GRLIB-AN-0009.pdf  */
#ifdef FIX_UT699
# define NOP_FIX_GRLIB_TN_0009 nop
#else
# define NOP_FIX_GRLIB_TN_0009
#endif
