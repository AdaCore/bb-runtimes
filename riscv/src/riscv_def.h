/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                               S P A R C                                  *
 *                                                                          *
 *                             C Header File                                *
 *                                                                          *
 *                       Copyright (C) 2019, AdaCore                        *
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

#ifndef _RISCV_DEF_H_
#define _RISCV_DEF_H_

#if __riscv_xlen == 64
# define LREG ld
# define SREG sd
# define REGBYTES 8

#elif __riscv_xlen == 32
# define LREG lw
# define SREG sw
# define REGBYTES 4

# ifdef __riscv_float_abi_double
#  error Double-Precision Floating-Point on 32-bit CPU not supported
# endif

#else
# error XLEN value not supported
#endif

#ifndef __riscv_float_abi_soft
# if __riscv_flen == 64
#  define FLREG fld
#  define FSREG fsd
# elif __riscv_flen == 32
#  define FLREG flw
#  define FSREG fsw
# else
#  error Invalid FLEN value
# endif
#endif

#ifdef __riscv_float_abi_soft
# define FRAME_SIZE (32*REGBYTES)
#else
# define FRAME_SIZE (64*REGBYTES) /* 32 integer + 32 float registers */
#endif

#define MSTATUS_MIE         0x00000008
#define MSTATUS_MPIE        0x00000080
#define MSTATUS_MPP         0x00001800
#define MSTATUS_FS          0x00006000

#endif /* ! _RISCV_DEF_H_ */
