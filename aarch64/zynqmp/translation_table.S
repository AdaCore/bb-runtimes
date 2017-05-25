/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                                  C R T 0                                 *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *                          Copyright (C) 2017 AdaCore                      *
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

/* This file is specific to the Cortex-A53 for UltraZed board.  */

.globl  MMUTableL0
.globl  MMUTableL1
.globl  MMUTableL2

.set reserved,	0x0 					/* Fault*/
.set Memory,	0x405 | (3 << 8) | (0x0)		/* normal writeback write allocate inner shared read write */
.set Device,	0x409 | (1 << 53)| (1 << 54) |(0x0)	/* strongly ordered read write non executable*/
.set DDR_0_START, 0
.set DDR_0_SIZE, 0x80000000
.set DDR_1_REG, 0

.section .mmu_tbl0,"a"

MMUTableL0:

.set SECT, MMUTableL1		/* 0x0000_0000 -  0x7F_FFFF_FFFF */
.8byte	SECT + 0x3
.set SECT, MMUTableL1+0x1000	/* 0x80_0000_0000 - 0xFF_FFFF_FFFF */
.8byte	SECT + 0x3

	.section .mmu_tbl1,"a"

MMUTableL1:

.set SECT, MMUTableL2		/* 0x0000_0000 - 0x3FFF_FFFF */
.8byte	SECT + 0x3		/* 1GB DDR */

.rept	0x3			/* 0x4000_0000 - 0xFFFF_FFFF */
.set SECT, SECT + 0x1000	/*1GB DDR, 1GB PL, 2GB other devices n memory */
.8byte	SECT + 0x3
.endr

.set SECT,0x100000000
.rept	0xC			/* 0x0001_0000_0000 - 0x0003_FFFF_FFFF */
.8byte	SECT + reserved		/* 12GB Reserved */
.set SECT, SECT + 0x40000000
.endr

.rept	0x10			/* 0x0004_0000_0000 - 0x0007_FFFF_FFFF */
.8byte	SECT + Device		/* 8GB PL, 8GB PCIe */
.set SECT, SECT + 0x40000000
.endr


.set UNDEF_1_REG, 0x20 - DDR_1_REG

.rept	DDR_1_REG			/* DDR based on size in hdf*/
.8byte	SECT + Memory
.set	SECT, SECT+0x40000000
.endr

.rept	UNDEF_1_REG		/* reserved for region where ddr is absent */
.8byte	SECT + reserved
.set	SECT, SECT+0x40000000
.endr

.rept	0x1C0			/* 0x0010_0000_0000 - 0x007F_FFFF_FFFF */
.8byte	SECT + Device		/* 448 GB PL */
.set SECT, SECT + 0x40000000
.endr


.rept	0x100			/* 0x0080_0000_0000 - 0x00BF_FFFF_FFFF */
.8byte	SECT + Device		/* 256GB PCIe */
.set SECT, SECT + 0x40000000
.endr


.rept	0x100			/* 0x00C0_0000_0000 - 0x00FF_FFFF_FFFF */
.8byte	SECT + reserved		/* 256GB reserved */
.set SECT, SECT + 0x40000000
.endr


.section .mmu_tbl2,"a"

MMUTableL2:

.set SECT, 0

.set DDR_0_REG, DDR_0_SIZE/0x200000
.set UNDEF_0_REG, 0x400 - DDR_0_REG

.rept	DDR_0_REG			/* DDR based on size in hdf*/
.8byte	SECT + Memory
.set	SECT, SECT+0x200000
.endr

.rept	UNDEF_0_REG		/* reserved for region where ddr is absent */
.8byte	SECT + reserved
.set	SECT, SECT+0x200000
.endr

.rept	0x0200			/* 0x8000_0000 - 0xBFFF_FFFF */
.8byte	SECT + Device		/* 1GB lower PL */
.set	SECT, SECT+0x200000
.endr

.rept	0x0100			/* 0xC000_0000 - 0xDFFF_FFFF */
.8byte	SECT + Device		/* 512MB QSPI */
.set	SECT, SECT+0x200000
.endr

.rept	0x080			/* 0xE000_0000 - 0xEFFF_FFFF */
.8byte	SECT + Device		/* 256MB lower PCIe */
.set	SECT, SECT+0x200000
.endr

.rept	0x040			/* 0xF000_0000 - 0xF7FF_FFFF */
.8byte	SECT + reserved		/* 128MB Reserved */
.set	SECT, SECT+0x200000
.endr

.rept	0x8			/* 0xF800_0000 - 0xF8FF_FFFF */
.8byte	SECT + Device		/* 16MB coresight */
.set	SECT, SECT+0x200000
.endr

/* 1MB RPU LLP is marked for 2MB region as the minimum block size in
   translation table is 2MB and adjacent 63MB reserved region is
   converted to 62MB */

.rept	0x1			/* 0xF900_0000 - 0xF91F_FFFF */
.8byte	SECT + Device		/* 2MB RPU low latency port */
.set	SECT, SECT+0x200000
.endr

.rept	0x1F			/* 0xF920_0000 - 0xFCFF_FFFF */
.8byte	SECT + reserved		/* 62MB Reserved */
.set	SECT, SECT+0x200000
.endr

.rept	0x8			/* 0xFD00_0000 - 0xFDFF_FFFF */
.8byte	SECT + Device		/* 16MB FPS */
.set	SECT, SECT+0x200000
.endr

.rept	0xE			/* 0xFE00_0000 -  0xFFBF_FFFF */
.8byte	SECT + Device		/* 28MB LPS */
.set	SECT, SECT+0x200000
.endr

				/* 0xFFC0_0000 - 0xFFDF_FFFF */
.8byte	SECT + Device 		/*2MB PMU/CSU */

.set	SECT, SECT+0x200000	/* 0xFFE0_0000 - 0xFFFF_FFFF*/
.8byte  SECT + Memory		/*2MB OCM/TCM*/

.end
/**
* @} End of "addtogroup a53_64_boot_code".
*/
