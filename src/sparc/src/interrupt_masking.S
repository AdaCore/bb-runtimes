/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                    I N T E R R U P T _ M A S K I N G                     *
 *                                                                          *
 *                             Assembly File                                *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2019 AdaCore                      *
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
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a SPARC specific implementation */

/*
   Interrupt enabling/disabling primitives.

   These routines are responsible for changing the value of the PIL field
   in the PSR for enabling and disabling interrupts (asynchronous traps).

   The reason why the PIL is changed within a trap is to guarantee atomic
   changes to the PSR. In a context switch, the content of the PSR may vary
   just after the context switch (the EF bit is reset to disable the FP
   unit and hence implement the lazy FP context switch). If we try to change
   the PIL by simply reading the current PSR, changing the PIL, and writing
   back the PSR, a context switch may arrive between reading and writing
   the PSR, and hence an incorrect PSR may be written (unexpected value in
   the EF bit).

   We use trap 8 to avoid traps 0x1n and 0x5n that cause unexpected clearing
   of interrupts on AT697F (see Atmel AT697F errata sheet 41003B-AERO-4).
 */

#include "sparc.h"

	.text
	.align 4

/* procedure disable_interrupts */

	.global disable_interrupts
	.type   disable_interrupts,@function
disable_interrupts:
	mov	0xF, %o0			! set maximum PIL
	ta	8				! call the trap handler in
						! charge of changing PIL
	retl					! return from leaf subroutine
	 nop

	.size disable_interrupts, . - disable_interrupts

/* procedure enable_interrupts (interrupt_level).
   The first and only argument (passed through o0) determines the interrupt
   level above which interrupts will be acknowledged. */

	.global enable_interrupts
	.type   enable_interrupts,@function
enable_interrupts:
	ta	8				! call the trap handler in
						! charge of changing PIL
	retl					! return from leaf subroutine
	 nop

	.size enable_interrupts, . - enable_interrupts
